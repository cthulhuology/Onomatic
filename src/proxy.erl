%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proxy 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The proxy manages routing client requests to the correct backend
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(proxy).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, peer/2, forward/1, copy/0 ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	{ ok, #proxy{ peer_table = ets:new(?MODULE, Args) }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

forward(Request) ->
	gen_server:call(?MODULE, { forward , Request }).

peer(IpAddress,Pid) ->
	gen_server:call(?MODULE, { peer, IpAddress, Pid }).

relay(Response) ->
	gen_server:call(?MODULE, { relay, Response }).

copy() ->
	gen_server:call(?MODULE, copy ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

respond_404() ->
	{ Response, _ } = response:parse(<<"HTTP/1.1 404 File Not Found\r\n\r\n">>),
	Response.

rewritten(Request) ->
	Request#request{ path = routes:lookup(Request#request.path) }.

cache(Request) ->
	Response = backend:relay(rewritten(Request)),
	caches:store(Request,Response),
	Response.

lookup(Request) ->
	case caches:find(Request) of
		{ ok, Response } -> Response;
		_ -> cache(Request)
	end.

process(Request) ->
	case permissions:permitted(Request#request.method, Request#request.path,"127.0.0.1") of 
		true -> lookup(Request);
		_ -> respond_404()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call({ forward, Request }, _From, State) ->
	Response = process(Request),
	{ reply, Response, State };	

handle_call({ peer, Pid }, _From, State) ->
	ets:insert(State#proxy.peer_table, { Pid }),
	{ reply, ok, State };

handle_call(copy, _From, State) ->
	{ reply, ets:tab2list(State#proxy.peer_table), State };

handle_call(stop, _From, State) ->
	{ stop, normal, stopped, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests with no response
handle_cast(_Msg, State) ->
	{ noreply, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles all out of band requests
handle_info(_Info, State) ->
	{ noreply, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Termination handler
terminate(_Reason,_State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code Change handler, mutate state before update
code_change(_Old, State, _Extra) ->
	{ ok, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
