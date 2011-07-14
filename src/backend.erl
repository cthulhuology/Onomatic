%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backend 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The backend manages queries the backend processes
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(backend).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, relay/1 ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

-define(LOG,error_logger:info_msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	Config = config:new(Args),
	{ ok, #backend{ config = Config } }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

relay(Request) ->
	gen_server:call(?MODULE, { relay, Request }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

respond_500() ->
	response:parse(<<"HTTP/1.1 500 Service Unavailable\r\n\r\n">>).

handle_content(Requestor, Pid,Bin) ->
	?LOG("Received data ~p", [ Bin ]),
	case http:has_header(Bin) of
		false -> 
			wait_response(Requestor, Pid);
		_ -> 
			Requestor ! response:parse(Bin)
	end.

wait_response(Requestor, Pid) ->
	receive
		{ sent, Pid } ->
			?LOG("Sent request to ~w",[Pid]),
			wait_response(Requestor, Pid);
		{ done, Pid, Bin } ->
			handle_content(Requestor,Pid,Bin);
		{ ok, Pid, Bin } ->
			handle_content(Requestor,Pid,Bin);
		{ error, Pid, Reason } ->
			error_logger:error_msg("Failed to process response ~w", [ Reason ]),
			Requestor ! respond_500()
	end.
	
remote(Requestor,Request) ->
	{ Host, Port } = request:destination(Request),
	{ connect, Pid } = sockets:connect(Host,Port),
	Bin = request:pack(Request),
	sockets:write(Pid,Bin),		
	wait_response(Requestor, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call({ relay, Request }, { From, _tag }, State) ->
	Pid = spawn_link(?MODULE,remote,[ From, Request ]),
	{ reply, { relay, Pid }, State };

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
