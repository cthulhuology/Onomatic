%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% routes 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The routes provides basic routing functionality.
%%

-module(routes).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, add/2, lookup/1, copy/0 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	{ ok, #router{ routing_table = ets:new(?MODULE, Args) }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

add(Source,Destination) ->
	gen_server:call(?MODULE, { add, Source, Destination }).

lookup(Source) ->
	gen_server:call(?MODULE, { lookup, Source }).

copy() ->
	gen_server:call(?MODULE, copy ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

path(Source) ->
	string:tokens(Source,"/").

route(Table, []) ->
	case ets:lookup(Table,[]) of
		[{ _, Route }] -> Route;
		_ -> []
	end;
route(Table, Path) ->
	io:format("Looking up ~p ~n", [Path]),
	case ets:lookup(Table,Path) of
		[] -> route(Table,lists:sublist(Path,length(Path)-1));
		[{ _, Route }] -> Route;
		_ -> []
	end.

-ifdef(TEST).
route_test() ->
	Table = ets:new(?MODULE,[]),
	ets:insert(Table, { path("/foo/bar/"), [ "/foo", "ws://dloh.org/foo" ] }),
%%	?assertNot(route(Table,path("/baz/bar/baz")) =:= [ "/foo", "ws://dloh.org/foo" ]),
	?assert(route(Table,path("/foo/bar/baz")) =:= [ "/foo", "ws://dloh.org/foo" ]),
	?assert(route(Table,path("/foo/bar")) =:= [ "/foo", "ws://dloh.org/foo" ]),
	?assertNot(route(Table,path("/foo/baz")) =:= [ "/foo", "ws://dloh.org/foo" ]),
	?assertNot(route(Table,path("/foo")) =:= [ "/foo", "ws://dloh.org/foo" ]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses
handle_call({ add, Source, Destination }, _From, State) ->
	ets:insert(State#router.routing_table,{ string:tokens(Source,"/"), Destination }),
	{ reply, ok, State };
handle_call({ lookup, Source }, _From, State) ->
	{ reply, route(State#router.routing_table,string:tokens(Source,"/")), State };
handle_call(copy, _From, State) ->
	{ reply, ets:tab2list(State#router.routing_table), State };
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
