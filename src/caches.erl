%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% caches 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The caches caches responses to avoid backend lookups
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(caches).
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, copy/0, store/2, find/1  ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	{ ok, #cache{ cache_table = ets:new(?MODULE, Args) }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

copy() ->
	gen_server:call(?MODULE, copy ).

store(Request,Response) ->
	gen_server:call(?MODULE, { store, Request, Response }).

find(Request) ->
	gen_server:call(?MODULE, { find, Request }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call(stop, _From, State) ->
	{ stop, normal, stopped, State };

handle_call(copy, _From, State) ->
	{ reply, ets:tab2list(State#cache.cache_table), State };

handle_call( { store, Request, Response }, _From, State ) ->
	ok = ets:insert(State#cache.cache_table, { Request#request.path, Response }),
	{reply, ok, State }; 

handle_call( { find, Request }, _From, State ) ->
	[{ _, Response }|_] = ets:lookup(Request#request.path),
	{ reply, Response, State }.

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
