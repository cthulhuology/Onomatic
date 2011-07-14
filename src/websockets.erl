%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% websockets 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The websockets is a consumer for websocket requests
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(websockets).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright(<<"© 2011 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/1, stop/0, adopt/2, relay/1, clients/0, dump/0 ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

-define(LOG,error_logger:info_msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	Config = config:new(Args),	
	process_flag(trap_exit, true),
	?LOG("~p starting~n",[?MODULE]),
	{ ok, #websocket{ config = Config, clients = []}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
	gen_server:call(?MODULE, stop).

adopt(Pid,Request) ->
	gen_server:call(?MODULE, { adopt, Pid, Request }).

relay(Data) ->
	gen_server:call(?MODULE, { relay, Data }).

clients() ->
	gen_server:call(?MODULE, clients ).

dump() ->
	gen_server:call(?MODULE, dump ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

clients(#websocket{ clients = Clients }) ->
	Clients.

consume(Pid) ->
	sockets:read(Pid),
	consumer().

consumer() ->
	receive 
		{ ok, Pid, Data } -> 
			?LOG("[WS ~w] Got data ~w from ~w", [ self(), Data, Pid ]),
			consumer();
		{ send, Pid, Data } ->
			?LOG("[WS ~w] Sending data ~w to ~w", [ self(), Data, Pid ]),
			consumer();
		{ peer, Client } ->
			?LOG("[WS ~w] Connected to client ~w",[ self(), Client]),
			consumer();
		{ done, Pid, Data } ->
			?LOG("[WS ~w] Connection done ~w with pending data ~w",[ self(), Pid,Data]);
		{ error, Pid, Reason } ->
			?LOG("[WS ~w] Connection errored: ~w with reason ~w",[ self(), Pid,Reason]);
		Any -> 
			?LOG("[WS ~w] Consumer: ~w ~n", [ self(), Any ]),
			consumer()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call(stop, _From, State) ->
	{ stop, normal, stopped, State };

handle_call({ adopt, Pid, Request }, _From, State) ->
	?LOG("Adopting pid~p\n",[Pid]),
	?LOG("Got request ~w",[Request]),
	Client = spawn_link(?MODULE, consume, [Pid]),
	{ reply, { ok, Pid } , State#websocket{clients= [Client|clients(State)] }};
		
handle_call(clients, _From, State) ->
	{ reply, { clients, clients(State) }, State };

handle_call(dump, _From, State) ->
	{ reply, { dump, State }, State }.

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
	?LOG("~p stopping~n",[?MODULE]),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code Change handler, mutate state before update
code_change(_Old, State, _Extra) ->
	{ ok, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
