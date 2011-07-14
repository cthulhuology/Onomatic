%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sockets 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The sockets manages client and backend communications
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(sockets).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright(<<"© 2011 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, connect/2, listen/1, accept/0, read/1, read/2, write/2, peer/1, connections/0, close/1 ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

-define(LOG,error_logger:info_msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(_Args) ->
	process_flag(trap_exit, true),
	?LOG("~p starting~n",[?MODULE]),
	{ ok, #sockets{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

connect(Host,Port) ->
	gen_server:call(?MODULE, { connect, Host, Port }).

listen(Port) ->
	gen_server:call(?MODULE, { listen, Port }).

accept() ->
	gen_server:cast(?MODULE, { accept, self() }).

read(Pid) ->
	read(Pid,<<>>).

read(Pid,Data) ->
	gen_server:cast(?MODULE, { read, self(), Pid, Data }).

write(Pid,Data) ->
	gen_server:cast(?MODULE, { write, self(), Pid, Data }).

peer(Pid) ->
	gen_server:cast(?MODULE, { peer, Pid, self() }).

close(Pid) ->
	gen_server:cast(?MODULE, { close, self(), Pid }).

connections() ->
	gen_server:call(?MODULE, connections ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

receive_loop(Consumer, Socket, Data) ->
	?LOG("Socket Server looping~n"),
	ok = inet:setopts(Socket, [{ active, once }]),
	receive
		% Process incoming data, and notify consumer
		{ tcp, Socket, Bin } ->
			?LOG("[SS ~w] Received data ~p for consumer ~w", [ self(), binary:list_to_bin([ Data, Bin ]), Consumer ]),
			Consumer ! { ok, self(), binary:list_to_bin([ Data, Bin ]) },
			?LOG("[SS ~w] Relayed data to consumer ~w", [ self(), Consumer ]),
			receive_loop(Consumer,Socket, binary:list_to_bin([ Data, Bin ]));
		% Process socket disconnects and notify consumer
		{ tcp_closed, Socket } ->
			?LOG("[SS ~w] Connection closed ~w ~n", [ self(), inet:peername(Socket) ]),
			Consumer ! { done, self(), Data };
		{ close, Requestor } ->
			gen_tcp:close(Socket),
			Requestor ! { done, self(), Data };
		% Process consumer change
		{ read, NewConsumer, Data } ->
			?LOG("[SS ~w] Consumer changed to ~w~n", [ self(), NewConsumer ]),
			NewConsumer ! { ok, self() },
			receive_loop(NewConsumer,Socket,Data);
		% Process producer write directives
		{ write, Provider, Message } ->
			?LOG("[SS ~w] Provider ~w writing ~w~n", [ self(),  Provider, Message ]),
			send_data(Provider,Socket,Message),
			receive_loop(Consumer,Socket,Data);
		% Process requests for information about our peer
		{ peer, Requestor  } ->
			?LOG("[SS ~w] Peer request from ~p for ~w ~n", [ self(), Requestor, inet:peername(Socket) ]),
			Requestor ! { peer, inet:peername(Socket)},
			receive_loop(Consumer,Socket,Data);
		% Handle any additional spurrious errors passing back to consumer
		{ error, Reason } ->
			error_logger:error_msg("[SS ~w] client ~w experienced ~p~n", [ self(), inet:peername(Socket), Reason ]),
			Consumer ! { error, self(), Reason }
	end.

send_data(Provider, Socket,Data) ->
	case gen_tcp:send(Socket,Data) of
		ok -> 
			Provider ! { sent, self() };
		{ error, Reason } -> 
			Provider ! { error, self(), Reason }
	end.

listener(#sockets{ listen = ListenSocket }) ->
	ListenSocket.

connections(#sockets{ connections = Connections }) ->
	Connections.

listen_on(Port) ->
	case gen_tcp:listen( Port, [ binary, { packet, 0}, {active, true }, {keepalive, true }, {reuseaddr, true}]) of
		{ ok, Listen } -> Listen;
		Error -> error_logger:error_msg(Error)
	end.

accept_on(Consumer, Listener) ->
	{ ok,  Socket } = gen_tcp:accept(Listener),
	?LOG("Got connection from ~w ~n", [ inet:peername(Socket) ]),
	receive_loop(Consumer,Socket,<<>>).

remote(Consumer, Host, Port) ->
	{ ok, Socket } = gen_tcp:connect(Host,Port, [ binary, { packet, 0}, {active, once} ]),
	?LOG("Connected to ~w", [ inet:peername(Socket) ]),
	receive_loop(Consumer, Socket, <<>>).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call(stop, _From, State) ->
	{ stop, normal, stopped, State };

handle_call({ listen, Port }, _From, State) ->
	Socket = listen_on(Port),
	{ reply, { ok, Socket }, State#sockets{ port = Port, listen = Socket }};

handle_call(connections, _From, State) ->
	{ reply, { clients, connections(State) }, State };

handle_call( { connect, Host, Port }, { From, _tag }, State) ->
	Pid = spawn_link(?MODULE,remote,[ From, Host, Port ]),
	{ reply, { connect,  Pid }, State#sockets{ connections=[ Pid | connections(State) ] }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests with no response
handle_cast({ accept, Consumer }, State) ->
	Pid = spawn_link(?MODULE, accept_on, [ Consumer, listener(State) ]),
	{ noreply, State#sockets{ connections=[ Pid | connections(State) ] } };

handle_cast({ read, Consumer, Pid, Data }, State) ->
	Pid ! { read, Consumer, Data },
	{ noreply, State };	

handle_cast({ write, Producer, Pid, Data }, State) ->
	?LOG("Handling write request ~w ~w ~p", [Producer, Pid, Data]), 
	Pid ! { write, Producer, Data },
	{ noreply, State };

handle_cast({ close, Requestor, Pid }, State) ->
	Pid ! { close, Requestor },
	{ noreply, State };

handle_cast({ peer, Pid, Requestor }, State) ->
	Pid ! { peer, Requestor },
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
