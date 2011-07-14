%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% frontend
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The frontend manages client requests
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(frontend).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright(<<"© 2011 David J. Goehrig"/utf8>>).
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, start_link/1, start/0, stop/0, forward/1, deliver/1, clients/0, dump/0 ]).

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
	websockets:start_link(Args),
	{ ok, Server } = sockets:start_link(),
	{ ok, Socket } = sockets:listen(Config:port()),
	{ ok, #frontend{ config = Config, server = Server, socket = Socket, clients = []}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start() ->
	gen_server:call(?MODULE, start).

stop() ->
	gen_server:call(?MODULE, stop).

forward(Request) ->
	gen_server:call(?MODULE, { forward, Request }).

deliver(Response) ->
	gen_server:call(?MODULE, { deliver, Response }).

clients() ->
	gen_server:call(?MODULE, clients ).

dump() ->
	gen_server:call(?MODULE, dump ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

clients(#frontend{ clients = Clients }) ->
	Clients.

orphan_websocket(Pid,Request) ->
	{ ok, Pid } = websockets:adopt(Pid,Request).

consume() ->
	sockets:accept(),
	consumer().

consumer() ->
	receive 
		{ ok, Pid, Data } -> 
			case http:has_header(Data) of 	
				false -> 
					?LOG("Got partial request ~w", [Data ]),
					consumer();
				_ -> { Request, Remainder }  = request:parse(Data),
					?LOG("Got request ~w ~n", [ Request ]),
					case proplists:get_value("upgrade",Request#request.headers) of
						"websocket" ->
							orphan_websocket(Pid,{Request,Remainder}),
							consume();
						_ -> ?LOG("Not upgrading")
					end,
					{ Response, _ } = response:parse(<<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\nContent-Type: text/plain\r\n\r\nHello World\n">>),
					Bin = response:pack(Response),
					?LOG("Wrting response ~w ~n", [ Bin ]),
					sockets:write(Pid, Bin),
					?LOG("Continuing"),
					consumer()
			end;
		{ peer, Client } ->
			?LOG("Connected to client ~w",Client),
			consumer();
		{ done, Pid, Data } ->
			?LOG("Connection done ~w with pending data ~w",Pid,Data),
			sockets:accept();
		{ error, Pid, Reason } ->
			?LOG("Connection errored: ~w with reason ~w",Pid,Reason),
			sockets:accept();
		Any -> 
			?LOG("Consumer: ~w ~n", [ Any ])
	end.

process_requests(MaxConns,MaxConns) ->
	{ started, MaxConns };
process_requests(I,MaxConns) when I <  MaxConns ->
	spawn(?MODULE,consume,[]),
	process_requests(I+1,MaxConns).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses

handle_call(start, _From, State) ->
	Config = State#frontend.config,
	Pid = spawn_link(?MODULE, process_requests, [0,Config:max_connections()]),
	{ reply, { ok }, State#frontend{ watcher = Pid } };

handle_call(stop, _From, State) ->
	{ stop, normal, stopped, State };

handle_call({ forward, Request }, _From, State) ->
	?LOG("Got request ~p\n",[Request]),
	{ reply, { ok, Request }, State };
		
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
