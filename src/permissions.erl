%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% permissions 
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	This server provides access control list functionality
%%

-module(permissions).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-behavior(gen_server).

-include("onomatic.hrl").

%% API
-export([ start_link/0, stop/0, allow/3, deny/3, permitted/3, copy/0 ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup the 
init(Args) ->
	{ ok, #permissions{ permission_table = ets:new(?MODULE, Args) }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

allow(Method,Path,Host) ->
	gen_server:call(?MODULE, { allow, Method, Path, Host}). 

deny(Method,Path,Host) ->
	gen_server:call(?MODULE, { deny,  Method, Path, Host}). 

permitted(Method,Path,Host) ->
	gen_server:call(?MODULE, { is_permitted, Method, Path, Host }).

copy() ->
	gen_server:call(?MODULE, copy ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

add_permission([],Method,Host) ->
	case Method of
		get -> #acl{ get=[ Host ] };
		put -> #acl{ put=[ Host ] };
		post -> #acl{ post=[ Host ] };
		delete -> #acl{ delete=[ Host ] }
	end;
add_permission(Acl,Method,Host) ->
	[ { _, A }] = Acl,
	case Method of
		get -> A#acl{ get=lists:umerge( A#acl.get, [ Host ] ) };
		put -> A#acl{ put=lists:umerge( A#acl.put, [ Host ] ) };
		post -> A#acl{ post=lists:umerge( A#acl.post, [ Host ] ) };
		delete -> A#acl{ delete=lists:umerge( A#acl.delete, [ Host ] ) }
	end.


remove_permission([], _Method,_Host) ->
	[];
remove_permission(Acl,Method,Host) ->
	[ { _, A }] = Acl,
	case Method of
		get -> A#acl{ get=lists:subtract( A#acl.get, [ Host ] ) };
		put -> A#acl{ put=lists:subtract( A#acl.put, [ Host ] ) };
		post -> A#acl{ post=lists:subtract( A#acl.post, [ Host ] ) };
		delete -> A#acl{ delete=lists:subtract( A#acl.delete, [ Host ] ) }
	end.

permitted(Acl,Method) ->
	case Method of
		get -> Acl#acl.get;
		put -> Acl#acl.put;
		post -> Acl#acl.post;
		delete -> Acl#acl.delete
	end.
	
is_permitted([], _Method, _Host) ->
	true;
is_permitted(Acl,Method,Host) ->
	[{ _, A }] = Acl,
	Permitted = permitted(A,Method),
	Permitted =:= [] orelse lists:member(Host,Permitted).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handles incoming requests which expect responses
handle_call({ allow, Method, Path, Host }, _From, State) ->
	Acl = ets:lookup(State#permissions.permission_table,Path),
	io:format("Found ~p for path ~s ~n",[Acl,Path]),
	ets:insert(State#permissions.permission_table, { Path, add_permission(Acl,Method,Host) }),
	{ reply, ok, State };
handle_call({ deny, Method, Path, Host }, _From, State) ->
	Acl = ets:lookup(State#permissions.permission_table,Path),
	io:format("Found ~p for path ~s ~n",[Acl,Path]),
	ets:insert(State#permissions.permission_table, { Path, remove_permission(Acl,Method,Host) }),
	{ reply, ok, State };
handle_call({is_permitted, Method, Path, Host }, _From, State) ->
	Acl = ets:lookup(State#permissions.permission_table, Path),
	io:format("Found ~p for path ~s ~n",[Acl,Path]),
	{ reply, is_permitted(Acl,Method,Host), State };
handle_call(copy, _From, State) ->
	{ reply, ets:tab2list(State#permissions.permission_table), State };
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
