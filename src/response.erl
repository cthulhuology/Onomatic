%% response.erl
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(response).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-include("onomatic.hrl").

%% API
-export([parse/1, pack/1]).

-compile(export_all).

-import(http, [ headers_to_bin/1, directive/1, content/1, headers/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack(Response) ->
	binary:list_to_bin([Response#response.protocol, " ", 
		integer_to_list(Response#response.status), " ", 
		Response#response.message, 13, 10,
		headers_to_bin(Response#response.headers), 13, 10,
		Response#response.content]).

parse(Bin) ->
	Directive = directive(Bin),
	{Content,Remainder} = content(Bin),
	{ #response{ 
		protocol = protocol(Directive), 
		status = status(Directive), 
		message = message(Directive), 	
		headers = headers(Bin),
		content = Content
	}, Remainder }.

protocol([Protocol,_,_]) ->
	Protocol.

status([_,Status,_]) ->
	list_to_integer(binary:bin_to_list(Status)).

message([_,_,Message]) ->
	binary:bin_to_list(Message).

