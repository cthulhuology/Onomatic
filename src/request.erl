%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% request.erl
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(request).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-include("onomatic.hrl").

%% API
-export([parse/1, pack/1, url/1, destination/1]).

-compile(export_all).
-import(http, [ headers_to_bin/1, directive/1, content/1, headers/1, find/2 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

url(Request) ->
	"http://" ++ find(Request,"Host") ++ Request#request.path.

destination(Request) ->
	HostPort = find(Request,"host"),	
	case string:str(HostPort,":") of
		0 -> { Host, Port } = { HostPort, "80" };
		_ -> [ Host, Port ] = string:tokens(HostPort,":")
	end,
	{ Host, list_to_integer(Port) }.

pack(Request) ->
	binary:list_to_bin([Request#request.method, " ", 
		Request#request.path, " ", 
		Request#request.protocol, 13, 10,
		headers_to_bin(Request#request.headers), 13, 10,
		Request#request.content]).

parse(Bin) ->
	Directive = directive(Bin),
	{ Content, Remainder } = content(Bin),
	{ #request{ 
		method = method(Directive), 
		path = path(Directive), 
		protocol = protocol(Directive), 	
		headers = headers(Bin),
		content = Content
		}, Remainder }. 

method([Method,_,_]) ->
	Method.

path([_,Path,_]) ->
	binary:bin_to_list(Path).

protocol([_,_,Protocol]) ->
	Protocol.

