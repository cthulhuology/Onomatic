%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http.erl
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(http).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-include("onomatic.hrl").

-export([directive/1, headers/1, content/1, get/2, find/2, headers_to_bin/1, has_header/1, transfer_encoding/1 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type header() :: list({ string() , string() }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:has_header finds the boundary between the header and the body of the request

-spec has_header(binary()) -> {number(),number()}.

has_header(Data) ->
	binary:match(Data,<<13,10,13,10>>).

-ifdef(TEST).
has_header_test() ->
	?assert({14,4} == has_header(<<"GET / HTTP/1.1\r\n\r\n">>)),
	?assertNot({14,4} == has_header(<<"GET /HTTP/1.1\r\n">>)).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:directive splits out each of the 3 elements [ method, path, protocol ]

-spec directive(binary()) -> list(binary()). 

directive(Bin) ->
	[Line|_] = binary:split(Bin,<<13,10>>),
	binary:split(Line,<<" ">>,[global]).

-ifdef(TEST).
directive_test() ->
	?assert([<<"GET">>,<<"/foo">>,<<"HTTP/1.1">>] == directive(<<"GET /foo HTTP/1.1\r\n">>)).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:headers breaks out a list of [ { key , value  } ] property lists 

-spec headers(binary()) -> header().

headers(Bin) ->
	{[_|Lines],_} = lists:splitwith(fun(A) -> A =/= <<>> end, binary:split(Bin,<<13,10>>,[ global ])),
	lists:map( fun([X,Y]) -> { string:to_lower(binary:bin_to_list(X)), binary:bin_to_list(Y) } end, 
		lists:map(fun (B) -> binary:split(B,<<": ">>) end, Lines)).

-ifdef(TEST).
headers_test() ->
	?assert([ { "content-type" , "text/html" }, { "content-length", "11" } ] ==
		headers(<<"GET / HTTP/1.1\r\nContent-Type: text/html\r\nContent-Length: 11\r\n\r\n">>)).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:get looks up the given header string in the binary request

-spec get(string(),binary()) -> string().

get(Key,Bin) ->
	proplists:get_value(string:to_lower(Key),headers(Bin)).

-ifdef(TEST).
get_test() ->
	?assert( "text/html" ==
		get("Content-Type",<<"GET / HTTP/1.1\r\nContent-Type: text/html\r\nContent-Length: 11\r\n\r\n">>)),
	?assert( "11" ==
		get("Content-Length",<<"GET / HTTP/1.1\r\nContent-Type: text/html\r\nContent-Length: 11\r\n\r\n">>)).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:find looks up the header value in the headers

-spec find(header(),string()) -> string().

find(Key,Headers) ->
	proplists:get_value(string:to_lower(Key),Headers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:transfer_encoding detects whether or not the data is chunked

-spec transfer_encoding(header()) -> term().

transfer_encoding(Header) ->
	case find("transfer-encoding",Header) of
		"chunked" -> chunked;
		_ -> identity
	end.

-ifdef(TEST).	
transfer_encoding_test() ->
	?assert(transfer_encoding(headers(<<"GET / HTTP/1.1\r\nContent-Length: 11\r\n\r\n">>)) =:= identity),
	?assert(transfer_encoding(headers(<<"GET / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n">>)) =:= chunked).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http:content_length returns the content length if any, 0 if none (ie. chunked)

-spec content_length(header()) -> non_neg_integer().

content_length(Header) ->
	case Length = find("content-length", Header) of
		undefined -> 0;
		_ -> list_to_integer(Length)
	end.

-ifdef(TEST).
content_length_test() ->
	?assert(11 =:= content_length(headers(<<"GET / HTTP/1.1\r\nContent-Type: text/html\r\nContent-Length: 11\r\n\r\n">>))),
	?assert(0 =:= content_length(headers(<<"GET / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n">>))).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hex returns the hex value of a list of bytes

-spec hex(list(byte())) -> non_neg_integer().

hex(L) -> hex(L,0).

hex([],Acc) -> Acc;
hex([H|T],Acc) when H >= $0, H =< $9 ->
	hex(T,Acc*16 + (H - $0));
hex([H|T],Acc) when H >= $a, H =< $f ->
	hex(T,Acc*16 + (H - $a)+10);
hex([H|T],Acc) when H >= $A, H =< $F ->
	hex(T,Acc*16 + (H - $A)+10).

-ifdef(TEST).
hex_test() ->
	?assert(hex([]) =:= 0),
	?assert(hex([$a]) =:= 10),
	?assert(hex([$a,$b]) =:= 171),
	?assert(hex([$a,$b,$0]) =:= 2736).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% chunked_content returns

chunked_content(Bin) ->
	chunked_content(Bin,<<>>).

chunked_content(Bin,Content) ->
	[ ChunkSize, Data ] = binary:split(Bin,<<13,10>>),
	io:format("Chunk ~p\n", [ChunkSize ]),
	ChunkLength = hex(binary:bin_to_list(ChunkSize)),
	io:format("Chunk Length ~p\n", [ChunkLength ]),
	case ChunkLength of 
		0 -> { Content, Data };
		_ -> chunked_content(binary:part(Data,ChunkLength,size(Data)-ChunkLength),
				binary:list_to_bin([Content, binary:part(Data, 0, ChunkLength)])) 
	end.

content(Bin) ->
	[Header,Content] = binary:split(Bin,<<13,10,13,10>>),
	case transfer_encoding(Header) of
		chunked -> chunked_content(Content);
		identity -> 
			ContentLength = content_length(Header),
			{ binary:part(Content,0,ContentLength), binary:part(Content, ContentLength, size(Content) - ContentLength) }
	end.

header_keys(Header) ->
	lists:map( fun ({ A, _ }) -> A end, Header ).

header_values(Header) ->
	lists:map( fun ({ _, B }) -> B end, Header ).

headers_to_bin(Header) ->
	binary:list_to_bin(lists:flatten(lists:zipwith( fun(A,B) -> [ A, ": ", B, "\r\n" ] end, 
		header_keys(Header), header_values(Header)))).

