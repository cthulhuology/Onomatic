%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% http.erl
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(http).
-author({ "David J. Goehrig", "dave@nexttolast.com" }).
-copyright("© 2011 David J. Goehrig").
-include("onomatic.hrl").

-export([directive/1, headers/1, content/1, get/2, find/2, headers_to_bin/1, has_header/1 ]).

has_header(Data) ->
	binary:match(Data,<<13,10,13,10>>).

directive(Bin) ->
	[Line|_] = binary:split(Bin,<<13,10>>),
	binary:split(Line,<<" ">>,[global]).

headers(Bin) ->
	{[_|Lines],_} = lists:splitwith(fun(A) -> A =/= <<>> end, binary:split(Bin,<<13,10>>,[ global ])),
	lists:map( fun([X,Y]) -> { string:to_lower(binary:bin_to_list(X)), binary:bin_to_list(Y) } end, 
		lists:map(fun (B) -> binary:split(B,<<": ">>) end, Lines)).

get(Key,Bin) ->
	proplists:get_value(string:to_lower(Key),headers(Bin)).

find(Request,Key) ->
	proplists:get_value(string:to_lower(Key),Request#request.headers).


transfer_encoding(Header) ->
	case get("transfer-encoding",Header) of
		"chunked" -> chunked;
		_ -> identity
	end.

content_length(Header) ->
	case Length = get("content-length", Header) of
		undefined -> 0;
		_ -> list_to_integer(Length)
	end.

chunked_content(Bin) ->
	chunked_content(Bin,<<>>).

hex(L) -> hex(L,0).

hex([],Acc) -> Acc;
hex([H|T],Acc) when H >= $0, H =< $9 ->
	hex(T,Acc*16 + (H - $0));
hex([H|T],Acc) when H >= $a, H =< $f ->
	hex(T,Acc*16 + (H - $a)+10);
hex([H|T],Acc) when H >= $A, H =< $F ->
	hex(T,Acc*16 + (H - $A)+10).

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

