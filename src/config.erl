%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%
%%	Onomatic is a HTTP request rewriting and  routing proxy
%%	The config manages configuration info.  This will change! 
%%
%% © 2011 David J. Goehrig <dave@nexttolast.com>
%%

-module(config, [ Args ]).

-export([ port/0, max_connections/0 ]).

-compile(export_all).

port() ->
	case proplists:get_value(port,Args) of
		undefined -> 8080;
		Any -> Any
	end.

max_connections() ->
	case proplists:get_value(max_connections, Args) of
		undefined -> 10;
		Any -> Any
	end.



