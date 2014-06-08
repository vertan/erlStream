%% @author Filip Hedman <hedman.filip@gmail.com>
%% @author Jeanette Castillo <jeanette.cas@hotmail.com>
%% @author Robert Kallgren <robertkallgren@gmail.com>
%% @author Oscar Mangard <oscarmangard@gmail.com>
%% @author Mikael Sernheim <mikael.sernheim@gmail.com>
%% @doc Contains utility functions used by the erlStream server program.

-module(utils).

-export([timestamp/0, socket_to_address/1]).

%% @doc Returns the current time in `[HH:MM:SS]' format.
-spec timestamp() -> string().

timestamp() ->
    {_Date, {H, M, S}} = calendar:local_time(),
    io_lib:format("[~2..0w:~2..0w:~2..0w]", [H, M, S]).

%% @doc Returns the peername of the given socket, or `"Unknown"' if it could not be determined.
-spec socket_to_address(Socket) -> Address when
      Socket :: port(),
      Address :: string().
 
socket_to_address(Socket) ->
    case inet:peername(Socket) of
	{ok, {Address, _Port}} ->
	    inet_parse:ntoa(Address);
	_ ->
	    "Unknown"
    end.
