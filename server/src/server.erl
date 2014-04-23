%%%         File  : server.erl
%%%      Authors  : Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>,
%%%                 Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>,
%%%                 Mikael Sernheim <mikael.sernheim@gmail.com>
%%%  Description  : The content server handling all requests for the erlStream service.
%%%      Created  : 16 Apr 2014
%%%      Version  : 0.01

-module(server).
-vsn(0.01).
-import(gen_tcp).
-export([start/0]).

start() ->
    io:format("Welcome to the server!~n"),
    {ok,Lsocket} = gen_tcp:listen(8181, [binary, {active, once}]),
    accept(Lsocket).


accept(Lsocket)	->
    io:format("accepterar ~n"),
    %{ok,Socket} = gen_tcp:accept(Lsocket),
    {error,Reason} = gen_tcp:accept(Lsocket),
    io:format("ERROR: ~s", [Reason]).
    %spawn(fun() -> loop(Socket) end),
    %loop(Socket),
    %accept(Lsocket).
	
loop(Socket) ->
    io:format("Nu loopas det"),
    case gen_tcp:recv(Socket,0) of
	{ok,Data} ->
	    io:format("HEJ"),
	    io:format("Test: ~s",[Data]),
	    loop(Socket);
	{error,_} ->
	    ok
    end.
	
