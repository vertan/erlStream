%%%         File  : server.erl
%%%      Authors  : Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>,
%%%                 Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>,
%%%                 Mikael Sernheim <mikael.sernheim@gmail.com>
%%%  Description  : The content server handling all requests for the erlStream service.
%%%      Created  : 16 Apr 2014
%%%      Version  : 0.01

-module(server).
-vsn(0.01).
-export([start/0]).

start() ->
    io:format("Welcome to the server!~n"),
    {ok,Lsocket} = gen_tcp:listen(1337, [{active, false}]),
    accept(Lsocket).

accept(Lsocket)	->
    {ok,Socket} = gen_tcp:accept(Lsocket),
    loop(Socket),
    accept(Lsocket).

loop(Socket) ->
    case gen_tcp:recv(Socket,0) of
	{ok,Data} ->
	    io:format("Message received: ~p~n",[Data]),
	    loop(Socket);
	{error,closed} ->
	    ok;
	{error,Reason} ->
	    io:format("Error: ~s~n", [Reason]),
	    ok
    end.
