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
    {ok,InSocket} = gen_tcp:listen(1340, [{active, false}]),
    {ok,OutSocket} = gen_tcp:listen(1341, [{active, false}]),
    accept(InSocket, OutSocket).

accept(InSocket, OutSocket) ->
    {ok,InSocketOpen} = gen_tcp:accept(InSocket),
    {ok,OutSocketOpen} = gen_tcp:accept(OutSocket),
    io:format("dags att loopa"),
    loop(InSocketOpen, OutSocketOpen, ""),
    accept(InSocket, OutSocket).

loop(InSocket, OutSocket, Message) ->
    case gen_tcp:recv(InSocket,0) of
	{ok,Data} ->
       	    loop(InSocket, OutSocket, lists:append([Message | [Data]]));
	{error,closed} ->
	    io:format("Message received: ~p~n",[Message]),
	    gen_tcp:send(OutSocket, "Tack!"),
	    gen_tcp:close(OutSocket),
	    ok;
	{error,Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.
