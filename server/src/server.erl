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
    {ok,Lsocket} = gen_tcp:listen(1339, [{active, false}]),
    accept(Lsocket).

accept(Lsocket)	->
    {ok,Socket} = gen_tcp:accept(Lsocket),
    loop(Socket, ""),
    accept(Lsocket).

loop(Socket, Message) ->
    case gen_tcp:recv(Socket,0) of
	{ok,Data} ->
       	    loop(Socket, lists:append([Message | [Data]]));
	{error,closed} ->
	    io:format("Message received: ~p~n",[Message]),
	    gen_tcp:send(Socket, "Tack!"),
	    ok;
	{error,Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.
