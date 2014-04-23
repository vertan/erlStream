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
    {ok,Lsocket} = gen_tcp:listen(8181, [{active, false}]),
    accept(Lsocket).


accept(Lsocket)	->
    io:format("Accepterar... ~n"),
    {ok,Socket} = gen_tcp:accept(Lsocket),
    loop(Socket),
    accept(Lsocket).

loop(Socket) ->
    case gen_tcp:recv(Socket,5) of
	{ok,Data} ->
	    io:format("Received: ~p~n",[Data]),
	    loop(Socket);
	{error,Reason} ->
	    io:format("~s~n", [Reason]),
	    ok
    end.

