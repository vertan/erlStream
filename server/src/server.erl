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
	{ok,Lsocket} = gen_tcp:listen(1337, [binary]),
	accept(Lsocket).


accept(Lsocket)	->
	{ok,Socket} = gen_tcp:accept(Lsocket),
	spawn(fun() -> loop(Socket) end),
	accept(Lsocket).
	
loop(Socket) ->
	case gen_tcp:recv(Socket,0) of
		{ok,Data} -> io:format(Data),
			loop(Socket);
		{error,_} ->
			ok
	end.
	
