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

-include_lib("eunit/include/eunit.hrl").

%% @doc Listens for commands and sends back the appropriate data.
%%
%% === Example ===
%%
%% <div class="example">```
%% > utils:start().
%% Command received: "list"
%% '''
%% </div>
-spec start() -> ok.

start() ->
    io:format("Welcome to the server!~n"),
    {ok,InSocket} = gen_tcp:listen(1340, [{active, false}]),
    {ok,OutSocket} = gen_tcp:listen(1341, [{active, false}]),
    accept(InSocket, OutSocket).

accept(InSocket, OutSocket) ->
    {ok,InSocketOpen} = gen_tcp:accept(InSocket),
    {ok,OutSocketOpen} = gen_tcp:accept(OutSocket),
    loop(InSocketOpen, OutSocketOpen, ""),
    accept(InSocket, OutSocket).

loop(InSocket, OutSocket, Message) ->
    case gen_tcp:recv(InSocket,0) of
	{ok,Data} ->
       	    loop(InSocket, OutSocket, lists:append([Message | [Data]]));
	{error, closed} ->
	    io:format("Command received: ~p~n", [Message]),
	    ReturnMessage = parseCommand(string:tokens(Message, " ")),
	    gen_tcp:send(OutSocket, ReturnMessage),
	    gen_tcp:close(OutSocket);
	{error,Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.

parseCommand([]) ->
    "Please enter a command!";
parseCommand([Command | Arguments]) ->
    case Command of
	"list" ->
	    os:cmd("ls ../files");
	"play" ->
	    play(Arguments);
	_ ->
	    ["Unknown command '", Command, "'!"]
    end.
    
play([]) ->
    "No file given!";
play([File|_]) ->
    FilePath = lists:append("../files/", File),
    case file:read_file(FilePath) of
	{ok, Binary} ->
	    binary_to_list(Binary);
	{error, Reason} ->
	    "Could not open file!"
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

parseCommand_test() ->
    tbi.
