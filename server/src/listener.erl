%%%        File : listener.erl
%%%      Author : Filip Hedman <hedman.filip@gmail.com>, Jeanette Castillo <jeanette.cas@hotmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
%%% Description: Handles communication with clients

-module(listener).
-export([start/3, stop/1]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [{active, false}, {reuseaddr, true}]).

%% Starts the listener.
start(Database, InPort, OutPort) ->
    %% TODO: Take care of other return types.
    {ok,InSocket} = gen_tcp:listen(InPort, ?TCP_OPTIONS),
    {ok,OutSocket} = gen_tcp:listen(OutPort, ?TCP_OPTIONS),
    %% accept(InSocket, OutSocket).
    accept({Database, [], InSocket, OutSocket}).

%% Waits for clients to connect.
accept({Database, Workers, InSocket, OutSocket}) ->
    {ok,InSocketOpen} = gen_tcp:accept(InSocket),
    {ok,OutSocketOpen} = gen_tcp:accept(OutSocket),
    loop(Database, InSocketOpen, OutSocketOpen),
    accept({Database, Workers, InSocket, OutSocket}).

%% Reads and acts upon requests from clients.
loop(Database, InSocket, OutSocket) ->
    loop(Database, InSocket, OutSocket, "", "Unknown").

loop(Database, InSocket, OutSocket, Message, Address) ->
    case gen_tcp:recv(InSocket,0) of
	{ok,Data} ->
	    case inet:peername(InSocket) of
		{ok, {UpdatedAddress, _Port}} ->
		    loop(Database, InSocket, OutSocket, lists:append([Message | [Data]]), UpdatedAddress);
		{error, _} ->
		    loop(Database, InSocket, OutSocket, lists:append([Message | [Data]]), Address)
	    end;
	{error, closed} ->
	    io:format("Command received from ~s: ~s~n", [inet_parse:ntoa(Address), Message]),
	    spawn(fun() -> worker(Database, OutSocket, string:tokens(Message, " ")) end);
	{error, Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.

%% Stops the listener.
stop(Listener) ->
    %% [exit(Worker, shutdown) || Worker <- Workers],
    exit(Listener, shutdown).

%% Sends data.
send_data(OutSocket, Data) ->
    gen_tcp:send(OutSocket, Data),
    gen_tcp:close(OutSocket).

%% Parses the given command and sends the data back with the sockets.
worker(_, _, []) ->
    "Unknown command!";
worker(Database, OutSocket, [Command | Arguments]) ->
    case Command of
	"list" ->
	    Songs = database:list(Database),
	    SongTitles = [[Song#song.filename, "|", Song#song.title, "|", Song#song.artist, "|", Song#song.album, "|", integer_to_list(Song#song.duration), "\n"] || Song <- Songs],
	    SongTitlesApp = lists:append(SongTitles),
	    send_data(OutSocket, SongTitlesApp);
	"play" ->
	    case database:play(Database, Arguments) of
		{ok, Data} ->
		    send_data(OutSocket, Data);
		{error, Reason} ->
		    send_data(OutSocket, Reason)
	    end;
	_ ->
	    ["Unknown command '", Command, "'!"]
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

start_test() ->
    tbi.
