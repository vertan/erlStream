%%%        File : listener.erl
%%%      Author : Filip Hedman <hedman.filip@gmail.com>, Jeanette Castillo <jeanette.cas@hotmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
%%% Description: Handles communication with clients

-module(listener).
-export([start/2, stop/1]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [{packet, line}, {active, false}, {reuseaddr, true}]).

%% Starts the listener.
start(Database, Port) ->
    %% TODO: Take care of other return types.
    {ok,ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(Database, ListenSocket).

%% Waits for clients to connect.
accept(Database, ListenSocket) ->
    {ok,Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> loop(Database, Socket) end),
    accept(Database, ListenSocket).

%% Reads and acts upon requests from clients.
loop(Database, Socket) ->
    Address = case inet:peername(Socket) of
		  {ok, {Addr, _Port}} ->
		      inet_parse:ntoa(Addr);
		  {error, _} ->
		      "Unknown"
	      end,
    case gen_tcp:recv(Socket,0) of
	{ok,Data} ->
	    Message = string:strip(Data, both, $\n),
	    io:format("Command received from ~s: ~s~n", [Address, Message]),
	    [Command|Arguments] = string:tokens(Message, " "),
	    case Command of
		"list" ->
		    worker(Database, Socket, list),
		    loop(Database, Socket);
		"play" ->
		    worker(Database, Socket, {play, Arguments})
	    end;
	{error, closed} ->
	    io:format("~s disconnected!~n", [Address]);
	{error, Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.

%% Stops the listener.
stop(Listener) ->
    %% [exit(Worker, shutdown) || Worker <- Workers],
    exit(Listener, shutdown).

%% Sends data.
send_data(Socket, Data) ->
    gen_tcp:send(Socket, Data).

%% Sends a song and closes the socket.
send_song(Socket, Data) ->
    gen_tcp:send(Socket, Data),
    gen_tcp:close(Socket).

%% Does the given task and sends the data back through the socket.
worker(Database, Socket, Command) ->
    case Command of
	list ->
	    Songs = database:list(Database),
	    SongTitles = [[Song#song.filename, "|", Song#song.title, "|", Song#song.artist, "|", Song#song.album, "|", integer_to_list(Song#song.duration), "\n"] || Song <- Songs],
	    SongTitlesApp = lists:append(SongTitles),
	    send_data(Socket, SongTitlesApp ++ "end\n");
	{play, Arguments} ->
	    case database:play(Database, Arguments) of
		{ok, Data} ->
		    send_song(Socket, Data);
		{error, Reason} ->
		    send_data(Socket, Reason)
	    end
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

start_test() ->
    tbi.
