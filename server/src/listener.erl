-module(listener).
-export([start/3, stop/1]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Starts the listener.
start(Database, InPort, OutPort) ->
    %% TODO: Take care of other return types.
    {ok,InSocket} = gen_tcp:listen(InPort, [{active, false}, {reuseaddr, true}]),
    {ok,OutSocket} = gen_tcp:listen(OutPort, [{active, false}, {reuseaddr, true}]),
    %% accept(InSocket, OutSocket).
    loop({Database, [], InSocket, OutSocket}).

%% Handles requests.
loop({Database, Workers, InSocket, OutSocket}) ->
    {ok,InSocketOpen} = gen_tcp:accept(InSocket),
    {ok,OutSocketOpen} = gen_tcp:accept(OutSocket),
    listen(Database, InSocketOpen, OutSocketOpen),
    loop({Database, Workers, InSocket, OutSocket}).

%% Listens for requests from clients
listen(Database, InSocket, OutSocket) ->
    listen(Database, InSocket, OutSocket, "").

listen(Database, InSocket, OutSocket, Message) ->
    case gen_tcp:recv(InSocket,0) of
	{ok,Data} ->
       	    listen(Database, InSocket, OutSocket, lists:append([Message | [Data]]));
	{error, closed} ->
	    io:format("Command received: ~p~n", [Message]),
	    spawn(fun() -> worker(Database, OutSocket, string:tokens(Message, " ")) end);
	{error, Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.

%% Stops the listener.
stop(Listener) ->
    %% [exit(Worker, shutdown) || Worker <- Workers],
    exit(Listener, shutdown).

%% Sends data through
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
