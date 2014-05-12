-module(server).
-export([start/0, start_cli/0, list/1, refresh/1, stop/1]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Starts the server.
start() ->
    process_flag(trap_exit, true),

    io:format("Starting database... "),
    Database = spawn_link(database, start, []),
    io:format("~w songs loaded!~n", [length(database:list(Database))]),

    InPort = 1340,
    OutPort = 1341,
    io:format("Starting listener... "),
    Listener = spawn_link(listener, start, [Database, InPort, OutPort]),
    io:format("ok!~n"),

    Server = spawn_link(fun() -> loop({Listener, Database}) end),
    io:format("Server started!~n"),
    Server.

%% Starts the server along with a command line interface.
start_cli() ->
    Server = start(),
    cli_start(Server).

%% Handles requests.
loop({Listener, Database}) ->
    receive
	list ->
	    Songs = database:list(Database),
	    io:format("~w songs:~n~n", [length(Songs)]),
	    [io:format("~s~n", [Song#song.filename]) || Song <- Songs],
	    loop({Listener, Database});
	refresh ->
	    io:format("Loading songs into database... "),
	    database:refresh(Database),
	    Songs = database:list(Database),
	    io:format("~w songs loaded!~n", [length(Songs)]),
	    loop({Listener, Database});
	stop ->
	    listener:stop(Listener),
	    receive
		{'EXIT', Listener, normal} ->
		    io:format("listener exited.~n")
	    after 5000 ->
		    erlang:error(timeout)
	    end,
	    %% database:stop(Database),
	    io:format("Server stopped.")
    end.

%% List the available songs on the server.
list(Server) ->
    Server ! list,
    ok.

%% Reloads the servers database.
refresh(Server) ->
    Server ! refresh,
    ok.

%% Stops the server.
stop(Server) ->
    Server ! stop,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			             CLI                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Starts the command line interface.
cli_start(Server) ->
    io:format("~n"),
    io:format("##########################~n"),
    io:format("# Welcome to the server! #~n"),
    io:format("##########################~n"),    
    print_help(),
    cli(Server).

%% Prints the available commands.
print_help() ->
    io:format("~n"),
    io:format("Available commands~n"),
    io:format("------------------~n"),
    io:format("list \t\tList available files~n"),
    io:format("refresh \tLoad files to database~n"),
    io:format("help \t\tShow this dialog~n"),
    io:format("stop \t\tStop the server~n"),
    io:format("~n").

%% Parses user input and interacts with the server.
cli(Server) ->
    case io:get_line("> ") of
	"list\n" ->
	    list(Server),
	    cli(Server);
	"refresh\n" ->
	    refresh(Server),
	    cli(Server);
	"help\n" ->
	    print_help(),
	    cli(Server);
	"stop\n" ->
	    stop(Server);
	"\n" ->
	    cli(Server);
	_ ->
	    io:format("Unkown command!~n"),
	    cli(Server)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

list_test() ->
    tbi.
