%% @author Filip Hedman (hedman.filip@gmail.com), Jeanette Castillo (jeanette.cas@hotmail.com), Robert Kallgren (robertkallgren@gmail.com), Oscar Mangard (oscarmangard@gmail.com), Mikael Sernheim (mikael.sernheim@gmail.com)
%% @doc Main code which handles requests

-module(server).
-export([start/0, start_cli/0, list/1, refresh/1, stop/1]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Initializes and starts actors for server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start() -> pid().

start() ->
    process_flag(trap_exit, true),

    io:format("Starting database... "),
    database:start("../files"),
    io:format("~w songs loaded!~n", [length(database:list())]),

    Port = 1340,
    io:format("Starting listener... "),
    Listener = spawn_link(listener, start, [Port]),
    io:format("ok!~n"),

    Server = spawn_link(fun() -> loop(Listener) end),
    io:format("Server started!~n"),
    Server.

%% @doc Starts the server along with a command line interface.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start_cli() -> ok.

start_cli() ->
    Server = start(),
    cli_start(Server).

%% @doc Handles requests
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
loop(Listener) ->
    receive
	{Pid, list} ->
	    Pid ! database:list(),
	    loop(Listener);
	{Pid, refresh} ->
	    database:refresh(),
	    Pid ! database:list(),
	    loop(Listener);
	{Pid, stop} ->
	    listener:stop(Listener),
	    %% receive
	    %% 	{'EXIT', Listener, normal} ->
	    %% 	    io:format("listener exited.~n")
	    %% after 5000 ->
	    %% 	    erlang:error(timeout)
	    %% end,
	    %% database:stop(Database),
	    Pid ! ok
    end.

%% @doc Returns a string with the filenames of the available songs on the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec list(Server) -> ok when
      Server :: pid().

list(Server) ->
    Server ! {self(), list},
    receive
	Songs ->
	    io:format("~p songs:~n~n", [length(Songs)]),
	    [io:format("~s~n", [Song#song.filename]) || Song <- Songs],
	    io:format("~n")
    end.

%% @doc Reloads the servers database.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec refresh(Server) -> ok when
      Server :: pid().

refresh(Server) ->
    Server ! {self(), refresh},
    io:format("Loading songs into database... "),
    receive
	Songs ->
	    io:format("~p songs loaded!~n", [length(Songs)])
    end.

%% @doc Stops the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec stop(Server) -> ok when
      Server :: pid().
 
stop(Server) ->
    Server ! {self(), stop},
    io:format("Stopping server... "),
    receive
	ok ->
	    io:format("server stopped.~n")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			             CLI                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the command line interface.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec cli_start(Server) -> ok when
      Server :: pid().

cli_start(Server) ->
    io:format("~n"),
    io:format("##########################~n"),
    io:format("# Welcome to the server! #~n"),
    io:format("##########################~n"),    
    print_help(),
    cli(Server).

%% @doc Prints the available commands.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec print_help() -> ok.

print_help() ->
    io:format("~n"),
    io:format("Available commands~n"),
    io:format("------------------~n"),
    io:format("ls \t\tList available songs~n"),
    io:format("refresh \tLoad files to database~n"),
    io:format("help \t\tShow this dialog~n"),
    io:format("stop \t\tStop the server~n"),
    io:format("~n").

%% @doc Parses user input and interacts with the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec cli(Server) -> ok when
      Server :: pid().
 
cli(Server) ->
    case io:get_line("> ") of
	"ls\n" ->
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
	    io:format("Unkown command. Type 'help' to see available options.~n"),
	    cli(Server)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

list_test() ->
    tbi.
