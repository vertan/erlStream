%% @author Filip Hedman (hedman.filip@gmail.com), Jeanette Castillo (jeanette.cas@hotmail.com), Robert Kallgren (robertkallgren@gmail.com), Oscar Mangard (oscarmangard@gmail.com), Mikael Sernheim (mikael.sernheim@gmail.com)
%% @doc Entry point to the server

-module(server).

-export([start/0, start_cli/0, clients/0, list/0, stop/0]).

-include_lib("eunit/include/eunit.hrl").
-include("song.hrl").
-include("client.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Initializes and starts actors for server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start() -> ok.

start() ->
    process_flag(trap_exit, true),

    Port = 1340,
    UpdateInterval = 10000,

    io:format("Starting database... "),
    database:start("../files", UpdateInterval),
    io:format("~w songs loaded!~n", [length(database:list())]),

    io:format("Starting client manager... "),
    client_manager:start(),
    io:format("ok!~n"),

    io:format("Starting listener... "),
    listener:start(Port),
    io:format("ok!~n"),

    io:format("Server started!~n").

%% @doc Starts the server along with a command line interface.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start_cli() -> ok.

start_cli() ->
    start(),
    cli_start().

%% @doc Prints a list of the connected clients.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec clients() -> ok.

clients() ->
    case client_manager:list() of
	[] ->
	    io:format("No clients connected.~n");
	Clients ->
	    io:format("~p client(s):~n~n", [length(Clients)]),
	    [io:format("~s (~s)~n", [Client#client.address, Client#client.name]) || Client <- Clients],
	    io:format("~n")
    end.

%% @doc Prints a list of the available songs on the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec list() -> ok.

list() ->
    case database:list() of
	[] ->
	    io:format("No songs available.~n");
	Songs ->
	    io:format("~p song(s):~n~n", [length(Songs)]),
	    [io:format("~s~n", [Song#song.title]) || Song <- Songs],
	    io:format("~n")
    end.

%% @doc Stops the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec stop() -> ok.
 
stop() ->
    io:format("Stopping server... "),
    client_manager:broadcast("exit:Shutdown"),
    listener:stop(),
    client_manager:stop(),
    database:stop(),
    io:format("server stopped.~n").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			             CLI                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cli_start() ->
    io:format("~n"),
    io:format("##########################~n"),
    io:format("# Welcome to the server! #~n"),
    io:format("##########################~n"),    
    print_help(),
    cli().

print_help() ->
    io:format("~n"),
    io:format("Available commands~n"),
    io:format("------------------~n"),
    io:format("clients \tList connected clients~n"),
    io:format("ls \t\tList available songs~n"),
    io:format("help \t\tShow this dialog~n"),
    io:format("stop \t\tStop the server~n"),
    io:format("~n").

cli() ->
    case io:get_line("> ") of
	"clients\n" ->
	    clients(),
	    cli();
	"ls\n" ->
	    list(),
	    cli();
	"help\n" ->
	    print_help(),
	    cli();
	"stop\n" ->
	    stop();
	"\n" ->
	    cli();
	_ ->
	    io:format("Unkown command. Type 'help' to see available options.~n"),
	    cli()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             EUnit Test Cases                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by server:test()

list_test() ->
    tbi.
