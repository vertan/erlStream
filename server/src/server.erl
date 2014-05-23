%% @author Filip Hedman (hedman.filip@gmail.com), Jeanette Castillo (jeanette.cas@hotmail.com), Robert Kallgren (robertkallgren@gmail.com), Oscar Mangard (oscarmangard@gmail.com), Mikael Sernheim (mikael.sernheim@gmail.com)
%% @doc Main code which handles requests

-module(server).
-behavior(gen_server).

-export([start/0, start_cli/0, list/0, refresh/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("song.hrl").

-record(state, {listener}).

%% @doc Initializes and starts actors for server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start() -> {ok, pid()}.

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the server along with a command line interface.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec start_cli() -> ok.

start_cli() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    cli_start().

%% @doc Returns a string with the filenames of the available songs on the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec list() -> ok.

list() ->
    Songs = gen_server:call(server, list),
    io:format("~p songs:~n~n", [length(Songs)]),
    [io:format("~s~n", [Song#song.filename]) || Song <- Songs],
    io:format("~n").

%% @doc Reloads the server's database.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec refresh() -> ok.

refresh() ->
    io:format("Loading songs into database... "),
    Songs = gen_server:call(server, refresh),
    io:format("~p songs loaded!~n", [length(Songs)]).

%% @doc Stops the server.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec stop() -> ok.
 
stop() ->
    io:format("Stopping server... "),
    gen_server:cast(server, stop),
    io:format("server stopped.~n").


%%%%%%%%%%%%%%%%%%%%%%
%% Server functions %%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    process_flag(trap_exit, true),

    io:format("Starting database... "),
    database:start("../files"),
    io:format("~w songs loaded!~n", [length(database:list())]),

    Port = 1340,
    io:format("Starting listener... "),
    Listener = spawn_link(listener, start, [Port]),
    io:format("ok!~n"),

    io:format("Server started!~n"),
    {ok, #state{listener=Listener}}.

handle_call(list, _From, State) ->
    Songs = database:list(),
    {reply, Songs, State};
handle_call(refresh, _From, State) ->
    database:refresh(),
    Songs = database:list(),
    {reply, Songs, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Server: Unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(shutdown, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			             CLI                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the command line interface.
%%
%% === Example ===
%%<div class="example">'''
%%'''
%%</div>
-spec cli_start() -> ok.

cli_start() ->
    io:format("~n"),
    io:format("##########################~n"),
    io:format("# Welcome to the server! #~n"),
    io:format("##########################~n"),    
    print_help(),
    cli().

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
-spec cli() -> ok.
 
cli() ->
    case io:get_line("> ") of
	"ls\n" ->
	    list(),
	    cli();
	"refresh\n" ->
	    refresh(),
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
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

list_test() ->
    tbi.
