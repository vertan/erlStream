%% @author Filip Hedman <hedman.filip@gmail.com>
%% @author Jeanette Castillo <jeanette.cas@hotmail.com>
%% @author Robert Kallgren <robertkallgren@gmail.com>
%% @author Oscar Mangard <oscarmangard@gmail.com>
%% @author Mikael Sernheim <mikael.sernheim@gmail.com>
%% @doc Listens for incoming requests from clients and performs the appropriate actions.

-module(listener).
-behavior(gen_server).

-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("song.hrl").

-record(state, {port, listensocket=null}).

-define(TCP_OPTIONS, [{packet, line}, {active, false}, {reuseaddr, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the listener. Refer to the official `gen_server' documentation for 
%% further information about the different return values.
-spec start(Port) -> Result when
      Port :: integer(),
      Result :: {ok, Pid} | ignore | {error, Reason},
      Pid :: pid(),
      Reason :: {already_started, ?MODULE} | term().

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%% @doc Stops the listener.
-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Server functions                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Port) ->
    process_flag(trap_exit, true),
    State = #state{port=Port},
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, ListenSocket} ->
	    {ok, spawn_accept(State#state{listensocket=ListenSocket})};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State) ->
    {noreply, spawn_accept(State)};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Listener: Unexpected message: ~p~n", [Info]),
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
%%                            Internal functions                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spawn_accept(State = #state{listensocket=ListenSocket}) ->
    proc_lib:spawn(fun() -> accept(ListenSocket) end),
    State.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(?MODULE, {accepted, self()}),
    handle_request(Socket).

handle_request(Socket) ->
    Address = utils:socket_to_address(Socket),
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    [Command|Arguments] = message_to_list(Data),
	    case Command of
		"connect" ->
		    connect(Socket, Arguments),
		    handle_client(Socket);
		"play" ->
		    io:format("~s <~s>: ~s~n", [utils:timestamp(), Address, string:strip(Data, right, $\n)]),
		    play(Socket, Arguments)
	    end;
	{error, closed} ->
	    client_manager:disconnect(Socket);
	{error, Reason} ->
	    io:format("Listener recv error: ~s~n", [Reason])
    end.

connect(Socket, []) ->
    connect(Socket, ["Unknown"]);
connect(Socket, [Name|_]) ->
    client_manager:connect(Socket, Name),
    Songs = database:list(),
    SongInfo = [Song#song.title ++ ":" ++ Song#song.artist ++ ":"
		  ++ Song#song.album ++ ":" ++ integer_to_list(Song#song.duration) ++ "\n" || Song <- Songs],
    SongInfoApp = lists:append(SongInfo),
    gen_tcp:send(Socket, "connect:ok\n" ++  SongInfoApp ++ "end\n").

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    [Command|_Arguments] = message_to_list(Data),
	    case Command of
		"disconnect" ->
		    client_manager:disconnect(Socket);
		_ ->
		    handle_client(Socket)
	    end;
	{error, closed} ->
	    client_manager:disconnect(Socket)
    end.

play(_Socket, Arguments) when length(Arguments) =/= 2 ->
    ok;
play(Socket, Arguments) ->
    [Offset, Title] = Arguments,
    {StartMS, _StartRest} = string:to_integer(Offset),
    case database:play(Title, StartMS) of
	{ok, Data} ->
	    gen_tcp:send(Socket, "play:ok\n" ++ Data),
	    gen_tcp:close(Socket);
	{error, offset_too_high} ->
	    gen_tcp:send(Socket, "play:error:Offset too high\n"),
	    gen_tcp:close(Socket);
	{error, read_failed} ->
	    gen_tcp:send(Socket, "play:error:Read failed on server side\n"),
	    gen_tcp:close(Socket);
	{error, no_such_file} ->
	    gen_tcp:send(Socket, "play:error:Song does not exist\n"),
	    gen_tcp:close(Socket)
    end.

message_to_list(Message) ->    
    StrippedMessage = string:strip(Message, right, $\n),
    case string:tokens(StrippedMessage, ":") of
	[] ->
	    [""];
	List ->
	    List
    end.    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             EUnit Test Cases                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by listener:test()

start_test() ->
    tbi.
