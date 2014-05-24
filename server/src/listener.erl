%%%        File : listener.erl
%%%      Author : Filip Hedman <hedman.filip@gmail.com>, Jeanette Castillo <jeanette.cas@hotmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
%%% Description: Handles communication with clients

-module(listener).
-behavior(gen_server).

-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {port, listensocket=null}).

-define(TCP_OPTIONS, [{packet, line}, {active, false}, {reuseaddr, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Starts the listener.
start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%% Stops the listener.
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
    loop(Socket).

%% Reads and acts upon requests from clients.
loop(Socket) ->
    Address = case inet:peername(Socket) of
		  {ok, {Addr, _Port}} ->
		      inet_parse:ntoa(Addr);
		  {error, _} ->
		      "Unknown"
	      end,
    case gen_tcp:recv(Socket,0) of
	{ok, Data} ->
	    Message = string:strip(Data, both, $\n),
	    io:format("~s Command received from ~s: ~s~n", [timestamp(), Address, Message]),
	    [Command|Arguments] = string:tokens(Message, " "),
	    case Command of
		"list" ->
		    worker(Socket, list),
		    loop(Socket);
		"play" ->
		    worker(Socket, {play, Arguments})
	    end;
	{error, closed} ->
	    io:format("~s ~s disconnected!~n", [timestamp(), Address]);
	{error, Reason} ->
	    io:format("Error: ~s~n", [Reason])
    end.

%% Sends data.
send_data(Socket, Data) ->
    gen_tcp:send(Socket, Data).

%% Sends a song and closes the socket.
send_song(Socket, Data) ->
    gen_tcp:send(Socket, Data),
    gen_tcp:close(Socket).

%% Does the given task and sends the data back through the socket.
worker(Socket, Command) ->
    case Command of
	list ->
	    Songs = database:list(),
	    SongTitles = [[Song#song.filename, "|", Song#song.title, "|", Song#song.artist, "|", Song#song.album, "|", integer_to_list(Song#song.duration), "\n"] || Song <- Songs],
	    SongTitlesApp = lists:append(SongTitles),
	    send_data(Socket, SongTitlesApp ++ "end\n");
	{play, Arguments} ->
	    [Offset|File] = Arguments,
	    Filename = string:join(File, " "),
	    {StartMS, _StartRest} = string:to_integer(Offset),
	    case database:play(Filename, StartMS) of
		{ok, Data} ->
		    send_song(Socket, Data);
		{error, Reason} ->
		    send_data(Socket, Reason)
	    end
    end.
    
timestamp() ->
    {_Date, {H, M, S}} = calendar:local_time(),
    io_lib:format("[~2..0w:~2..0w:~2..0w]", [H, M, S]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             EUnit Test Cases                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by listener:test()

start_test() ->
    tbi.
