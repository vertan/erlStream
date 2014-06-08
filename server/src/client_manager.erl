%% @author Filip Hedman <hedman.filip@gmail.com>
%% @author Jeanette Castillo <jeanette.cas@hotmail.com>
%% @author Robert Kallgren <robertkallgren@gmail.com>
%% @author Oscar Mangard <oscarmangard@gmail.com>
%% @author Mikael Sernheim <mikael.sernheim@gmail.com>
%% @doc Handles information about clients.

-module(client_manager).
-behavior(gen_server).

-export([start/0, list/0, connect/2, disconnect/1, broadcast/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the client manager. Refer to the official gen_server
%% documentation for further information about the different return values.
-spec start() -> Result when
      Result :: {ok, Pid} | ignore | {error, Reason},
      Pid :: pid(),
      Reason :: {already_started, ?MODULE} | term().

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns a list with a client record for each client in the manager.
-spec list() -> [Client] when
      Client :: #client{socket :: port(), address :: string(), name :: string()}.

list() ->
    gen_server:call(?MODULE, list).

%% @doc Creates and adds a new client with the the given socket and name to the manager.
-spec connect(Socket, Name) -> ok when
      Socket :: port(),
      Name :: string().

connect(Socket, Name) ->
    gen_server:cast(?MODULE, {connect, Socket, Name}).

%% @doc Removes the client with the given socket from the client manager.
-spec disconnect(Socket) -> ok when
      Socket :: port().

disconnect(Socket) ->
    gen_server:cast(?MODULE, {disconnect, Socket}).

%% @doc Send the given message to all clients in the manager.
-spec broadcast(Message) -> ok when
      Message :: string().

broadcast(Message) ->
    gen_server:cast(?MODULE, {broadcast, Message}).

%% Stops the client manager.
-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Server functions                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{clients=[]}}.

handle_call(list, _From, State = #state{clients=Clients}) ->
    {reply, Clients, State}.

handle_cast({connect, Socket, Name}, State = #state{clients=Clients}) ->
    NewClient = #client{socket=Socket, address=utils:socket_to_address(Socket), name=Name},
    io:format("~s Client connected: ~s (~s)~n", [utils:timestamp(), NewClient#client.address, NewClient#client.name]),
    {noreply, State#state{clients=[NewClient|Clients]}};
handle_cast({disconnect, Socket}, State = #state{clients=Clients}) ->
    Client = get_client(Socket, Clients),
    io:format("~s Client disconnected: ~s (~s)~n", [utils:timestamp(), Client#client.address, Client#client.name]),
    UpdatedClients = lists:delete(Client, Clients),
    {noreply, State#state{clients=UpdatedClients}};
handle_cast({broadcast, Message}, State = #state{clients=Clients}) ->
    [gen_tcp:send(Client#client.socket, Message ++ "\n") || Client <- Clients],
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Client Manager: Unexpected message: ~p~n", [Info]),
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

get_client(Socket, Clients) ->
    %% This could be improved
    {value, Client} = lists:keysearch(Socket, #client.socket, Clients),
    Client.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             EUnit Test Cases                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by server:test()

list_test() ->
    tbi.
