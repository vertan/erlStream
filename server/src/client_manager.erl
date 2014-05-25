%%%        File : database.erl
%%%      Author : Filip Hedman <hedman.filip@gmail.com>, Jeanette Castillo <jeanette.cas@hotmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
%%% Description: Loading and handling songs on the server side

-module(client_manager).
-behavior(gen_server).

-export([start/0, list/0, connect/1, disconnect/1, broadcast/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list() ->
    gen_server:call(?MODULE, list).

connect(Socket) ->
    gen_server:cast(?MODULE, {connect, Socket}).

disconnect(Socket) ->
    gen_server:cast(?MODULE, {disconnect, Socket}).

broadcast(Message) ->
    gen_server:cast(?MODULE, {broadcast, Message}).

stop() ->
    gen_server:cast(?MODULE, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Server functions                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{clients=[]}}.

handle_call(list, _From, State = #state{clients=Clients}) ->
    {reply, Clients, State}.

handle_cast({connect, Socket}, State = #state{clients=Clients}) ->
    NewClient = #client{socket=Socket, address=utils:socket_to_address(Socket), name="Unknown"},
    io:format("~s Client connected: ~s (~s)~n", [utils:timestamp(), NewClient#client.address, NewClient#client.name]),
    {noreply, State#state{clients=[NewClient|Clients]}};
handle_cast({disconnect, Socket}, State = #state{clients=Clients}) ->
    Client = get_client(Socket, Clients),
    io:format("~s Client disconnected: ~s (~s)~n", [utils:timestamp(), Client#client.address, Client#client.name]),
    UpdatedClients = lists:delete(Client, Clients),
    {noreply, State#state{clients=UpdatedClients}};
handle_cast({broadcast, Message}, State = #state{clients=Clients}) ->
    lists:foreach(fun(Client) -> gen_tcp:send(Client#client.socket, Message ++ "\n") end, Clients),
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
