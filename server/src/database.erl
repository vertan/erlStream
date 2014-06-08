%% @author Filip Hedman <hedman.filip@gmail.com>
%% @author Jeanette Castillo <jeanette.cas@hotmail.com>
%% @author Robert Kallgren <robertkallgren@gmail.com>
%% @author Oscar Mangard <oscarmangard@gmail.com>
%% @author Mikael Sernheim <mikael.sernheim@gmail.com>
%% @doc `Database' implements a simple database to store information about songs. In order for lengths to be calculated correctly, the songs must have a bitrate of 192kbps.

-module(database).
-behavior(gen_server).

-export([start/2, list/0, get_directory/0, exists/1, play/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("song.hrl").

-record(state, {directory, songs, updater}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    API                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the database and loads information about the available songs in 
%% the given directory. The database will automatically update its information 
%% at the specified interval in milliseconds. Refer to the official `gen_server'
%% documentation for further information about the different return values.
-spec start(Directory, UpdateInterval) -> Result when
      Directory :: string(),
      UpdateInterval :: integer(),
      Result :: {ok, Pid} | ignore | {error, Reason},
      Pid :: pid(),
      Reason :: {already_started, ?MODULE} | term().

start(Directory, UpdateInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Directory, UpdateInterval}, []).

%% @doc Returns a list with a `song' record for each song in the database.
-spec list() -> [Song] when
      Song :: #song{title :: string(), artist :: string(), album :: string(), duration :: integer()}.

list() ->
    gen_server:call(?MODULE, list).

%% @doc Returns the directory given when starting the database.
-spec get_directory() -> Directory when
      Directory :: string().

get_directory() ->
    gen_server:call(?MODULE, get_directory).

%% @doc Returns `true' if a song with the given title exists in the database.
-spec exists(Title) -> Result when
      Title :: string(),
      Result :: true | false.

exists(Title) ->
    gen_server:call(?MODULE, {exists, Title}).

%% Stops the database.
-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc Returns binary data for a song with the given title in the database.
%% `Offset' is the number of milliseconds to skip ahead.
-spec play(Title, Offset) -> Result when
      Title :: string(),
      Offset :: integer(),
      Result :: {ok, binary()} | {error, Reason},
      Reason :: offset_too_high | read_failed | no_such_file.

play(Title, Offset) ->
    case exists(Title) of
	true ->
	    FilePath = get_directory() ++ "/" ++ Title ++ ".mp3",
	    Bitrate = 192000 div 8,
	    BitrateMS = Bitrate / 1000,
	    StartOffset = round(Offset * BitrateMS),
	    case file:read_file(FilePath) of
		{ok, Binary} when byte_size(Binary) >= StartOffset ->
		    <<_OffsetChunk:StartOffset/binary, RestChunk/bitstring>> = Binary,
		    {ok, RestChunk};
		{ok, _Binary} ->
		    {error, offset_too_high};
		{error, _Reason} ->
		    {error, read_failed}
	    end;
	false ->
	    {error, no_such_file}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Server functions                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Directory, UpdateInterval}) ->
    process_flag(trap_exit, true), %% Is this needed?
    Updater = proc_lib:spawn_link(fun() -> updater(Directory, UpdateInterval) end),
    Songs = load(Directory),
    {ok, #state{directory=Directory, songs=Songs, updater=Updater}}.

handle_call(list, _From, State = #state{songs=Songs}) ->
    {reply, Songs, State};
handle_call(get_directory, _From, State = #state{directory=Directory}) ->
    {reply, Directory, State};
handle_call({exists, Title}, _From, State = #state{songs=Songs}) ->
    Reply = case lists:filter(fun(Song) -> Song#song.title =:= Title end, Songs) of
		[] ->
		    false;
		_ ->
		    true
	    end,
    {reply, Reply, State}.

handle_cast({update, UpdatedSongs}, State) ->
    io:format("~s Database updated, notifying clients...~n", [utils:timestamp()]),
    SongInfo = [Song#song.title ++ ":" ++ Song#song.artist ++ ":"
		  ++ Song#song.album ++ ":" ++ integer_to_list(Song#song.duration) ++ "\n" || Song <- UpdatedSongs],
    SongInfoApp = lists:append(SongInfo),
    client_manager:broadcast("update:\n" ++ SongInfoApp ++ "end"),
    {noreply, State#state{songs=UpdatedSongs}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Database: Unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, #state{updater=Updater}) ->
    exit(Updater, normal),
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

updater(Directory, Interval) ->
    SongString = os:cmd("ls " ++ Directory ++ "/ | egrep '.*.mp3'"),
    updater(Directory, Interval, SongString).

updater(Directory, Interval, SongString) ->
    timer:sleep(Interval),
    UpdatedSongString = os:cmd("ls " ++ Directory ++ "/ | egrep '.*.mp3'"),
    if
	UpdatedSongString =/= SongString ->
	    UpdatedSongs = load(Directory),
	    gen_server:cast(?MODULE, {update, UpdatedSongs});
	true ->
	    ok
    end,
    updater(Directory, Interval, UpdatedSongString).

load(Directory) ->
    FilesString = os:cmd("ls " ++ Directory ++ "/ | egrep '.*.mp3'"),
    Filenames = string:tokens(FilesString, "\n"),
    load(Directory, Filenames, []).

load(_Directory, [], Songs) ->
    lists:reverse(Songs);
load(Directory, [Filename|Rest], Songs) ->
    FilePath = Directory ++ "/" ++ Filename,
    case get_tags(FilePath) of
	{ok, Tags} ->
	    {_Title, Artist, Album, _Year} = Tags,
	    Duration = get_duration(FilePath),
	    [Basename|_] = string:tokens(Filename, "."),
	    Song = #song{title=Basename, artist=Artist, album=Album, duration=Duration},
	    UpdatedSongs = [Song|Songs];
	{error, _Reason} ->
	    UpdatedSongs = Songs
    end,
    load(Directory, Rest, UpdatedSongs).

get_tags(FilePath) ->
    case file:open(FilePath, [read, binary]) of
	{ok, MP3} ->
	    Result = case file:pread(MP3, {eof, -128}, 128) of
			 {ok, <<"TAG", Tags/binary>>} -> 
			     parse_tags(Tags);
			 _ ->
			     {"Unknown", "Unknown", "Unknown", "Unknown"}
		     end,
	    file:close(MP3),
	    {ok, Result};
	{error, Reason} ->
	    {error, Reason}
    end.

parse_tags(Tags) ->
    <<Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, _Comment:30/binary, _Genre:1/binary>> = Tags,
    {string:strip(binary:bin_to_list(Title)), 
     string:strip(binary:bin_to_list(Artist)),
     string:strip(binary:bin_to_list(Album)),
     string:strip(binary:bin_to_list(Year))}.

get_duration(FilePath) ->
    case file:read_file_info(FilePath) of
	{ok, FileInfo} ->
	    FileInfo#file_info.size div 24000;
	{error, _Reason} ->
	    0
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             EUnit Test Cases                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by database:test()

list_test() ->
    tbi.
