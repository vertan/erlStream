%%%        File : database.erl
%%%      Author : Filip Hedman <hedman.filip@gmail.com>, Jeanette Castillo <jeanette.cas@hotmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
%%% Description: Loading and handling songs on the server side

-module(database).
-behavior(gen_server).

-export([start/1, list/0, get_directory/0, exists/1, refresh/0, play/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("song.hrl").

-record(state, {directory, songs}).

start(Directory) ->
    gen_server:start_link({local, database}, ?MODULE, Directory, []).

list() ->
    gen_server:call(database, list).

get_directory() ->
    gen_server:call(database, get_directory).

exists(Filename) ->
    gen_server:call(database, {exists, Filename}).

refresh() ->
    gen_server:cast(database, refresh).

stop() ->
    gen_server:cast(database, terminate).

%% Not executed by the atual server, for performance reasons.
play(Filename, Offset) ->
    case exists(Filename) of
	true ->
	    FilePath = get_directory() ++ "/" ++ Filename ++ ".mp3",
	    Bitrate = 192000 div 8,
	    BitrateMS = Bitrate / 1000,
	    StartOffset = round(Offset * BitrateMS),
	    case file:read_file(FilePath) of
		{ok, Binary} ->
		    <<_OffsetChunk:StartOffset/binary, RestChunk/bitstring>> = Binary,
		    {ok, RestChunk};
		{error, _Reason} ->
		    {error, read_failed}
	    end;
	false ->
	    {error, no_such_file}
    end.


%%%%%%%%%%%%%%%%%%%%%%
%% Server functions %%
%%%%%%%%%%%%%%%%%%%%%%

init(Directory) ->
    process_flag(trap_exit, true), %% Is this needed?
    Songs = load(Directory),
    {ok, #state{directory=Directory, songs=Songs}}.

handle_call(list, _From, State = #state{songs=Songs}) ->
    {reply, Songs, State};
handle_call(get_directory, _From, State = #state{directory=Directory}) ->
    {reply, Directory, State};
handle_call({exists, Filename}, _From, State = #state{songs=Songs}) ->
    Reply = case lists:filter(fun(Song) -> Song#song.filename =:= Filename end, Songs) of
		[] ->
		    false;
		_ ->
		    true
	    end,
    {reply, Reply, State}.

handle_cast(refresh, State = #state{directory=Directory}) ->
    UpdatedSongs = load(Directory),
    {noreply, State#state{songs=UpdatedSongs}};
handle_cast(terminate, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Database: Unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

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
	    {Title, Artist, Album, _Year} = Tags,
	    Duration = get_duration(FilePath),
	    [Basename|_] = string:tokens(Filename, "."),
	    Song = #song{filename=Basename, title=Title, artist=Artist, album=Album, duration=Duration},
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
