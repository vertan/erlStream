-module(database).
-export([start/0, list/1, refresh/1, play/2]).
-include("song.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Starts the file manager.
start() ->
    Songs = load(),
    loop(Songs).

%% Handles requests.
loop(Songs) ->
    receive
	{Pid, list} ->
	    Pid ! {songs, Songs},
	    loop(Songs);
	refresh ->
	    UpdatedSongs = load(),
	    loop(UpdatedSongs);
	{Pid, exists, Song} ->
	    case lists:filter(fun(Element) -> Element#song.filename =:= Song end, Songs) of
		[] ->
		    Pid ! false;
		_ ->
		    Pid ! true
	    end,
	    loop(Songs)
    end.

%% Returns the song list.
list(Database) ->
    Database ! {self(), list},
    receive
	{songs, Songs} ->
	    Songs
    end.

%% Refreshes and returns the song list.
refresh(Database) ->
    Database ! refresh,
    list(Database).
 
%% Loads song list from hard drive.
load() ->
    FilesString = os:cmd("ls ../files/ | egrep '*.mp3'"),
    SongNames = string:tokens(FilesString, "\n"),
    load(SongNames).

load(SongNames) ->
    load(SongNames, []).

load([], Songs) ->
    lists:reverse(Songs);
load([SongName|Rest], Songs) ->
    %% Parse ID3
    {Title, Artist, Album, _} = get_tags(SongName),
    Song = #song{filename=SongName,title=Title, artist=Artist, album=Album, duration=210},
    load(Rest, [Song|Songs]).

%% Checks if the given song exists in the given Database, and if so, returns {song, Data}
play(_, []) ->
    {error, "No file given"};
play(Database, [File|OffsetTime]) ->
    Database ! {self(), exists, File},
    receive
	true ->
	    FilePath = lists:append("../files/", File),
	    Bitrate = 24000,
	    BitrateMS = Bitrate / 1000,
	    [StartTime|_] = OffsetTime,
	    {StartMS, StartRest} = string:to_integer(StartTime),
	    StartOffset = round(StartMS * BitrateMS),
	    case file:read_file(FilePath) of
		{ok, Binary} ->
		    <<OffsetChunk:StartOffset/binary, RestChunk/bitstring>> = Binary,
		    {ok, RestChunk};
		{error, Reason} ->
		    {error, "Failed to read file"}
	    end;
	false ->
	    {error, "File does not exist in the database"}
    end.

get_tags(File) ->
    FilePath = lists:append("../files/", File),
    case file:open(FilePath, [read, binary]) of
	{ok, MP3} ->
	    Result = case file:pread(MP3, {eof, -128}, 128) of
			 {eof} ->
			     eof;
			 {error, Reason} ->
			     Reason;
			 {ok, <<"TAG", Tags/binary>>} -> 
			     parse_tags(Tags);
			 {ok, _} ->
			     no_id3
		     end,
	    file:close(MP3),
	    Result;
	{error, Reason} ->
	    io:format("Could not open file! ~s~n", [Reason])
    end.

parse_tags(Tags) ->
    <<Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary, Comment:30/binary, Genre:1/binary>> = Tags,
    {string:strip(binary:bin_to_list(Title)), 
     string:strip(binary:bin_to_list(Artist)),
     string:strip(binary:bin_to_list(Album)),
     string:strip(binary:bin_to_list(Year))}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending with _test() or _test_() will be
%% called automatically by add:test()

list_test() ->
    tbi.