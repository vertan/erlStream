import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.*;
import java.io.*;
import java.net.*;
import java.util.Random;
import java.util.Comparator;
import java.util.Collections;

/**
 * The AudioManager class handles playback of songs and provides an API for UI development.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class AudioManager {
    private class TransitionHandler extends PlaybackListener {
	public void playbackStarted(PlaybackEvent e) {
	    startTime = System.currentTimeMillis();
	    playing = true;
	}

	public void playbackFinished(PlaybackEvent e) {
	    pausePosition = e.getFrame();
	    pauseTime = System.currentTimeMillis();

	    try {
		next();
	    } catch (Throwable t) {
		t.printStackTrace();
	    }
	}
    }

    private class PlayerThread implements Runnable {
	public void run() {
	    try {
		player.play();
	    } catch (Throwable e) {
		playing = false;
		currentSong = null;
	    }
	}
    }

    static class BadSongException extends Exception {
	public BadSongException(String message) {
	    super(message);
	}
    }

    private class TitleComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getFileName().toLowerCase().compareTo(b.getFileName().toLowerCase());
	}
    }

    private class ArtistComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getArtist().toLowerCase().compareTo(b.getArtist().toLowerCase());
	}
    }

    private class AlbumComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getAlbum().toLowerCase().compareTo(b.getAlbum().toLowerCase());
	}
    }

    private class DurationComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    Integer aDur = Integer.valueOf(a.getDuration());
	    Integer bDur = Integer.valueOf(b.getDuration());
	    return aDur.compareTo(bDur);
	}
    }

    private Communicator communicator;
    private AdvancedPlayer player;
    private PlaybackListener listener;
    private List<Song> songs;
    private Song currentSong;
    private boolean playing, paused, shuffle = false, repeat = true;
    private long startTime, pauseTime;
    private int pausePosition, offset, sortmode;

    /**
     * Initializes a newly created AudioManager object.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public AudioManager(String address, int inPort, int outPort) throws Exception {
	communicator = new Communicator(address, inPort, outPort);
	listener = new TransitionHandler();
	songs = getSongs();
    }

    /**
     * Starts playing from the first song in the queue.
     */
    public void play() throws Exception {
	if (isPaused()) {
	    play(currentSong, (pausePosition / 1000) + offset);
	    return;
	}

	if (songs.isEmpty()) {
	    return; // throw?
	}
	
	play(songs.get(0), 0);
    }

    /**
     * Stops any currently playing songs and plays the given song.
     *
     * @param song The song to play
     * @param offset The number of seconds to skip ahead
     */
    public void play(Song song, int offset) throws Exception {
	if (!exists(song)) {
	    throw new BadSongException(song.getTitle());
	}

	this.offset = offset;
	paused = false;

	if (isPlaying()) player.close();
	
	currentSong = song;

	InputStream audio = communicator.play(song, offset);
	player = new AdvancedPlayer(audio);
	player.setPlayBackListener(listener);

	startTime = System.currentTimeMillis(); // Preliminary guess
	playing = true; // Probably risky, but needed for getPosition()
	new Thread(new PlayerThread()).start();
    }

    /**
     * Stops any currently playing songs and plays the song with the given title.
     *
     * @param title The title of the song
     * @param offset The number of seconds to skip ahead
     */
    public void playSongByTitle(String title, int offset) throws Exception {
	Song song = getSongByTitle(title);
	if (song == null) {
	    throw new BadSongException(title);
	}

	play(song, offset);
    }

    /**
     * Returns the song with the given title if it exists in this AudioManager.
     *
     * @param title The title of the song
     * @return The song with the given title if it exists in this AudioManager
     */
    public Song getSongByTitle(String title) {
	for (Song song : songs) {
	    if (song.getFileName().equals(title)) return song;
	}

	return null;
    }

    /**
     * Returns true if the given song exists in this AudioManager.
     *
     * @param The song
     * @return true if the song exists in this AudioManager, false otherwise.
     */
    public boolean exists(Song song) {
	// return songs.contains(song);
	for (Song songInList : songs) {
	    if (song.equals(songInList)) return true;
	}
	return false;
    }

    /**
     * Pauses the currently playing song.
     */
    public void pause() {
	if (isPlaying()) {
	    playing = false;
	    paused = true;
	    player.stop();
	}
    }

    /**
     * Stops the playback.
     */
    public void stop() {
	if (player != null) {
	    player.close();
	    playing = false;
	    paused = false;
	    currentSong = null;
	}
    }

    /**
     * Plays the next song in the queue.
     */
    public void next() throws Exception {
	if (isPlaying()) {
	    Song next;

	    if (shuffleIsOn()) {
		next = songs.get(new Random().nextInt(songs.size()));
	    } else {
		int currentSongIndex = songs.indexOf(currentSong);

		if ((currentSongIndex == (songs.size() - 1)) && !repeatIsOn() ) { // If last and repeat off
		    stop();
		    return;
		}

		next = songs.get((currentSongIndex + 1) % songs.size());
	    }

	    try {
		play(next, 0);		    
	    } catch (BadSongException e) { // Should not happen, but just in case
		stop();
		throw e;
	    }
	}
    }

    /**
     * Plays the previous song in the queue.
     */
    public void previous() throws Exception {
	if (isPlaying()) {
	    // TODO: Implement history?
	    Song next;
	
	    if (shuffleIsOn()) {
		next = songs.get(new Random().nextInt(songs.size()));
	    } else {
		int currentSongIndex = songs.indexOf(currentSong);

		if (currentSongIndex < 1) {
		    next = songs.get(songs.size() - 1);
		} else {
		    next = songs.get(currentSongIndex - 1);
		}
	    }

	    try {
		play(next, 0);		    
	    } catch (BadSongException e) { // Should not happen, but just in case
		stop();
		throw e;
	    }
	}
    }

    /**
     * Returns the currently playing song.
     *
     * @return The currently playing song
     */
    public Song getCurrentSong() {
	return currentSong;
    }

    /**
     * Returns the current playback position in seconds.
     *
     * @return The current playback position in seconds
     */
    public int getPosition() {
	if (isPlaying()) {
	    long now = System.currentTimeMillis();
	    long diff = now - startTime;
	    return (int) (diff / 1000) + offset;
	} else if (isPaused()) {
	    long diff = pauseTime - startTime;
	    return (int) (diff / 1000) + offset;
	} else {
	    return 0;
	}
    }

    /**
     * Returns true if a song is playing.
     *
     * @return true if a song is currently playing, false otherwise
     */
    public boolean isPlaying() {
	return playing;
    }

    /**
     * Returns true if a song is paused.
     *
     * @return true if a song is paused, false otherwise
     */
    public boolean isPaused() {
	return paused;
    }

    /**
     * Turns shuffle mode on or off.
     *
     * @param shuffle true to turn shuffle mode on, false to turn off
     */
    public void setShuffle(boolean shuffle) {
	this.shuffle = shuffle;
    }
    
    /**
     * Turns repeat mode on or off.
     *
     * @param repeat true to turn repeat mode on, false to turn off
     */
    public void setRepeat(boolean repeat) {
	this.repeat = repeat;
    }

    /**
     * Returns true if shuffle mode is on.
     *
     * @return true if shuffle mode is on, false otherwise
     */
    public boolean shuffleIsOn() {
	return shuffle;
    }

    /**
     * Returns true if repeat mode is on.
     *
     * @return true if repeat mode is on, false otherwise
     */
    public boolean repeatIsOn() {
	return repeat;
    }

    /**
     * Checks whether this AudioManager is connected to a server.
     *
     * @return true if this AudioManager is connected to a server, false otherwise
     */
    public boolean isConnected() {
	return true;
    }

    /**
     * Returns the available songs.
     *
     * @return The available songs
     */
    public List<Song> getSongs() throws Exception {
	songs = communicator.list();

	if (sortmode != 0) {
	    sort(sortmode);
	}

	return songs;
    }

    /**
     * Sorts the songs by title, artist, album or duration.
     * 
     * @param sortmode 0/1 for title ascending/descending, 2/3 for artist ascending/descending, 4/5 for album ascending/descending and 6/7 for duration ascending/descending
     * @return The available songs, sorted by title
     */
    public List<Song> sort(int sortmode) {
	this.sortmode = sortmode;

	switch(sortmode) {
	case 0:
	    Collections.sort(songs, new TitleComparator());
	    break;
	case 1:
	    Collections.sort(songs, new TitleComparator());
	    Collections.reverse(songs);
	    break;
	case 2:
	    Collections.sort(songs, new ArtistComparator());
	    break;
	case 3:
	    Collections.sort(songs, new ArtistComparator());
	    Collections.reverse(songs);
	    break;
	case 4:
	    Collections.sort(songs, new AlbumComparator());
	    break;
	case 5:
	    Collections.sort(songs, new AlbumComparator());
	    Collections.reverse(songs);
	    break;
	case 6:
	    Collections.sort(songs, new DurationComparator());
	    break;
	case 7:
	    Collections.sort(songs, new DurationComparator());
	    Collections.reverse(songs);
	    break;
	default:
	    // throw new IllegalArgumentException(String.valueOf(sortmode));
	    break;
	}

	return songs;
    }
}