import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.AdvancedPlayer;
import java.io.*;
import java.net.*;
import java.util.Random;

/**
 * The AudioManager class handles playback of songs and provides an API for UI development.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class AudioManager {
    private class PlayerThread implements Runnable {
	public void run() {
	    try {
		player.play();
		next();
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	}
    }

    static class BadSongException extends Exception {
	public BadSongException(String message) {
	    super(message);
	}
    }

    private Communicator communicator;
    private AdvancedPlayer player;
    private List<Song> songs;
    private Song currentSong;
    private boolean shuffle, repeat;

    /**
     * Initializes a newly created AudioManager object.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public AudioManager(String address, int inPort, int outPort) throws Exception {
	communicator = new Communicator(address, inPort, outPort);
	songs = new ArrayList<Song>();
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

	if (isPlaying()) stop();
	
	currentSong = song;
	InputStream audio = communicator.play(song, offset);
	player = new AdvancedPlayer(audio);

	System.out.println("Now playing \"" + song.getFileName() + "\" by " + song.getArtist() + ".");
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
	
    }

    /**
     * Stops the playback.
     */
    public void stop() {
	if (player != null) {
	    player.close();
	    currentSong = null;
	}
    }

    /**
     * Plays the next song in the queue.
     */
    public void next() throws Exception {
	if (isPlaying()) {
	    if (shuffleIsOn()) {
		Song nextSong = songs.get(new Random().nextInt(songs.size()) - 1);
		stop();
		play(nextSong, 0);
	    } else {
		// TODO: Check repeat
		int currentSongIndex = songs.indexOf(currentSong);
		Song nextSong = songs.get((currentSongIndex + 1) % songs.size());
		stop();
		play(nextSong, 0);
	    }
	}
    }

    /**
     * Plays the previous song in the queue.
     */
    public void previous() throws Exception {
	// TODO: Implement history
	if (isPlaying()) {
	    if (shuffleIsOn()) {
		Song nextSong = songs.get(new Random().nextInt(songs.size()) - 1);
		stop();
		play(nextSong, 0);
	    } else {
		// TODO: Check repeat
		int currentSongIndex = songs.indexOf(currentSong);

		Song nextSong;
		if (currentSongIndex < 1) {
		    nextSong = songs.get(songs.size() - 1);
		} else {
		    nextSong = songs.get(currentSongIndex - 1);
		}

		stop();
		play(nextSong, 0);
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
     * Returns the elapsed time of the song currently playing.
     *
     * @return The elapsed time of the song currently playing in seconds.
     */
    public int getElapsedPlaybackTime() {
	return 0;
    }

    /**
     * Returns true if a song is playing.
     *
     * @return true if a song is currently playing, false otherwise
     */
    public boolean isPlaying() {
	return currentSong != null;
    }

    /**
     * Returns true if a song is paused.
     *
     * @return true if a song is paused, false otherwise
     */
    public boolean isPaused() {
	return false;
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

	return songs;
    }

    /**
     * Sorts the songs by title.
     * 
     * @return The available songs, sorted by title
     */
    public List<Song> sortByTitle() throws Exception {
	return getSongs();
    }
}