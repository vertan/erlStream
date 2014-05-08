import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.AdvancedPlayer;
import java.io.*;
import java.net.*;

/**
 * The AudioManager class handles playback of songs and provides an API for UI development.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class AudioManager {
    private Communicator communicator;
    private AdvancedPlayer player;
    private List<Song> songs;

    /**
     * Initializes a newly created AudioManager object.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public AudioManager(String address, int inPort, int outPort) throws Exception {
	communicator = new Communicator(address, inPort, outPort);
    }

    /**
     * Plays the given song.
     *
     * @param song The song to play
     */
    public void play(Song song, int offset) throws Exception {
	InputStream audio = communicator.play(song, offset);
	player = new AdvancedPlayer(audio);
	player.play();
    }

    /**
     * Plays the song with the given title.
     *
     * @param title The title of the song 
     */
    public void playSongByTitle(String title, int offset) throws Exception {
	Song song = new Song(title, "Unknown Title", "Unknown Artist", "Unknown Album", 60);
	InputStream audio = communicator.play(song, offset);
	player = new AdvancedPlayer(audio);
	player.play();	
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

    }

    /**
     * Plays the next song in the queue
     */
    public void next() {

    }

    /**
     * Plays the previous song in the queue.
     */
    public void previous() {
	
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