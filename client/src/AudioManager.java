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
 * @see UpdateListener
 * @see Song
 */
public class AudioManager extends PlaybackListener implements UpdateListener {
    private class PlayerThread implements Runnable {
	public void run() {
	    try {
		player.play();
	    } catch (Exception e) {
		playing = false;
		currentSong = null;
	    }
	}
    }

    private Communicator communicator;
    private AdvancedPlayer player;
    private List<StatusListener> observers;

    private volatile List<Song> songs;
    private volatile Song currentSong;
    private volatile boolean playing, paused, shuffle = false, repeat = true;
    private volatile long startTime, pauseTime;
    private volatile int pausePosition, offset, sortmode;

    /**
     * Initializes a newly created AudioManager object.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     *
     * @throws UnknownHostException If the given address is invalid
     * @throws IOException If a network error occured 
     */
    public AudioManager(String address, int port) throws UnknownHostException, IOException {
	communicator = new Communicator(address, port);
	songs = communicator.connect();
	communicator.addUpdateListener(this);
	observers = new ArrayList<StatusListener>();
	sort(sortmode);
    }

    /**
     * Starts playing from the first song in the list.
     *
     * @throws BadSongException If no song exists in this AudioManager
     * @throws UnknownHostException If the address given when creating this AudioManager is invalid
     * @throws IOException If a network error occured
     * @throws PlaybackFailedException If the playback libary failed
     */
    public void play() throws BadSongException, UnknownHostException, IOException, PlaybackFailedException  {
	if (isPaused()) {
	    play(currentSong, pausePosition + offset);
	    return;
	}

	if (songs.isEmpty()) {
	    throw new BadSongException("No songs available");
	}

	play(songs.get(0), 0);
    }

    /**
     * Stops any currently playing songs and plays the given song.
     *
     * @param song The song to play
     * @param offset The number of milliseconds to skip ahead
     */
    public void play(Song song, int offset) throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (!exists(song)) {
	    throw new BadSongException(song.getFileName());
	}

	this.offset = offset;
	paused = false;

	// Maybe this check isn't neccessary
	if (isPlaying()) player.close();
	
	currentSong = song;

	InputStream audio = communicator.play(song, offset);

	try {
	    player = new AdvancedPlayer(audio);
	} catch (Exception e) {
	    playing = false;
	    throw new PlaybackFailedException("Playback library error");
	}

	player.setPlayBackListener(this);

	startTime = System.currentTimeMillis(); // Preliminary guess
	playing = true; // Probably risky, but needed for getPosition()
	new Thread(new PlayerThread()).start();
    }

    public void playbackStarted(PlaybackEvent e) {
	startTime = System.currentTimeMillis();
	playing = true;
    }

    public void playbackFinished(PlaybackEvent e) {
	pausePosition = e.getFrame();
	pauseTime = System.currentTimeMillis();

	if (isPlaying()) { // If the song ended
	    try {
		next();
	    } catch (Exception ex) {
		// Probably best to be quiet about this
	    }
	}
    }

    /**
     * Stops any currently playing songs and plays the song with the given title.
     *
     * @param title The title of the song
     * @param offset The number of milliseconds to skip ahead
     */
    public void playSongByTitle(String title, int offset) throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
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
    public void next() throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (isPlaying() || isPaused()) {
	    Song next;

	    if (shuffleIsOn()) {
		while ((next = songs.get(new Random().nextInt(songs.size()))) == getCurrentSong());
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
	    } catch (Exception e) { // Should not happen, but just in case
		stop();
		throw e;
	    }
	}
    }

    /**
     * Plays the previous song in the queue.
     */
    public void previous() throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (isPlaying() || isPaused()) {
	    // TODO: Implement history?
	    Song next;
	
	    if (shuffleIsOn()) {
		while ((next = songs.get(new Random().nextInt(songs.size()))) == getCurrentSong());
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
	    } catch (Exception e) { // Should not happen, but just in case
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
	    return (int) ((diff + offset) / 1000);
	} else if (isPaused()) {
	    long diff = pauseTime - startTime;
	    return (int) ((diff + offset) / 1000);
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
	return communicator.isConnected();
    }

    /**
     * Returns the available songs.
     *
     * @return The available songs
     */
    public synchronized List<Song> getSongs() {
	return songs;
    }

    /**
     * Returns the address to the server currently connected to.
     *
     * @return The address to the server currently connected to
     */
    public String getAddress() {
	return communicator.getAddress();
    }

    /**
     * Returns the port to the server to communicate with.
     *
     * @return The port to the server to communicate with
     */
    public int getPort() {
	return communicator.getPort();
    }

    /**
     * Stops any current playback and closes the connection to the server.
     */
    public void close() {
	stop();
	communicator.disconnect();
    }

    /**
     * Sorts the songs by title, artist, album or duration.
     * 
     * @param sortmode 0/1 for title ascending/descending, 2/3 for artist ascending/descending, 4/5 for album ascending/descending and 6/7 for duration ascending/descending
     * @return The available songs, sorted according to the argument
     */
    public synchronized List<Song> sort(int sortmode) {
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

    public void songsUpdated(List<Song> newSongs) {
	songs = newSongs;
	sort(sortmode);
	for (StatusListener observer: observers) {
	    observer.songsUpdated(songs);
	}
    }

    public void serverShutdown() {
	for (StatusListener observer: observers) {
	    observer.serverShutdown();
	}
    }

    public void connectionLost() {
	for (StatusListener observer: observers) {
	    observer.connectionLost();
	}
    }

    public void addStatusListener(StatusListener observer) {
	observers.add(observer);
    }

    public void removeStatusListener(StatusListener observer) {
	observers.remove(observer);
    }
}
