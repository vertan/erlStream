import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import javazoom.jl.player.advanced.AdvancedPlayer;
import javazoom.jl.player.advanced.PlaybackEvent;
import javazoom.jl.player.advanced.PlaybackListener;

/**
 * The <code>AudioManager</code> represents a music player built to play songs from a server using the {@link Communicator} class.
 * It provides methods for common music player tasks such as play, pause, next, previous and sorting.
 *
 * <p>
 * When a song finishes, the next is automatically started. 
 * If the connection is lost, the <code>AudioManager</code> will automatically try to reconnect.
 * </p>
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see Communicator
 */
public class AudioManager extends PlaybackListener implements UpdateListener {
    /*
     * This class is used to play audio in the background using AdvancedPlayer.
     */
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
     * Initializes a newly created <code>AudioManager</code> object.
     *
     * @param address the address to the server
     * @param port the port to the server
     * @throws UnknownHostException if the given address could not be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     */
    public AudioManager(String address, int port) throws UnknownHostException, IOException {
	communicator = new Communicator(address, port);
	songs = communicator.connect();
	communicator.addUpdateListener(this);
	observers = new ArrayList<StatusListener>();
	sort(sortmode);
    }

    /**
     * Returns the songs available on the server.
     *
     * @return the songs available on the server
     */
    public synchronized List<Song> getSongs() {
	return new ArrayList<Song>(songs); // To prevent modification from outside
    }

    /**
     * If stopped, starts playing from the first song in this <code>AudioManager</code>. If paused, resumes playback.
     *
     * @throws BadSongException if no songs exist on the server
     * @throws UnknownHostException if the address given when this <code>AudioManager</code> was created could no be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     * @throws PlaybackFailedException if the playback library failed
     * @see #play(Song, int)
     * @see #playSongByTitle(String, int)
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
     * Plays the given song. If a song is already playing it is stopped and the new song is started.
     *
     * @param song the song to play
     * @param offset the number of milliseconds to skip ahead
     * @throws BadSongException if the given song does not exist on the server
     * @throws UnknownHostException if the address given when this <code>AudioManager</code> was created could no be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     * @throws PlaybackFailedException if the playback library failed
     * @see #play()
     * @see #playSongByTitle(String, int)
     */
    public void play(Song song, int offset) throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (!exists(song)) {
	    throw new BadSongException(song.getTitle());
	}

	this.offset = offset;
	paused = false;

	// Maybe this check isn't necessary
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

    @Override
    public void playbackStarted(PlaybackEvent e) {
	startTime = System.currentTimeMillis();
	playing = true;
    }

    @Override
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
     * Plays the given song. If a song is already playing it is stopped and the new song is started.
     *
     * @param title the title of the song
     * @param offset the number of milliseconds to skip ahead
     * @see #play()
     * @see #play(Song, int)
     * @see #getSongByTitle(String)
     */
    public void playSongByTitle(String title, int offset) throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	Song song = getSongByTitle(title);
	if (song == null) {
	    throw new BadSongException(title);
	}

	play(song, offset);
    }

    /**
     * Returns the song with the given title in this <code>AudioManager</code>.
     *
     * @param title the title of the song
     * @return the song with the given title, or <code>null</code> if it does not exist in this <code>AudioManager</code>
     */
    public Song getSongByTitle(String title) {
	for (Song song : songs) {
	    if (song.getTitle().equals(title)) return song;
	}

	return null;
    }

    /**
     * Returns <code>true</code> if the given song exists in this <code>AudioManager</code>.
     *
     * @param song the song
     * @return <code>true</code> if the song exists in this <code>AudioManager</code>, <code>false</code> otherwise.
     */
    public boolean exists(Song song) {
	return songs.contains(song);
    }

    /**
     * Pauses the currently playing song. If no song is playing, or if a song is already paused, no exception is thrown and no action is performed. The playback can be resumed again using {@link #play()}.
     *
     * @see #play()
     */
    public void pause() {
	if (isPlaying()) {
	    playing = false;
	    paused = true;
	    player.stop();
	}
    }

    /**
     * Stops the currently playing or paused song without pausing. If no song is playing or paused, no exception is thrown and no action is performed.
     *
     * @see #pause()
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
     * Plays the next song in this <code>AudioManager</code>. If no song is playing or paused, or if the current song is the last song in this <code>AudioManager</code> and repeat and shuffle are off, no exception is thrown and no action is performed.
     *
     * @throws BadSongException if the next song in this <code>AudioManager</code> does not exist on the server
     * @throws UnknownHostException if the address given when this <code>AudioManager</code> was created could no be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     * @throws PlaybackFailedException if the playback library failed
     * @see #previous()
     * @see #repeatIsOn()
     */
    public void next() throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (isPlaying() || isPaused()) {
	    Song next;

	    if (shuffleIsOn()) {
		while ((next = songs.get(new Random().nextInt(songs.size()))) == getCurrentSong());
	    } else {
		int currentSongIndex = songs.indexOf(currentSong);

		if ((currentSongIndex == (songs.size() - 1)) && !repeatIsOn() ) { // If last and repeat is off
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
     * Plays the previous song in this <code>AudioManager</code>. If no song is playing or paused, no exception is thrown and no action is performed.
     *
     * @throws BadSongException if the previous song in this <code>AudioManager</code> does not exist on the server
     * @throws UnknownHostException if the address given when this <code>AudioManager</code> was created could no be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     * @throws PlaybackFailedException if the playback library failed
     * @see #next()
     */
    public void previous() throws BadSongException, UnknownHostException, IOException, PlaybackFailedException {
	if (isPlaying() || isPaused()) {
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
     * Returns the current song.
     *
     * @return the current song, or <code>null</code> if no song is playing or paused
     */
    public Song getCurrentSong() {
	return currentSong;
    }

    /**
     * Returns the current playback position in seconds.
     *
     * @return the current playback position in seconds, or <code>0</code> if no song is playing or paused
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
     * Returns <code>true</code> if a song is currently playing.
     *
     * @return <code>true</code> if a song is currently playing, <code>false</code> otherwise
     * @see #play()
     * @see #play(Song, int)
     * @see #playSongByTitle(String, int)
     */
    public boolean isPlaying() {
	return playing;
    }

    /**
     * Returns <code>true</code> if a song is currently paused.
     *
     * @return <code>true</code> if a song is currently paused, <code>false</code> otherwise
     * @see #pause()
     */
    public boolean isPaused() {
	return paused;
    }

    /**
     * Sets whether shuffle mode is enabled. If enabled, the songs in this <code>AudioManager</code> will play in random order.
     *
     * @param enable <code>true</code> to enable, <code>false</code> to disable
     * @see #shuffleIsOn()
     */
    public void setShuffle(boolean enable) {
	shuffle = enable;
    }
    
    /**
     * Sets whether repeat mode is enabled. If enabled, this <code>AudioManager</code> will start playing again at the first song after the last song has finished. If shuffle is on, repeat has no effect.
     *
     * @param enable <code>true</code> to enable, <code>false</code> to disable
     * @see #repeatIsOn()
     * @see #shuffleIsOn()
     */
    public void setRepeat(boolean enable) {
	repeat = enable;
    }

    /**
     * Returns <code>true</code> if shuffle mode is enabled.
     *
     * @return <code>true</code> if shuffle mode is enabled, <code>false</code> otherwise
     * @see #setShuffle(boolean)
     */
    public boolean shuffleIsOn() {
	return shuffle;
    }

    /**
     * Returns <code>true</code> if repeat mode is enabled.
     *
     * @return <code>true</code> if repeat mode is enabled, <code>false</code> otherwise
     * @see #setRepeat(boolean)
     */
    public boolean repeatIsOn() {
	return repeat;
    }

    /**
     * Returns <code>true</code> if this <code>AudioManager</code> is connected to the server.
     *
     * @return <code>true</code> if this <code>AudioManager</code> is connected to the server, <code>false</code> otherwise
     */
    public boolean isConnected() {
	return communicator.isConnected();
    }

    /**
     * Returns the address to the server given when this <code>AudioManager</code> was created.
     *
     * @return the address to the server
     */
    public String getAddress() {
	return communicator.getAddress();
    }

    /**
     * Returns the port to the server given when this <code>AudioManager</code> was created.
     *
     * @return the port to the server
     */
    public int getPort() {
	return communicator.getPort();
    }

    /**
     * Stops any current playback and closes the connection to the server. In order to access the server again, a new <code>AudioManager</code> must be created.
     */
    public void close() {
	stop();
	communicator.disconnect();
    }

    /**
     * Sorts the songs in this <code>AudioManager</code> by title, artist, album or duration.
     * 
     * @param sortmode 0/1 for title ascending/descending, 2/3 for artist ascending/descending, 4/5 for album ascending/descending and 6/7 for duration ascending/descending
     * @return the songs available on the server, sorted in the given order
     * @throws IllegalArgumentException if the argument is not an integer between 0 and 7
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
	    throw new IllegalArgumentException(String.valueOf(sortmode));
	}

	return getSongs();
    }

    /*
     * Compares two songs by title.
     */
    private class TitleComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getTitle().toLowerCase().compareTo(b.getTitle().toLowerCase());
	}
    }

    /*
     * Compares two songs by artist.
     */
    private class ArtistComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getArtist().toLowerCase().compareTo(b.getArtist().toLowerCase());
	}
    }

    /*
     * Compares two songs by album.
     */
    private class AlbumComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    return a.getAlbum().toLowerCase().compareTo(b.getAlbum().toLowerCase());
	}
    }

    /*
     * Compares two songs by duration.
     */
    private class DurationComparator implements Comparator<Song> {
	public int compare(Song a, Song b) {
	    Integer aDur = Integer.valueOf(a.getDuration());
	    Integer bDur = Integer.valueOf(b.getDuration());
	    return aDur.compareTo(bDur);
	}
    }

    @Override
    public void songsUpdated(List<Song> newSongs) {
	songs = newSongs;
	sort(sortmode);
	for (StatusListener observer: observers) {
	    observer.songsUpdated(getSongs());
	}
    }

    @Override
    public void serverShutdown() {
	for (StatusListener observer: observers) {
	    observer.serverShutdown();
	}
    }

    @Override
    public void connectionLost() {
	for (StatusListener observer: observers) {
	    observer.connectionLost();
	}
    }

    @Override
    public void connectionRegained(List<Song> songs) {
	this.songs = songs;
	sort(sortmode);
	for (StatusListener observer: observers) {
	    observer.connectionRegained(getSongs());
	}
    }

    /**
     * Adds the specified <code>StatusListener</code> to receive song and connection updates from this <code>AudioManager</code>. 
     * If listener <code>l</code> is <code>null</code>, no exception is thrown and no action is performed.
     *
     * @param l the <code>StatusListener</code>
     * @see StatusListener
     * @see #removeStatusListener(StatusListener)
     */
    public void addStatusListener(StatusListener l) {
	if (l != null) observers.add(l);
    }

    /**
     * Removes the specified <code>StatusListener</code> so that it no longer receives song and connection updates from this <code>AudioManager</code>. 
     * This method performs no function, nor does it throw an exception, if the listener specified by the argument was not previously added to this <code>AudioManager</code>.
     * If listener <code>l</code> is <code>null</code>, no exception is thrown and no action is performed.
     *
     * @param l the <code>StatusListener</code>
     * @see StatusListener
     * @see #addStatusListener(StatusListener)
     */
    public void removeStatusListener(StatusListener l) {
	if (l != null) observers.remove(l);
    }
}
