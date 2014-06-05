/**
 * The Song class is used to store meta data about a song.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 */
public class Song {
    private String title, album, artist;
    private int duration;

    /**
     * Initializes a newly created Song object.
     *
     * @param title the title of the song
     * @param album the album of the song
     * @param artist the artist of the song
     * @param duration the duration of the song in seconds
     */
    public Song(String title, String album, String artist, int duration) {
	this.title = title;
	this.album = album;
	this.artist = artist;
	this.duration = duration;
    }

    /**
     * Returns the title of this song.
     *
     * @return the title of this song
     */
    public String getTitle() {
	return this.title;
    }

    /**
     * Returns the album of this song.
     *
     * @return the album of this song
     */
    public String getAlbum() {
	return this.album;
    }

    /**
     * Returns the artist of this song.
     *
     * @return the artist of this song
     */
    public String getArtist() {
	return this.artist;
    }

    /**
     * Returns the duration of this song.
     *
     * @return the duration of this song in seconds
     */
    public int getDuration() {
	return this.duration;
    }

    /**
     * Returns the duration of this song.
     *
     * @return the duration of this song in m:ss format
     */
    public String getDurationString() {
	return secondsToString(duration);
    }

    /**
     * Converts a number of seconds to a String in m:ss format.
     *
     * @param seconds the number of seconds to convert
     * @return the given number of seconds in m:ss format
     */
    public static String secondsToString(int seconds) {
	// int hours = seconds / 3600;
	int minutes = (seconds % 3600) / 60;
	seconds = seconds % 60;

	return /* twoDigitString(hours) + ":" + */ minutes + ":" + twoDigitString(seconds);
    }

    private static String twoDigitString(int number) {
	if (number / 10 == 0) return "0" + number;
	return String.valueOf(number);
    }

    /**
     * Returns the string repesentation of this song. A song is represented by its title.
     *
     * @return the string representation of this song
     */
    public String toString() {
	return getTitle();
    }
    
    /**
     * Compares two songs for equality.
     *
     * @return <code>true</code> if this song is equal to song, <code>false</code> otherwise
     */
    public boolean equals(Song song) {
	return (song.getTitle().equals(getTitle()) && song.getAlbum().equals(getAlbum())
		&& song.getArtist().equals(getArtist()) && song.getDuration() == getDuration());
    }
}
