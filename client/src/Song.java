/**
 * The Song class represents meta data about a song.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 */
public class Song {
    private String filename, title, album, artist;
    private int duration;

    /**
     * Initializes a newly created Song object.
     *
     * @param filename The filename of the song
     * @param title The title of the song
     * @param album The album of the song
     * @param artist The artist of the song
     * @param duration The duration of the song in seconds
     */
    public Song(String filename, String title, String album, String artist, int duration) {
	this.filename = filename;
	this.title = title;
	this.album = album;
	this.artist = artist;
	this.duration = duration;
    }

    /**
     * Returns the filename of this song.
     *
     * @return The filename of this song
     */
    public String getFileName() {
	return this.filename;
    }

    /**
     * Returns the title of this song.
     *
     * @return The title of this song
     */
    public String getTitle() {
	return this.title;
    }

    /**
     * Returns the album of this song.
     *
     * @return The album of this song
     */
    public String getAlbum() {
	return this.album;
    }

    /**
     * Returns the artist of this song.
     *
     * @return The artist of this song
     */
    public String getArtist() {
	return this.artist;
    }

    /**
     * Returns the duration of this song.
     *
     * @return The duration of this song in seconds
     */
    public int getDuration() {
	return this.duration;
    }

    /**
     * Returns the duration of this song.
     *
     * @return The duration of this song in hh:mm:ss format
     */
    public String getDurationString() {
	int hours = duration / 3600;
	int minutes = (duration % 3600) / 60;
	int seconds = duration % 60;

	return twoDigitString(hours) + ":" + twoDigitString(minutes) + ":" + twoDigitString(seconds);
    }

    private String twoDigitString(int number) {
	if (number / 10 == 0) return "0" + number;
	return String.valueOf(number);
    }

    /**
     * Returns the filename of this song.
     *
     * @return The filename of this song
     */
    public String toString() {
	return this.filename;
    }
    
    /**
     * Compares two songs for equality.
     *
     * @return true if this song is equal to song, false otherwise
     */
    public boolean equals(Song song) {
	return (song.getFileName().equals(getFileName()) && song.getTitle().equals(getTitle()) &&
		song.getAlbum().equals(getAlbum()) && song.getArtist().equals(getArtist()) &&
		song.getDuration() == getDuration());
    }
}
