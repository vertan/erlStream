/**
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * The Song class stores meta data about a song.
 */
public class Song {
    private String filename, title, album, artist;
    private int length;

    public Song(String filename, String title, String album, String artist, int length) {
	this.filename = filename;
	this.title = title;
	this.album = album;
	this.artist = artist;
	this.length = length;
    }

    /**
     * Returns the filename of the song.
     *
     * @return The filename of the song
     */
    public String getFileName() {
	return this.filename;
    }

    /**
     * Returns the title of the song.
     *
     * @return The title of the song
     */
    public String getTitle() {
	return this.title;
    }

    /**
     * Returns the album of the song.
     *
     * @return The album of the song
     */
    public String getAlbum() {
	return this.album;
    }

    /**
     * Returns the artist of the song.
     *
     * @return The artist of the song
     */
    public String getArtist() {
	return this.artist;
    }

    /**
     * Returns the length of the song.
     *
     * @return The length of the song in seconds
     */
    public int getLength() {
	return this.length;
    }
}
