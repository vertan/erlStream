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

    public String getFileName() {
	return this.filename;
    }

    public String getTitle() {
	return this.title;
    }

    public String getAlbum() {
	return this.album;
    }

    public String getArtist() {
	return this.artist;
    }

    public int getLength() {
	return this.length;
    }
}
