public class Song {
    private String title, album, artist;
    private int length;

    public Song(String title, String album, String artist, int length) {
	this.title = title;
	this.album = album;
	this.artist = artist;
	this.length = length;
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
