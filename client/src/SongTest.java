import static org.junit.Assert.*;
import org.junit.Test;

public class SongTest {    
    @Test
	public void testSong() {
	Song s = new Song("Filename", "Title", "Album", "Artist", 100);

	assertTrue(s.getClass() != null);	
    }

    @Test
	public void testSecondsToString() {
	assertTrue(Song.secondsToString(40).equals("0:40"));
    }

    @Test
	public void testEquals() {
	Song s1 = new Song("Filename", "Title", "Album", "Artist", 100);
	Song s2 = new Song("Filename", "Title", "Album", "Artist", 100);
	Song s3 = new Song("Different", "Title", "Album", "Artist", 100);
	Song s4 = new Song("Filename", "Different", "Album", "Artist", 100);
	Song s5 = new Song("Filename", "Title", "Different", "Artist", 100);
	Song s6 = new Song("Filename", "Title", "Album", "Different", 100);
	Song s7 = new Song("Filename", "Title", "Album", "Artist", 0);

	assertTrue(s1.equals(s2));
	assertFalse(s1.equals(s3));
	assertFalse(s1.equals(s4));
	assertFalse(s1.equals(s5));
	assertFalse(s1.equals(s6));
	assertFalse(s1.equals(s7));
    }
}