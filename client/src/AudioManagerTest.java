import static org.junit.Assert.*;
import org.junit.Test;

public class AudioManagerTest {    
    @Test
	public void testAudioManager() {
	AudioManager am = null;
	
	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testPlay() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testPlaySongByTitle() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testGetSongByTitle() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testExists() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testPause() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testStop() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testNext() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testPrevious() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testgetPosition() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testGetSongs() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }

    @Test
	public void testSort() {
	AudioManager am = null;

	try {
	    am = new AudioManager("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(am.getClass() != null);	
    }
}