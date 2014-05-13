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
}