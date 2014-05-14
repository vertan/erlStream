import static org.junit.Assert.*;
import org.junit.Test;

public class CommunicatorTest {    
    @Test
	public void testCommunicator() {
	Communicator c = null;
	
	try {
	    c = new Communicator("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(c.getClass() != null);	
    }

    public void testList() {
	Communicator c = null;
	
	try {
	    c = new Communicator("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(c.getClass() != null);	
    }

    public void testPlay() {
	Communicator c = null;
	
	try {
	    c = new Communicator("localhost", 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
	
	assertTrue(c.getClass() != null);	
    }
}