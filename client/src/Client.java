/**
 * The <code>Client</code> class is the entry point to the erlStream client program. It starts a user interface for the <code>AudioManager</code> class.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see UI
 * @see CLI
 * @see GUI
 */
public class Client {
    /**
     * Starts a user interface for the <code>AudioManager</code> class.
     */
    public static void main(String[] args) {
	UI ui;

	if (args.length > 0 && args[0].equals("cli")) {
	    ui = new CLI();
	} else {
	    ui = new GUI();
	}

	ui.start();
    }
}
