/**
 * The Client class represents a music player with either a graphical or a command line interface.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see CLI
 * @see GUI
 */
public class Client {
    public static void main(String[] args) {
	if (args.length > 0 && args[0].equals("gui")) {
	    //new GUI().start();
	} else {
	    new CLI().start();
	}
    }
}
