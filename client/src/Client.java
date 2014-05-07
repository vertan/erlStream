/**
 * The Client class represents a music player.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see UI
 * @see CLI
 * @see GUI
 */
public class Client {
    public static void main(String[] args) {
	UI ui;

	//if (args.length > 0 && args[0].equals("gui")) {
	//ui = new GUI();
	//} else {
	ui = new CLI();
	//}

	ui.start();
    }
}
