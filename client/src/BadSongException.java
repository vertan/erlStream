/**
 * Thrown to indicate that an <code>AudioManager</code> was given a song that does not exist in its library.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see AudioManager
 */
public class BadSongException extends Exception {
    /**
     * Constructs a new <code>BadSongException</code> with no detail message.
     */
    public  BadSongException() {
	super();
    }
    
    /**
     * Constructs a new <code>BadSongException</code> with the specified detail message.
     *
     * @param message the detail message
     */
    public BadSongException(String message) {
	super(message);
    }
}