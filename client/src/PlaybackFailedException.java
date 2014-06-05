/**
 * Thrown to indicate that an <code>AudioManager</code> encountered an error when trying to play a song.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see AudioManager
 */
public class PlaybackFailedException extends Exception {
    /**
     * Constructs a new <code>PlaybackFailedException</code> with no detail message.
     */
    public PlaybackFailedException() {
	super();
    }

    /**
     * Constructs a new <code>PlaybackFailedException</code> with the specified detail message.
     *
     * @param message the detail message
     */
    public PlaybackFailedException(String message) {
	super(message);
    }
}