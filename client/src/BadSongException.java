/**
 * Signals that an AudioManager was given a song that does not exist in it's library.
 *
 * @see AudioManager
 */
public class BadSongException extends Exception {
    public BadSongException(String message) {
	super(message);
    }
}