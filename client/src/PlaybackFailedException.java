/**
 * Signals that an error occured when trying to play audio.
 *
 * @see AudioManager
 */
public class PlaybackFailedException extends Exception {
    public PlaybackFailedException(String message) {
	super(message);
    }
}