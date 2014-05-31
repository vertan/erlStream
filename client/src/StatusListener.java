import java.util.List;

/**
 * This interface is used to receive status updates from the AudioManager class.
 *
 * @see AudioManager
 */
public interface StatusListener {
    public void songsUpdated(List<Song> newSongs);
    public void serverShutdown();
    public void connectionLost();
    public void connectionRegained(List<Song> songs);
}
