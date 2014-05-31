import java.util.List;

/**
 * This interface is used to receive updates from the Communicator class.
 *
 * @see Communicator
 */
public interface UpdateListener {
    public void songsUpdated(List<Song> newSongs);
    public void serverShutdown();
    public void connectionLost();
    public void connectionRegained(List<Song> songs);
}
