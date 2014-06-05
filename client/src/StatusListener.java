import java.util.List;

/**
 * This interface is used to receive status updates from the <code>AudioManager</code> class.
 *
 * @see AudioManager
 */
public interface StatusListener {
    /**
     * Invoked if the <code>AudioManager</code> receives an updated song list from the server.
     *
     * @param newSongs the songs available on the server
     */
    public void songsUpdated(List<Song> newSongs);

    /**
     * Invoked if the server is shut down.
     */
    public void serverShutdown();

    /**
     * Invoked if the <code>AudioManager</code> loses its connection to the server.
     */
    public void connectionLost();

    /**
     * Invoked if the <code>AudioManager</code> regains connection to the server after being disconnected.
     *
     * @param songs the songs available on the server
     */
    public void connectionRegained(List<Song> songs);
}
