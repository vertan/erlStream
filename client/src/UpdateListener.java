import java.util.List;

/**
 * This interface is used to receive updates from the <code>Communicator</code> class.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see Communicator
 */
public interface UpdateListener {
    /**
     * Invoked if the <code>Communicator</code> receives an updated song list from the server.
     *
     * @param newSongs the songs available on the server
     */
    public void songsUpdated(List<Song> newSongs);

    /**
     * Invoked if the server is shut down.
     */
    public void serverShutdown();

    /**
     * Invoked if the <code>Communicator</code> loses its connection to the server.
     */
    public void connectionLost();

    /**
     * Invoked if the <code>Communicator</code> regains connection to the server after being disconnected.
     *
     * @param songs the songs available on the server
     */
    public void connectionRegained(List<Song> songs);
}

