/**
 * The <code>UI</code> interface should be implemented by any class that intends to be used as a user interface to the <code>AudioManager</code> class. The class must define a method of no arguments called <code>start</code>.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 * @see AudioManager
 */
public interface UI {
    /**
     * Starts the UI.
     */
    public void start();
}