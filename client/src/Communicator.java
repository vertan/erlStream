import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

/**
 * The <code>Communicator</code> class provides methods to connect to and fetch audio data from a server that is running the erlStream server program.
 *
 * <p>
 * If the connection is lost, the <code>Communicator</code> will automatically try to reconnect.
 * </p>
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>
 * @author Filip Hedman <hedman.filip@gmail.com>
 * @author Robert Kallgren <robertkallgren@gmail.com>
 * @author Oscar Mangard <oscarmangard@gmail.com>
 * @author Mikael Sernheim <mikael.sernheim@gmail.com>
 */
public class Communicator {
    /*
     * This class is used to listen for messages from the server and notify the Communicators observers.
     */
    private class ListenThread extends Thread {
	public void run() {
	    String data;
	    List<String> message;
	    
	    while(listening) {
		try {
		    data = fromServer.readLine();

		    if (data == null) {
			if (listening) { // If not listening, disconnect has been called
			    connected = false;
			    for (UpdateListener observer : observers) {
				observer.connectionLost();
			    }
			    fromServer.close();
			    retryConnection();
			}
			continue;
		    }

		    message = messageToList(data);

		    switch(message.get(0)) {
		    case "update":
			List<Song> songs = readSongs();
			for (UpdateListener observer : observers) {
			    observer.songsUpdated(songs);
			}
			break;
		    case "exit":
			connected = false;
			for (UpdateListener observer : observers) {
			    observer.serverShutdown();
			}
			fromServer.close();
			retryConnection();
			continue;
		    default:
			;
		    }
		} catch (IOException e) {
		}
	    }
	}

	/*
	 * Retries the connection to the server with a set interval.
	 */
	private void retryConnection() {
	    while (!connected && listening) {
		try {
		    Thread.sleep(5000);
		    List<Song> songs = setupConnection();
		    for (UpdateListener observer : observers) {
			observer.connectionRegained(songs);
		    }
		} catch (Exception e) {
		}
	    }
	}
    }

    private InetAddress address;
    private int port;
    private Socket connection;
    private DataOutputStream toServer;
    private volatile BufferedReader fromServer;
    private Thread listenThread;
    private List<UpdateListener> observers;

    private volatile boolean listening;
    private volatile boolean connected = false;

    /**
     * Initializes a newly created <code>Communicator</code> object.
     *
     * @param address the address to the server
     * @param port the port to the server
     * @throws UnknownHostException if the given address could not be determined
     */
    public Communicator(String address, int port) throws UnknownHostException {
	this.address = InetAddress.getByName(address);
	this.port = port;
	observers = new ArrayList<UpdateListener>();
    }

    /**
     * Connects this <code>Communicator</code> to the server. If already connected, the old connection is closed and a new one is created.
     *
     * @return the songs available on the server
     * @throws UnknownHostException if the address given when this <code>Communicator</code> was created could not be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     * @see Song
     */
    public List<Song> connect() throws UnknownHostException, IOException {
	if (connected) disconnect();

	List<Song> songs = setupConnection();
	listenThread = new ListenThread();
	listening = true;
	listenThread.start(); // Hand the listening over to a separate thread
	return songs;
    }

    /*
     * Initializes the socket and streams, connects to the server and returns the available songs.
     */
    private List<Song> setupConnection() throws UnknownHostException, IOException {
	connection = new Socket(address, port);
	toServer = new DataOutputStream(connection.getOutputStream());
	fromServer = new BufferedReader(new InputStreamReader(connection.getInputStream()));

	toServer.writeBytes("connect:Java erlStream Client\n");

	List<String> message = messageToList(fromServer.readLine());
	if (message.size() == 2 && message.get(0).equals("connect")) {
	    switch(message.get(1)) {
	    case "ok":
		break;
	    default:
		throw new IOException();
	    }
	} else {
	    throw new IOException();
	}

	List<Song> songs = readSongs();
	connected = true;
	return songs;
    }

    /*
     * Reads from fromServer and constructs a list of songs.
     */
    private List<Song> readSongs() throws IOException {
	List<Song> songs = new ArrayList<Song>();
	String fileinfo;

	while((fileinfo = fromServer.readLine()) != null && !fileinfo.equals("end")) {
	    String[] split = fileinfo.split(":");
	    String title = split[0];
	    String artist = split[1];
	    String album = split[2];
	    int duration = Integer.parseInt(split[3]);
	    songs.add(new Song(title, album, artist, duration));
	}

	return songs;
    }

    /**
     * Returns a stream object from the server containing the data for the given song. The stream should be closed after use to avoid resource leaks.
     *
     * @param song the song which data to return
     * @param offset the number of milliseconds to skip ahead
     * @return a stream containing the data for the given song if the server request succeeded, an empty stream otherwise
     * @throws UnknownHostException if the address given when this <code>Communicator</code> was created could not be determined
     * @throws IOException if an I/O error occurs when connecting to the server
     */
    public InputStream play(Song song, int offset) throws UnknownHostException, IOException {
	Socket temp = new Socket(address, port);
	DataOutputStream out = new DataOutputStream(temp.getOutputStream());
	BufferedReader in = new BufferedReader(new InputStreamReader(temp.getInputStream()));

	out.writeBytes("play:" + offset + ":" + song.getTitle() + "\n");
    
	List<String> message = messageToList(in.readLine());
	if (message.size() >= 2 && message.get(0).equals("play")) {
	    switch(message.get(1)) {
	    case "ok":
		break;
	    case "error":
		System.out.println("Play error: " + message.get(2));
	    }
	}

	// If an error occured, this stream should be empty and closed on the server side
	return temp.getInputStream();
    }

    /*
     * Returns input as a list with elements split with the delimiter ":".
     */
    private List<String> messageToList(String input) {
	String[] splitInput = input.split(":");
	
	List<String> message = new ArrayList<String>();
	
	for (String s : splitInput) {
	    message.add(s);
	}
	
	return message;
    }
    
    /**
     * Returns <code>true</code> if this <code>Communicator</code> is connected to the server.
     *
     * @return <code>true</code> if this <code>Communicator</code> is connected to the server, <code>false</code> otherwise
     */
    public boolean isConnected() {
	return connected;
    }

    /**
     * Returns an <code>InetAddress</code> constructed from the address to the server given when this <code>Communicator</code> was created.
     *
     * @return the address to the server
     */
    public InetAddress getAddress() {
	return address;
    }

    /**
     * Returns the port to the server given when this <code>Communicator</code> was created.
     *
     * @return the port to the server
     */
    public int getPort() {
	return port;
    }

    /**
     * Closes the connection to the server. A new connection can be established again using {@link #connect()}.
     *
     * @see #connect()
     */
    public void disconnect() {
	try {
	    toServer.writeBytes("disconnect\n");
	} catch (Exception e) {
	}
	
	if (listenThread != null) {
	    listening = false;
	    
	    listenThread.interrupt(); // In case it's sleeping
	    
	    try {
		fromServer.close(); // To unblock listenThread in case it's reading
	    } catch (Exception e) {
	    }
	    try {
		listenThread.join();
	    } catch (InterruptedException e) {
	    }
	}
	
	// In case listenThread opened a new connection
	try {
	    fromServer.close();
	} catch (Exception e) {
	}

	connected = false;
    }

    /**
     * Adds the specified <code>UpdateListener</code> to receive song and connection updates from this <code>Communicator</code>. 
     * If listener <code>l</code> is <code>null</code>, no exception is thrown and no action is performed.
     *
     * @param l the <code>UpdateListener</code>
     * @see UpdateListener
     * @see #removeUpdateListener(UpdateListener)
     */
    public void addUpdateListener(UpdateListener l) {
	if (l != null) observers.add(l);
    }

    /**
     * Removes the specified <code>UpdateListener</code> so that it no longer receives song and connection updates from this <code>Communicator</code>. 
     * This method performs no function, nor does it throw an exception, if the listener specified by the argument was not previously added to this <code>Communicator</code>.
     * If listener <code>l</code> is <code>null</code>, no exception is thrown and no action is performed.
     *
     * @param l the <code>UpdateListener</code>
     * @see UpdateListener
     * @see #addUpdateListener(UpdateListener)
     */
    public void removeUpdateListener(UpdateListener l) {
	if (l != null) observers.remove(l);
    }
}
