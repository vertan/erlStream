import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.AdvancedPlayer;
import java.io.*;
import java.net.*;

/**
 * The Communicator class provides methods to list and play audio files on a server that is running the erlStream server program.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 */
public class Communicator {
    private String address;
    private int inPort, outPort;

    private Socket inSocket, outSocket;
    private DataOutputStream toServer;
    private BufferedReader fromServer;
    
    /**
     * Initializes a newly created Communicator object.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public Communicator(String address, int inPort, int outPort) throws Exception {
	this.address = address;
	this.inPort = inPort;
	this.outPort = outPort;
    }

    /*
     * Returns filename without it's extension.
     */
    private String getBaseName(String filename) {
	String[] splitname = filename.split("\\.(?=[^\\.]+$)");
	return splitname[0];
    }

    /*
     * Initializes the variables inSocket, outSocket, toServer and fromServer.
     */
    private void initConnection() throws Exception {
	inSocket = new Socket(address, inPort);
	outSocket = new Socket(address, outPort);
	toServer = new DataOutputStream(outSocket.getOutputStream());
	fromServer = new BufferedReader(new InputStreamReader(inSocket.getInputStream()));
    }

    /**
     * Checks whether the server is reachable or not.
     *
     * @return true if the server is reachable, false otherwise
     */
    public boolean ping() {
	try {
	    initConnection();
	}
	catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	    return false;
	}
	return true;
    }
    
    /**
     * Returns the available songs on the server.
     *
     * @return Song objects representing the songs available on the server
     * @see Song
     */
    public List<Song> list() throws Exception {
	initConnection();
	toServer.writeBytes("list");
	outSocket.close();
	
	List<Song> songs = new ArrayList<Song>();
	String filename;

	while((filename = fromServer.readLine()) != null) {
	    songs.add(new Song(getBaseName(filename), "Unknown Title", "Unknown Album", "Unknown Artist", filename.length() * 16));
	}

	return songs;
    }

    /**
     * Plays the given song if it exists on the server.
     *
     * @param song The song to play 
     * @param offset The number of milliseconds to skip ahead
     * @return true if the file exists on the server and the playback succeeded, false otherwise
     * @see Song
     */
    public InputStream play(Song song, int offset) throws Exception {
	initConnection();
	toServer.writeBytes("play " + song.getFileName() + ".mp3 " + offset);
	outSocket.close();
    
	return inSocket.getInputStream();
    }

    /**
     * Pauses the currently playing song.
     */
    public void pause() {
	// TODO: Implement the method...
    }
}
