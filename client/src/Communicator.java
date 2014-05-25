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
    private int port;

    private Socket connection;
    private DataOutputStream toServer;
    private BufferedReader fromServer;

    private List<Song> songs;
    
    /**
     * Initializes a newly created Communicator object and requests songs.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public Communicator(String address, int port) throws Exception {
	this.address = address;
	this.port = port;
	connect(address, port);
    }

    /*
     * Attempts to connect to the given server.
     */
    private void connect(String address, int port) throws Exception {
	connection = new Socket(address, port);
	toServer = new DataOutputStream(connection.getOutputStream());
	fromServer = new BufferedReader(new InputStreamReader(connection.getInputStream()));

	toServer.writeBytes("connect:erlStream Java Client\n");
	
	songs = new ArrayList<Song>();
	String fileinfo;

	while((fileinfo = fromServer.readLine()) != null && !fileinfo.equals("end")) {
	    String[] split = fileinfo.split("\\|");
	    String name = split[0];
	    String title = split[1];
	    String artist = split[2];
	    String album = split[3];
	    int duration = Integer.parseInt(split[4]);
	    songs.add(new Song(name, title, album, artist, duration));
	}
    }

    /**
     * Returns an InputStream object containing the data for the given song.
     *
     * @param song The song which data to return
     * @param offset The number of milliseconds to skip ahead
     * @return true if the file exists on the server and the playback succeeded, false otherwise
     * @see Song
     */
    public InputStream play(Song song, int offset) throws Exception {
	Socket tempSocket = new Socket(address, port);
	DataOutputStream out = new DataOutputStream(tempSocket.getOutputStream());

	out.writeBytes("play:" + offset + ":" + song.getFileName() + "\n");
    
	return tempSocket.getInputStream();
    }

    public List<Song> getSongs() {
	return songs;
    }

    /**
     * Returns the address to the server to communicate with.
     *
     * @return The address to the server to communicate with
     */
    public String getAddress() {
	return address;
    }

    /**
     * Returns the port to the server to communicate with.
     *
     * @return The port to the server to communicate with
     */
    public int getPort() {
	return port;
    }
}
