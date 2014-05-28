import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.AdvancedPlayer;
import java.io.*;
import java.net.*;

/**
 * The Communicator class provides methods to list and play audio files on a server that is running the erlStream server program.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see UpdateListener
 */
public class Communicator {
    private class ListenThread implements Runnable {
	private volatile boolean running = true;

	public void run() {
	    String data;
	    
	    while(running) {
		try {
		    data = fromServer.readLine();

		    if (data == null) {
			connected = false;
			for (UpdateListener observer : observers) {
			    observer.connectionLost();
			}
			return;
		    }

		    List<String> message = messageToList(data);

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
			return;
		    default:
			;
		    }
		} catch (IOException e) {
		    // System.out.println("Error while reading from server!");
		}
	    }
	}

	public void stop() {
	    running = false;
	}
    }

    private String address;
    private int port;

    private Socket connection;
    private DataOutputStream toServer;
    private BufferedReader fromServer;

    private ListenThread listener;
    private List<UpdateListener> observers;
    
    private boolean connected = false;

    /**
     * Initializes a newly created Communicator object and requests songs.
     *
     * @param address The address to the server to communicate with
     * @param inPort The port to read data from
     * @param outPort The port to write data to
     */
    public Communicator(String address, int port) {
	this.address = address;
	this.port = port;
	observers = new ArrayList<UpdateListener>();
    }

    /**
     * Attempts to connect to the given server, and returns Song-object for each available song on the server.
     */
    public List<Song> connect() throws UnknownHostException, IOException {
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
	listener = new ListenThread();
	new Thread(listener).start(); // Hand the listening over to a separate thread
	return songs;
    }

    /*
     * Reads from fromServer and constructs a list of songs
     */
    private List<Song> readSongs() throws IOException {
	List<Song> songs = new ArrayList<Song>();
	String fileinfo;

	while((fileinfo = fromServer.readLine()) != null && !fileinfo.equals("end")) {
	    String[] split = fileinfo.split(":");
	    String name = split[0];
	    String title = split[1];
	    String artist = split[2];
	    String album = split[3];
	    int duration = Integer.parseInt(split[4]);
	    songs.add(new Song(name, title, album, artist, duration));
	}

	return songs;
    }

    /**
     * Returns an InputStream object containing the data for the given song.
     *
     * @param song The song which data to return
     * @param offset The number of milliseconds to skip ahead
     * @return true if the file exists on the server and the playback succeeded, false otherwise
     * @see Song
     */
    public InputStream play(Song song, int offset) throws UnknownHostException, IOException {
	Socket temp = new Socket(address, port);
	DataOutputStream out = new DataOutputStream(temp.getOutputStream());
	BufferedReader in = new BufferedReader(new InputStreamReader(temp.getInputStream()));

	out.writeBytes("play:" + offset + ":" + song.getFileName() + "\n");
    
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

    private List<String> messageToList(String input) {
	String[] splitInput = input.split(":");
	
	List<String> message = new ArrayList<String>();
	
	for (String s : splitInput) {
	    message.add(s);
	}
	
	return message;
    }
    
    /**
     * Returns true if this Communicator is connected to a server.
     *
     * @return true if this Communicator is connected to a server, false otherwise
     */
    public boolean isConnected() {
	return connected;
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

    /**
     * Closes the connection to the server.
     */
    public void disconnect() {
	if (listener != null) listener.stop();

	try {
	    toServer.writeBytes("disconnect\n");
	    toServer.close();
	} catch (IOException e) {
	}

	try {
	    connection.close();
	} catch (IOException e) {
	    e.printStackTrace();
	} catch (NullPointerException e) {
	    // Should not happen?
	}
    }

    public void addUpdateListener(UpdateListener observer) {
	observers.add(observer);
    }

    public void removeUpdateListener(UpdateListener observer) {
	observers.remove(observer);
    }
}
