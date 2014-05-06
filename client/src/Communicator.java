import java.util.List;
import java.util.ArrayList;
import javazoom.jl.player.advanced.AdvancedPlayer;
import java.io.*;
import java.net.*;

/**
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * This class communicates with a server
 */
public class Communicator {
    private String address;
    private int inPort;
    private int outPort;

    private Socket inSocket, outSocket;
    private DataOutputStream toServer;
    private BufferedReader fromServer;
    
    public Communicator(String address, int inPort, int outPort) {
	this.address = address;
	this.inPort = inPort;
	this.outPort = outPort;
    }

    private String getBaseName(String filename) {
	String[] splitname = filename.split("\\.(?=[^\\.]+$)");
	return splitname[0];
    }
    
    /**
     * Returns a list containing the the available songs on the server
     *
     * @return The songs available on the server
     * @see Song
     */
    public List<Song> list() throws Exception {
	inSocket = new Socket(address, inPort);
	outSocket = new Socket(address, outPort);
	toServer = new DataOutputStream(outSocket.getOutputStream());
	fromServer = new BufferedReader(new InputStreamReader(inSocket.getInputStream()));
	
	toServer.writeBytes("list");
	outSocket.close();
	
	List<Song> songs = new ArrayList<Song>();

	String answer = fromServer.readLine();
	
	while(answer != null) {
	    songs.add(new Song(getBaseName(answer), "Unknown Title", "Unknown Artist", "Unknown Album", 60));
	    answer = fromServer.readLine();
	}

	return songs;
    }

    /**
     * Plays the song song if it exists on the server.
     *
     * @param song The song to play 
     * @param time The number of seconds to skip
     * @return true if the file existed on the server and the playback succeeded, else false
     */
    public boolean play(Song song, int time) throws Exception {
	inSocket = new Socket(address, inPort);
	outSocket = new Socket(address, outPort);
	toServer = new DataOutputStream(outSocket.getOutputStream());
	fromServer = new BufferedReader(new InputStreamReader(inSocket.getInputStream()));
    
	toServer.writeBytes("play " + song.getFileName() + ".mp3 " + time);
	outSocket.close();
    
	InputStream audioData = inSocket.getInputStream();
	AdvancedPlayer musicPlayer = new AdvancedPlayer(audioData);
	musicPlayer.play();

	// TODO: Check whether playback succeeded
	return true;
    }

    /**
     * Pauses the current playing song.
     */
    public void pause() {
	// TODO: Comm
    }
}
