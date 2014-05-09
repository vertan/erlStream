import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.*;
import java.net.*;

/**
 * The CLI class provides a Command Line Interface for the AudioManager module.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class CLI extends UI {
    private AudioManager player;
    private boolean quitPending, connected;
    private Scanner sc;

    /**
     * Initializes a newly created CLI object.
     */
    public CLI() {
	sc = new Scanner(System.in);
    }

    /*
     * Prompts for server address and returns true if a connection is established.
     */
    private boolean setupConnection() {
	return connect(new ArrayList<String>());
    }

    /*
     * Connects to a server and returns true if succeeded.
     */
    private boolean connect(List<String> arguments) {
	if (arguments.size() > 1) {
	    printUsage("connect / connect <address>");
	    return false;
	}

	String address = "localhost";

	if (arguments.size() > 0) {
	    address = arguments.get(0);
	} else {
	    System.out.print("Server address (Press RETURN for localhost): ");
	    String input = sc.nextLine();
	    
	    if (!input.equals("")) {
		address = input;
	    }
	}

	System.out.print("Connecting... ");

	try {
	    player = new AudioManager(address, 1341, 1340);
	} catch (Throwable e) {
	    System.out.println("fail!");
	    connected = false;
	}

	System.out.println("connected!");
	connected = true;

	return connected;
    }

    /*
     * Prints a string prepended with "Error: ".
     */
    private void printError(String message) {
	System.out.println("Error: " + message);
    }

    /*
     * Prints a string prepended with "Usage: ".
     */
    private void printUsage(String instruction) {
	System.out.println("Usage: " + instruction);
    }

    /*
     * Prints the available commands.
     */
    private void printHelp() {
	String format = "%-14s %s\n";

	System.out.println("");
	System.out.println("Available commands");
	System.out.println("------------------");
	if (connected) {
	    System.out.printf(format, "ls", "List available songs");
	    System.out.printf(format, "play <Title>", "Play song");
	    System.out.printf(format, "pause", "Pause the current song");
	    System.out.printf(format, "next", "Play the next song in the queue");
	    System.out.printf(format, "previous",  "Play the previous song in the queue");
	    System.out.printf(format, "stop", "Stop the playback");
	    System.out.printf(format, "status", "Get playback status");
	    System.out.printf(format, "shuffle", "Toggle shuffle mode");
	    System.out.printf(format, "repeat", "Toggle repeat mode");
	}
	System.out.printf(format, "help", "Show this dialog");
	System.out.printf(format, "connect", "Change server");
	System.out.printf(format, "quit", "Exit the client");
	System.out.println("");
    }

    /*
     * Returns a list with the words in a string.
     */
    private List<String> stringToCommand(String input) {
	String[] splitInput = input.trim().split("\\s+");

	List<String> command = new ArrayList<String>();

	for (String s : splitInput) {
	    command.add(s);
	}

	return command;
    }

    /*
     * Prints the given songs in a nice fashion.
     */
    private void printSongTable(List<Song> songs) {
	String[] headers = {"Title", "Artist", "Album", "Duration"};

	int longestFileName = headers[0].length();
	int longestArtist = headers[1].length();
	int longestAlbum = headers[2].length();
	int longestDuration = headers[3].length();
	int longestIndex = String.valueOf(songs.size()).length() + 1;
	
	for (Song song : songs) {
	    longestFileName = song.getFileName().length() > longestFileName ? song.getFileName().length() : longestFileName;
	    longestArtist = song.getArtist().length() > longestArtist ? song.getArtist().length() : longestArtist;
	    longestAlbum = song.getAlbum().length() > longestAlbum ? song.getAlbum().length() : longestAlbum;
	}

	String format = "%-" + longestIndex + "s " + "%-" + longestFileName + "s  " + "%-" + 
	    longestArtist + "s  " + "%-" + longestAlbum + "s  " + "%" + longestDuration + "s";

	String top = String.format(format, "", headers[0], headers[1], headers[2], headers[3]);

	String line = "";
	for (int i = 0; i < top.length(); i++) {
	    line += "-";
	}

	System.out.println("");
	System.out.println(top);
	System.out.println(line);
	
	for (int i = 0; i < songs.size(); i++) {
	    System.out.printf(format + "\n", (i+1) + ".", songs.get(i).getFileName(), songs.get(i).getArtist(),
			      songs.get(i).getAlbum(), songs.get(i).getDurationString());
	}

	System.out.println(line);
	format = "%" + top.length() + "s\n";
	String shuffle = "[Shuffle " + (player.shuffleIsOn() ? "on" : "off") + "]";
	String repeat = "[Repeat " + (player.repeatIsOn() ? "on" : "off") + "]";
	System.out.printf(format, shuffle + " " + repeat);

	System.out.println("");
    }

    /*
     * Prints a list with the available songs.
     */
    private void list() {
	List<Song> songs;
	
	try {
	    songs = player.getSongs();
	} catch (ConnectException e) {
	    printError("Failed to connect to server!");
	    return;
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	    return;
	}

	printSongTable(songs);
    }
    
    /*
     * Plays a song.
     */
    private void play(List<String> arguments) {
	if (arguments.isEmpty()) {
	    try {
		player.play();
		Song song = player.getCurrentSong();
		String elapsedTime = Song.secondsToString(player.getPosition());
		System.out.println("Playing: \"" + song.getFileName() + "\" by " + song.getArtist() + 
				   " [" + elapsedTime + " / " + song.getDurationString() + "]");
		return;
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	}

	if (arguments.size() > 2) {
	    printUsage("play / play <Title> / play <Title> <time>");
	    return;
	}
	
	int offset = 0;
	
	if(arguments.size() > 1) {
	    try {
		offset = Integer.parseInt(arguments.get(1));
	    } catch (NumberFormatException e) {
		printUsage("play / play <Title> / play <Title> <time>");
		return;
	    }
	}	
	
	try {
	    player.playSongByTitle(arguments.get(0), offset);
	    Song song = player.getCurrentSong();
	    String elapsedTime = Song.secondsToString(player.getPosition());
	    System.out.println("Playing: \"" + song.getFileName() + "\" by " + song.getArtist() + 
			       " [" + elapsedTime + " / " + song.getDurationString() + "]");
	} catch (AudioManager.BadSongException e) {
	    System.out.println("There is no song titled \"" + arguments.get(0) + "\".");
	} catch (ConnectException e) {
	    printError("Failed to connect to server!");
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
    }

    /*
     * Pauses the current song.
     */
    private void pause() {
	if (player.isPlaying()) {
	    player.pause();
	    System.out.println("Playback paused.");
	} else if (player.isPaused()) {
	    System.out.println("Already paused.");
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Stops the current song.
     */
    private void stop() {
	if (player.isPlaying() || player.isPaused()) {
	    try {
		player.stop();
		System.out.println("Playback stopped.");
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Prints status information.
     */
    private void status() {
	if (player.isPlaying() || player.isPaused()) {
	    Song song = player.getCurrentSong();
	    String state = player.isPlaying() ? "Playing" : "Paused";
	    String elapsedTime = Song.secondsToString(player.getPosition());
	    System.out.println(state + ": \"" + song.getFileName() + "\" by " + song.getArtist() + 
			       " [" + elapsedTime + " / " + song.getDurationString() + "]");
	} else {
	    System.out.println("Stopped.");
	}
    }

    /*
     * Plays the next song.
     */
    private void next() {
	if (player.isPlaying()) {
	    try {
		player.next();
		Song song = player.getCurrentSong();
		
		if (song == null) {
		    System.out.println("Playback Stopped.");
		} else {
		    String elapsedTime = Song.secondsToString(player.getPosition());
		    System.out.println("Playing: \"" + song.getFileName() + "\" by " + song.getArtist() + 
				       " [" + elapsedTime + " / " + song.getDurationString() + "]");
		}
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Plays the next song.
     */
    private void previous() {
	if (player.isPlaying()) {
	    try {
		player.previous();
		Song song = player.getCurrentSong();
		String elapsedTime = Song.secondsToString(player.getPosition());
		System.out.println("Playing: \"" + song.getFileName() + "\" by " + song.getArtist() + 
				   " [" + elapsedTime + " / " + song.getDurationString() + "]");
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Toggles shuffle mode.
     */
    private void shuffle() {
	player.setShuffle(!player.shuffleIsOn());
	System.out.println("Shuffle mode " + (player.shuffleIsOn() ? "on" : "off") + ".");
    }

    /*
     * Toggles repeat mode.
     */
    private void repeat() {
	player.setRepeat(!player.repeatIsOn());
	System.out.println("Repeat mode " + (player.repeatIsOn() ? "on" : "off") + ".");
    }

    /*
     * Parses the users input and takes appropriate action.
     */
    private void parseInput(String input) {
	List<String> command = stringToCommand(input);
	if (command.size() == 0) return;
	
	switch(command.get(0).toLowerCase()) {
	case "ls":
	    list();
	    break;
	case "play":
	    command.remove(0);
	    play(command);
	    break;
	case "pause":
	    pause();
	    break;
	case "stop":
	    stop();
	    break;
	case "next":
	    next();
	    break;
	case "previous":
	    previous();
	    break;
	case "status":
	    status();
	    break;
	case "shuffle":
	    shuffle();
	    break;
	case "repeat":
	    repeat();
	    break;
	case "help":
	    printHelp();
	    break;
	case "connect":
	    command.remove(0);
	    connect(command);
	    break;
	case "quit":
	    player.stop();
	    quitPending = true;
	    break;
	case "":
	    break;
	default:
	    System.out.println("Unknown command '" + command.get(0).toLowerCase() + 
			       "'. Type 'help' to see available options.");
	}	
    }

    /**
     * Starts the CLI.
     */
    public void start() {
	setupConnection();

	if (connected) {
	    printHelp();
	    list();

	    while(!quitPending) {
		System.out.print("> ");
		parseInput(sc.nextLine());
	    }
	}

	sc.close();
	
	System.out.println("Client exited.");
    }
}
