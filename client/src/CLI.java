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

    /**
     * Starts the CLI.
     */
    public void start() {
	setupConnection();
	help();
	
	while(!quitPending) {
	    System.out.print("> ");
	    parseInput(sc.nextLine());
	}

	sc.close();	
	System.out.println("Client exited.");
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
     * Prints a notification that the user is not connected to a server.
     */
    private void printConnectionMessage() {
	System.out.println("Not connected to a server.");
    }

    /*
     * Parses the users input and takes appropriate action.
     */
    private void parseInput(String input) {
	List<String> command = stringToCommand(input);
	if (command.size() == 0) return;
	
	switch(command.get(0).toLowerCase()) {
      	case "help":
	    help();
	    break;
	case "serverinfo":
	    serverinfo();
	    break;
	case "connect":
	    command.remove(0);
	    connect(command);
	    break;
	case "quit":
	    quit();
	    break;
	case "":
	    break;
	case "ls":
	    list();
	    break;
	case "play":
	    command.remove(0);
	    play(command, 0);
	    break;
	case "playat":
	    command.remove(0);
	    playat(command);
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
	case "sort":
	    command.remove(0);
	    sort(command);
	    break;
	default:
	    System.out.println("Unknown command '" + command.get(0).toLowerCase() + 
			       "'. Type 'help' to see available options.");
	}
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
	    AudioManager test = new AudioManager(address, 1341, 1340);
	    if (player != null) player.stop();
	    player = test;
	    connected = true;
	    System.out.println("connected!");
	} catch (Throwable e) {
	    System.out.println("failed!");
	}

	return connected;
    }

    /*
     * Prints the available commands.
     */
    private void help() {
	String format = "%-22s %s\n";

	System.out.println("");
	System.out.println("Available commands");
	System.out.println("------------------");
	if (connected) {
	    System.out.printf(format, "ls", "List available songs");
	    System.out.printf(format, "play <Title>", "Play song");
	    System.out.printf(format, "playat <time> <Title>", "Play song at the given time (seconds)");
	    System.out.printf(format, "pause", "Pause the current song");
	    System.out.printf(format, "next", "Play the next song in the queue");
	    System.out.printf(format, "previous",  "Play the previous song in the queue");
	    System.out.printf(format, "stop", "Stop the playback");
	    System.out.printf(format, "status", "Show playback status");
	    System.out.printf(format, "sort", "Sort the list");
	    System.out.printf(format, "shuffle", "Toggle shuffle mode");
	    System.out.printf(format, "repeat", "Toggle repeat mode");
	    System.out.printf(format, "serverinfo", "Show server info");
	}
	System.out.printf(format, "help", "Show this dialog");
	System.out.printf(format, "connect", "Change server");
	System.out.printf(format, "quit", "Exit the client");
	System.out.println("");
    }

    /*
     * Prints server information.
     */
    private void serverinfo() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}

	if (player != null) {
	    System.out.println("Server address: " + player.getAddress());
	    System.out.println("In port: " + player.getInPort());
	    System.out.println("Out port: " + player.getOutPort());
	}
    }

    /*
     * Stops the playback and turns the quitPending flag on.
     */
    private void quit() {
	if (player != null) player.stop();
	quitPending = true;	
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

	String format = "%-" + longestIndex + "s " + "%-" + longestFileName + "s   " + "%-" + 
	    longestArtist + "s   " + "%-" + longestAlbum + "s   " + "%" + longestDuration + "s";

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
	if (!connected) {
	    printConnectionMessage();
	    return;
	}

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
    private void play(List<String> arguments, int offset) {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (arguments.isEmpty()) {
	    try {
		player.play();
		status();
	    } catch (Throwable e) {
		System.out.println("Error " + e.getMessage());
		e.printStackTrace();
	    }
	    return;
	}

	// TODO: Allow more than one whitespace in song names?
	String title = arguments.get(0);
	if (arguments.size() > 1) {
	    for (int i = 1; i < arguments.size(); i++) {
		title += " " + arguments.get(i);
	    }	
	}
		
	try {
	    player.playSongByTitle(title, offset);
	    status();
	} catch (AudioManager.BadSongException e) {
	    System.out.println("There is no song titled \"" + title + "\".");
	} catch (ConnectException e) {
	    // TODO: Handle this better
	    printError("Failed to connect to server!");
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
    }
    
    /*
     * Plays a song with an offset.
     */
    private void playat(List<String> arguments) {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (arguments.size() < 2) {
	    printUsage("playat <time> <Title>");
	    return;
	}

	int offset = 0;
	try {
	    offset = Integer.parseInt(arguments.get(0)) * 1000;
	} catch (NumberFormatException e) {
	    printUsage("playat <time> <Title>");
	    return;
	}

	arguments.remove(0);
	play(arguments, offset);
    }

    /*
     * Pauses the current song.
     */
    private void pause() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (player.isPlaying() || player.isPaused()) {
	    player.pause();
	    status();
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Stops the current song.
     */
    private void stop() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (player.isPlaying() || player.isPaused()) {
	    player.stop();
	    System.out.println("Playback stopped.");
	} else {
	    System.out.println("No song is playing.");
	}
    }

    /*
     * Prints status information.
     */
    private void status() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}

	if (player.isPlaying() || player.isPaused()) {
	    Song song = player.getCurrentSong();
	    String state = player.isPlaying() ? "Playing" : "Paused";
	    System.out.println("[" + state + "] \"" + song.getFileName() + "\" by " + song.getArtist());

	    // Position bar
	    int barLength = 40;
	    int passedTime = player.getPosition();
	    double passedPerc = (double) passedTime / song.getDuration();
	    int passedLength = (int) (barLength * passedPerc);

	    System.out.print("[" + Song.secondsToString(passedTime) + "] ");
	    for (int i = 0; i < passedLength; i++) {
		System.out.print("|");
	    }
	    for (int i = 0; i < (barLength - passedLength); i++) {		
		System.out.print("-");
	    }
	    System.out.println(" [" + song.getDurationString() + "]");
	} else {
	    System.out.println("Stopped.");
	}
    }

    /*
     * Plays the next song.
     */
    private void next() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (player.isPlaying() || player.isPaused()) {
	    try {
		player.next();
		Song song = player.getCurrentSong();
		
		if (song == null) {
		    System.out.println("Playback Stopped.");
		} else {
		    status();
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
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	if (player.isPlaying() || player.isPaused()) {
	    try {
		player.previous();
		status();
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
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	player.setShuffle(!player.shuffleIsOn());
	System.out.println("Shuffle mode " + (player.shuffleIsOn() ? "on" : "off") + ".");
    }

    /*
     * Toggles repeat mode.
     */
    private void repeat() {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	player.setRepeat(!player.repeatIsOn());
	System.out.println("Repeat mode " + (player.repeatIsOn() ? "on" : "off") + ".");
    }

    /*
     * Sorts the songs and prints the list.
     */
    private void sort(List<String> arguments) {
	if (!connected) {
	    printConnectionMessage();
	    return;
	}
	
	String usage = "sort <title/artist/album/duration> <asc/desc>";

	if (arguments.size() != 2) {
	    printUsage(usage);
	    return;
	}

	switch(arguments.get(0).toLowerCase()) {
	case "title":
	    switch(arguments.get(1).toLowerCase()) {
	    case "asc":
		player.sort(0);
		System.out.println("Sorting titles in ascending order...");
		break;
	    case "desc":
		player.sort(1);
		System.out.println("Sorting titles in descending order...");
		break;
	    default:
		printUsage(usage);
		return;
	    }
	    break;
	case "artist":
	    switch(arguments.get(1).toLowerCase()) {
	    case "asc":
		player.sort(2);
		System.out.println("Sorting artists in ascending order...");
		break;
	    case "desc":
		player.sort(3);
		System.out.println("Sorting artists in descending order...");
		break;
	    default:
		printUsage(usage);
		return;
	    }
	    break;
	case "album":
	    switch(arguments.get(1).toLowerCase()) {
	    case "asc":
		player.sort(4);
		System.out.println("Sorting albums in ascending order...");
		break;
	    case "desc":
		player.sort(5);
		System.out.println("Sorting albums in descending order...");
		break;
	    default:
		printUsage(usage);
		return;
	    }
	    break;
	case "duration":
	    switch(arguments.get(1).toLowerCase()) {
	    case "asc":
		player.sort(6);
		System.out.println("Sorting durations in ascending order...");
		break;
	    case "desc":
		player.sort(7);
		System.out.println("Sorting durations in descending order...");
		break;
	    default:
		printUsage(usage);
		return;
	    }
	    break;
	default:
	    printUsage(usage);
	    return;
	}

	list();
    }
}
