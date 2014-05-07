import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.*;
import java.net.*;

/**
 * The CLI class provides a Command Line Interface for the Communicator module.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class CLI extends UI {
    private Communicator accomodator;
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

	accomodator = new Communicator(address, 1341, 1340);

	System.out.print("Connecting... ");

	// TODO: Verify connection

	if (true) {
	    System.out.println("connected!");
	    connected = true;
	} else {
	    System.out.println("fail!");
	    connected = false;
	}

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
	System.out.println("");
	System.out.println("Available commands");
	System.out.println("------------------");
	if (connected) {
	    System.out.println("list / ls: List available songs");
	    System.out.println("play <songname>: Play song");
	}
	System.out.println("help: Show this dialog");
	System.out.println("connect <address>: Change server");
	System.out.println("quit: Exit the client");
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
	
	for (Song song : songs) {
	    longestFileName = song.getFileName().length() > longestFileName ? song.getFileName().length() : longestFileName;
	    longestArtist = song.getArtist().length() > longestArtist ? song.getArtist().length() : longestArtist;
	    longestAlbum = song.getAlbum().length() > longestAlbum ? song.getAlbum().length() : longestAlbum;
	}

	String format = "%-" + longestFileName + "s  " + "%-" + longestArtist + "s  " + "%-" + 
	    longestAlbum + "s  " + "%" + longestDuration + "s";

	String top = String.format(format, headers[0], headers[1], headers[2], headers[3]);

	String line = "";
	for (int i = 0; i < top.length(); i++) {
	    line += "-";
	}

	System.out.println("");
	System.out.println(top);
	System.out.println(line);
	
	for (Song song : songs) {
	    System.out.printf(format + "\n", song.getFileName(), song.getArtist(),
			      song.getAlbum(), song.getDurationString());
	}

	System.out.println("");
    }

    /*
     * Prints a list with the available songs.
     */
    private void list(List<String> arguments) {
	if (arguments.size() > 0) {
	    printUsage("list / ls");
	    return;
	}
	
	List<Song> songs;
	
	try {
	    songs = accomodator.list();
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
	if (arguments.size() < 1 || arguments.size() > 2) {
	    printUsage("play <songname> / play <songname> <time>");
	    return;
	}
	
	Song song = new Song(arguments.get(0), "Unknown Title", "Unknown Artist", "Unknown Album", 60);
	int offset = 0;
	
	if(arguments.size() > 1) {
	    try {
		offset = Integer.parseInt(arguments.get(1));
	    } catch (NumberFormatException e) {
		printUsage("play <songname> / play <songname> <time>");
		return;
	    }
	}	
	
	try {
	    accomodator.play(song, offset);
	} catch (ConnectException e) {
	    printError("Failed to connect to server!");
	} catch (Throwable e) {
	    System.out.println("Error " + e.getMessage());
	    e.printStackTrace();
	}
    }

    /*
     * Parses the users input and takes appropriate action.
     */
    private void parseInput(String input) {
	List<String> command = stringToCommand(input);
	if (command.size() == 0) return;
	
	switch(command.get(0).toLowerCase()) {
	case "list":
	    command.remove(0);
	    list(command);
	    break;
	case "ls":
	    command.remove(0);
	    list(command);
	    break;
	case "play":
	    command.remove(0);
	    play(command);
	    break;
	case "help":
	    printHelp();
	    break;
	case "connect":
	    command.remove(0);
	    connect(command);
	    break;
	case "quit":
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

	    while(!quitPending) {
		System.out.print("> ");
		parseInput(sc.nextLine());
	    }
	}
	
	System.out.println("Client exited.");
    }
}
