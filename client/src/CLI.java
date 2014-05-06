import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.*;

/**
 * The CLI class provides a Command Line Interface for the Communicator module.
 *
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * @see Communicator
 */
public class CLI {
    private Communicator accomodator;
    private boolean running, connected;
    private Scanner sc;
    private String input;

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
	String address = "localhost";

	System.out.print("Server address (Press RETURN for localhost): ");
	input = sc.nextLine();

	if (!input.equals("")) {
	    address = input;
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
     * Prints a message prepended with "Error: ".
     */
    private void printError(String message) {
	System.out.println("Error: " + message);
    }

    /*
     * Prints the available commands.
     */
    private void printHelp() {
	System.out.println("");
	System.out.println("Available commands");
	System.out.println("------------------");
	if (connected) {
	    System.out.println("list: List available songs");
	    System.out.println("play <songname>: Play song");
	}
	System.out.println("help: Show this dialog");
	System.out.println("connect: Change server");
	System.out.println("quit: Exit the client");
	System.out.println("");
    }

    /*
     * Runs the CLI.
     */
    private void run() {
	running = true;
	printHelp();
	
	while(running) {
	    System.out.print("> ");
	    input = sc.nextLine();
	    String[] command = input.split(" ");
	    
	    switch(command[0]) {
	    case "list":
		try {
		    List<Song> songs = accomodator.list();		    
		    for (int i = 0; i < songs.size(); i++) {
			System.out.println(songs.get(i).getFileName());
		    }
		} catch (Throwable e) {
		    System.out.println("Error " + e.getMessage());
		    e.printStackTrace();
		}

		break;
	    case "play":
		try {
		    int offset = 0;
		    
		    if(command.length > 2) {
			offset = Integer.parseInt(command[2]);
		    }
		    accomodator.play(new Song(command[1], "Unknown Title", "Unknown Artist", "Unknown Album", 60), offset);
		} catch (Throwable e) {
		    System.out.println("Error " + e.getMessage());
		    e.printStackTrace();
		}
		break;
	    case "help":
		printHelp();
		break;
	    case "connect":
		setupConnection();
		break;
	    case "quit":
		running = false;
		break;
	    default:
		System.out.println("Unknown command, type 'help' to see available options.");
	    }
	}
    }

    /**
     * Starts the CLI.
     */
    public void start() {
	if (setupConnection()) {
	    run();
	}
	
	System.out.println("Client exited.");
    }    
}
