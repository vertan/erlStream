import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.*;

/**
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * This class provides a Command Line Interface for the Communicator module.
 *
 * @see Communicator
 */
public class CLI {
    private Communicator accomodator;
    private Scanner sc;
    private boolean running;
    private String answer;

    public CLI() {
	String address = "localhost";
	String input;

	sc = new Scanner(System.in);
	System.out.print("Server address (Press ENTER for localhost): ");
	input = sc.nextLine();

	if (!input.equals("")) {
	    address = input;
	}	    

	accomodator = new Communicator(address, 1341, 1340);
    }

    public void start() {
	// TODO: Verify connection before printing anything

	System.out.println("Welcome to the client!");
	System.out.println("Type your commands:");

	running = true;

	while(running) {
	    System.out.print("> ");
	    String input = sc.nextLine();

	    String[] command = input.split(" ");
	    
	    switch(command[0]) {
	    case "quit":
		running = false;
		break;
	    case "list":
		try {
		    List<Song> songs = accomodator.list();		    
		    for (int i = 0; i < songs.size(); i++) {
			System.out.println(songs.get(i).getFileName());
		    }
		}
		catch (Throwable e) {
		    System.out.println("Error " + e.getMessage());
		    e.printStackTrace();
		}

		break;
	    case "play":
		try {
		    int time = 0;

		    if(command.length > 2) {
			time = Integer.parseInt(command[2]);
		    }
		    accomodator.play(new Song(command[1], "Unknown Title", "Unknown Artist", "Unknown Album", 60), time);
		}
		catch (Throwable e) {
		    System.out.println("Error " + e.getMessage());
		    e.printStackTrace();
		}

		break;
	    default:
		System.out.println("Unknown command");
	    }
	}
	
	System.out.println("Goodbye!");
    }
}
