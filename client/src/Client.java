import java.io.*;
import java.net.*;
import java.util.Scanner;

/**
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * The client class provides methods to communicate with a server.
 *
 * @param address The address of the server
 * @param port The port of the server
 */
public class Client {

    private static boolean running = true;

    public static void main(String[] args) throws Exception {
	String address = "localhost";
	int port = 1337;

	if (args.length > 0) {
	    address = args[0];
	    
	    if (args.length > 1) {
		port = Integer.parseInt(args[1]);
	    }
	}

	System.out.println("Welcome to the client!");

	Socket clientSocket = new Socket(address, port);
	DataOutputStream toServer = new DataOutputStream(clientSocket.getOutputStream());
	BufferedReader fromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

	Scanner sc = new Scanner(System.in);
	System.out.println("Type your commands:");

	while(running) {
	    System.out.print("> ");
	    String message = sc.nextLine();

	    switch(message) {
	    case "quit":
		running = false;
		break;
	    default:
		toServer.writeBytes(message);
	    }

	    // String answer = fromServer.readLine();
	    // System.out.println("Answer from server: " + answer);
	}
	
	clientSocket.close();
	System.out.println("Goodbye!");
    }

}
