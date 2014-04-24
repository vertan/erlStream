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
	int port = 1339;

	if (args.length > 0) {
	    address = args[0];
	    
	    if (args.length > 1) {
		port = Integer.parseInt(args[1]);
	    }
	}

	System.out.println("Welcome to the client!");

	Socket clientSocket;
	DataOutputStream toServer;
	BufferedReader fromServer;

	Scanner sc = new Scanner(System.in);
	System.out.println("Type your commands:");

	while(running) {
	    System.out.print("> ");
	    String message = sc.nextLine();

	    clientSocket = new Socket(address, port);
	    toServer = new DataOutputStream(clientSocket.getOutputStream());
	    fromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

	    switch(message) {
	    case "quit":
		clientSocket.close();
		running = false;
		break;
	    default:
		toServer.writeBytes(message);
		clientSocket.close();
	    }

	    // String answer = fromServer.readLine();
	    // System.out.println("Answer from server: " + answer);
	}
	
	System.out.println("Goodbye!");
    }

}
