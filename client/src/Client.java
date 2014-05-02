import java.io.*;
import java.net.*;
import java.util.Scanner;

/**
 * @author Jeanette Castillo <jeanette.cas@hotmail.com>, Filip Hedman <hedman.filip@gmail.com>, Robert Kallgren <robertkallgren@gmail.com>, Oscar Mangard <oscarmangard@gmail.com>, Mikael Sernheim <mikael.sernheim@gmail.com>
 *
 * The client class provides methods to communicate with a server.
 *
 * @param address The address of the server
 * @param port The in port of the server
 */
public class Client {

    private static boolean running = true;

    public static void main(String[] args) throws Exception {
	String address = "localhost";
	int inPort = 1341;
	int outPort = 1340;

	if (args.length > 0) {
	    address = args[0];
	    
	    if (args.length > 1) {
		outPort = Integer.parseInt(args[1]);
		inPort = outPort + 1;
	    }
	}

	System.out.println("Welcome to the client!");

	Socket inSocket, outSocket;
	DataOutputStream toServer;
	BufferedReader fromServer;

	Scanner sc = new Scanner(System.in);
	System.out.println("Type your commands:");

	while(running) {
	    System.out.print("> ");
	    String message = sc.nextLine();

	    inSocket = new Socket(address, inPort);
	    outSocket = new Socket(address, outPort);
	    toServer = new DataOutputStream(outSocket.getOutputStream());
	    fromServer = new BufferedReader(new InputStreamReader(inSocket.getInputStream()));

	    switch(message) {
	    case "quit":
		inSocket.close();
		outSocket.close();
		running = false;
		break;
	    default:
		toServer.writeBytes(message);
		outSocket.close();
	    }

	    System.out.println("Answer from server:");
	    String answer = fromServer.readLine();

	    while(answer != null && answer.length() != 0) {
		System.out.println(answer);
		answer = fromServer.readLine();
	    }
	}
	
	System.out.println("Goodbye!");
    }

}
