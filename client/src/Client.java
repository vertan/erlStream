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

	Scanner sc = new Scanner(System.in);
	String input;

	System.out.print("Server address (Press ENTER for localhost): ");
	input = sc.nextLine();

	if (!input.equals("")) {
	    address = input;
	}	    

	// TODO: Verify connection before printing anything

	System.out.println("Welcome to the client!");
	System.out.println("Type your commands:");

	Socket inSocket, outSocket;
	DataOutputStream toServer;
	BufferedReader fromServer;

	while(running) {
	    System.out.print("> ");
	    input = sc.nextLine();
	    
	    switch(input) {
	    case "quit":
		running = false;
		break;
	    default:
		inSocket = new Socket(address, inPort);
		outSocket = new Socket(address, outPort);
		toServer = new DataOutputStream(outSocket.getOutputStream());
		fromServer = new BufferedReader(new InputStreamReader(inSocket.getInputStream()));
		
		toServer.writeBytes(input);
		outSocket.close();
	    
		System.out.println("Answer from server:");
		String answer = fromServer.readLine();
		
		while(answer != null) {
		    System.out.println(answer);
		    answer = fromServer.readLine();
		}
	    }
	}
	
	System.out.println("Goodbye!");
    }

}
