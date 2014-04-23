import java.io.*;
import java.net.*;
import java.util.Scanner;

public class Client {

    private static boolean running = true;

    public static void main(String[] args) throws Exception {
	System.out.println("Welcome to the client!");

	Socket clientSocket = new Socket("localhost", 1337);
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
