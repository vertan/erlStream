import java.io.*;
import java.net.*;

public class Client {

    public static void main(String[] args) throws Exception {
	System.out.println("Welcome to the client!");

	Socket clientSocket = new Socket("localhost", 1337);
	DataOutputStream toServer = new DataOutputStream(clientSocket.getOutputStream());
	BufferedReader fromServer = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
	String testString = "Hi my sir!";
	toServer.writeBytes(testString + '\n');
	String answer = "Test"; //fromServer.readLine();

	System.out.println("Answer from server: " + answer);
	clientSocket.close();
    }

}
