/*
 * IPWorks 2022 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworks
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import java.util.Scanner;
import ipworks.*;

public class wsclient {
	private static Wsclient websocketclient;
	private static boolean received;

	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		String url = "ws://echo.websocket.events/.ws";
		String command;
		String response;
		try {
			websocketclient = new Wsclient();
			System.out.println("*******************************************************************************************************************");
			System.out.println("* This is a demo to show how to connect to a remote WebSocket server, send data, and receive the echoed response.*");
			System.out.println("******************************************************************************************************************\n");

			websocketclient.addWsclientEventListener(new DefaultWsclientEventListener() {
				public void dataIn(WsclientDataInEvent e) {
					System.out.println("Received: " + new String(e.text));
					received = true;
				}

				public void connected(WsclientConnectedEvent e) {
					System.out.println("Connected.");
				}

				public void disconnected(WsclientDisconnectedEvent e) {
					System.out.println("Disconnected.");
				}
			});

			System.out.print("Connect to " + url + "? (y/n): ");
			response = scanner.nextLine();
			if (response.charAt(0) == 'n') {
				System.out.println("Please enter the URL: ");
				url = scanner.nextLine();
			}
			websocketclient.setTimeout(10);
			websocketclient.connectTo(url);
			do {
				System.out
						.print("\r\nPlease input command: \r\n1) Send Data \r\n2) Exit\r\n>");
				command = scanner.nextLine();
				if (command.charAt(0) == '1') {
					received = false;
					System.out.print("Please input sending data: ");
					websocketclient.setDataToSend(scanner.nextLine());
					while(!received){websocketclient.doEvents();}; //To ensure we get a response before asking for more input.
				} else if (command.charAt(0) == '2')
					break;
				else
					System.out.println("Invalid command. Please try again.");		
			} while (true);
			scanner.close();
			websocketclient.disconnect();
		} catch (IPWorksException ex) {
			System.out.println("\n\nError: " + ex.getMessage() + "\n");
			System.exit(ex.getCode());
		} catch (Exception ex) {
			System.out.println(ex.getMessage());
		}
	}

}
class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksException) {
      System.out.print(" (" + ((IPWorksException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



