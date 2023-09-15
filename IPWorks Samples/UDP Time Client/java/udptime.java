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
import ipworks.*;

public class udptime extends ConsoleDemo {
	
	private static boolean gotResponse = false;

	public static void main(String[] args) {
		Udp udp = new Udp();

		try {
			udp.addUdpEventListener(new DefaultUdpEventListener() {
        
				public void dataIn(UdpDataInEvent arg) {
					int length = arg.datagram.length;
					while (length > 0 && arg.datagram[length - 1] < ' ') {
						length--;      //first do some cleanup...
					}
					System.out.println(arg.sourceAddress + " :\t" + new String(arg.datagram, 0, length));
					gotResponse = true;
				}
			});

			udp.setRemoteHost("255.255.255.255"); //broadcast address
			udp.setRemotePort(13);                //daytime service

			udp.setActive(true);

			System.out.println("Source:\tDate and time");
			udp.setDataToSend("hello?"); //send anything and the server will send the time

			//Wait for the response -  5 seconds will do
			long limit = System.currentTimeMillis() + 5000;
			while (!gotResponse && System.currentTimeMillis() < limit) {
				udp.doEvents();
			}
		} catch (Exception e) {
			displayError(e);
		}

		try {
			udp.setActive(false);
		} catch (Exception e) {
			displayError(e);
			System.exit(1);
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



