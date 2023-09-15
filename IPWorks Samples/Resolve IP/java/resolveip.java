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

public class resolveip extends ConsoleDemo {
	
	private static boolean isHostAddr = true;
	private static boolean eventProcessed = false;

	private static Ipinfo ipinfo1 = null;

	public static void main(String[] args) {
		
		if(args.length!=2) {
			   
			System.out.println("usage: resolveip command host");
			System.out.println("");
			System.out.println("  command  one of the following: a for Resolve Host Address or n for Resolve Host Name");
			System.out.println("  host     host address or host name to resolve based on the command");			
			System.out.println("\r\nExample: resolveip n google.com");
			
		} else {
			ipinfo1 = new Ipinfo();
			try {
				ipinfo1.addIpinfoEventListener(new DefaultIpinfoEventListener() {
					
					public void requestComplete(IpinfoRequestCompleteEvent e) {
						if (e.statusCode != 0) {
							System.out.println("Request #" + e.requestId + " failed: " + e.description);
						} else if (isHostAddr) {
							System.out.println("Host Name: " + ipinfo1.getHostName() + "\n");
						} else {
							System.out.println("Host Address: " + ipinfo1.getHostAddress());
							System.out.println("Host Aliases:" + ipinfo1.getHostAliases());
							System.out.println("Alternate Addresses: " + ipinfo1.getOtherAddresses());
						}
						eventProcessed = true;
					}
				});
			
					
					if (args[0].equals("a")) {
						eventProcessed = false;
						isHostAddr = true;

						ipinfo1.setHostAddress(args[1]);
					} else {
						eventProcessed = false;
						isHostAddr = false;

						ipinfo1.setHostName(args[1]);
					} 
					
					while (!eventProcessed)
						ipinfo1.doEvents();
				
			} catch (Exception e) {
				displayError(e);
			}
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



