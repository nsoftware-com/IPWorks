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

public class mx extends ConsoleDemo {
	
	public static void main(String[] args) {
		
		if(args.length!=2) {
			
			System.out.println("usage: mx server email");
			System.out.println("");			
			System.out.println("  server  the name or address of the DNS server");
			System.out.println("  email   the email address to resolve");
			System.out.println("\r\nExample: mx 4.2.2.1 billg@microsoft.com");
		
		} else {
			
			Mx mx = new Mx();

			try {
				
				mx.addMxEventListener(new DefaultMxEventListener() {
					
					public void error(MxErrorEvent e) {
						System.out.println("\nError " + e.errorCode + ": " + e.description);
					}

					public void response(MxResponseEvent e) {
						if (e.statusCode != 0)
							System.out.println(e.description);
						else
							System.out.println(e.domain + " --> " + e.mailServer);
					}
				});
			
				System.out.println("Looking up address...");

				mx.setTimeout(10);
				mx.setDNSServer(args[0]);
				mx.resolve(args[1]); // email address
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



