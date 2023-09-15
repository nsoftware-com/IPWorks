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

public class snpp extends ConsoleDemo {
	
	
	public static void main(String[] args) {
		
		if(args.length!=3) {
			
			System.out.println("usage: snpp server pagerid message");
			System.out.println("");
			System.out.println("  server   the name or address of the SNPP server");			
			System.out.println("  pagerid  the identifying number of the pager to send a Message to");			
			System.out.println("  message  the message content");
			System.out.println("\r\nExample: snpp SnppServer pagerId \"test message\"");
		
		} else {
			
			Snpp snpp1 = new Snpp();
			
			try {
				snpp1.addSnppEventListener(new DefaultSnppEventListener(){
					
					public void PITrail(SnppPITrailEvent e) {		
						System.out.println(e.direction + "  " + e.message);	
					}					
					public void SSLServerAuthentication(SnppSSLServerAuthenticationEvent e) {
						e.accept=true; //this will trust all certificates and it is not recommended for production use	
					}					
					public void error(SnppErrorEvent e) {
						System.out.println("Error " + e.errorCode + ": " + e.description);		
					}
									
		        });
					
				snpp1.setServerName(args[0]);
				snpp1.setPagerId(args[1]);
				snpp1.setMessage(args[2]);

				snpp1.connect();
				snpp1.send();
				System.out.println("Page sent.");
			} catch (Exception e) {
				displayError(e);
			}

			try {
				snpp1.disconnect();
			} catch (Exception e) {
				displayError(e);
				System.exit(1);
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



