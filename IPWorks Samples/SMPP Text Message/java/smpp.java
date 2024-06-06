/*
 * IPWorks 2024 Java Edition - Sample Project
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

public class smpp extends ConsoleDemo {
	
	public static void main(String[] args) {
		
		if(args.length<6) {
			
			System.out.println("usage: smpp [options] server port user password buddy message");
			System.out.println("Options: ");
			System.out.println("  -s        the type of system during a connection(some SMS servers require that a system type be supplied during connection)");
			System.out.println("  server    the SMPP entity to which the component will connect");			
			System.out.println("  port      the server port for secure SMPP (default 2775)");
			System.out.println("  user      used for identification with the SMPP service");
			System.out.println("  password  the user's password");
			System.out.println("  buddy     the ID or phone number of the recipient");
			System.out.println("  message   the message content");
			System.out.println("\r\nExample: smpp SMPPServer 2775 username password recipient \"test message\"");
		
		} else {
			
			SMPP smpp1 = new SMPP();

			try {
				smpp1.addSMPPEventListener(new DefaultSMPPEventListener(){
					
					public void SSLServerAuthentication(SMPPSSLServerAuthenticationEvent e) {
						e.accept = true; //this will trust all certificates and it is not recommended for production use		
					}															
					public void connected(SMPPConnectedEvent e) {
						System.out.println("Connected");
					}															
					public void disconnected(SMPPDisconnectedEvent e) {
						System.out.println("Disconnected");		
					}								
		        });

				smpp1.setSMPPServer(args[args.length - 6]);
				
				if (args[args.length - 5].compareTo("") != 0) {
					smpp1.setSMPPPort(Integer.parseInt(args[args.length - 5]));
				}
				
				for (int i=0; i<args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-s")) smpp1.setSystemType(args[i+1]); // args[i+1] corresponds to the value of argument [i]						
					}
				}
				smpp1.connectTo(args[args.length - 4], args[args.length - 3]);
				smpp1.addRecipient(MessageRecipient.rtTo, args[args.length - 2]);
				smpp1.sendMessage(args[args.length - 1]); //returns server-assigned id of the message.
				System.out.println("Message sent!");
			} catch (Exception e) {
				displayError(e);
			}

			try {
				smpp1.disconnect();
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
      String response = input();
      if (response.equals(""))
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



