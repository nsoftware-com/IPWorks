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


public class httpurl {
	
	public static void main(String[] args) {
	  
	  if (args.length != 1) {

			System.out.println("usage: httpurl url");
			System.out.println("");
			System.out.println("  url  the url to fetch");
			System.out.println("\r\nExample: httpurl https://www.google.com");

		} else {
			
			Http http = new Http();					
			try {
				http.addHttpEventListener(new DefaultHttpEventListener(){
					
					public void SSLServerAuthentication(HttpSSLServerAuthenticationEvent arg0) {
						arg0.accept = true; //this will trust all certificates and it is not recommended for production use
					}
					public void transfer(HttpTransferEvent e) {
						System.out.print(new String(e.text));
					}
		        });
				http.setFollowRedirects(2);
			    http.get(args[0]); //url
			} catch (Exception ex) {
				System.out.println(ex.getMessage());
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



