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

public class netclock extends ConsoleDemo {
  
	public static void main(String[] args) {
    
		if(args.length!=1) {
			
			System.out.println("usage: netclock server");
			System.out.println("");			
			System.out.println("  server  the time server from which to request the time");						
			System.out.println("\r\nExample: netclock time.nist.gov");
		
		} else {
			
			Netclock myClock = new Netclock();

			//Uses the TIME protocol - you may specify SNTP by using the Protocol property
			//myClock.setProtocol(myClock.tpSNTP);

			try {
											
			System.out.println("Using: " + args[0]);
			myClock.setTimeServer(args[0]);
			myClock.getTime();

			System.out.println("System date and time   : " + new java.util.Date().toString());
			System.out.println("Internet date and time : " + myClock.getLocalTime());
			
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



