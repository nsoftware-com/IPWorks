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


public class syslog extends ConsoleDemo {
	
	private static boolean stop = false;

	public static void main(String[] args) {
		
		SysLog syslog1 = new SysLog();

		Thread thread = new Thread() {
			public void run() {
				input();
				stop = true;
			}
		};
		thread.start();

		try {
			syslog1.addSysLogEventListener(new DefaultSysLogEventListener() {
				public void packetIn(SysLogPacketInEvent e) {
					System.out.println("Host: " + e.hostname);
					System.out.println("Facility: " + e.facility);
					System.out.println("Severify: " + e.severity);
					System.out.println("Time: " + e.timestamp);
					System.out.println("Message: " + e.message);
					System.out.println();
				}
			});
      
			syslog1.activate();
			System.out.println("SysLog server started. To stop, press any key.\n");

			while (!stop) {
				syslog1.doEvents();
			}
		} catch (Exception e) {
			displayError(e);
		}

		try {
			syslog1.deactivate();
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



