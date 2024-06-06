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
import java.io.Console;
import java.sql.Connection;

import ipworks.*;

public class udpserver extends ConsoleDemo {

	private static UDP udpserver;

	public static void main(String[] args) {
    if (args.length != 1) {
      System.out.println("usage: udpserver port");
      System.out.println("  port    the port on which the server will listen");
      System.out.println("Example: udpserver 777");
    } else {
      try {
        udpserver = new UDP();

        System.out.println("*****************************************************************");
        System.out.println("* This demo shows how to set up an echo server using UDP.       *");
        System.out.println("*****************************************************************");

        udpserver.addUDPEventListener(new DefaultUDPEventListener() {
          public void dataIn(UDPDataInEvent e) {
            try {
              System.out.println("Echoing '" + new String(e.datagram) + "' back to client " + e.sourceAddress + ":" + e.sourcePort + ".");
              udpserver.setRemoteHost(e.sourceAddress);
              udpserver.setRemotePort(e.sourcePort);
              udpserver.sendText(new String(e.datagram));
            } catch (IPWorksException e1) {
              e1.printStackTrace();
            }
          }
          public void error(UDPErrorEvent e) {
            System.out.println("Error: " + e.description);
          }
        });

        udpserver.setLocalPort(Integer.parseInt(args[0]));
        udpserver.activate();

        System.out.println("Listening on port " + udpserver.getLocalPort() + "... press Ctrl-C to shutdown.");  

        while (true) {
          udpserver.doEvents();
        }
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



