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

public class udpclient extends ConsoleDemo {

  private static UDP udpclient;

  public static void main(String[] args) {
    if (args.length != 2) {
      System.out.println("usage: udpclient host port");
      System.out.println("  host    the address of the remote host");
      System.out.println("  port    the port of the remote host");
      System.out.println("Example: udpclient localhost 777");
      System.out.println("         udpclient 255.255.255.255 777 (broadcast)");
    } else {
      try {
        udpclient = new UDP();
        udpclient.addUDPEventListener(new DefaultUDPEventListener() {
          public void dataIn(UDPDataInEvent e) {
            System.out.println("Received '" + new String(e.datagram) + "' from " + e.sourceAddress + ":" + e.sourcePort + ".");
          }
          public void error(UDPErrorEvent e) {
            System.out.println("Error: " + e.description);
          }
        });
        udpclient.setRemoteHost(args[0]);
        udpclient.setRemotePort(Integer.parseInt(args[1]));
        udpclient.activate();
        udpclient.doEvents();
        
        System.out.println("\r\nType and press enter to send. Press Ctrl-C to exit the application.");

        while (true) {
          String data = input();
          udpclient.sendText(data);
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
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



