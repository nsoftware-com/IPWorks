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
import java.io.*;

import java.io.*;

import ipworks.*;

public class tftpserver extends ConsoleDemo {

    private static Tftpserver tftpserver1 = null;
	
	public static void main(String[] args) {

        System.out.println("************************************************************************");
        System.out.println("* This demo shows how to set up a TFTP Server on your system.*");
        System.out.println("************************************************************************\n");

        if (args.length != 1) {
            System.out.println("usage: tftpserver path");
            System.out.println("  path    the file path to the server's local directory.");
            System.out.println("\r\nExample: tftpserver C:\\temp");
        } else {
            try {
                tftpserver1 = new Tftpserver();
                tftpserver1.addTftpserverEventListener(new DefaultTftpserverEventListener() {
                    public void connected(TftpserverConnectedEvent e) {
                        System.out.println(tftpserver1.getConnections().item(e.connectionId).getRemoteHost() + " connected.");
                    }

                    @Override
                    public void connectionRequest(TftpserverConnectionRequestEvent e) {
                        System.out.println(e.remoteHost + ": Attempting to connect.");
                    }

                    public void disconnected(TftpserverDisconnectedEvent e) {
                        System.out.println("Remote host disconnected: " + e.description);
                    }

                    @Override
                    public void endTransfer(TftpserverEndTransferEvent e) {
                        System.out.println("Transfer complete");
                    }

                    @Override
                    public void error(TftpserverErrorEvent e) {
                        System.out.println("Error: " + e.description + " [" + e.errorCode + "]");
                    }

                    @Override
                    public void startTransfer(TftpserverStartTransferEvent tftpserverStartTransferEvent) {
                        System.out.println("Transfer started");
                    }

                    @Override
                    public void transfer(TftpserverTransferEvent tftpserverTransferEvent) {
                        System.out.println("Transferring data");
                    }
                });


                tftpserver1.setLocalDir(args[0]);

                tftpserver1.startListening();

                System.out.println("Server is listening on port: " + tftpserver1.getLocalPort());
                System.out.println("Press Q to exit.\r\n\r\n");

                while(true)
                {
                    if(System.in.available()>0)
                    {
                        if(String.valueOf(read()).equalsIgnoreCase("Q"))
                        {
                            System.out.println("Server shutting down. Goodbye!");
                            tftpserver1.stopListening();
                        }
                    }
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



