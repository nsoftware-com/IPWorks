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

public class rexec extends ConsoleDemo {
	
	private static boolean waiting;

	public static void main(String[] args) {
		
		if(args.length!=4) {
		   
			System.out.println("usage: rexec host user password command");
			System.out.println("");
			System.out.println("  host      the address of the remote host");
			System.out.println("  user      the id of the user on the remote host");
			System.out.println("  password  the password of the user on the remote host");
			System.out.println("  command   the command to be sent to the remote host");
			System.out.println("\r\nExample: rexec MyHostNameOrIP user password ls");
			
		} else {
			
			Rexec rexec1 = new Rexec();
			try {
				  rexec1.addRexecEventListener(new DefaultRexecEventListener() {
					  public void stderr(RexecStderrEvent e) {
						  System.out.print(new String(e.text));
						  if (e.EOL) {
							  System.out.println();
						  }
					  }

					  public void stdout(RexecStdoutEvent e) {
						  System.out.print(new String(e.text));
						  if (e.EOL) {
							  System.out.println();
						  }
					  }

					  public void disconnected(RexecDisconnectedEvent e) {
						  waiting = false;
					  }
				  });

				  rexec1.setRemoteHost(args[0]);
				  rexec1.setRemoteUser(args[1]);
				  rexec1.setRemotePassword(args[2]);

				  waiting = true;
				  rexec1.setCommand(args[3]);

				  //now wait for command completion
				  while (waiting)
					  rexec1.doEvents();
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



