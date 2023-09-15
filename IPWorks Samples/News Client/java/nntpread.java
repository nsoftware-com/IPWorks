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


public class nntpread extends ConsoleDemo {
	
	private static int lines;
	
	public static void main(String[] args) {
		
		if(args.length!=1) {
			
			System.out.println("usage: nntpread server ");
			System.out.println("");
			System.out.println("  server  the name or address of the NNTP server (e.g. nntp.aioe.org)");						
			System.out.println("\r\nExample: nntpread nntp.aioe.org");
			
		} else {
			
			Nntp nntp = new Nntp();

			int msgnum = 0; // current message number for next command

			try {
				
				nntp.addNntpEventListener(new DefaultNntpEventListener(){
					
					public void SSLServerAuthentication(NntpSSLServerAuthenticationEvent arg0) {
						arg0.accept=true; //this will trust all certificates and it is not recommended for production use	
					}							
					public void error(NntpErrorEvent e) {
						System.out.println("\nError " + e.errorCode + ": " + e.description);		
					}					
					public void groupList(NntpGroupListEvent e) {
						System.out.println(e.group);

						if (++lines == 23) {
							prompt("Press ENTER to continue", "...");
							lines = 0;
						}		
					}					
					public void groupOverview(NntpGroupOverviewEvent e) {
						System.out.println(e.articleNumber + "  " + e.subject);

						if (++lines == 23) {
							prompt("Press ENTER to continue", "...");
							lines = 0;
						}		
					}														
					public void header(NntpHeaderEvent e) {
						System.out.println(e.field + ": " + e.value);

						if (++lines == 23) {
							prompt("Press ENTER to continue", "...");
							lines = 0;
						}	
					}
					public void transfer(NntpTransferEvent e) {
						System.out.println(new String(e.text));

						if (e.EOL) {
							System.out.println();
							if (++lines == 23) {
								prompt("Press ENTER to continue", "...");
								lines = 0;
							}
						}	
					}
		        }); 
	    	
				nntp.setNewsServer(args[0]);
				nntp.connect();
				DisplayMenu();

				while (true) {
					String command = prompt("", ">");
					String[] argument = command.split("\\s");

					if (argument.length == 0 || argument[0].length() == 0) {
						// do nothing
					} else if (argument[0].equals("l")) {
						nntp.listGroups();
					} else if (argument[0].equals("s")) {
						if (argument.length > 1) {
							nntp.setCurrentGroup(argument[1]);
						} else {
							System.out.println("No newsgroup specified.");
						}
					} else if (argument[0].equals("v")) {
						msgnum = Integer.parseInt(argument[1]);
						nntp.setCurrentArticle(argument[1]);
						nntp.fetchArticle();
					} else if (argument[0].equals("n")) {
						msgnum++;
						nntp.setCurrentArticle(String.valueOf(msgnum));
						nntp.fetchArticle();
					} else if (argument[0].equals("h")) {
						nntp.setOverviewRange("0-");
						nntp.groupOverview();
					} else if (argument[0].equals("q")) {
						nntp.disconnect();
						break;					 
					} else {
						try {
							msgnum = Integer.parseInt(command);
							// allow user to enter the number of the message they want to view
							nntp.setCurrentArticle(command);
							nntp.fetchArticle();
						} catch (NumberFormatException e) {
							System.out.println("Bad command / Not implemented in demo.");
						}
					} // end of command checking
				} // end of main while loop
			} catch (Exception e) {
				displayError(e);
			}
		}
		
	}
	private static void DisplayMenu(){
		System.out.println("Readnews Commands");
		System.out.println("l                    list all available newsgroups");
		System.out.println("s <newsgroup>        select newsgroup");
		System.out.println("h                    give head lines of messages");
		System.out.println("v <message number>   view the content of selected message");
		System.out.println("n                    goto and view next message");		
		System.out.println("q                    quit, saving unresolved messages in mbox");
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



