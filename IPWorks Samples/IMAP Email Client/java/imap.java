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

public class imap extends ConsoleDemo {

	static int lines = 0;

	static String command; // user's command
	static String buffer; // text buffer
	static String[] argument; // arguments following command

	static int msgnum = 0; // current message number for next command
	static String msg_range;
	static String msg_limit;

	public static void main(String[] args) {

		if (args.length != 3) {

			System.out.println("usage: imap server username password");
			System.out.println("");
			System.out.println("  server    the name or address of the mail server (IMAP server)");
			System.out.println("  username  the user name used to authenticate to the MailServer ");
			System.out.println("  password  the password used to authenticate to the MailServer ");
			System.out.println("\r\nExample: imap 127.0.0.1 username password");

		} else {

			try {
				Imap imap1 = new Imap();
				imap1.addImapEventListener(new DefaultImapEventListener() {

					public void SSLServerAuthentication(ImapSSLServerAuthenticationEvent e) {
						e.accept = true; // this will trust all certificates and it is not
															// recommended for production use
					}

					public void mailboxList(ImapMailboxListEvent e) {
						System.out.println(e.mailbox);
						lines++;
						if (lines == 22) {
							System.out.print("Press enter to continue...");
							try {
								System.in.read();
								System.in.skip(System.in.available());
							} catch (Exception ex) {
							}
							lines = 0;
						}
					}

					public void messageInfo(ImapMessageInfoEvent e) {
						System.out.print(e.messageId + "  ");
						System.out.print(e.subject + "  ");
						System.out.print(e.messageDate + "  ");
						System.out.println(e.from);
						lines++;
						if (lines == 22) {
							System.out.print("Press enter to continue...");
							try {
								System.in.read();
								System.in.skip(System.in.available());
							} catch (Exception ex) {
							}
							lines = 0;
						}
					}

					public void transfer(ImapTransferEvent e) {
						System.out.println(e.text);
						lines++;
						if (lines == 22) {
							System.out.print("Press enter to continue...");
							try {
								System.in.read();
								System.in.skip(System.in.available());
							} catch (Exception ex) {
							}
							lines = 0;
						}
					}
				});
				imap1.setMailServer(args[0]);
				imap1.setUser(args[1]);
				imap1.setPassword(args[2]);
				imap1.connect();
				DisplayMenu();
				while (true) {
					lines = 0;
					System.out.print("imap> ");
					command = input();
					argument = command.split("\\s");
					if (argument.length == 0)
						continue;
					switch (argument[0].charAt(0)) {
					case 's':
						imap1.setMailbox(argument[1]);
						imap1.selectMailbox();
						break;
					case 'h':
						if (imap1.getMessageCount() > 0) {
							imap1.fetchMessageInfo();
						} else {
							System.out.println("No messages in this mailbox.");
						}
						break;
					case 'l':
						if (argument.length < 2) {
							buffer = "*";
							imap1.setMailbox(buffer);
						} else {
							imap1.setMailbox(argument[1]);
						}
						imap1.listMailboxes();
						break;
					case 'n':
						msgnum++;
						buffer = String.valueOf(msgnum);
						imap1.setMessageSet(buffer);
						imap1.fetchMessageText();
						break;
					case 'q':
						imap1.disconnect();
						return;
					case 'v':
						msgnum = Integer.parseInt(argument[1]);
						imap1.setMessageSet(argument[1]);
						imap1.fetchMessageText();
						break;
					case '?':
						DisplayMenu();
						break;
					default: // allow user to enter only the number of the message they
										// want to view
						try {
							msgnum = Integer.parseInt(command);
							imap1.setMessageSet(command);
							imap1.fetchMessageText();
						} catch (NumberFormatException e) {
							System.out.println("Bad command / Not implemented in demo.");
						}
					}
				}
			} catch (Exception ex) {
				System.out.println(ex.getMessage());
			}
		}
	}

	private static void DisplayMenu() {
		System.out.println("IMAP Commands");
		System.out.println("l                   list mailboxes");
		System.out.println("s <mailbox>         select mailbox");
		System.out.println("v <message number>  view the content of selected message");
		System.out.println("n                   goto and view next message");
		System.out.println("h                   print out active message headers");
		System.out.println("?                   display options");
		System.out.println("q                   quit");
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



