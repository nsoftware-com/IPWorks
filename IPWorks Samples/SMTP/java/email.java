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

public class email extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("usage: email [options] server from to");
			System.out.println("Options: ");
			System.out.println("  -c      a comma separated list of addresses for carbon copies ");
			System.out.println("  -s      the subject of the mail message");
			System.out.println("  -m      the raw message content");
			System.out.println("  server  the name or address of a mail server (mail relay)");
			System.out.println("  from    the email address of the sender");
			System.out.println("  to      a comma separated list of addresses for destinations");
			System.out.println("\r\nExample: email -c cc@mail.local -s test -m \"test message\" mail.local sender@mail.com recipient@mail.local");
		} else {
			try {
				Smtp smtp1 = new Smtp();
				System.out.println("***************************************************************************************************");
				System.out.println("* This demo shows how to use Email component to send an email message. By default, the connection *");
				System.out.println("* will be attempted in plaintext. If SSL is desired, simply set SSLStartMode.                     *");
				System.out.println("***************************************************************************************************\n");

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-c"))
							smtp1.setCc(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-s"))
							smtp1.setSubject(args[i + 1]);
						if (args[i].equals("-m"))
							smtp1.setMessageText(args[i + 1]);
					}
				}

				smtp1.addSmtpEventListener(new DefaultSmtpEventListener() {
					public void SSLServerAuthentication(SmtpSSLServerAuthenticationEvent arg0) {
						arg0.accept = true; // this will trust all certificates and it is not recommended for production use
					}
				});

				smtp1.setMailServer(args[args.length - 3]);
				smtp1.setFrom(args[args.length - 2]);
				smtp1.setSendTo(args[args.length - 1]);
				smtp1.setTimeout(25);

				System.out.println("Sending message...");
				smtp1.send();
				System.out.println("Message sent.");

				smtp1.disconnect();

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



