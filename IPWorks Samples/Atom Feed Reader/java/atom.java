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

public class atom extends ConsoleDemo {
  
	public static void main(String[] args) {
		
		Atom atom1 = new Atom();
		
		try {
			atom1.addAtomEventListener(new DefaultAtomEventListener(){
				public void SSLServerAuthentication(AtomSSLServerAuthenticationEvent e) {
					e.accept = true; //this will trust all certificates and it is not recommended for production use
				}
			});						
			System.out.println("Fetching feed...");      
			atom1.setFollowRedirects(Atom.frAlways);
			atom1.getFeed("https://news.google.com/news?ned=us&topic=h&output=atom");
			System.out.println("Atom Channel: " + atom1.getChannel().getTitle());
			String subtitle = atom1.getChannel().getSubtitle();
			if (subtitle != null && !subtitle.equals("")) {
				System.out.print(" - " + subtitle);
			}
			for (int i = 1; i <= atom1.getEntries().size(); i++) {
				AtomEntry atomEntry = atom1.getEntries().item(i-1);
			    System.out.println(i + ". " + atomEntry.getTitle());
			}

			while (true) {
				System.out.println("\nThere are " + atom1.getEntries().size() + " entries in this feed.");
				String command = prompt("Atom Entry (Q to quit)");
				if (command.equals("Q") || command.equals("q")) {
					break;
				}

				int index = Integer.parseInt(command);
				AtomEntry atomEntry = atom1.getEntries().item(index-1);

				System.out.println("\n" + atomEntry.getContent());
				System.out.println("\nFind full article at " + atomEntry.getLinkHref());
		
			}
			
		} catch (Exception e) {
			displayError(e);
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



