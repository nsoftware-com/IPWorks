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

public class rss extends ConsoleDemo {
	
	public static void main(String[] args) {
		
		RSS rss1 = new RSS();

		try {
			
			rss1.addRSSEventListener(new DefaultRSSEventListener(){
				public void SSLServerAuthentication(RSSSSLServerAuthenticationEvent arg0) {
					arg0.accept=true; //this will trust all certificates and it is not recommended for production use		
				}								
	        });
			System.out.println("Fetching feed...");
			rss1.setFollowRedirects(RSS.frAlways);
			rss1.getFeed("https://news.google.com/news?ned=us&topic=h&output=rss");

			System.out.println("RSS Channel" + rss1.getChannel().getTitle());
			System.out.println(rss1.getChannel().getDescription());

			for (int i = 1; i <= rss1.getItems().size(); i++) {
				RSSItem rssItem = rss1.getItems().item(i-1);
				System.out.println(i + ". " + rssItem.getTitle());
			}

			while (true) {
				System.out.println("\nThere are " + rss1.getItems().size() + " items in this feed.");
				String command = prompt("RSS Item (Q to quit)");

				if (command.equals("Q") || command.equals("q")) {
					break;
				}

				int index = Integer.parseInt(command);
				RSSItem rssItem = rss1.getItems().item(index-1);

				System.out.println("\n" + rssItem.getDescription());
				System.out.println("\nFind full article at " + rssItem.getLink());
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



