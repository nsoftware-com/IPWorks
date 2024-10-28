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

public class ldap extends ConsoleDemo {

	public static void main(String[] args) {

		String binddn = "";
		String password = "";
		String searchdn = "";

		if (args.length < 1) {
			System.out.println("usage: ldap server binddn password");
			System.out.println("Options:");
			System.out.println("  -s        the dn used as the base for operations (optional)");
			System.out.println("  -u        the dn used for authentication (optional)");
			System.out.println("  -p				the password used to authenticate to the server (optional)");
			System.out.println("  server    the name or address of the LDAP server");
			System.out.println("\r\nExample: ldap -s CN=Users,DC=Domain -u DOMAIN\\Username -p password server");
		} else {
			LDAP ldap1 = new LDAP();
			try {
				ldap1.addLDAPEventListener(new DefaultLDAPEventListener() {

					public void SSLServerAuthentication(LDAPSSLServerAuthenticationEvent arg0) {
						arg0.accept = true; // this will trust all certificates and it is not recommended for production use
					}

					public void error(LDAPErrorEvent e) {
						System.out.println("\nError " + e.errorCode + ": " + e.description);
					}

					public void result(LDAPResultEvent e) {
						System.out.println(e.resultCode + "  " + e.resultDescription);
					}

					public void searchComplete(LDAPSearchCompleteEvent e) {
						System.out.println(e.resultCode + "  " + e.resultDescription);
					}

					public void searchResult(LDAPSearchResultEvent e) {
						System.out.println(e.DN);
					}
				});

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-s"))
							searchdn = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-u"))
							binddn = args[i + 1];
						if (args[i].equals("-p"))
							password = args[i + 1];
					}
				}

				ldap1.setServerName(args[args.length - 1]);
				if (binddn.length() > 0) {
					ldap1.setDN(binddn);
					ldap1.setPassword(password);
					ldap1.bind();
				}
				do {
					ldap1.setLDAPVersion(3);
					ldap1.setSearchSizeLimit(100);
					ldap1.getAttributes().add(new LDAPAttribute("mail", ""));
					String cn = prompt("Search for");
					System.out.println("Sending search request...");
					ldap1.setTimeout(10); // synchronous operation - to use asynchronous, set timeout to zero
					ldap1.setDN(searchdn);
					ldap1.search("CN=" + cn);
				} while (ask("Perform another search") != 'n');
			} catch (Exception e) {
				displayError(e);
			}
			try {
				ldap1.unbind();
			} catch (Exception e) {
				displayError(e);
				System.exit(1);
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



