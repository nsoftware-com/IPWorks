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

import java.util.TooManyListenersException;
import ipworks.*;

public class oauth extends ConsoleDemo {

	private static OAuth oauth1 = null;
	private static JSON json1 = null;
	private static HTTP http1 = null;
	
	public static void main(String[] args) {
		
		if (args.length != 5) {

			System.out.println("usage: oauth clientID clientSecret serverAuthURL serverTokenURL authScope");
			System.out.println("");
			System.out.println("  clientID        the id of the client assigned when registering the application (e.g. 723966830965.apps.googleusercontent.com)");
			System.out.println("  clientSecret    the secret value for the client assigned when registering the application (e.g. _bYMDLuvYkJeT_99Q-vkP1rh)");
			System.out.println("  serverAuthURL   the URL of the authorization (e.g. server.https://accounts.google.com/o/oauth2/auth");
			System.out.println("  serverTokenURL  the URL used to obtain the access token (e.g. https://accounts.google.com/o/oauth2/token)");
			System.out.println("  authScope       the scope request or response parameter used during authorization (e.g. https://www.googleapis.com/auth/userinfo.email)");
			System.out.println("\r\nExample: oauth 723966830965.apps.googleusercontent.com _bYMDLuvYkJeT_99Q-vkP1rh https://accounts.google.com/o/oauth2/auth https://accounts.google.com/o/oauth2/token https://www.googleapis.com/auth/userinfo.email");
			
		} else {
		
			try { 
				oauth1 = new OAuth();
				json1 = new JSON();
				http1 = new HTTP();
				
				oauth1.addOAuthEventListener(new DefaultOAuthEventListener(){
					
					public void SSLServerAuthentication(OAuthSSLServerAuthenticationEvent arg0) {
						arg0.accept=true; //this will trust all certificates and it is not recommended for production use
					}						
					public void launchBrowser(OAuthLaunchBrowserEvent arg0) {
						// Normally, the component will execute the command property to launch the browser for authorization.
						// Setting the command to an empty string will prevent a browser from opening the URL. The following 
						// line can be un-commented to exhibit this behavior.
						//arg0.command = "";
						System.out.println("Authorization URL: " + arg0.URL + "");
					}
		        });
			
				/*This application demonstrates how to use the OAuth component to authenticate with Google using OAuth 2.0 (Device Profile). 
			   	  It also demonstrates how to use the retrieved Authorization String with the HTTP and JSON components to retrieve user information. 
			   	  It will guide you through the steps to perform authorization using OAuth. 
			      Please see the Introduction page within the help for more detailed instructions.
						
				/*Client ID and Client Secret
			  	  Obtain and set your Client ID and Client Secret. For Google, these values can be found in the API Console:
			      https://code.google.com/apis/console#access
			      The values that are given as an example are from a Google test account that we have setup for you to easily run this demo. */
			
				String clientID = args[0];
				String clientSecret = args[1];
						
			
				/*Server Auth URL, Server Token URL, and Authorization Scope
			      You can also set Server Auth URL, Server Token URL, and Authorization Scope to the values desired.
			      These are preset to values for Google's User Info service.*/
			
				String serverAuthURL = args[2];
				String serverTokenURL = args [3];
				String authScope = args[4];
			
			
				// Get Authorize URL for user to authenticate
				oauth1.setClientId(clientID);
				oauth1.setClientSecret(clientSecret);
				oauth1.setServerAuthURL(serverAuthURL);
				oauth1.setServerTokenURL(serverTokenURL);
				oauth1.setAuthorizationScope(authScope);
			
				/*The following URL will open in a web browser to authenticate to the
			      service. Upon successfully authenticating and allowing access, the user will
			      be redirected back to an embedded web server within the component.
			      The Authorization String will then be set to the 'Authorization' property
			      of the HTTP component and used to retrieve the user info for the authenticated
			      client.*/
			
				String authString = oauth1.getAuthorization();
			
				// Retrieve the user info for the authenticated client.
				System.out.println("\nAuthorization String received. Retrieving user info for the authenticated client...\n");
				http1.setAuthorization(authString);
				http1.get("https://www.googleapis.com/oauth2/v1/userinfo");
				json1.setInputData(new String(http1.getTransferredData()));
				json1.parse();
				json1.setXPath("/json/email");
				System.out.println("Email: " + json1.getXText().replace("\"", ""));
				json1.setXPath("/json/verified_email");
				System.out.println("Verified: " + json1.getXText().replace("\"", ""));
								
			} catch (IPWorksException e) {
				System.out.println(e.getMessage());
		        System.exit(e.getCode());
		        return;
			} catch (TooManyListenersException e) {
				System.out.println(e.toString());
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



