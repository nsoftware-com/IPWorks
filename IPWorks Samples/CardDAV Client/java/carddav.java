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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import ipworks.CardCustomProp;
import ipworks.CardDAV;
import ipworks.CardDAVContactDetailsEvent;
import ipworks.CardDAVSSLServerAuthenticationEvent;
import ipworks.DefaultCardDAVEventListener;
import ipworks.IPWorksException;
import ipworks.OAuth;

public class carddav extends ConsoleDemo {

    private static CardDAV carddav = new CardDAV();
    private static OAuth oauth = new OAuth();
    private static String[] argument;
    private static String clientID = "";
    private static String clientSecret = "";
    private static List<String> resourceURIs = new ArrayList<>();
    private static boolean exportingContact = false;
    private static int contactNumber = 1;
    private static String user;
    private static String password;

    public static void main(String[] args) {
        
        if (args.length < 3) {
            System.out.println("usage: carddav [options] URI username password");
            System.out.println("Options");
            System.out.println("  -i        OAuth Client ID");
            System.out.println("  -s        OAuth Client Secret");
            System.out.println("  URI       the URI of the address book");
            System.out.println("  username  the username to login");
            System.out.println("  password  the password to login");
            System.out.println("\r\nExample: carddav -i OAuthClientID -s OAuthClientSecret URI username password");
        } else {
            try {
                carddav.addCardDAVEventListener(new DefaultCardDAVEventListener() {
                    public void SSLServerAuthentication(CardDAVSSLServerAuthenticationEvent e) {
                        e.accept = true; //this will trust all certificates and it is not recommended for production use
                    }
                    public void contactDetails(CardDAVContactDetailsEvent e) {
                        if (!exportingContact) {
                            resourceURIs.add(e.resourceURI);
                            String phoneNumber = (carddav.getPhoneNumbers().size() > 0 && !carddav.getPhoneNumbers().get(0).getValue().trim().isEmpty()) ? carddav.getPhoneNumbers().get(0).getValue() : " - ";
                            String email = (carddav.getEMails().size() > 0 && !carddav.getEMails().get(0).getValue().trim().isEmpty()) ? carddav.getEMails().get(0).getValue() : " - ";
                            System.out.printf("%-5s %-40s %-30s %-30s %-30s%n", (contactNumber + ")"), carddav.getFormattedName(), phoneNumber, email, e.resourceURI);
                            contactNumber++;
                        }
                    }

                });
                System.out.println("********************************************************************************");
                System.out.println("* This demo shows how to use the CardDAV component to list contacts from an    *");
                System.out.println("* existing Google or NextCloud address book. You can also create a new         *");
                System.out.println("* contact, delete a contact, or export a contact to a vcf file.                *");
                System.out.println("********************************************************************************\n");
                System.out.println("Type \"?\" for a list of commands.");

                while (true) {
                    try {
                        exportingContact = false;
                        System.out.print("> ");
                        argument = input().split(" ");

                        for (int i = 0; i < args.length; i++) {
                            if (args[i].startsWith("-")) {
                                if (args[i].equals("-i")) {
                                    clientID = args[i + 1];
                                }
                                if (args[i].equals("-s")){
                                    clientSecret = args[i + 1];
                                } 
                            }
                        }

                        user = args[args.length - 2];
                        password = args[args.length - 1];
                        String mainAddressbookURL = args[args.length - 3];
                        
                        if (argument[0].equals("?")) {
                            System.out.println("1) List Contacts");
                            System.out.println("2) Add Contact");
                            System.out.println("3) Delete Contact");
                            System.out.println("4) Export VCF File");
                            System.out.println("?) Help menu.");
                            System.out.println("Q) Quit.");

                        } else if (argument[0].equals("1")) {

                            resourceURIs.clear();
                            contactNumber = 1;
                            System.out.printf("%-5s %-40s %-30s %-30s %-30s%n", "", "Name", "PhoneNumber", "Email", "ResourceURI\n");
                            getAuthorization();
                            carddav.getAddressbookReport(mainAddressbookURL);
                            carddav.reset();

                        } else if (argument[0].equals("2")) {

                            getAuthorization();
                            carddav.setUID(String.valueOf(System.currentTimeMillis()));
                            carddav.setFormattedName(prompt("Full Name"));
                            carddav.getPhoneNumbers().add(new CardCustomProp("TEL", prompt("Phone number")));
                            carddav.getEMails().add(new CardCustomProp("TEL", prompt("Email")));
                            carddav.getAddresses().add(new CardCustomProp("TEL", prompt("Address")));
                            carddav.createContact(mainAddressbookURL + "/" + carddav.getUID() + ".vcf");
                            System.out.println("Contact successfully added");

                        } else if (argument[0].equals("3")) {

                            String resource = prompt("Give name of file to delete (find the file names by running option #1)");
                            getAuthorization();
                            carddav.deleteContact(mainAddressbookURL + "/" + resource);
                            System.out.println("Contact successfully deleted");

                        } else if (argument[0].equals("4")) {

                            String resource = prompt("Give name of the .vcf file (find the file names by running option #1)");
                            getAuthorization();
                            System.out.printf("%-5s %-40s %-30s %-30s %-30s%n", "", "Name", "PhoneNumber", "Email", "ResourceURI\n");
                            carddav.getContact(mainAddressbookURL + "/" + resource);
                            //File.WriteAllText(resource, carddav.ExportVCF());
                        } else if (argument[0].toLowerCase().equals("q")) {

                            System.exit(0);
                            return;

                        } else if (argument[0].equals("")) {
                            // do nothing
                        } else {
                            System.out.println("Bad command / Not implemented in demo.");
                        }
                    } catch (IPWorksException e) {
                        System.out.println(e.getMessage());
                        System.exit(0);
                        return;
                    }
                }
            } catch (Exception e) {
                displayError(e);
            }
        }
    }

    private static void getAuthorization() throws IPWorksException {
        carddav.setUser(user);
        if(clientID.isEmpty() && clientSecret.isEmpty()) {
            carddav.setPassword(password);
        }
        else {  
            oauth.setClientId(clientID);
            oauth.setClientSecret(clientSecret);
            oauth.setServerAuthURL("https://accounts.google.com/o/oauth2/auth");
            oauth.setServerTokenURL("https://accounts.google.com/o/oauth2/token");
            oauth.setAuthorizationScope("https://www.googleapis.com/auth/carddav");
            carddav.setAuthScheme(CardDAV.authOAuth);
            carddav.setAuthorization(oauth.getAuthorization());
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



