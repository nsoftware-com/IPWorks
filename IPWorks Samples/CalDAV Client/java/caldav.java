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
import java.io.*;
import ipworks.*;
import java.util.*;
import java.text.*;

public class caldav extends ConsoleDemo {

    static CalDAV caldav = new CalDAV();

    static OAuth oauth = new OAuth();
    static String command;               // user's command
    static String[] argument;            // arguments to the user's command
    static String clientID = "";

    static String clientSecret = "";
    static List<String> resourceURIs = new ArrayList<>();
    static boolean exportingEvent = false;
    static int eventNumber = 1;

    public static void main(String[] args) {

        if (args.length < 3) {

            System.out.println("usage: caldav [options] provider username password");
            System.out.println("Options");
            System.out.println("  -i        OAuth Client ID");
            System.out.println("  -s        OAuth Client Secret");
            System.out.println("  provider  the provider name: Google");
            System.out.println("  username  the username to login");
            System.out.println("  password  the password to login");
            System.out.println("\r\nExample: caldav -n \"Calendar name\" google username password");

        } else {
            try {
                caldav.addCalDAVEventListener(new DefaultCalDAVEventListener() {
                    public void SSLServerAuthentication(CalDAVSSLServerAuthenticationEvent e) {
                        e.accept = true; //this will trust all certificates and it is not recommended for production use
                    }
                    public void eventDetails(CalDAVEventDetailsEvent e) {
                        if (!exportingEvent) {
                            resourceURIs.add(e.resourceURI);

                            String mStartDate = caldav.getStartDate();

                            try {
                                SimpleDateFormat inputFormat = new SimpleDateFormat("yyyyMMdd'T'HHmmss");
                                SimpleDateFormat outputFormat = new SimpleDateFormat("MM/dd HH:mm:ss");
                                mStartDate = outputFormat.format(inputFormat.parse(mStartDate));
                            } catch (Exception ignored) {
                            }

                            System.out.format("%2d) %2$-50.50s %3$-20s%4$-30.30s %5$-100s\n", eventNumber, caldav.getSummary(), mStartDate, caldav.getLocation(), e.resourceURI);
                            eventNumber++;
                        }
                    }

                });

                System.out.println("******************************************************************************");
                System.out.println("* This demo shows how to use the CalDAV component to list upcoming events    *");
                System.out.println("* from an existing Google calendar. You can also create a new event,*");
                System.out.println("* delete an event, or export an event to an ics file.                        *");
                System.out.println("******************************************************************************\n");

                System.out.println("Type \"?\" for a list of commands.");

                while (true) {

                    try {

                        for (int i = 0; i < args.length; i++) {
                            if (args[i].startsWith("-")) {
                                if (args[i].equals("-i"))
                                    clientID = args[i + 1];// args[i+1] corresponds to the value of argument [i]
                                if (args[i].equals("-s"))
                                    clientSecret = args[i + 1];
                            }
                        }
                        String user = args[args.length - 2];

                        caldav.setUser(user);
                        oauth.setClientId(clientID);
                        oauth.setClientSecret(clientSecret);
                        // Google CalDAV requires OAuth 2
                        oauth.setServerAuthURL("https://accounts.google.com/o/oauth2/auth");
                        oauth.setServerTokenURL("https://oauth2.googleapis.com/token");
                        caldav.setAuthScheme(CalDAV.authOAuth);

                        exportingEvent = false; //This is used so output from EventDetails is not displayed when exporting an even to an ics file
                        System.out.print("> ");
                        command = input();
                        argument = command.split("\\s");

                        if (argument[0].equals("?")) {
                            System.out.println("1) List Events");
                            System.out.println("2) Add Event");
                            System.out.println("3) Delete Event");
                            System.out.println("4) Export ICS File");
                            System.out.println("?) This help menu.");
                            System.out.println("Q) Quit.");

                        } else if (argument[0].equals("1")) { //List Calendar Events
                            resourceURIs.clear();
                            eventNumber = 1;
                            System.out.format("    %1$-50.50s %2$-20s%3$-30.30s %4$-100s\n\n", "Summary", "Start Date", "Location", "ResourceURI");
                            oauth.setAuthorizationScope("https://www.googleapis.com/auth/calendar");
                            caldav.setAuthorization(oauth.getAuthorization());
                            caldav.getCalendarReport("https://apidata.googleusercontent.com/caldav/v2/" + user + "/events");

                        } else if (argument[0].equals("2")) {  //Add Event

                            SimpleDateFormat outputFormat = new SimpleDateFormat("yyyyMMdd'T'HHmmss");
                            SimpleDateFormat inputFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

                            caldav.reset();
                            caldav.setUser(args[args.length - 2]);

                            caldav.getReportFilter().setEventType(CalReportFilter.vtEvent);

                            String mStartDate = prompt("Start Date (MM/dd/yyyy HH:mm:ss)");
                            caldav.setStartDate(outputFormat.format(inputFormat.parse(mStartDate)));
                            String mEndDate = prompt("End Date (MM/dd/yyyy HH:mm:ss)");
                            caldav.setEndDate(outputFormat.format(inputFormat.parse(mEndDate)));

                            caldav.setLocation(prompt("Location"));
                            caldav.setSummary(prompt("Summary"));
                            caldav.setDescription(prompt("Description"));
                            caldav.setEventType(0);

                            caldav.setUID(caldav.getStartDate());

                            oauth.setAuthorizationScope("https://www.googleapis.com/auth/calendar");
                            caldav.setAuthorization(oauth.getAuthorization());
                            caldav.createEvent("https://apidata.googleusercontent.com/caldav/v2/" + user + "/events/" + caldav.getUID() + ".ics");


                            System.out.println("Event Successfully Added.");

                        } else if (argument[0].equals("3")) { //Delete Event

                            caldav.reset();
                            String resource = prompt("Give name of file to delete (find the file names by running option #1)");

                            oauth.setAuthorizationScope("https://www.googleapis.com/auth/calendar");
                            caldav.setAuthorization(oauth.getAuthorization());
                            caldav.deleteEvent("https://apidata.googleusercontent.com/caldav/v2/" + user + "/events/" + resource);

                            System.out.println("Event Successfully Deleted.");

                        } else if (argument[0].equals("4")) { //Get ICS Event

                            String file = prompt("Get name of .ICS file");
                            System.out.format("    %1$-50.50s %2$-20s%3$-30.30s %4$-100s\n\n", "Summary", "Start Date", "Location", "ResourceURI");
                            oauth.setAuthorizationScope("https://www.googleapis.com/auth/calendar");
                            caldav.setAuthorization(oauth.getAuthorization());
                            caldav.getEvent("https://apidata.googleusercontent.com/caldav/v2/" + user + "/events/" + file);
                            WriteToFile(file, caldav.exportICS());
                        } else if (argument[0].equalsIgnoreCase("q")) {
                            System.exit(0);
                            return;
                        } else if (argument[0].isEmpty()) {
                            // Do nothing
                        } else {
                            System.out.println("Bad command / Not implemented in demo.");
                        } // end of command checking
                    } catch (IPWorksException e) {
                        System.out.println(e.getMessage());
                        e.printStackTrace();
                        System.exit(e.getCode());
                        return;
                    }
                }
            } catch (Exception e) {
                displayError(e);
            }

        }
    }

    public static void WriteToFile(String path, String data) {

        try {
            File outputFile = new File(path);
            outputFile.createNewFile();
            FileOutputStream fw = new FileOutputStream(outputFile.getAbsoluteFile());
            fw.write(data.getBytes());
            fw.flush();
            fw.close();
        } catch (IOException ex) {
            System.out.println("Error writing file " + path + ": " + ex.getMessage());
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



