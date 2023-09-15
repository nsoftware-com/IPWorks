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
import java.io.*;
import java.nio.charset.StandardCharsets;

import ipworks.*;

public class webdav extends ConsoleDemo {

    static Webdav webdav = new Webdav();
    static String command;               // user's command
    static String[] argument;            // arguments to the user's command

    public static void main(String[] args) {

        if (args.length < 2) {

            System.out.println("usage: webdav [options] provider username password");
            System.out.println("Options");
            System.out.println("  ");
            System.out.println("  username  the username to login");
            System.out.println("  password  the password to login");
            System.out.println("\r\nExample: webdav username password");

        } else {
            try {
                webdav.addWebdavEventListener(new DefaultWebdavEventListener() {
                    public void SSLServerAuthentication(WebdavSSLServerAuthenticationEvent e) {
                        e.accept = true; //this will trust all certificates and it is not recommended for production use
                    }

                    public void connected(WebdavConnectedEvent e) {
                        System.out.println("Server connected");
                    }

                    public void connectionStatus(WebdavConnectionStatusEvent e) {
                        System.out.println("Status code " + e.statusCode + ": " + e.description);
                    }

                    public void disconnected(WebdavDisconnectedEvent e) {
                        System.out.println("Server disconnected");
                    }
                    // Resource will be empty if data is being posted to the server
                    public void transfer(WebdavTransferEvent e) {
                        System.out.println("Resource being received from server (in full text): \n" +
                                           "========================================= \n" + new String(e.text, StandardCharsets.UTF_8));

                    }

                });

                System.out.println("******************************************************************************");
                System.out.println("* This demo shows how to use the WebDAV component to make directories, move  *");
                System.out.println("* resources, delete resource, and to get resources.                          *");
                System.out.println("******************************************************************************\n");

                System.out.println("Type \"?\" for a list of commands.");

                while (true) {

                    try {
                        System.out.print("> ");
                        command = input();
                        argument = command.split("\\s");

                        if (argument[0].equals("?")) {
                            System.out.println("1) Make Directory");
                            System.out.println("2) Move Resource");
                            System.out.println("3) Get Resource");
                            System.out.println("4) Delete Resource");
                            System.out.println("5) Put Resource");
                            System.out.println("?) This help menu.");
                            System.out.println("Q) Quit.");

                        } else if (argument[0].equals("1")) { // Make Directory
                            webdav.reset();
                            webdav.setUser(args[args.length - 2]);
                            webdav.setPassword(args[args.length - 1]);
                            String server = prompt("Name server where you wish to create a directory (ex. localhost:443)");
                            String dir = prompt("Name directory to create (ex. directory/folder)");
                            webdav.makeDirectory(server + "/" + dir);


                        } else if (argument[0].equals("2")) {  // Move Resource
                            webdav.reset();
                            webdav.setUser(args[args.length - 2]);
                            webdav.setPassword(args[args.length - 1]);
                            String server = prompt("Name server (ex. localhost:443)");
                            String src = prompt("Name source of the resource (ex. myoldfolder/myfile.txt)");
                            String dest = prompt("Name destination of the resource (ex. mynewfolder/myfile.txt)");
                            webdav.moveResource(server + "/" + src, "/" + dest);


                        } else if (argument[0].equals("3")) { // Get Resource
                            webdav.reset();
                            webdav.setUser(args[args.length - 2]);
                            webdav.setPassword(args[args.length - 1]);
                            String get = prompt("Name URI of resource you wish to get");
                            webdav.getResource(get);

                        } else if (argument[0].equals("4")) { // Delete Resource
                            webdav.reset();
                            webdav.setUser(args[args.length - 2]);
                            webdav.setPassword(args[args.length - 1]);
                            String del = prompt("Name URI of resource you wish to delete");
                            webdav.deleteResource(del);

                        } else if (argument[0].equals("5")) { // Put Resource on server, can create resource on server or replace old resource
                            webdav.reset();
                            webdav.setUser(args[args.length - 2]);
                            webdav.setPassword(args[args.length - 1]);
                            String file = prompt("Name path of file you wish to put on server");
                            webdav.setLocalFile(file);
                            String put = prompt("Name URI of resource you wish to put on server ");
                            webdav.putResource(put);

                        }  else if (argument[0].toLowerCase().equals("q")) {
                            System.exit(0);
                            return;
                        } else if (argument[0].equals("")) {
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



