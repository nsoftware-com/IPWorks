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

import ipworks.*;

public class s3 extends ConsoleDemo {
  public s3() {
    S3 s31 = new S3();
    String buffer = "";
    try {
      System.out.println("\nWelcome to the IPWorks S3 Demo.");
      System.out.println("This demo shows how to use the IPWorks S3 component to interact with S3 compatible storage providers.");
      System.out.println("It is assumed that you already signed up for a service (for example, at aws.amazon.com/s3) and have an Access Key and Secret Key which are required to use the service.\n");
      System.out.println("------------------------------------------------------------");
      s31.addS3EventListener(new S3Events());
      while (true) {
        System.out.println("\n0     -   Amazon S3");
        System.out.println("1     -   Digital Ocean");
        System.out.println("2     -   Google Storage");
        System.out.println("3     -   Wasabi");
        System.out.println("4     -   Backblaze B2");
        System.out.println("5     -   Huawei");
        System.out.println("6     -   Alibaba");
        System.out.println("7     -   IBM");
        System.out.println("9     -   Linode");
        System.out.println("255   -   Custom\n");

        System.out.print("Select Service Provider (enter number): ");
        try {
          s31.setServiceProvider(Integer.parseInt(input().trim()));
          break;
        } catch (Exception ex) {
          System.err.println(ex.getMessage());
        }
      }
      if (s31.getServiceProvider() == S3.spCustom) {
        while (true) {
          System.out.print("Enter custom provider URL: ");
          try {
            s31.config("URL=" + input());
            break;
          } catch (Exception ex) {
            System.err.println(ex.getMessage());
          }
        }
      }
      System.out.print("Access Key: ");
      s31.setAccessKey(input());
      System.out.print("Secret Key: ");
      s31.setSecretKey(input());
      System.out.println("\nBucket List:\n");
      s31.listBuckets();
      printMenu();
      while (!buffer.equals("q")) {
        System.out.print("Enter command: ");
        buffer = input();
        if (buffer.toLowerCase().equals("cd")) {
          System.out.print("Enter Bucket: ");
          s31.setBucket(input());
        } else if (buffer.toLowerCase().equals("lb")) {
          s31.listBuckets();
        } else if (buffer.toLowerCase().equals("lo")) {
          s31.listObjects();
        } else if (buffer.toLowerCase().equals("get")) {
          System.out.print("Which object: ");
          s31.getObject(input());
          System.out.println("The content of the object:\n" + new String(s31.getObjectData()));
        } else if (buffer.toLowerCase().equals("put")) {
          String name;
          System.out.print("Name of new object:");
          name = input();
          System.out.print("Local file to upload:");
          s31.setLocalFile(input());
          s31.createObject(name);
          System.out.println("Object created.");
        } else if (buffer.toLowerCase().equals("q")) {
          System.exit(0);
        } else if (buffer.toLowerCase().equals("?")) {
          printMenu();
        } else {
          System.out.println("Command not recognized.");
        }
      }
    } catch (IPWorksException e) {
      System.out.println(e.getMessage());
      System.exit(e.getCode());
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
  }

  private void printMenu() {
    System.out.println("\n?\t-\tHelp\n" +
      "cd\t-\tChange to Bucket\n" +
      "lb\t-\tList Buckets\n" +
      "lo\t-\tList Objects\n" +
      "get\t-\tGet Object\n" +
      "put\t-\tPut Object\n" +
      "q\t-\tQuit\n");
  }

  public static void main(String[] args) {
    try {
      new s3();
    } catch (Exception ex) {
      System.out.println("Exception: " + ex.getMessage());
    }
  }
}

class S3Events extends DefaultS3EventListener {
  public void bucketList(S3BucketListEvent arg) {
    System.out.println(arg.bucketName);
  }

  public void objectList(S3ObjectListEvent arg) {
    System.out.println(arg.objectName);
  }

  public void SSLServerAuthentication(S3SSLServerAuthenticationEvent arg) {
    arg.accept = true;
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



