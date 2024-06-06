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


public class htmlmailer extends ConsoleDemo {
	
	public static void main(String[] args) {
		
		HTMLMailer htmlmailer = new HTMLMailer();
    
		if(args.length<3) {
		
			System.out.println("usage: htmlmailer [options] server from to");
			System.out.println("Options: ");
			System.out.println("  -s      the subject of the mail message");
			System.out.println("  -m      the HTML version of the message content");
			System.out.println("  -a      the path of file to attach to the message");
			System.out.println("  server  the name or address of a mail server (mail relay)");			
			System.out.println("  from    the email address of the sender");
			System.out.println("  to      a comma separated list of addresses for destinations");
			System.out.println("\r\nExample: htmlmailer -s test -m \"<b>Hello</b>, my name is <i>Tom</i>\" -a FileToAttach mail.local sender@mail.com recipient@mail.local");
		
		} else {
		
			try {
			
				htmlmailer.addHTMLMailerEventListener(new DefaultHTMLMailerEventListener(){
					
					public void SSLServerAuthentication(HTMLMailerSSLServerAuthenticationEvent arg0) {
						arg0.accept = true; //this will trust all certificates and it is not recommended for production use
					}			    	  			    	 																					    				    				    			      				      			      					      					      					      	            		            	
		        });
				htmlmailer.setMailServer(args[args.length - 3]);
				htmlmailer.setFrom(args[args.length - 2]);
				htmlmailer.setSendTo(args[args.length - 1]);
		      
				for (int i=0; i<args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-s")) htmlmailer.setSubject(args[i+1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-m")) htmlmailer.setMessageHTML(args[i+1]);
						if (args[i].equals("-a")) htmlmailer.addAttachment(args[i+1]); //if you want to add attachment
					}
				}
		      
			    //Use these properties for client authentication
			    //htmlmailer.setUser(prompt("User"));
			    //htmlmailer.setPassword(prompt("Password"));
			     
			    System.out.println("Sending message ...");
			    htmlmailer.send();
	
			    System.out.println("Message sent successfully");
			    System.exit(0);
			} catch (Exception e) {
				System.out.println(e.getMessage());
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



