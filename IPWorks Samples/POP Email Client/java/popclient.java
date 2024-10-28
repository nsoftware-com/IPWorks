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

public class popclient extends ConsoleDemo {
    
	public static final int HEADERS = 1;
    public static final int FULL = 2;
    public static final int REPLY = 3;
    static int state;
    static String reply;
    static String msg_from;
    static String msg_date;
    static String msg_subject;
    static int msg_field;
    static int lines;
    static POP pop;
 
    public static void main(String[] args) {
    	
    	if(args.length!=5) {
			System.out.println("* This demo shows how to use POP component to view messages in the mailbox and reply to an email with the Filemailer component. *");
			System.out.println("usage: popclient popserver user password smtpserver from");
			System.out.println("");
			System.out.println("  popserver   the name or address of a mail server (internet post office server)");
			System.out.println("  user        the user identifier for the mailbox");
			System.out.println("  password    the password for the mailbox user");
			System.out.println("  smtpserver  the name or address of a mail server (mail relay)");
			System.out.println("  from        the email address of the sender ");
			System.out.println("\r\nExample: popclient mail.local username password mail.local user@service.com");
			
		} else {
			state = HEADERS;
			msg_field = 0;
			lines = 0;
			reply = "";
        
			pop = new POP();                // POP object
			SMTP smtp = new SMTP();   // SMTP object

			String command;     			// user's command
			String buffer;      			// text buffer
			String[] argument;        		// arguments to the user's command

			int k;         					// counter for use in for() loops
			String msg;            			// storage for mail message
			int msgnum = 0;             	// current message number for next command

			try {
				pop.addPOPEventListener(new DefaultPOPEventListener(){      		
					public void SSLServerAuthentication(POPSSLServerAuthenticationEvent e) {
						e.accept=true; //this will trust all certificates and it is not recommended for production use	
					}
					public void header(POPHeaderEvent e) {
						if (state == FULL){
							System.out.println(e.field+": "+e.value);
						} else if (e.field.equals("From") ){
							msg_from = e.value;
							msg_field++;
						} else if (e.field.equals("Date") ){       	        
							msg_date = e.value;
							msg_field++;
						}else if (e.field.equals("Subject") ){
							msg_subject = e.value;
							msg_field++;
							if ( state == REPLY ) {    //  change the subject to Re:
								String temp;
								temp = msg_subject;
								msg_subject = "Re: ";
								msg_subject += temp;
							}
						}
						if (state == HEADERS && msg_field == 3){
							System.out.print(pop.getMessageNumber()+" ");
							System.out.print(msg_from+" ");
							System.out.println(msg_date+" "+msg_subject);
							msg_field = 0;
						}
					}       		
					public void transfer(POPTransferEvent e) {
						String s = new String(e.text);
						if (state == FULL){
							System.out.println(s);
							lines++;
							if ( lines == 22 ){
								System.out.print("Press ENTER to continue...");
								try { System.in.read(); } catch(IOException ex) {}
								lines = 0;
							}
						} else if (state == REPLY) {   // if replying, add original message text to reply text
							reply += s;
							reply += "\n";
						}
					}        		      									
				});
            
	            pop.setMailServer( args[0] ); // pop server       
	            pop.setUser( args[1] ); // user
	            pop.setPassword( args[2] ); // password
	            pop.connect();
	
	            smtp.setMailServer( args[3]); // smtp server
	            smtp.setFrom( args[4] );
	
	            DisplayMenu();
        
	            while(true)
	            {
	                msg = "";
	                System.out.print( "> " );
	                command = input();
	                argument = command.split("\\s");
	
	                if ( argument.length == 0 || argument[0].length() == 0)
	                {
	                    // do nothing
	                }               
	                else if ( argument[0].equals("d") )
	                {
	                    if ( argument.length > 1 )
	                    {
	                        msgnum = Integer.parseInt(argument[1]);
	                        pop.setMessageNumber(msgnum);
	                        pop.delete();
	                    }
	                    else
	                    {
	                        System.out.println( "No message specified.");
	                    }
	                }
	                else if ( argument[0].equals("h") )
	                {   // get header info for every message
	                    state = HEADERS;
	                    pop.setMaxLines(1);
	                    for (k = 1; k <= pop.getMessageCount(); k++) {
	                        pop.setMessageNumber(k);
	                        pop.retrieve();
	                    }
	                    pop.setMaxLines(0);
	                }
	                else if ( argument[0].equals("m") )
	                {
	                    if ( argument.length < 2 )
	                    {  // if no recipient specified
	                        System.out.print( "To: " );
	                        buffer = input();
	                        smtp.setSendTo( buffer );
	                    }
	                    else
	                    {
	                        smtp.setSendTo(argument[1]);
	                    }
	
	                    System.out.print("Subject: ");
	                    buffer = input();
	                    smtp.setSubject( buffer );
	                                   
	                    System.out.println("Type your message here.Type \".\" on a line by itself to end message.");
	                    msg = "\n";                          // initialize the message string
	                    buffer = input();                    // get first line
	                    while( !buffer.equals(".") )
	                    {              // until period on a line by itself
	                        msg += buffer;                    // add a line
	                        msg += "\n";                      // with an endline
	                        buffer = input();                 // get next line
	                    }
	
	                    smtp.setMessageText(msg);
	
	                    System.out.print( "Cc: " );
	                    buffer = input();
	                    smtp.setCc(buffer);
	
	                    smtp.send();
	                }
	                else if ( argument[0].equals("n") )
	                {
	                    msgnum++;
	                    state = FULL;
	                    pop.setMessageNumber(msgnum);
	                    pop.retrieve();
	                }
	                else if ( argument[0].equals("q") )
	                {
	                    System.out.print("Save changes to inbox? (y/n): ");
	                    int n = System.in.read();
	                    if ( n == 'n' )
	                    {
	                        pop.reset();
	                    }
	                    pop.disconnect();
	                    System.exit(0);
	                    return;
	                }
	                else if ( argument[0].equals("r") )
	                {
	                    if ( argument.length > 1 )
	                    {  // get number of message to reply to
	                        msgnum = Integer.parseInt(argument[1]);
	                        System.out.println("Type your message here. Type \".\" on a line by itself to continue.");
	                    }
	                    if(msgnum == 0)
	                    {
	                        System.out.println("No message specified.");
	                    }
	                    else
	                    {
	                        state = REPLY;
	                        pop.setMessageNumber(msgnum);
	                        pop.retrieve();
	                        smtp.setSendTo(msg_from);
	                        smtp.setSubject(msg_subject);
	
	                        buffer = input();
	                        while( !buffer.equals(".") )
	                        {
	                            msg += buffer;
	                            msg += "\n";
	                            buffer = input();
	                        }
	
	                        msg += msg_from;  // insert name of previous message author
	                        msg += " wrote: \n";
	                        msg += reply;     // insert previous message
	                        reply = "";      // reset reply field for future use
	
	                        smtp.setMessageText(msg);
	                        System.out.print( "Cc: " );
	                        buffer = input();
	                        smtp.setCc(buffer);
	                        smtp.send();
	                    }
	                }
	                else if ( argument[0].equals("v") )
	                {
	                    if ( argument.length > 1 )
	                    {
	                        msgnum = Integer.parseInt(argument[1]);
	                        state = FULL;
	                        pop.setMessageNumber(msgnum);
	                        pop.retrieve();
	                    }
	                    else
	                    {
	                        System.out.println( "No message specified." );
	                    }
	                }
	                else
	                {
	                    try
	                    {
	                        // allow user to enter only the number
	                        //  of the message they want to view
	                        msgnum = Integer.parseInt(command);
	                        state = FULL;
	                        pop.setMessageNumber(msgnum);
	                        pop.retrieve();
	                    }
	                    catch(NumberFormatException e)
	                    {
	                        System.out.println( "Bad command / Not implemented in demo." );
	                    }
	                } // end of command checking
	            }  // end of main while loop
	        }
	        catch(Exception e)
	        {
	        	displayError(e);
	        }
		}        
    }

    private static void DisplayMenu() {
    	System.out.println("Mail Commands");
        System.out.println("v <message number>     view the content of selected message");
        System.out.println("n                      goto and view next message");
        System.out.println("h                      give head lines of messages");
        System.out.println("d <message number>     delete selected message");
        System.out.println("m <user>               mail to specific users");
        System.out.println("q                      quit, saving unresolved messages in mbox");       
        System.out.println("r <message number>     reply to message");	
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



