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

public class ftp extends ConsoleDemo {
	
	private static FTP ftp1 = null;
	private static int verbose;
	private static long transtime;
	private static long transbytes;

	public static void main(String[] args) {
		
		if (args.length != 3) {

			System.out.println("usage: ftp server username password");
			System.out.println("");
			System.out.println("  server    the domain name or IP address of the FTP server");
			System.out.println("  username  the user identifier to use for login");
			System.out.println("  password  the password to log in.");
			System.out.println("\r\nExample: ftp 127.0.0.1 username password");

		} else {
			
			try {
			      ftp1 = new FTP();			      
			      ftp1.addFTPEventListener(new DefaultFTPEventListener(){
			    	  
			    	  public void PITrail(FTPPITrailEvent e) {
			    		  if (verbose == 1)
			    		      System.out.println(e.message);	
			    		}			    		
			    		public void SSLServerAuthentication(FTPSSLServerAuthenticationEvent arg0) {
			    			arg0.accept=true; //this will trust all certificates and it is not recommended for production use
			    		}				    					    					    					    		
			    		public void dirList(FTPDirListEvent e) {
			    			System.out.println(e.dirEntry);	
			    		}
			    		public void endTransfer(FTPEndTransferEvent e) {
			    			long endtime;
			    		    endtime = System.currentTimeMillis();
			    		    transtime = endtime - transtime;		
			    		}			    		
			    		public void error(FTPErrorEvent e) {
			    			System.out.println("\nError " + e.errorCode + ": " + e.description);
			    		}
			    		public void startTransfer(FTPStartTransferEvent e) {
			    			transtime = System.currentTimeMillis();			    			
			    		}
			    		public void transfer(FTPTransferEvent e) {
			    			transbytes = e.bytesTransferred;	
			    		}  
																						    				    				    			      				      			      					      					      					      	            		            	
		            }); 		      
			      
			      //logon
			      ftp1.setRemoteHost(args[0]); //server
			      logon(args[1], args[2]);
			      System.out.println("Type \"?\" for a list of commands.");
			      String command;
			      String[] arguments; 	  
			      while (true) {
			        ftp1.setRemoteFile("");			        
			        command = prompt("ftp", ">");
			        arguments = command.split("\\s");

			        if (arguments[0].equals("?") || arguments[0].equals("help")) {
			          System.out.println("?        bye     help     put     rmdir");
			          System.out.println("append   cd      ls       pwd     verbose");
			          System.out.println("ascii    close   mkdir    quit");
			          System.out.println("binary   get     open     rm");
			        } else if (arguments[0].equals("append")) {
			          ftp1.setLocalFile(arguments[1]);
			          ftp1.setRemoteFile(arguments[2]);
			          ftp1.append();
			        } else if (arguments[0].equals("ascii")) {
			          ftp1.changeTransferMode(FTP.tmASCII);
			        } else if (arguments[0].equals("binary")) {
			          ftp1.changeTransferMode(FTP.tmBinary);
			        } else if (arguments[0].equals("bye") || arguments[0].equals("quit") || arguments[0].equals("exit")) {
			          ftp1.logoff();
			          break;
			        } else if (arguments[0].equals("close")) {
			          ftp1.logoff();
			        } else if (arguments[0].equals("cd")) {
			          if (arguments.length > 0)
			            ftp1.changeRemotePath(arguments[1]);
			        } else if (arguments[0].equals("get")) {
			          ftp1.setRemoteFile(arguments[1]);
			          ftp1.setLocalFile(arguments[1]);
			          ftp1.download();
			          updateTime();
			        } else if (arguments[0].equals("ls")) {
			          if (arguments.length > 1) {
			            String pathname = ftp1.queryRemotePath();
			            ftp1.changeRemotePath(arguments[1]);
			            ftp1.listDirectoryLong();
			            ftp1.changeRemotePath(pathname);
			          } else			        	
			            ftp1.listDirectoryLong();
			        } else if (arguments[0].equals("mkdir")) {
			          if (arguments.length > 1)
			            ftp1.makeDirectory(arguments[1]);
			        } else if (arguments[0].equals("mv")) {
			          ftp1.setRemoteFile(arguments[1]);
			          ftp1.renameFile(arguments[2]);
			        } else if (arguments[0].equals("open")) {
			          ftp1.logoff();
			          ftp1.setRemoteHost(arguments[1]);
			          logon(args[1], args[2]);
			        } else if (arguments[0].equals("passive")) {
			          if (arguments.length > 1) {
			            if ((arguments[1].equals("on")) && !ftp1.isPassive()) {
			              ftp1.setPassive(true);
			              System.out.println("Passive mode ON.");
			            } else if ((arguments[1].equals("off")) && ftp1.isPassive()) {
			              ftp1.setPassive(false);
			              System.out.println("Passive mode OFF.");
			            }
			          }
			        } else if (arguments[0].equals("put")) {
			          ftp1.setRemoteFile(arguments[2]);
			          ftp1.setLocalFile(arguments[1]);
			          ftp1.upload();
			          updateTime();
			        } else if (arguments[0].equals("pwd")) {
			          System.out.println(ftp1.queryRemotePath());
			        } else if (arguments[0].equals("rm")) {
			          if (arguments.length > 1)
			            ftp1.deleteFile(arguments[1]);
			        } else if (arguments[0].equals("rmdir")) {
			          if (arguments.length > 1)
			            ftp1.removeDirectory(arguments[1]);
			        } else if (arguments[0].equals("verbose")) {
			          if (arguments.length > 1) {
			            if ((arguments[1].equals("on")) && verbose == 0) {
			              toggle_verbose();
			            } else if ((arguments[1].equals("off")) && verbose == 1) {
			              toggle_verbose();
			            }
			          } else {
			            toggle_verbose();
			          }
			        } else if (arguments[0].equals("")) {
			          // Do nothing
			        } else {
			          System.out.println("Bad command / Not implemented in demo.");
			        } // end of command checking
			      }
			    } catch (IPWorksException e) {
			      System.out.println(e.getMessage());
			      System.exit(e.getCode());
			    } catch (Exception e) {
			      System.out.println(e.getMessage());
			    }
		}	    
	}
	
	private static void updateTime() {
	    System.out.print(transbytes);
	    System.out.print(" bytes sent in ");
	    System.out.print(((float) transtime / 1000));
	    System.out.print(" seconds.  (");
	    System.out.print(((float) transbytes) / transtime);
	    System.out.println("KBps)");
	}

	static void logon(String arg1, String arg2) throws IPWorksException {
	    ftp1.setUser(arg1);
	    ftp1.setPassword(arg2);
	    ftp1.logon();
	}

	public static void toggle_verbose() {
	    verbose = 1 - verbose;
	    System.out.print("Verbose mode ");

	    if (verbose == 1)
	      System.out.println("ON.");
	    else
	      System.out.println("OFF.");
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



