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
import ipworks.*;

public class odata extends ConsoleDemo {

	
	private static Odata odata1 = null;	
	
	
	public static void main(String[] args) {
		
		try { 
			
			odata1 = new Odata();
			String firstName = "";
			String lastName = "";
			String email = "";
			String gender = "";
			String city = "";
			String country = "";
					
			// Print welcome message and list of commands.
			System.out.println("This application demonstrates how to use the OData component to query OData's TripPin service.\r\n");
			
			// User input
			lastName = prompt("Filter by Last Name: (leave blank to get all entries)");
			
			System.out.println("Searching for '" + lastName + "'...\r\n");
			
						
			odata1.setServiceRootURI("http://services.odata.org/TripPinRESTierService/");
			odata1.setResourcePath("People");
			odata1.setQueryFilter("contains(LastName, '" + lastName + "')");		
			odata1.setQueryTop("20");
			odata1.queryService();
			
			for (int i = 0; i <= odata1.getEntryCount()-1; i++)
			{
				odata1.setEntryIndex(i);
				for (int j = 0; j <= odata1.getEntryProperties().size()-1; j++)
				{
					if (odata1.getEntryProperties().item(j).getName().equals("FirstName")) 
					{
						firstName=odata1.getEntryProperties().item(j).getValue();
					}
					if (odata1.getEntryProperties().item(j).getName().equals("LastName")) 
					{
						lastName=odata1.getEntryProperties().item(j).getValue();
					}
					if (odata1.getEntryProperties().item(j).getName().equals("Emails/[1]")) 
					{
						email=odata1.getEntryProperties().item(j).getValue();
					}
					if (odata1.getEntryProperties().item(j).getName().equals("Gender")) 
					{
						gender=odata1.getEntryProperties().item(j).getValue();
					}
					if (odata1.getEntryProperties().item(j).getName().equals("AddressInfo/[1]/City/Name")) 
					{
						city=odata1.getEntryProperties().item(j).getValue();
					}
					if (odata1.getEntryProperties().item(j).getName().equals("AddressInfo/[1]/City/CountryRegion")) 
					{
						country=odata1.getEntryProperties().item(j).getValue();
					}
				}
		        
		        				
				System.out.println(firstName + " " + lastName);
				if(!email.equals("")) System.out.println(email);
				else System.out.println("Email not specified");
				if(!gender.equals("")) System.out.println(gender);
				else System.out.println("Gender not specified");
				if(!city.equals("")) System.out.println(city);
				else System.out.println("City not specified");
				if(!country.equals("")) System.out.println(country + "\r\n");
				else System.out.println("Country not specified\r\n");
			}
				
						
		} catch (IPWorksException e) {
			System.out.println(e.getMessage());
	        System.exit(e.getCode());
	        return;
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



