/*
 * IPWorks 2024 .NET Edition - Sample Project
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
 * 
 */


using nsoftware.IPWorks;
using System.Collections.Generic;
using System;
using System.IO;

namespace carddav {
  class carddavDemo : ConsoleDemo {

    static CardDAV carddav = new CardDAV();

    static OAuth oauth = new OAuth();
    static String[] argument;            // arguments to the user's command
    static String clientID = "";
    static String clientSecret = "";
    static List<string> resourceURIs = new List<string>();
    static bool exportingContact = false;
    static int contactNumber = 1;
    static string user;
    static string password; 

    static void Main(string[] args) {
      if (args.Length < 3) {
        Console.WriteLine("usage: carddav [options] provider username password");
        Console.WriteLine("Options");
        Console.WriteLine("  -i        OAuth Client ID");
        Console.WriteLine("  -s        OAuth Client Secret");
        Console.WriteLine("  URI       the URI of the address book");
        Console.WriteLine("  username  the username to login");
        Console.WriteLine("  password  the password to login");
        Console.WriteLine("\r\nExample: carddav -i OAuthClientID -s OAuthClientSecret URI username password");
      } else {
        try {
          carddav.OnSSLServerAuthentication += (s, e) => {
            e.Accept = true; //this will trust all certificates and it is not recommended for production use
          };
          carddav.OnContactDetails += (s, e) => {
            if (!exportingContact) {
              resourceURIs.Add(e.ResourceURI);
              string phoneNumber = (carddav.PhoneNumbers.Count > 0 && !string.IsNullOrEmpty(carddav.PhoneNumbers[0].Value.Trim())) ? carddav.PhoneNumbers[0].Value : " - ";
              string email = (carddav.EMails.Count > 0 && !string.IsNullOrEmpty(carddav.EMails[0].Value.Trim())) ? carddav.EMails[0].Value : " - ";
              Console.WriteLine("{0, -5} {1,-40} {2,-30} {3,-30} {4,-30}", contactNumber + ")", carddav.FormattedName, phoneNumber, email, e.ResourceURI);
              contactNumber++;
            }
          };
          Console.WriteLine("********************************************************************************");
          Console.WriteLine("* This demo shows how to use the CardDAV component to list contacts from an    *");
          Console.WriteLine("* existing Google or NextCloud address book. You can also create a new         *");
          Console.WriteLine("* contact, delete a contact, or export a contact to a vcf file.                *");
          Console.WriteLine("********************************************************************************\n");

          Console.WriteLine("Type \"?\" for a list of commands.");

          while (true) {
            try {
              exportingContact = false;
              argument = Prompt("> ", "").Split(' ');

              for (int i = 0; i < args.Length; i++) {
                if (args[i].StartsWith("-")) {
                  if (args[i] == "-i")
                    clientID = args[i + 1];
                  if (args[i] == "-s")
                    clientSecret = args[i + 1];
                }
              }

              user = args[args.Length - 2];
              password = args[args.Length - 1];
              string mainAddressbookURL = args[args.Length - 3];


              if (argument[0] == "?") {

                Console.WriteLine("1) List Contacts");
                Console.WriteLine("2) Add Contact");
                Console.WriteLine("3) Delete Contact");
                Console.WriteLine("4) Export VCF File");
                Console.WriteLine("?) Help menu.");
                Console.WriteLine("Q) Quit.");

              } else if (argument[0] == "1") {

                resourceURIs.Clear();
                contactNumber = 1;
                Console.WriteLine("{0, -5} {1,-40} {2,-30} {3,-30} {4,-30}", "", "Name", "PhoneNumber", "Email", "ResourceURI\n");
                GetAuthorization();
                carddav.GetAddressbookReport(mainAddressbookURL);
                carddav.Reset();

              } else if (argument[0] == "2") {

                GetAuthorization();
                carddav.UID = DateTime.Now.ToString("yyyyMMddTHHmmssZ");
                carddav.FormattedName = Prompt("Full Name", "");
                carddav.PhoneNumbers.Add(new CardCustomProp("TEL", Prompt("Phone number", "")));
                carddav.EMails.Add(new CardCustomProp("TEL", Prompt("Email", "")));
                carddav.Addresses.Add(new CardCustomProp("TEL", Prompt("Address", "")));
                carddav.CreateContact(mainAddressbookURL + "/" + carddav.UID + ".vcf");
                Console.WriteLine("Contact successfully added");

              } else if (argument[0] == "3") {

                String resource = Prompt("Give name of file to delete (find the file names by running option #1)", "");
                GetAuthorization();
                carddav.DeleteContact(mainAddressbookURL + "/" + resource);
                Console.WriteLine("Contact successfully deleted");

              } else if (argument[0] == "4") {

                String resource = Prompt("Give name of the .vcf file (find the file names by running option #1)", "");
                GetAuthorization();
                Console.WriteLine("{0, -5} {1,-40} {2,-30} {3,-30} {4,-30}", "", "Name", "PhoneNumber", "Email", "ResourceURI\n");
                carddav.GetContact(mainAddressbookURL + "/" + resource);
                File.WriteAllText(resource, carddav.ExportVCF());
              } else if (string.Equals(argument[0].ToLower(), "q")) {

                Environment.Exit(0);
                return;

              } else if (string.Equals(argument[0], "")) {
                // do nothing
              } else {
                Console.WriteLine("Bad command / Not implemented in demo.");
              }
            } catch (IPWorksException e) {
              Console.WriteLine(e.Message);
              Environment.Exit(0);
              return;
            }
          }
        } catch (Exception e) {
          Console.WriteLine(e);
        }
      }
    }

    private static void GetAuthorization() {
      carddav.User = user;
      if (String.IsNullOrEmpty(clientID) && String.IsNullOrEmpty(clientSecret)) {
        carddav.Password = password;
      } else {
        oauth.ClientId = clientID;
        oauth.ClientSecret = clientSecret;
        oauth.ServerAuthURL = "https://accounts.google.com/o/oauth2/auth";
        oauth.ServerTokenURL = "https://accounts.google.com/o/oauth2/token";
        oauth.AuthorizationScope = "https://www.googleapis.com/auth/carddav";
        carddav.AuthScheme = CardDAVAuthSchemes.authOAuth;
        carddav.Authorization = oauth.GetAuthorization();
      }
    }

  }

}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}