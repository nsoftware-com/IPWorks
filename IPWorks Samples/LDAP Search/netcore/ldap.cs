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

﻿using System;
using nsoftware.IPWorks;

class ldapDemo
{
  private static LDAP ldap;

  private static void ldap_OnConnected(object sender, LDAPConnectedEventArgs e)
  {
    Console.WriteLine("Successfully connected to " + ldap.ServerName + ".");
  }

  private static void ldap_OnDisconnected(object sender, LDAPDisconnectedEventArgs e)
  {
    Console.WriteLine("Successfully disconnected from " + ldap.ServerName + ".");
  }

  private static void ldap_OnSearchResult(object sender, LDAPSearchResultEventArgs e)
  {
    if (ldap.SearchScope == LDAPSearchScopes.ssBaseObject)
    {
      Console.WriteLine("Base DN Attributes:");
      for (int attributeNumber = 0; attributeNumber < ldap.Attributes.Count; attributeNumber++)
      {
        if (string.IsNullOrEmpty(ldap.Attributes[attributeNumber].AttributeType))
        {
          ldap.Attributes[attributeNumber].AttributeType = ldap.Attributes[attributeNumber - 1].AttributeType;
        }
        Console.WriteLine("\t" + ldap.Attributes[attributeNumber].AttributeType + "\t\t\t" + ldap.Attributes[attributeNumber].Value);
      }
    }
    else
    {
      Console.WriteLine("\t" + e.DN);
    }
  }

  private static void ldap_OnSSLServerAuthentication(object sender, LDAPSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    ldap = new LDAP();

    if (args.Length < 1)
    {
      Console.WriteLine("usage: ldap [options] server");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -DN        the distinguished name used as the base for LDAP operations");
      Console.WriteLine("  -password  the password used to authenticate to the LDAP server");
      Console.WriteLine("  server     the name or address of the LDAP server");
      Console.WriteLine("\r\nExample: ldap -DN dc=umich,dc=edu ldap-master.itd.umich.edu");
    }
    else
    {
      ldap.OnConnected += ldap_OnConnected;
      ldap.OnDisconnected += ldap_OnDisconnected;
      ldap.OnSearchResult += ldap_OnSearchResult;
      ldap.OnSSLServerAuthentication += ldap_OnSSLServerAuthentication;

      try
      {
        // Parse arguments into component.
        ldap.ServerName = args[args.Length - 1];

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-DN"))
            {
              ldap.DN = args[i + 1];  // args[i + 1] corresponds to the value of args[i]
            }
            else if (args[i].Equals("-password"))
            {
              ldap.Password = args[i + 1];  // args[i + 1] corresponds to the value of args[i]
            }
          }
        }

        // Attempt to bind to the LDAP server.
        ldap.Bind();

        // Process user commands.
        Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
        string command;
        string[] arguments;

        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0].Equals("?") || arguments[0].Equals("help"))
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                            display the list of valid commands");
            Console.WriteLine("  help                         display the list of valid commands");
            Console.WriteLine("  search <base DN>             search the server using the specified base object");
            Console.WriteLine("  quit                         exit the application");
          }
          else if (arguments[0].Equals("search"))
          {
            if (arguments.Length > 1)
            {
              Console.WriteLine("Searching...");
              // Account for DN whitespace being parsed into multiple arguments.
              ldap.DN = "";
              for (int argumentNumber = 1; argumentNumber < arguments.Length; argumentNumber++)
              {
                ldap.DN += arguments[argumentNumber] + " ";
              }
              // Perform the base object search for attributes.
              ldap.SearchScope = LDAPSearchScopes.ssBaseObject;
              ldap.Search("objectClass=*");
              // Perform a single-level search for children if the previous search was successful.
              if (ldap.ResultCode == 0)
              {
                Console.WriteLine("Child Entries:");
                ldap.SearchScope = LDAPSearchScopes.ssSingleLevel;
                ldap.Search("objectClass=*");
                Console.WriteLine("LDAP search complete.");
              }
              else
              {
                Console.WriteLine("LDAP search failed.  " + ldap.ResultCode + ": " + ldap.ResultDescription);
              }
            }
            else
            {
              Console.WriteLine("Please supply a valid base DN in order to search.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            ldap.Unbind();
            break;
          }
          else if (arguments[0].Equals(""))
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          }

          Console.Write("ldap> ");
        }
      }
      catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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