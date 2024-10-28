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

class smppDemo
{
  private static SMPP smpp;

  private static void smpp_OnSSLServerAuthentication(object sender, SMPPSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    smpp = new SMPP();

    if (args.Length < 3)
    {
      Console.WriteLine("usage: smpp [options] server user password");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -ssl        whether or not to use SSL/TLS (default false)");
      Console.WriteLine("  -port       the port for secure SMPP (default 2775)");
      Console.WriteLine("  server      the SMPP entity to which the component will connect");
      Console.WriteLine("  user        the user identification for the SMPP entity");
      Console.WriteLine("  password    the valid password for the SMPP entity user");
      Console.WriteLine("\r\nExample: smpp -ssl false -port 2775 mysmppserver myuserid mypassword");
    }
    else
    {
      smpp.OnSSLServerAuthentication += smpp_OnSSLServerAuthentication;

      try
      {
        // Parse arguments into component.
        smpp.SMPPServer = args[args.Length - 3];
        smpp.UserId = args[args.Length - 2];
        smpp.Password = args[args.Length - 1];

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-ssl")) smpp.SSLEnabled = bool.Parse(args[i + 1]);     // args[i + 1] corresponds to the value of args[i]
            else if (args[i].Equals("-port")) smpp.SMPPPort = int.Parse(args[i + 1]);
          }
        }

        if (smpp.SSLEnabled) smpp.SSLStartMode = SMPPSSLStartModes.sslAutomatic;

        // Attempt to connect to the SMPP server.
        smpp.Connect();

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
            Console.WriteLine("  add <recipient>              add a recipient to the recipient list (should either be a dotted IPv4 address or directory number of a mobile phone)");
            Console.WriteLine("  list                         display the list of recipients");
            Console.WriteLine("  send <message>               send a message to all recipients in the recipient list");
            Console.WriteLine("  quit                         exit the application");
          }
          else if (arguments[0].Equals("add"))
          {
            if (arguments.Length > 1) smpp.AddRecipient(0, arguments[1]);
            else Console.WriteLine("Please supply a recipient to add.");
          }
          else if (arguments[0].Equals("list"))
          {
            Console.WriteLine("Listing recipients...");
            for (int i = 0; i < smpp.Recipients.Count; i++)
            {
              Console.WriteLine("\t" + smpp.Recipients[i].Address);
            }
          }
          else if (arguments[0].Equals("send"))
          {
            if (arguments.Length > 1)
            {
              string fullMessage = "";
              for (int i = 1; i < arguments.Length; i++)
              {
                fullMessage += arguments[i] + " ";
              }
              smpp.SendMessage(fullMessage);
            }
            else
            {
              Console.WriteLine("Please supply a message to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            smpp.Disconnect();
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

          Console.Write("smpp> ");
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