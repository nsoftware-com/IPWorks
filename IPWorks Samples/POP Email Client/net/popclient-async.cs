/*
 * IPWorks 2022 .NET Edition - Sample Project
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

using System.Collections.Generic;
﻿using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorks;

class popclientDemo
{
  private static Pop pop;

  private static void pop_OnHeader(object sender, PopHeaderEventArgs e)
  {
    Console.WriteLine(e.Field + ": " + e.Value);
  }

  private static void pop_OnSSLServerAuthentication(object sender, PopSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void pop_OnTransfer(object sender, PopTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
  }

  static async Task Main(string[] args)
  {
    pop = new Pop();

    if (args.Length < 3)
    {
      Console.WriteLine("usage: popclient [options] server user password");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -ssl       whether or not to use SSL/TLS (default true)");
      Console.WriteLine("  -port      the port of a POP mail server (default 995)");
      Console.WriteLine("  server     the name or address of a POP mail server");
      Console.WriteLine("  user       the user identifier for the mailbox");
      Console.WriteLine("  password   the password for the mailbox user");
      Console.WriteLine("\r\nExample: popclient -ssl true -port 995 mypopserver myusername mypassword");
    }
    else
    {
      pop.OnHeader += pop_OnHeader;
      pop.OnSSLServerAuthentication += pop_OnSSLServerAuthentication;
      pop.OnTransfer += pop_OnTransfer;

      try
      {
        // Parse arguments into component.
        pop.MailServer = args[args.Length - 3];
        pop.User = args[args.Length - 2];
        pop.Password = args[args.Length - 1];

        bool sslParamSet = false, portParamSet = false;
        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-ssl"))
            {
              pop.SSLEnabled = bool.Parse(args[i + 1]); // args[i + 1] corresponds to the value of args[i]
              sslParamSet = true;
            }
            else if (args[i].Equals("-port"))
            {
              pop.MailPort = int.Parse(args[i + 1]);  // args[i + 1] corresponds to the value of args[i]
              portParamSet = true;
            }
          }
        }

        if (!portParamSet) pop.MailPort = 995;
        if (!sslParamSet) pop.SSLEnabled = true;
        if (pop.SSLEnabled) pop.SSLStartMode = PopSSLStartModes.sslAutomatic;

        // Attempt to connect to the POP server.
        await pop.Connect();

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
            Console.WriteLine("  retrieve <message number>    display the contents of the specified message");
            Console.WriteLine("  quit                         exit the application");
          }
          else if (arguments[0].Equals("retrieve"))
          {
            if (arguments.Length > 1 && int.TryParse(arguments[1], out int messageNumber) && messageNumber > 0 && messageNumber <= pop.MessageCount)
            {
              pop.MessageNumber = messageNumber;
              await pop.Retrieve();
            }
            else
            {
              Console.WriteLine("Please supply a number between 1 and " + pop.MessageCount + " (inclusive) corresponding to the message in the mailbox you would like to retrieve.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            await pop.Disconnect();
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

          Console.Write("pop> ");
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
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}