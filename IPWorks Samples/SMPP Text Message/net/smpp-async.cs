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
using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorks;

class smppDemo
{
  private static Smpp smpp;

  private static void smpp_OnSSLServerAuthentication(object sender, SmppSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static async Task Main(string[] args)
  {
    smpp = new Smpp();

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

        if (smpp.SSLEnabled) smpp.SSLStartMode = SmppSSLStartModes.sslAutomatic;

        // Attempt to connect to the SMPP server.
        await smpp.Connect();

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
            if (arguments.Length > 1) await smpp.AddRecipient(0, arguments[1]);
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
              await smpp.SendMessage(fullMessage);
            }
            else
            {
              Console.WriteLine("Please supply a message to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            await smpp.Disconnect();
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