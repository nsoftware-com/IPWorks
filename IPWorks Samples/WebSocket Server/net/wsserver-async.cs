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

class wsserverDemo
{
  private static Wsserver wsserver;

  private static void wsserver_OnConnected(object sender, WsserverConnectedEventArgs e)
  {
    Console.WriteLine(wsserver.Connections[e.ConnectionId].RemoteHost + " has connected.");
  }

  private static void wsserver_OnDataIn(object sender, WsserverDataInEventArgs e)
  {
    Console.WriteLine("Received '" + e.Text + "'.");
  }

  private static void wsserver_OnDisconnected(object sender, WsserverDisconnectedEventArgs e)
  {
    Console.WriteLine(wsserver.Connections[e.ConnectionId].RemoteHost + " has disconnected - " + e.Description + ".");
  }

  private static void wsserver_OnError(object sender, WsserverErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  static async Task Main(string[] args)
  {
    wsserver = new Wsserver();

    if (args.Length < 2)
    {
      Console.WriteLine("usage: wsserver [options] port");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -cert      the subject of a certificate in the user's certificate store to be used during SSL/TLS negotiation (default no SSL/TLS)");
      Console.WriteLine("  port       the TCP port in the local host where the component listens");
      Console.WriteLine("\r\nExample: wsserver -cert certSubject 4444");
    }
    else
    {
      wsserver.OnConnected += wsserver_OnConnected;
      wsserver.OnDataIn += wsserver_OnDataIn;
      wsserver.OnDisconnected += wsserver_OnDisconnected;
      wsserver.OnError += wsserver_OnError;

      try
      {
        // Parse arguments into component.
        wsserver.LocalPort = int.Parse(args[args.Length - 1]);

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-cert"))
            {
              wsserver.SSLCert = new Certificate(CertStoreTypes.cstUser, "MY", "", args[i + 1]);  // args[i + 1] corresponds to the value of args[i]
              wsserver.UseSSL = true;
            }
          }
        }

        // Start lisenting for connections.
        await wsserver.StartListening();

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
            Console.WriteLine("  send <text>                  send data to connected clients");
            Console.WriteLine("  quit                         exit the application");
          }
          else if (arguments[0].Equals("send"))
          {
            if (arguments.Length > 1)
            {
              string textToSend = "";
              for (int i = 1; i < arguments.Length; i++)
              {
                if (i < arguments.Length - 1) textToSend += arguments[i] + " ";
                else textToSend += arguments[i];
              }
              foreach (WSConnection connection in wsserver.Connections.Values)
              {
                await wsserver.SendText(connection.ConnectionId, textToSend);
              }
            }
            else
            {
              Console.WriteLine("Please supply the text that you would like to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            await wsserver.Shutdown();
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

          Console.Write("wsserver> ");
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