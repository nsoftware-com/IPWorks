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

class tcpechoDemo
{
  private static TCPServer server;

  private static void server_OnConnected(object sender, TCPServerConnectedEventArgs e)
  {
    Console.WriteLine(server.Connections[e.ConnectionId].RemoteHost + " has connected - " + e.Description + ".");
    server.Connections[e.ConnectionId].EOL = "\r\n";
  }

  private static void server_OnDataIn(object sender, TCPServerDataInEventArgs e)
  {
    Console.WriteLine("Echoing '" + e.Text + "' back to client " + server.Connections[e.ConnectionId].RemoteHost + ".");
    server.SendText(e.ConnectionId, e.Text);
  }

  private static void server_OnDisconnected(object sender, TCPServerDisconnectedEventArgs e)
  {
    Console.WriteLine(server.Connections[e.ConnectionId].RemoteHost + " has disconnected - " + e.Description + ".");
  }

  private static void server_OnError(object sender, TCPServerErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  static void Main(string[] args)
  {
    server = new TCPServer();

    if (args.Length < 1)
    {
      Console.WriteLine("usage: tcpecho [options] port");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -cert      the subject of a certificate in the user's certificate store to be used during SSL/TLS negotiation (default no SSL/TLS)");
      Console.WriteLine("  port       the TCP port to listen on");
      Console.WriteLine("\r\nExample: tcpecho -cert certSubject 4444");
    }
    else
    {
      server.OnConnected += server_OnConnected;
      server.OnDataIn += server_OnDataIn;
      server.OnDisconnected += server_OnDisconnected;
      server.OnError += server_OnError;

      try
      {
        // Parse arguments into component.
        server.LocalPort = int.Parse(args[args.Length - 1]);

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-cert"))
            {
              server.SSLCert = new Certificate(CertStoreTypes.cstUser, "MY", "", args[i + 1]);  // args[i + 1] corresponds to the value of args[i]
              server.SSLEnabled = true;
              server.SSLStartMode = TCPServerSSLStartModes.sslAutomatic;
            }
          }
        }

        // Start listening for connections.
        server.StartListening();

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
              foreach (Connection connection in server.Connections.Values)
              {
                server.SendText(connection.ConnectionId, textToSend);
              }
            }
            else
            {
              Console.WriteLine("Please supply the text that you would like to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            server.Shutdown();
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

          Console.Write("tcp> ");
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