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
  private static TCPClient ip;

  private static void ip_OnConnected(object sender, TCPClientConnectedEventArgs e)
  {
    if (e.Description.ToLower().Equals("ok")) Console.WriteLine("Successfully connected.");
    else Console.WriteLine(e.Description);
  }

  private static void ip_OnDataIn(object sender, TCPClientDataInEventArgs e)
  {
    Console.WriteLine("Received '" + e.Text + "' from " + ip.RemoteHost + ".");
  }

  private static void ip_OnDisconnected(object sender, TCPClientDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected.");
  }

  private static void ip_OnError(object sender, TCPClientErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  private static void ip_OnSSLServerAuthentication(object sender, TCPClientSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    ip = new TCPClient();

    if (args.Length < 2)
    {
      Console.WriteLine("usage: tcpecho [options] host port");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -ssl       whether or not to use SSL/TLS (default false)");
      Console.WriteLine("  host       the address of of the remote host");
      Console.WriteLine("  port       the TCP port of the remote host");
      Console.WriteLine("\r\nExample: tcpecho -ssl true 192.168.1.2 21");
    }
    else
    {
      ip.OnConnected += ip_OnConnected;
      ip.OnDataIn += ip_OnDataIn;
      ip.OnDisconnected += ip_OnDisconnected;
      ip.OnError += ip_OnError;
      ip.OnSSLServerAuthentication += ip_OnSSLServerAuthentication;

      try
      {
        // Parse arguments into component.
        ip.RemoteHost = args[args.Length - 2];
        ip.RemotePort = int.Parse(args[args.Length - 1]);

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-ssl"))
            {
              ip.SSLEnabled = bool.Parse(args[i + 1]);  // args[i + 1] corresponds to the value of args[i]
            }
          }
        }

        if (ip.SSLEnabled) ip.SSLStartMode = TCPClientSSLStartModes.sslAutomatic;

        // Attempt to connect to the remote server.
        ip.Connect();

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
            Console.WriteLine("  send <text>                  send data to the remote host");
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
              ip.SendLine(textToSend);
            }
            else
            {
              Console.WriteLine("Please supply the text that you would like to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            ip.Disconnect();
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