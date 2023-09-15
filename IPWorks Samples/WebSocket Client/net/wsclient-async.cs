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

class wsclientDemo
{
  private static Wsclient wsclient;

  private static void wsclient_OnConnected(object sender, WsclientConnectedEventArgs e)
  {
    Console.WriteLine(e.StatusCode == 0 ? "Connected.\r\n" : "Failed to connect. Reason: " + e.Description + ".\r\n");
  }

  private static void wsclient_OnDataIn(object sender, WsclientDataInEventArgs e)
  {
    Console.WriteLine("Received '" + e.Text + "'.");
  }

  private static void wsclient_OnDisconnected(object sender, WsclientDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected.\r\n");
  }

  private static void wsclient_OnError(object sender, WsclientErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  private static void wsclient_OnSSLServerAuthentication(object sender, WsclientSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static async Task Main(string[] args)
  {
    wsclient = new Wsclient();

    if (args.Length < 2)
    {
      Console.WriteLine("usage: wsclient [options] host port");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -ssl       whether or not to use SSL/TLS (default false)");
      Console.WriteLine("  host       the name of the local host or user-assigned IP interface through which connections are initiated or accepted");
      Console.WriteLine("  port       the TCP port in the local host where the component binds");
      Console.WriteLine("\r\nExample: wsclient -ssl true localhost 4444");
    }
    else
    {
      wsclient.OnConnected += wsclient_OnConnected;
      wsclient.OnDataIn += wsclient_OnDataIn;
      wsclient.OnDisconnected += wsclient_OnDisconnected;
      wsclient.OnError += wsclient_OnError;
      wsclient.OnSSLServerAuthentication += wsclient_OnSSLServerAuthentication;

      try
      {
        // Parse arguments into component.
        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-ssl"))
            {
              if (bool.Parse(args[i + 1]))  // args[i + 1] corresponds to the value of args[i]
              {
                wsclient.URL = "wss://" + args[args.Length - 2] + ":" + args[args.Length - 1];
              }
            }
          }
        }

        if (string.IsNullOrEmpty(wsclient.URL)) wsclient.URL = "ws://" + args[args.Length - 2] + ":" + args[args.Length - 1];

        // Attempt to connect.
        await wsclient.Connect();

        // Process user commands.
        Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
        string command;
        string[] arguments;

        while (true)
        {
          await wsclient.DoEvents();
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0].Equals("?") || arguments[0].Equals("help"))
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                            display the list of valid commands");
            Console.WriteLine("  help                         display the list of valid commands");
            Console.WriteLine("  send <text>                  send text data to the server");
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
              await wsclient.SendText(textToSend);
            }
            else
            {
              Console.WriteLine("Please supply the text that you would like to send.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            await wsclient.Disconnect();
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

          Console.Write("wsclient> ");
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