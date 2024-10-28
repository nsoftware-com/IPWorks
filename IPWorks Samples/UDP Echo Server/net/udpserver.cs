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

class udpserverDemo
{
  private static UDP server;

  private static void server_OnDataIn(object sender, UDPDataInEventArgs e)
  {
    Console.WriteLine("Echoing '" + e.Datagram + "' back to client " + e.SourceAddress + ":" + e.SourcePort + ".");
    server.RemoteHost = e.SourceAddress;
    server.RemotePort = e.SourcePort;
    server.SendText(e.Datagram);
  }

  private static void server_OnError(object sender, UDPErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  static void Main(string[] args)
  {
    server = new UDP();

    if (args.Length < 1)
    {
      Console.WriteLine("usage: udpserver port");
      Console.WriteLine("  port   the port on which the server will listen");
      Console.WriteLine("Example: udpserver 777");
    }
    else
    {
      server.OnDataIn += server_OnDataIn;
      server.OnError += server_OnError;

      Console.WriteLine("*****************************************************************");
      Console.WriteLine("* This demo shows how to set up an echo server using UDP.       *");
      Console.WriteLine("*****************************************************************");

      try
      {
        server.LocalPort = int.Parse(args[args.Length - 1]);
        server.Activate();

        Console.WriteLine("Listening on port " + server.LocalPort + "... press Ctrl-C to shutdown.");

        while (true)
        {
          server.DoEvents();
        }
      }
      catch (Exception e)
      {
        Console.WriteLine(e.Message);
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