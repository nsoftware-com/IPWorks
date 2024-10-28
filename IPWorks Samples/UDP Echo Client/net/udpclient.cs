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

class udpclientDemo
{
  private static UDP udp;

  private static void udp_OnDataIn(object sender, UDPDataInEventArgs e)
  {
    Console.WriteLine("Received '" + e.Datagram + "' from " + e.SourceAddress + ":" + e.SourcePort + ".");
  }

  private static void udp_OnError(object sender, UDPErrorEventArgs e)
  {
    Console.WriteLine(e.Description);
  }

  static void Main(string[] args)
  {
    udp = new UDP();

    if (args.Length < 2)
    {
      Console.WriteLine("usage: udpclient host port");
      Console.WriteLine("  host       the address of the remote host");
      Console.WriteLine("  port       the port of the remote host");
      Console.WriteLine("Example: udpclient 192.168.1.2 777");
      Console.WriteLine("Example: udpclient 255.255.255.255 777 (broadcast)");
    }
    else
    {
      udp.OnDataIn += udp_OnDataIn;
      udp.OnError += udp_OnError;
      try
      {
        // Parse arguments into component.
        udp.RemoteHost = args[args.Length - 2];
        udp.RemotePort = int.Parse(args[args.Length - 1]);
        udp.Activate();

        Console.WriteLine("Type and press enter to send. Press Ctrl-C to exit the application.");
        string data;

        while (true)
        {
          data = Console.ReadLine();
          udp.SendText(data);
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