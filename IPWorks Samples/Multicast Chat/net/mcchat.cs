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
using System.Collections.Generic;
using nsoftware.IPWorks;

class mcchatDemo
{
  private static MCast mcast1 = new nsoftware.IPWorks.MCast();
  private static Queue<string> messages = new Queue<string>();

  private static void mcast1_OnDataIn(object sender, nsoftware.IPWorks.MCastDataInEventArgs e)
  {
    messages.Enqueue("[" + e.SourceAddress + "] " + e.Datagram);
  }

  static void Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: mcchat user group");
      Console.WriteLine("  user   your display name");
      Console.WriteLine("  group  ip of the group");
      Console.WriteLine("Example: mcchat johndoe1 231.31.31.31");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {
      try
      {
        string user = args[0];
        string group = args[1];

        mcast1.OnDataIn += mcast1_OnDataIn;
        mcast1.LocalPort = 3333;
        mcast1.RemotePort = 3333;
        mcast1.Activate();
        mcast1.RemoteHost = group;
        mcast1.MulticastGroup = group;

        Console.WriteLine("Type \"?\" for a list of commands.");
        string rawline = "";
        string command = "";
        string argument = "";
        while (true)
        {
          Console.Write("mcchat> ");
          rawline = Console.ReadLine();
          if (rawline.IndexOf(" ") > 0)
          {
            command = rawline.Substring(0, rawline.IndexOf(" "));
            argument = rawline.Substring(rawline.IndexOf(" ") + 1);
          }
          else
          {
            command = rawline;
            argument = "";
          }

          if (command == "send")
          {
            mcast1.SendText(user + ": " + argument);
          }
          else if (command == "read")
          {
            while (messages.Count > 0)
            {
              Console.WriteLine(messages.Dequeue());
            }
          }
          else
          {
            Console.WriteLine("Commands");
            Console.WriteLine("  ?      send      read");
          } // end of command checking
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
      Console.WriteLine("\npress <return> to continue...");
      Console.Read();
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