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

class syslogDemo
{
  private static SysLog syslog = new nsoftware.IPWorks.SysLog();

  static void Main(string[] args)
  {
    syslog.OnPacketIn += syslog_OnPacketIn;

    try
    {
      syslog.Activate();
      Console.WriteLine("SysLog server started.");

      Console.WriteLine("Type \"?\" for a list of commands.");
      Console.Write("syslog> ");
      string command;
      string[] arguments;
      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  send                         send a test message");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0] == "quit" || arguments[0] == "exit")
        {
          syslog.Config("AcceptData=false");
          syslog.Deactivate();
          Console.WriteLine("SysLog server stopped.");
          break;
        }
        else if (arguments[0] == "send")
        {
          syslog.RemoteHost = "255.255.255.255";
          syslog.SendPacket(1, 5, "This is just a test"); // Log Alert, Informational Message
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("syslog> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void syslog_OnPacketIn(object sender, SysLogPacketInEventArgs e)
  {
    Console.WriteLine("Host: " + e.Hostname);
    Console.WriteLine("Facility: " + e.Facility);
    Console.WriteLine("Severity: " + e.Severity);
    Console.WriteLine("Time: " + e.Timestamp);
    Console.WriteLine("Message: " + e.Message);
    Console.Write("syslog> ");
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