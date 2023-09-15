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

class syslogDemo
{
  private static Syslog syslog = new nsoftware.async.IPWorks.Syslog();

  static async Task Main(string[] args)
  {
    syslog.OnPacketIn += syslog_OnPacketIn;

    try
    {
      await syslog.Activate();
      Console.WriteLine("Syslog server started.");

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
          await syslog.Config("AcceptData=false");
          await syslog.Deactivate();
          Console.WriteLine("Syslog server stopped.");
          break;
        }
        else if (arguments[0] == "send")
        {
          syslog.RemoteHost = "255.255.255.255";
          await syslog.SendPacket(1, 5, "This is just a test"); // Log Alert, Informational Message
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

  private static void syslog_OnPacketIn(object sender, SyslogPacketInEventArgs e)
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