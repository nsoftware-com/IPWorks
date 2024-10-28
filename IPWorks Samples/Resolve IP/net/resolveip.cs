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

public class resolveipDemo
{
  private static IPInfo ipinfo = new IPInfo();
  private static bool isHostAddress = false;

  static void Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: ipinfo /a hostaddress /n hostname");
      Console.WriteLine("  hostaddress  the host address to resolve (specify this or hostname, but not both)");
      Console.WriteLine("  hostname     the host name to resolve (specify this or hostaddress, but not both)");
      Console.WriteLine("\r\nExample: ipinfo /n www.google.com");
      Console.WriteLine("Example: ipinfo /a 8.8.8.8\n");
    }
    else
    {
      try
      {
        ipinfo.OnRequestComplete += ipinfo_OnRequestComplete;
        var parsedArgs = ConsoleDemo.ParseArgs(args);

        if (parsedArgs.ContainsKey("a"))
        {
          ipinfo.HostAddress = parsedArgs["a"];
          isHostAddress = true;
        }
        else if (parsedArgs.ContainsKey("n")) 
        { 
          ipinfo.HostName = parsedArgs["n"]; 
        }

        while (ipinfo.PendingRequests > 0)
        {
          ipinfo.DoEvents();
        }
      }
      catch (IPWorksException e)
      {
        Console.WriteLine("IPWorksException: " + e.Message);
      }
      catch (Exception e)
      {
        Console.WriteLine("Exception: " + e.Message);
      }
    }
  }

  private static void ipinfo_OnRequestComplete(object? sender, IPInfoRequestCompleteEventArgs e)
  {
    if (e.StatusCode != 0)
    {
      Console.WriteLine("Request #" + e.RequestId + " failed: " + e.Description);
      return;
    }

    if (isHostAddress)
    {
      Console.WriteLine("Host Name: " + ipinfo.HostName);
    }
    else
    {
      Console.WriteLine("Host Address: " + ipinfo.HostAddress);
      Console.WriteLine("Host Aliases: " + ipinfo.HostAliases);
      Console.WriteLine("Alternate Addresses: " + ipinfo.OtherAddresses);
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