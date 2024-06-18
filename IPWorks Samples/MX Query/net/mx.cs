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

public class mxDemo
{
  private static MX mx = new MX();

  static void Main(string[] args)
  {
    if (args.Length < 4)
    {
      Console.WriteLine("usage: mx /s server /e email");
      Console.WriteLine("  server  the name or address of the DNS server");
      Console.WriteLine("  email   the email address to resolve");
      Console.WriteLine("\r\nExample: mx /s 4.2.2.1 /e billg@microsoft.com");
    }
    else
    {
      try
      {
        mx.OnResponse += mx_OnResponse;
        mx.OnError += mx_OnError;

        // get args
        var parsedArgs = ConsoleDemo.ParseArgs(args);

        Console.WriteLine("Looking up address...\n");
        mx.DNSServer = parsedArgs["s"];
        mx.Resolve(parsedArgs["e"]);
      }
      catch (IPWorksException e)
      {
        Console.WriteLine("IPWorksException: " + e.Message);
      }
      catch (Exception e)
      {
        Console.WriteLine("Exception: "  + e.Message);
      }
    }
  }

  private static void mx_OnError(object? sender, MXErrorEventArgs e)
  {
    Console.WriteLine("Error Code: " + e.ErrorCode + " Description: " + e.Description);
  }

  private static void mx_OnResponse(object? sender, MXResponseEventArgs e)
  {
    Console.WriteLine(e.Description);
    Console.WriteLine(e.Domain + " --> " + e.MailServer);
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