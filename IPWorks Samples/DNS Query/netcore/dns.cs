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

public class dnsDemo
{
  private static DNS dns = new nsoftware.IPWorks.DNS();

  static void Main(string[] args)
  {
    if (args.Length < 4)
    {
      Console.WriteLine("usage: dns /s server /host hostname");
      Console.WriteLine("  server    the address of the DNS server");
      Console.WriteLine("  hostname  the host domain to query");
      Console.WriteLine("\r\nExample: dns /s 8.8.8.8 /host www.yahoo.com");
    }
    else
    {
      dns.OnError += dns_OnError;
      dns.OnResponse += dns_OnResponse;

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
        string domain = myArgs["host"];

        dns.DNSServer = myArgs["s"];

        Console.WriteLine("Type\tField\tValue\r\n-----------------------");

        foreach (DNSQueryTypes queryType in Enum.GetValues(typeof(DNSQueryTypes)))
        {
          dns.QueryType = queryType;
          dns.Query(domain);
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }

  private static void dns_OnError(object sender, DNSErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + "[" + e.Description + "].");
  }

  private static void dns_OnResponse(object sender, DNSResponseEventArgs e)
  {
    try
    {
      if (e.StatusCode == 0) // There was a record in the response.
      {
        DNSRecordList records = dns.Records;
        for (int i = 0; i < records.Count; i++)
        {
          DNSRecord record = records[i];
          for (int j = 0; j < record.FieldCount; j++)
          {
            record.FieldIndex = j;
            if (j == 0)
              Console.Write(record.RecordTypeName + "\t");
            else
              Console.Write("\t");

            Console.Write(record.FieldName + "\t" + record.FieldValue + "\r\n");
          }
        }
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
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