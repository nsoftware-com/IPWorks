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

public class dnsDemo
{
  private static Dns dns = new nsoftware.async.IPWorks.Dns();

  static async Task Main(string[] args)
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

        foreach (DnsQueryTypes queryType in Enum.GetValues(typeof(DnsQueryTypes)))
        {
          dns.QueryType = queryType;
          await dns.Query(domain);
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
    }
  }

  private static void dns_OnError(object sender, DnsErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + "[" + e.Description + "].");
  }

  private static void dns_OnResponse(object sender, DnsResponseEventArgs e)
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