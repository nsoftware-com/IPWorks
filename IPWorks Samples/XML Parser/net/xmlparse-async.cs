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

public class xmlparseDemo
{
  static Xml xml = new Xml();
  static Http http = new Http();

  private static void xml_OnStartElement(object? sender, XmlStartElementEventArgs e)
  {
    Console.WriteLine("Start Element: " + e.Element + "\r\n");
  }

  private static void xml_OnEndElement(object? sender, XmlEndElementEventArgs e)
  {
    Console.WriteLine("End Element: " + e.Element + "\r\n");
  }

  private static void http_OnSSLServerAuthentication(object? sender, HttpSSLServerAuthenticationEventArgs e)
  {
    e.Accept = true;
  }

  public static async Task Main(string[] args)
  {
    if (args.Length < 3)
    {
      Console.WriteLine("Usage: xmlparse /HTTP|File|String /a address /f inputfile /s inputstring");
      Console.WriteLine("  HTTP|File|String  Specifies whether to parse XML from an HTTP Get result, a local file, or an input string.");
      Console.WriteLine("  address     The HTTP address of the input XML to parse (specify if the chosen input type is \"HTTP\")");
      Console.WriteLine("  inputfile   The file that contains the input XML to parse (specify if the chosen input type is \"File\")");
      Console.WriteLine("  inputstring The string that contains the input XML to parse (specify if the chosen input type is \"String\")");
      Console.WriteLine("\nExample: /HTTP /a \"http://www.nsoftware.com/rss/\"");
      Console.WriteLine("\nExample: /File /f \"../../../test.xml\"");
      Console.WriteLine("\nExamle: /String /s \"<rss version=\\\"2.0\\\"><channel>inner</channel></rss>\"");
    }
    else
    {
      try
      {
        xml.OnStartElement += xml_OnStartElement;
        xml.OnEndElement += xml_OnEndElement;
        http.OnSSLServerAuthentication += http_OnSSLServerAuthentication;

        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        if (myArgs.ContainsKey("HTTP") && !myArgs.ContainsKey("File") && !myArgs.ContainsKey("String"))
        {
          // parse from http GET
          http.FollowRedirects = HttpFollowRedirects.frAlways;
          http.TransferredDataLimit = 0;
          await http.Get(myArgs["a"]);
          xml.InputData = http.TransferredData;
        }
        else if (myArgs.ContainsKey("File") && !myArgs.ContainsKey("String") && !myArgs.ContainsKey("HTTP"))
        {
          // parse from file
          xml.InputFile = myArgs["f"];
        }
        else if (myArgs.ContainsKey("String") && !myArgs.ContainsKey("File") && !myArgs.ContainsKey("HTTP"))
        {
          // parse from string
          xml.InputData = myArgs["s"];
        }
        else
        {
          throw new Exception("Invalid input type. You may only choose one from the following: [HTTP, File, String].");
        }

        Console.WriteLine("Parsing XML: ");
        await xml.Parse();
        Console.WriteLine("Parsing Complete.");
      }
      catch (IPWorksXmlException e)
      {
        Console.WriteLine("IPWorksXMLException: " + e.Message);
      }
      catch (Exception e)
      {
        Console.WriteLine("Exception: " + e.Message);
      }
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