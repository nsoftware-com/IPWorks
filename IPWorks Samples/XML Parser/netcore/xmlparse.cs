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
using System.Collections.Generic;

public class xmlparseDemo
{
  static XML xml = new XML();
  static HTTP http = new HTTP();

  private static void xml_OnStartElement(object? sender, XMLStartElementEventArgs e)
  {
    Console.WriteLine("Start Element: " + e.Element + "\r\n");
  }

  private static void xml_OnEndElement(object? sender, XMLEndElementEventArgs e)
  {
    Console.WriteLine("End Element: " + e.Element + "\r\n");
  }

  private static void http_OnSSLServerAuthentication(object? sender, HTTPSSLServerAuthenticationEventArgs e)
  {
    e.Accept = true;
  }

  public static void Main(string[] args) {
    if (args.Length < 2) {
      Console.WriteLine("Usage: xmlparse (/url||/file||/string) /url address /file inputfile /string inputstring");
      Console.WriteLine("  url|file|string  Specifies whether to parse XML from an HTTP Get result, a local file, or an input string.");
      Console.WriteLine("  address     The HTTP address of the input XML to parse (specify if the chosen input type is \"url\")");
      Console.WriteLine("  inputfile   The file that contains the input XML to parse (specify if the chosen input type is \"file\")");
      Console.WriteLine("  inputstring The string that contains the input XML to parse (specify if the chosen input type is \"string\")");
      Console.WriteLine("\nExample: /url \"http://www.nsoftware.com/rss/\"");
      Console.WriteLine("\nExample: /file \"../../../test.xml\"");
      Console.WriteLine("\nExample: /string \"<rss version=\\\"2.0\\\"><channel>inner</channel></rss>\"");
    } else {
      try {
        xml.OnStartElement += xml_OnStartElement;
        xml.OnEndElement += xml_OnEndElement;
        http.OnSSLServerAuthentication += http_OnSSLServerAuthentication;

        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        if (myArgs.ContainsKey("url") && !myArgs.ContainsKey("file") && !myArgs.ContainsKey("string")) {
          // parse from http GET
          http.FollowRedirects = HTTPFollowRedirects.frAlways;
          http.TransferredDataLimit = 0;
          http.Get(myArgs["url"]);
          xml.InputData = http.TransferredData;
        } else if (myArgs.ContainsKey("file") && !myArgs.ContainsKey("string") && !myArgs.ContainsKey("url")) {
          // parse from file
          xml.InputFile = myArgs["file"];
        } else if (myArgs.ContainsKey("string") && !myArgs.ContainsKey("file") && !myArgs.ContainsKey("url")) {
          // parse from string
          xml.InputData = myArgs["string"];
        } else {
          throw new Exception("Invalid input type. You may only choose one from the following: [url, file, string].");
        }

        Console.WriteLine("Parsing XML: ");
        xml.Parse();
        Console.WriteLine("Parsing Complete.");
      } catch (IPWorksException e) {
        Console.WriteLine("IPWorksException: " + e.Message);
      } catch (Exception e) {
        Console.WriteLine("Exception: " + e.Message);
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