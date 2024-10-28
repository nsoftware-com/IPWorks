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
using System.Text;
using nsoftware.IPWorks;


class webdavDemo
{
  private static WebDAV webdav;

  private static void webdav_OnSSLServerAuthentication(object sender, WebDAVSSLServerAuthenticationEventArgs e)
  {
    e.Accept = true; // This will trust all certificates and is not recommended for production use.
  }

  private static void webdav_OnConnected(object sender, WebDAVConnectedEventArgs e)
  {
    Console.WriteLine("Server connected");
  }

  private static void webdav_OnConnectionStatus(object sender, WebDAVConnectionStatusEventArgs e)
  {
    Console.WriteLine("Status code " + e.StatusCode + ": " + e.Description);
  }

  private static void webdav_OnDisconnected(object sender, WebDAVDisconnectedEventArgs e)
  {
    Console.WriteLine("Server disconnected");
  }

  private static void webdav_OnTransfer(object sender, WebDAVTransferEventArgs e)
  {
    Console.WriteLine("Resource being received from server (in full text): \n" +
                                    "========================================= \n" + e.Text);
  }

  static void Main(string[] args)
  {
    webdav = new WebDAV();

    if (args.Length < 2)
    {
      Console.WriteLine("usage: webdav [options] username password");
      Console.WriteLine("Options: ");
      Console.WriteLine("  ");
      Console.WriteLine("  username   the username to login");
      Console.WriteLine("  password   the password to login");
      Console.WriteLine("\r\nExample: webdav username password");
    }
    else
    {
      webdav.OnConnected += webdav_OnConnected;
      webdav.OnConnectionStatus += webdav_OnConnectionStatus;
      webdav.OnDisconnected += webdav_OnDisconnected;
      webdav.OnSSLServerAuthentication += webdav_OnSSLServerAuthentication;
      webdav.OnTransfer += webdav_OnTransfer;

      try
      {
        // Parse arguments into component.
        webdav.User = args[args.Length - 2];
        webdav.Password = args[args.Length - 1];

        // Process user commands.
        Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
        string command;
        string[] arguments;

        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0].Equals("?") || arguments[0].Equals("help"))
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                                      display the list of valid commands");
            Console.WriteLine("  help                                   display the list of valid commands");
            Console.WriteLine("  make <resource uri>                    make a new directory at the specified location (ex. make localhost:443/directoryName)");
            Console.WriteLine("  move <source uri> <destination uri>    move a specified resource to a new location (ex. move localhost:443/oldFolder/file.txt localhost:443/newFolder/file.txt)");
            Console.WriteLine("  get <resource uri>                     get a specified resource");
            Console.WriteLine("  delete <resource uri>                  delete a specified resource");
            Console.WriteLine("  put <local file> <resource uri>        send data to the server");
            Console.WriteLine("  quit                                   exit the application");
          }
          else if (arguments[0].Equals("make"))
          {
            if (arguments.Length > 1) webdav.MakeDirectory(arguments[1]);
            else Console.WriteLine("Please specify a resource URI.");
          }
          else if (arguments[0].Equals("move"))
          {
            if (arguments.Length > 2) webdav.MoveResource(arguments[1], arguments[2]);
            else Console.WriteLine("Please specify a source and destination URI.");
          }
          else if (arguments[0].Equals("get"))
          {
            if (arguments.Length > 1) webdav.GetResource(arguments[1]);
            else Console.WriteLine("Please specify a resource URI.");
          }
          else if (arguments[0].Equals("delete"))
          {
            if (arguments.Length > 1) webdav.DeleteResource(arguments[1]);
            else Console.WriteLine("Please specify a resource URI.");
          }
          else if (arguments[0].Equals("put"))
          {
            if (arguments.Length > 2)
            {
              webdav.LocalFile = arguments[1];
              webdav.PutResource(arguments[2]);
            }
            else
            {
              Console.WriteLine("Please specify a local file and resource URI.");
            }
          }
          else if (arguments[0].Equals("quit"))
          {
            break;
          }
          else if (arguments[0].Equals(""))
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          }
          Console.Write("webdav> ");
        }
      }
      catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
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