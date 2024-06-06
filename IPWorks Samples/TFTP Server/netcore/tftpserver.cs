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

class tftpserverDemo
{
  private static TFTPServer tftpserver = new nsoftware.IPWorks.TFTPServer();

  static void Main(string[] args) {
    tftpserver.OnConnected += tftpserver_OnConnected;
    tftpserver.OnConnectionRequest += tftpserver_OnConnectionRequest;
    tftpserver.OnDisconnected += tftpserver_OnDisconnected;
    tftpserver.OnEndTransfer += tftpserver_OnEndTransfer;
    tftpserver.OnError += tftpserver_OnError;
    tftpserver.OnStartTransfer += tftpserver_OnStartTransfer;
    tftpserver.OnTransfer += tftpserver_OnTransfer;

    if (args.Length < 1) {
      Console.WriteLine("usage: tftpserver [/port port] [/path path]");
      Console.WriteLine("  port    the port to listen on (optional, default 69)");
      Console.WriteLine("  path    the file path to the server's local directory (optional, default current directory)");
      Console.WriteLine("\nExample: tftpserver /port 1234 /path C:\\temp\n");
    }
    try {


      Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
      if (myArgs.ContainsKey("port")) tftpserver.LocalPort = int.Parse(myArgs["port"]);
      tftpserver.LocalDir = myArgs.ContainsKey("path") ? myArgs["path"] : "./";

      tftpserver.StartListening();
      Console.WriteLine("TFTP server started with local directory " + tftpserver.LocalDir + ". Listening on port " + tftpserver.LocalPort + ".");

      Console.WriteLine("\nType \"quit\" to stop the server and exit the application.");
      Console.Write("tftpserver> ");
      string command;
      string[] arguments;
      while (true) {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "quit" || arguments[0] == "exit") {
          tftpserver.StopListening();
          Console.WriteLine("TFTP server stopped.");
          break;
        } else if (arguments[0] == "") {
          // Do nothing.
        } else {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("tftpserver> ");
      }
    } catch (Exception ex) {
      Console.WriteLine("Error: " + ex.Message);
    }
  }

  private static void Log(string msg)
  {
    Console.WriteLine(msg);
  }

  private static void Log(string connID, string msg)
  {
    Log("[" + connID + "]: " + msg);
  }

  #region "Events"

  private static void tftpserver_OnConnected(object sender, TFTPServerConnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Connected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void tftpserver_OnConnectionRequest(object sender, TFTPServerConnectionRequestEventArgs e)
  {
    Log(e.RemoteHost + ":" + e.RemotePort.ToString() + " is attempting to connect.");
  }

  private static void tftpserver_OnDisconnected(object sender, TFTPServerDisconnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Disconnected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void tftpserver_OnEndTransfer(object sender, TFTPServerEndTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transfer complete");
  }

  private static void tftpserver_OnError(object sender, TFTPServerErrorEventArgs e)
  {
    Log("Error - " + e.Description + " (" + e.ErrorCode.ToString() + ")");
  }

  private static void tftpserver_OnStartTransfer(object sender, TFTPServerStartTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transfer started");
  }

  private static void tftpserver_OnTransfer(object sender, TFTPServerTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transferring data");
  }

  #endregion
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