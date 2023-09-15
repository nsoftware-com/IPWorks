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

class tftpserverDemo
{
  private static Tftpserver tftpserver = new nsoftware.async.IPWorks.Tftpserver();

  static async Task Main(string[] args)
  {
    tftpserver.OnConnected += tftpserver_OnConnected;
    tftpserver.OnConnectionRequest += tftpserver_OnConnectionRequest;
    tftpserver.OnDisconnected += tftpserver_OnDisconnected;
    tftpserver.OnEndTransfer += tftpserver_OnEndTransfer;
    tftpserver.OnError += tftpserver_OnError;
    tftpserver.OnStartTransfer += tftpserver_OnStartTransfer;
    tftpserver.OnTransfer += tftpserver_OnTransfer;

    try
    {
      Console.WriteLine("usage: tftpserver [/port port] [/path path]");
      Console.WriteLine("  port    the port to listen on (optional, default 69)");
      Console.WriteLine("  path    the file path to the server's local directory (optional, default current directory)");
      Console.WriteLine("\nExample: tftpserver /port 1234 /path C:\\temp");

      Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
      if (myArgs.ContainsKey("port")) tftpserver.LocalPort = int.Parse(myArgs["port"]);
      tftpserver.LocalDir = myArgs.ContainsKey("path") ? myArgs["path"] : "./";

      await tftpserver.StartListening();
      Console.WriteLine("TFTP server started with local directory " + tftpserver.LocalDir + ". Listening on port " + tftpserver.LocalPort + ".");

      Console.WriteLine("Type \"quit\" to stop the server and exit the application.");
      Console.Write("tftpserver> ");
      string command;
      string[] arguments;
      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "quit" || arguments[0] == "exit")
        {
          await tftpserver.StopListening();
          Console.WriteLine("TFTP server stopped.");
          break;
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("tftpserver> ");
      }
    }
    catch (Exception ex)
    {
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

  private static void tftpserver_OnConnected(object sender, TftpserverConnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Connected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void tftpserver_OnConnectionRequest(object sender, TftpserverConnectionRequestEventArgs e)
  {
    Log(e.RemoteHost + ":" + e.RemotePort.ToString() + " is attempting to connect.");
  }

  private static void tftpserver_OnDisconnected(object sender, TftpserverDisconnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Disconnected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void tftpserver_OnEndTransfer(object sender, TftpserverEndTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transfer complete");
  }

  private static void tftpserver_OnError(object sender, TftpserverErrorEventArgs e)
  {
    Log("Error - " + e.Description + " (" + e.ErrorCode.ToString() + ")");
  }

  private static void tftpserver_OnStartTransfer(object sender, TftpserverStartTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transfer started");
  }

  private static void tftpserver_OnTransfer(object sender, TftpserverTransferEventArgs e)
  {
    Log(e.ConnectionId, "Transferring data");
  }

  #endregion
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