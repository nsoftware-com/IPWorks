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

class tftpclientDemo
{
  private static TFTPClient tftp = new nsoftware.IPWorks.TFTPClient();

  static void Main(string[] args) {
    if (args.Length < 1) {
      Console.WriteLine("usage: tftp /s server [/port port]\n");
      Console.WriteLine("  server   the tftp server to exchange files with");
      Console.WriteLine("  port     the UDP port where the tftp server is listening (optional, default 69)");
      Console.WriteLine("\nExample: tftp /s mytftpserver /port 1234\n");
    } else {
      tftp.OnTransfer += tftp_OnTransfer;

      try {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        tftp.TFTPServer = myArgs["s"];
        if (myArgs.ContainsKey("port")) tftp.TFTPPort = int.Parse(myArgs["port"]);

        Console.WriteLine("Type \"?\" for a list of commands.");
        Console.Write("tftp> ");
        string command;
        string[] arguments;
        while (true) {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help") {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                                 display the list of valid commands");
            Console.WriteLine("  help                              display the list of valid commands");
            Console.WriteLine("  get <remote file> <destination>   download the specified file from the server");
            Console.WriteLine("  put <local file>  <destination>   upload the specified file to the server");
            Console.WriteLine("  quit                              exit the application");
          } else if (arguments[0] == "quit" || arguments[0] == "exit") {
            break;
          } else if (arguments[0] == "get") {
            if (arguments.Length > 2) {
              tftp.RemoteFile = arguments[1];
              tftp.LocalFile = arguments[2];
              tftp.GetFile();
              Console.WriteLine("File downloaded");
            }
          } else if (arguments[0] == "put") {
            if (arguments.Length > 2) {
              tftp.LocalFile = arguments[1];
              tftp.RemoteFile = arguments[2];
              tftp.PutFile();
              Console.WriteLine("File uploaded");
            }
          } else if (arguments[0] == "") {
            // Do nothing.
          } else {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          tftp.RemoteFile = "";
          Console.Write("tftp> ");
        }
      } catch (Exception ex) {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  private static void tftp_OnTransfer(object sender, TFTPClientTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
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