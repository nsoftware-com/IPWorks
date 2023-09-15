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

class tftpclientDemo
{
  private static Tftpclient tftp = new nsoftware.async.IPWorks.Tftpclient();

  static async Task Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: tftp /s server [/port port]\n");
      Console.WriteLine("  server   the tftp server to exchange files with");
      Console.WriteLine("  port     the UDP port where the tftp server is listening (optional, default 69)");
      Console.WriteLine("\nExample: tftp /s mytftpserver /port 1234\n");
    }
    else
    {
      tftp.OnTransfer += tftp_OnTransfer;

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        tftp.TFTPServer = myArgs["s"];
        if (myArgs.ContainsKey("port")) tftp.TFTPPort = int.Parse(myArgs["port"]);

        Console.WriteLine("Type \"?\" for a list of commands.");
        Console.Write("tftp> ");
        string command;
        string[] arguments;
        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("Commands: ");
            Console.WriteLine("  ?                                 display the list of valid commands");
            Console.WriteLine("  help                              display the list of valid commands");
            Console.WriteLine("  get <file>                        download the specified file from the server");
            Console.WriteLine("  put <local file> <destination>    upload the specified file to the server");
            Console.WriteLine("  quit                              exit the application");
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            break;
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              tftp.RemoteFile = arguments[1];
              tftp.LocalFile = arguments[1];
              await tftp.GetFile();
              Console.WriteLine("File downloaded");
            }
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              tftp.LocalFile = arguments[1];
              tftp.RemoteFile = arguments[2];
              await tftp.PutFile();
              Console.WriteLine("File uploaded");
            }
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          tftp.RemoteFile = "";
          Console.Write("tftp> ");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  private static void tftp_OnTransfer(object sender, TftpclientTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
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