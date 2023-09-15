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

class ftpDemo
{
  private static Ftp ftp1 = new Ftp();

  private static void ftp1_OnSSLServerAuthentication(object sender, FtpSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void ftp1_OnTransfer(object sender, FtpTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
  }

  private static void ftp1_OnDirList(object sender, FtpDirListEventArgs e)
  {
    Console.WriteLine(e.DirEntry);
  }

  static async Task Main(string[] args)
  {
    if (args.Length < 3)
    {
      Console.WriteLine("usage: ftp host user pass\n");
      Console.WriteLine("  host  the host to connect to");
      Console.WriteLine("  user  the username to use for authentication");
      Console.WriteLine("  pass  the password to use for authentication");
      Console.WriteLine("\nExample: ftp 192.168.1.2 myusername mypassword");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {

      ftp1.OnTransfer += ftp1_OnTransfer;
      ftp1.OnSSLServerAuthentication += ftp1_OnSSLServerAuthentication;
      ftp1.OnDirList += ftp1_OnDirList;

      try
      {
        ftp1.RemoteHost = args[args.Length - 3];
        ftp1.User = args[args.Length - 2];
        ftp1.Password = args[args.Length - 1];

        await ftp1.Logon();

        Console.WriteLine("Type \"?\" for a list of commands.");
        String command;
        String[] arguments;
        while (true)
        {
          ftp1.RemoteFile = "";
          Console.Write("ftp> ");
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("?       cd      ls      pwd");
            Console.WriteLine("get     put     rm      passive");
            Console.WriteLine("mkdir   rmdir   exit");
          }
          else if (arguments[0] == "bye" || arguments[0] == "quit" || arguments[0] == "exit")
          {
            await ftp1.Logoff();
            break;
          }
          else if (arguments[0] == "cd")
          {
            if (arguments.Length > 1)
              await ftp1.ChangeRemotePath(arguments[1]);
          }
          else if (arguments[0] == "get")
          {
            ftp1.RemoteFile = arguments[1];
            ftp1.LocalFile = arguments[1];
            await ftp1.Download();
            Console.WriteLine("File downloaded");
          }
          else if (arguments[0] == "ls")
          {
            if (arguments.Length > 1)
            {
              String pathname = await ftp1.QueryRemotePath();
              await ftp1.ChangeRemotePath(arguments[1]);
              await ftp1.ListDirectoryLong();
              await ftp1.ChangeRemotePath(pathname);
            }
            else
              await ftp1.ListDirectoryLong();
          }
          else if (arguments[0] == "mkdir")
          {
            if (arguments.Length > 1)
              await ftp1.MakeDirectory(arguments[1]);
          }
          else if (arguments[0] == "mv")
          {
            ftp1.RemoteFile = arguments[1];
            await ftp1.RenameFile(arguments[2]);
          }
          else if (arguments[0] == "passive")
          {
            if (arguments.Length > 1)
            {
              if ((arguments[1] == "on") && !ftp1.Passive)
              {
                ftp1.Passive = true;
                Console.WriteLine("Passive mode ON.");
              }
              else if ((arguments[1] == "off") && ftp1.Passive)
              {
                ftp1.Passive = false;
                Console.WriteLine("Passive mode OFF.");
              }
            }
          }
          else if (arguments[0] == "put")
          {
            ftp1.LocalFile = arguments[1];
            ftp1.RemoteFile = arguments[2];
            await ftp1.Upload();
            Console.WriteLine("File uploaded");
          }
          else if (arguments[0] == "pwd")
          {
            Console.WriteLine(await ftp1.QueryRemotePath());
          }
          else if (arguments[0] == "rm")
          {
            if (arguments.Length > 1)
              await ftp1.DeleteFile(arguments[1]);
          }
          else if (arguments[0] == "rmdir")
          {
            if (arguments.Length > 1)
              await ftp1.RemoveDirectory(arguments[1]);
          }
          else if (arguments[0] == "")
          {
            // Do nothing
          }
          else
          {
            Console.WriteLine("Bad command / Not implemented in demo.");
          } // end of command checking
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
      Console.WriteLine("\npress <return> to continue...");
      Console.Read();
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