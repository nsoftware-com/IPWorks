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
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using nsoftware.async.IPWorks;

class mcchatDemo
{
  private static Mcast mcast1 = new nsoftware.async.IPWorks.Mcast();
  private static Queue<string> messages = new Queue<string>();

  private static void mcast1_OnDataIn(object sender, nsoftware.async.IPWorks.McastDataInEventArgs e)
  {
    messages.Enqueue("[" + e.SourceAddress + "] " + e.Datagram);
  }

  static async Task Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: mcchat user group");
      Console.WriteLine("  user   your display name");
      Console.WriteLine("  group  ip of the group");
      Console.WriteLine("Example: mcchat johndoe1 231.31.31.31");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {
      try
      {
        string user = args[0];
        string group = args[1];

        mcast1.OnDataIn += mcast1_OnDataIn;
        mcast1.LocalPort = 3333;
        mcast1.RemotePort = 3333;
        await mcast1.Activate();
        mcast1.RemoteHost = group;
        mcast1.MulticastGroup = group;

        Console.WriteLine("Type \"?\" for a list of commands.");
        string rawline = "";
        string command = "";
        string argument = "";
        while (true)
        {
          Console.Write("mcchat> ");
          rawline = Console.ReadLine();
          if (rawline.IndexOf(" ") > 0)
          {
            command = rawline.Substring(0, rawline.IndexOf(" "));
            argument = rawline.Substring(rawline.IndexOf(" ") + 1);
          }
          else
          {
            command = rawline;
            argument = "";
          }

          if (command == "send")
          {
            await mcast1.SendText(user + ": " + argument);
          }
          else if (command == "read")
          {
            while (messages.Count > 0)
            {
              Console.WriteLine(messages.Dequeue());
            }
          }
          else
          {
            Console.WriteLine("Commands");
            Console.WriteLine("  ?      send      read");
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