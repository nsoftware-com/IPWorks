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

class jsonDemo
{
  private static Json json;

  static async Task Main(string[] args)
  {
    json = new Json();

    if (args.Length < 1)
    {
      Console.WriteLine("usage: json file");
      Console.WriteLine("  file   the file to process");
      Console.WriteLine("\r\nExample: json my/json/file.txt");
    }
    else
    {
      try
      {
        // Parse arguments into component.
        json.InputFile = args[args.Length - 1];
        json.OutputFile = args[args.Length - 1];

        // Attempt to parse the provided file.
        await json.Parse();
        await json.Config("PrettyPrint = true");
        json.Overwrite = true;
        json.XPath = "/json";

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
            Console.WriteLine("  ?                                  display the list of valid commands");
            Console.WriteLine("  help                               display the list of valid commands");
            Console.WriteLine("  children                           display the children of the currently selected element (if named)");
            Console.WriteLine("  insert <new name> <new value> <value type> <position>");
            Console.WriteLine("                                     insert the specified name and value at the specified position");
            Console.WriteLine("  parent                             display the parent of the currently selected element (if named)");
            Console.WriteLine("  path                               display the full path of the currently selected element");
            Console.WriteLine("  remove                             remove the currently selected element");
            Console.WriteLine("  rename <new name>                  set a new name for the currently selected element");
            Console.WriteLine("  revalue <new value> <value type>   set a new value for the currently selected element");
            Console.WriteLine("  select <element>                   navigate to the specified element by providing either an absolute or relative path");
            Console.WriteLine("  subtree                            display a snapshot of the currently selected element in the document");
            Console.WriteLine("  text                               display the text of the currently selected element");
            Console.WriteLine("  type                               display the data type of the currently selected element");
            Console.WriteLine("  quit                               exit the application");
          }
          else if (arguments[0].Equals("children"))
          {
            Console.WriteLine("Currently selected element contains " + json.XChildren.Count + " children.");
            if (json.XChildren.Count > 0)
            {
              foreach (JSONElement child in json.XChildren)
              {
                if (!string.IsNullOrEmpty(child.Name))
                {
                  Console.WriteLine(child.Name);
                }
              }
            }
          }
          else if (arguments[0].Equals("insert"))
          {
            Console.Write("Inserting element...");
            int valueType;
            int position;
            int.TryParse(arguments[3], out valueType);
            int.TryParse(arguments[4], out position);
            await json.InsertProperty(arguments[1], arguments[2], valueType, position);
            Console.WriteLine("finished!");
            await SaveAndReparse();
            Console.WriteLine("New element successfully inserted.");
          }
          else if (arguments[0].Equals("parent"))
          {
            if (!string.IsNullOrEmpty(json.XParent))
            {
              Console.WriteLine(json.XParent);
            }
            else
            {
              Console.WriteLine("Currently selected element either contains no parent, or the parent is unnamed.");
            }
          }
          else if (arguments[0].Equals("path"))
          {
            Console.WriteLine(json.XPath);
          }
          else if (arguments[0].Equals("remove"))
          {
            Console.Write("Removing element...");
            await json.Remove();
            Console.WriteLine("finished!");
            await SaveAndReparse();
            Console.WriteLine("Currently selected element successfully removed.");
          }
          else if (arguments[0].Equals("rename"))
          {
            Console.Write("Renaming element...");
            await json.SetName(arguments[1]);
            Console.WriteLine("finished!");
            await SaveAndReparse();
            Console.WriteLine("Currently selected element successfully renamed.");
          }
          else if (arguments[0].Equals("revalue"))
          {
            Console.Write("Changing element value...");
            int valueType;
            int.TryParse(arguments[2], out valueType);
            await json.SetValue(arguments[1], valueType);
            Console.WriteLine("finished!");
            await SaveAndReparse();
            Console.WriteLine("Currently selected element's value successfully changed.");
          }
          else if (arguments[0].Equals("select"))
          {
            bool validPath = await json.TryXPath(arguments[1]);
            if (validPath)
            {
              Console.WriteLine("Currently selected element path is now \"" + json.XPath + "\".");
            }
            else
            {
              Console.WriteLine("Invalid path supplied.");
            }
          }
          else if (arguments[0].Equals("subtree"))
          {
            Console.WriteLine(json.XSubTree);
          }
          else if (arguments[0].Equals("text"))
          {
            if (!string.IsNullOrEmpty(json.XText))
            {
              Console.WriteLine(json.XText);
            }
            else
            {
              Console.WriteLine("Currently selected element contains no text.");
            }
          }
          else if (arguments[0].Equals("type"))
          {
            Console.WriteLine(json.XElementType);
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

          Console.Write("json> ");
        }
      }
      catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }

    static async Task SaveAndReparse()
    {
      Console.Write("Saving changes...");
      await json.Save();
      Console.WriteLine("finished!");
      Console.Write("Reparsing...");
      await json.Parse();
      Console.WriteLine("finished!");
      json.XPath = "/json";
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