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

class jsonDemo
{
  private static JSON json;

  static void Main(string[] args)
  {
    json = new JSON();

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
        json.Parse();
        json.Config("PrettyPrint = true");
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
            json.InsertProperty(arguments[1], arguments[2], valueType, position);
            Console.WriteLine("finished!");
            SaveAndReparse();
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
            json.Remove();
            Console.WriteLine("finished!");
            SaveAndReparse();
            Console.WriteLine("Currently selected element successfully removed.");
          }
          else if (arguments[0].Equals("rename"))
          {
            Console.Write("Renaming element...");
            json.SetName(arguments[1]);
            Console.WriteLine("finished!");
            SaveAndReparse();
            Console.WriteLine("Currently selected element successfully renamed.");
          }
          else if (arguments[0].Equals("revalue"))
          {
            Console.Write("Changing element value...");
            int valueType;
            int.TryParse(arguments[2], out valueType);
            json.SetValue(arguments[1], valueType);
            Console.WriteLine("finished!");
            SaveAndReparse();
            Console.WriteLine("Currently selected element's value successfully changed.");
          }
          else if (arguments[0].Equals("select"))
          {
            bool validPath = json.TryXPath(arguments[1]);
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

    static void SaveAndReparse()
    {
      Console.Write("Saving changes...");
      json.Save();
      Console.WriteLine("finished!");
      Console.Write("Reparsing...");
      json.Parse();
      Console.WriteLine("finished!");
      json.XPath = "/json";
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