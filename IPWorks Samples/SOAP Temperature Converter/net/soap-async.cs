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

class TemperatureConverter
{
  private static Soap soap1 = new Soap();
  static async Task Main(string[] args)
  {
    string command = "";
    List<string> arguments = new List<string>();
    Console.WriteLine("This sample console application converts temperature units using SOAP calls. Type '?' for a list of commands.");
    while(true)
    {
      command = Prompt("> ");
      if (command.Length > 0)
      {
        arguments = new List<string>(command.Split()); 
        command = arguments[0].ToLower();
        arguments.RemoveAt(0);
      }

      if (command == "?")
      {
        Console.WriteLine("Commands:");
        Console.WriteLine(" help                 Open help menu.");
        Console.WriteLine(" ?                    Open help menu.");
        Console.WriteLine(" f2c <temperature>    Convert Fahrenheit value, <temperature>, to Celsius.");
        Console.WriteLine(" c2f <temperature>    Convert Celsius value, <temperature>, to Fahrenheit.");
        Console.WriteLine(" quit                 Quit/exit.");
      }
      else if (command == "f2c")
      {
        try
        {
          Console.Write("Converting... ");
          soap1.Reset();
          soap1.Config("MethodNamespacePrefix=");
          soap1.URL = "https://www.w3schools.com/xml/tempconvert.asmx";
          soap1.MethodURI = "https://www.w3schools.com/xml/";
          soap1.Method = "FahrenheitToCelsius";
          soap1.ActionURI = soap1.MethodURI + soap1.Method;
          await soap1.AddParam("Fahrenheit", (arguments.Count > 0 ? arguments[0] : "0"));
          await soap1.SendRequest();
          soap1.XPath = "/Envelope/Body/FahrenheitToCelsiusResponse/FahrenheitToCelsiusResult";
          Console.WriteLine(arguments[0] + "F is " + soap1.XText + "C");
        }
        catch(Exception ex)
        {
          Console.WriteLine("Could not convert: " + ex.Message);
        }
      }
      else if (command == "c2f")
      {
        try
        {
          Console.Write("Converting... ");
          soap1.Reset();
          soap1.Config("MethodNamespacePrefix=");
          soap1.URL = "https://www.w3schools.com/xml/tempconvert.asmx";
          soap1.MethodURI = "https://www.w3schools.com/xml/";
          soap1.Method = "CelsiusToFahrenheit";
          soap1.ActionURI = soap1.MethodURI + soap1.Method;
          await soap1.AddParam("Celsius", (arguments.Count > 0 ? arguments[0] : "0"));
          await soap1.SendRequest();
          soap1.XPath = "/Envelope/Body/CelsiusToFahrenheitResponse/CelsiusToFahrenheitResult";
          Console.WriteLine(arguments[0] + "C is " + soap1.XText + "F");
        }
        catch (Exception ex)
        {
          Console.WriteLine("Could not convert: " + ex.Message);
        }
      }
      else if(command == "quit")
      {
        Console.WriteLine("Goodbye.");
        break;
      }
      else
      {
        Console.WriteLine("Invalid command.");
      }
    }
  }

  static string Prompt(string Prompt)
  {
    Console.Write(Prompt);
    return Console.ReadLine();
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