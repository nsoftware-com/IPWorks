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

class restDemo
{
  private static NetCode netcode;
  private static REST rest;

  private static void rest_OnSSLServerAuthentication(object sender, RESTSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    netcode = new NetCode();
    rest = new REST();

    if (args.Length < 3)
    {
      Console.WriteLine("usage: rest key city state country");
      Console.WriteLine("  key      API key required for authentication (free and available at https://home.openweathermap.org/users/sign_up)");
      Console.WriteLine("  city     the city of the address for which to get weather data (underscore spaces in city names)");
      Console.WriteLine("  state    the state of the address for which to get weather data (only for the US)");
      Console.WriteLine("  country  the country of the address for which to get weather data (use ISO 3166 country codes)");
      Console.WriteLine("Further input documentation can be found at https://openweathermap.org/api/geocoding-api.");
      Console.WriteLine("\r\nExample: rest da9bd73746219f432ddb52abf6b3b087 Chapel_Hill NC US");
      Console.WriteLine("Example: rest da9bd73746219f432ddb52abf6b3b087 London GB");
    }
    else
    {
      rest.OnSSLServerAuthentication += rest_OnSSLServerAuthentication;

      try
      {
        // Parse arguments.
        string apiKey;
        string address;

        if (args.Length > 3 && args[args.Length - 1].Equals("US"))
        {
		  apiKey = args[args.Length - 4];
          address = args[args.Length - 3].Replace("_", " ") + "," + args[args.Length - 2] + ",US";
        }
        else if (args.Length > 2 && !args[args.Length - 1].Equals("US"))
        {
		  apiKey = args[args.Length - 3];
          address = args[args.Length - 2].Replace("_", " ") + "," + args[args.Length - 1];
        }
        else
        {
          throw new Exception("Invalid address provided.  Input documentation can be found at https://openweathermap.org/api/geocoding-api.");
        }
		
        // Geocode the address to retrieve its latitude and longitude.
        netcode.Format = NetCodeFormats.fmtURL;
        netcode.DecodedData = address;
        netcode.Encode();
        address = netcode.EncodedData;

        rest.Get("https://api.openweathermap.org/geo/1.0/direct?q=" + address + "&appid=" + apiKey);

        string latitude, longitude;
        rest.XPath = "/json/[1]/lat";
        latitude = rest.XText;
        rest.XPath = "../lon";
        longitude = rest.XText;

        // If you wish to see the entire geocoding REST response, uncomment the line below.
        //Console.WriteLine(rest.TransferredData);

        // Retrieve the weather at those coordinates.
        rest.Get("https://api.openweathermap.org/data/2.5/weather?lat=" + latitude + "&lon=" + longitude + "&appid=" + apiKey);

        // If you wish to see the entire weather REST response, uncomment the line below.
        //Console.WriteLine(rest.TransferredData);

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
            Console.WriteLine("  ?                            display the list of valid commands");
            Console.WriteLine("  help                         display the list of valid commands");
            Console.WriteLine("  clouds                       display information relating to the cloud cover");
            Console.WriteLine("  conditions                   display the main weather conditions and a description");
            Console.WriteLine("  temperature                  display information relating to the current temperature");
            Console.WriteLine("  visibility                   display information relating to the visibility");
            Console.WriteLine("  wind                         display information relating to the wind conditions");
            Console.WriteLine("  quit                         exit the application");
          }
          else if (arguments[0].Equals("clouds"))
          {
            rest.XPath = "/json/clouds/all";
            Console.WriteLine("\tCloud Cover:  " + rest.XText + "%");
          }
          else if (arguments[0].Equals("conditions"))
          {
            rest.XPath = "/json/weather/[1]/main";
            Console.WriteLine("\tMain Weather Conditions:  " + rest.XText);
            rest.XPath = "../description";
            Console.WriteLine("\tDescription:  " + rest.XText);
          }
          else if (arguments[0].Equals("temperature"))
          {
            rest.XPath = "/json/main/temp";
            Console.WriteLine("\tCurrent Temperature:  " + rest.XText + " K");
            rest.XPath = "../feels_like";
            Console.WriteLine("\tFeels Like:  " + rest.XText + " K");
            rest.XPath = "../temp_min";
            Console.WriteLine("\tMinimum Temperature:  " + rest.XText + " K");
            rest.XPath = "../temp_max";
            Console.WriteLine("\tMaximum Temperature:  " + rest.XText + " K");
            rest.XPath = "../pressure";
            Console.WriteLine("\tPressure:  " + rest.XText + " hPa");
            rest.XPath = "../humidity";
            Console.WriteLine("\tHumidity:  " + rest.XText + "%");
          }
          else if (arguments[0].Equals("visibility"))
          {
            rest.XPath = "/json/visibility";
            Console.WriteLine("\tVisibility:  " + rest.XText + " m");
          }
          else if (arguments[0].Equals("wind"))
          {
            rest.XPath = "/json/wind/speed";
            Console.WriteLine("\tWind Speed:  " + rest.XText + " m/s");
            rest.XPath = "../deg";
            Console.WriteLine("\tDegrees:  " + rest.XText + " degrees");
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

          Console.Write("rest> ");
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