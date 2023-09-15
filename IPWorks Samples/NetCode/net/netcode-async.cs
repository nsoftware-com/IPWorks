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
using System.Threading.Tasks;
using nsoftware.async.IPWorks;

class netcodeDemo
{
  private static Netcode netcode = new Netcode();

  static async Task Main(string[] args)
  {
    Console.Write("Would you like to encode[1] a message or decode[2]?\n> ");
    string mode = Console.ReadLine();
    bool encode = mode.ToLower() == "1" || mode.ToLower() == "encode";
    Console.WriteLine("What encoding would you like to use?");
    foreach (NetcodeFormats format in Enum.GetValues(typeof(NetcodeFormats)))
    {
      Console.WriteLine("{0}: {1}", ((int)format), format);
    }
    Console.Write("> ");
    netcode.Format = (NetcodeFormats)int.Parse(Console.ReadLine());
    Console.Write("What message would you like to " + (encode ? "encode" : "decode") + "?\n> ");
    string message = Console.ReadLine();
    if (encode)
    {
      netcode.DecodedData = message;
      await netcode.Encode();
      Console.WriteLine("Encoded message:\n{0}", netcode.EncodedData);
    } 
    else
    {
      netcode.EncodedData = message;
      await netcode.Decode();
      Console.WriteLine("Decoded message:\n{0}", netcode.DecodedData);
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