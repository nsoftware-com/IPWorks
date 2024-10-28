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

class emailDemo
{
  private static SMTP smtp = new nsoftware.IPWorks.SMTP();

  private static void smtp_OnSSLServerAuthentication(object sender, SMTPSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {
    if (args.Length < 3)
    {
      Console.WriteLine("usage: email [options] server from to");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -c      a comma-separated list of addresses for carbon copies");
      Console.WriteLine("  -s      the subject of the mail message");
      Console.WriteLine("  -m      the raw message content");
      Console.WriteLine("  server  the name or address of a mail server (mail relay)");
      Console.WriteLine("  from    the email address of the sender");
      Console.WriteLine("  to      a comma-separated list of addresses for destinations");
      Console.WriteLine("\r\nExample: email -c cc@mail.local -s test -m \"test message\" mail.local sender@mail.com recipient@mail.local");
    }
    else
    {
      try
      {
        smtp.OnSSLServerAuthentication += smtp_OnSSLServerAuthentication;

        smtp.MailServer = args[args.Length - 3];
        smtp.From = args[args.Length - 2];
        smtp.SendTo = args[args.Length - 1];
        smtp.Timeout = 25;

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i] == "-c") smtp.Cc = args[i + 1]; // args[i + 1] corresponds to the value of argument [i]
            if (args[i] == "-s") smtp.Subject = args[i + 1];
            if (args[i] == "-m") smtp.MessageText = args[i + 1];
          }
        }

        // If SSL is desired, set SSLStartMode.
        //smtp.SSLStartMode = SMTPSSLStartModes.sslAutomatic;

        // Use these properties for client authentication.
        //smtp.User = ConsoleDemo.Prompt("User", "");
        //smtp.Password = ConsoleDemo.Prompt("Password", "");

        Console.WriteLine("Sending message...");
        smtp.Send();

        Console.WriteLine("Message sent successfully");
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
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