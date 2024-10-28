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

class htmlmailerDemo
{
  private static HTMLMailer htmlmailer;

  private static void htmlmailer_OnSSLServerAuthentication(object sender, HTMLMailerSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  static void Main(string[] args)
  {

    htmlmailer = new HTMLMailer();

    if (args.Length < 3)
    {

      Console.WriteLine("usage: htmlmailer [options] server from to");
      Console.WriteLine("Options: ");
      Console.WriteLine("  -s      the subject of the mail message");
      Console.WriteLine("  -m      the HTML version of the message content");
      Console.WriteLine("  -a      the path of file to attach to the message");
      Console.WriteLine("  server  the name or address of a mail server (mail relay)");
      Console.WriteLine("  from    the email address of the sender");
      Console.WriteLine("  to      a comma separated list of addresses for destinations");
      Console.WriteLine("\r\nExample: htmlmailer -s test -m \"<b>Hello</b>, my name is <i>Tom</i>\" -a FileToAttach mail.local sender@mail.com recipient@mail.local");

    }
    else
    {
      try {
        htmlmailer.OnSSLServerAuthentication += htmlmailer_OnSSLServerAuthentication;

        htmlmailer.MailServer = args[args.Length - 3];
        htmlmailer.From = args[args.Length - 2];
        htmlmailer.SendTo = args[args.Length - 1];

        for (int i = 0; i < args.Length; i++)
        {
          if (args[i].StartsWith("-"))
          {
            if (args[i].Equals("-s")) htmlmailer.Subject = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
            if (args[i].Equals("-m")) htmlmailer.MessageHTML = args[i + 1];
            if (args[i].Equals("-a")) htmlmailer.AddAttachment(args[i + 1]); //if you want to add attachment
          }
        }

        //Use these properties for client authentication
        //htmlmailer.setUser(prompt("User"));
        //htmlmailer.setPassword(prompt("Password"));

        Console.WriteLine("Sending message ...");
        htmlmailer.Send();

        Console.WriteLine("Message sent successfully");
      } catch (Exception e)
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