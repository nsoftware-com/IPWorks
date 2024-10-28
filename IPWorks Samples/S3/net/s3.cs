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

class s3Demo
{
  private static S3 s3 = new nsoftware.IPWorks.S3();

  static void Main(string[] args)
  {
    try
    {
      s3.OnBucketList += s3_OnBucketList;
      s3.OnObjectList += s3_OnObjectList;
      s3.OnSSLServerAuthentication += s3_OnSSLServerAuthentication;

      Console.WriteLine("0     -   Amazon S3");
      Console.WriteLine("1     -   Digital Ocean");
      Console.WriteLine("2     -   Google Storage");
      Console.WriteLine("3     -   Wasabi");
      Console.WriteLine("4     -   Backblaze B2");
      Console.WriteLine("5     -   Huawei");
      Console.WriteLine("6     -   Alibaba");
      Console.WriteLine("7     -   IBM");
      Console.WriteLine("8     -   Oracle");
      Console.WriteLine("9     -   Linode");
      Console.WriteLine("10    -   Cloudflare R2");
      Console.WriteLine("11    -   Seagate Lyve Cloud");
      Console.WriteLine("255   -   Custom\n");
      Console.Write("Select service provider (enter number): ");

      // Prompt for authentication information.
      int servicePrompt = int.Parse(Console.ReadLine());
      SelectServiceProvider(servicePrompt);

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("s3> ");
      string command;
      string[] arguments;

      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  cd <bucket>                  change to the specified bucket");
          Console.WriteLine("  lb                           list all buckets");
          Console.WriteLine("  lo                           list all objects in the currently selected bucket");
          Console.WriteLine("  get <object>                 get the specified object");
          Console.WriteLine("  put <name> <file>            create a new object in the currently selected bucket");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0] == "cd")
        {
          if (arguments.Length > 1)
          {
            s3.Bucket = arguments[1];
          }
        }
        else if (arguments[0] == "lb")
        {
          s3.ListBuckets();
        }
        else if (arguments[0] == "lo")
        {
          s3.ListObjects();
        }
        else if (arguments[0] == "get")
        {
          if (arguments.Length > 1)
          {
            s3.GetObject(arguments[1]);
            Console.WriteLine("Content of the object:\n" + s3.ObjectData);
          }
        }
        else if (arguments[0] == "put")
        {
          if (arguments.Length > 2)
          {
            s3.LocalFile = arguments[2];
            s3.CreateObject(arguments[1]);
            Console.WriteLine("Object created.");
          }
        }
        else if (arguments[0] == "quit")
        {
          break;
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("s3> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void SelectServiceProvider(int servicePrompt)
  {
    Console.Write("Enter your access key: ");
    s3.AccessKey = Console.ReadLine();
    Console.Write("Enter your secret key: ");
    s3.SecretKey = Console.ReadLine();

    switch (servicePrompt)
    {
      case 0:
        s3.ServiceProvider = S3ServiceProviders.spAmazonS3;
        break;
      case 1:
        s3.ServiceProvider = S3ServiceProviders.spDigitalOcean;
        break;
      case 2:
        s3.ServiceProvider = S3ServiceProviders.spGoogleStorage;
        break;
      case 3:
        s3.ServiceProvider = S3ServiceProviders.spWasabi;
        break;
      case 4:
        s3.ServiceProvider = S3ServiceProviders.spBackblazeB2;
        break;
      case 5:
        s3.ServiceProvider = S3ServiceProviders.spHuawei;
        break;
      case 6:
        s3.ServiceProvider = S3ServiceProviders.spAlibaba;
        break;
      case 7:
        s3.ServiceProvider = S3ServiceProviders.spIBM;
        break;
      case 8:
        s3.ServiceProvider = S3ServiceProviders.spOracle;
        Console.Write("Enter Object Storage namespace: ");
        s3.Config("OracleNamespace=" + Console.ReadLine());
        break;
      case 9:
        s3.ServiceProvider = S3ServiceProviders.spLinode;
        break;
      case 10:
        s3.ServiceProvider = S3ServiceProviders.spCloudflareR2;
        Console.Write("Enter Cloudflare account ID: ");
        s3.Config("CloudflareAccountID=" + Console.ReadLine());
        break;
      case 11:
        s3.ServiceProvider = S3ServiceProviders.spSeagateLyveCloud;
        break;
      case 255:
        s3.ServiceProvider = S3ServiceProviders.spCustom;
        Console.Write("Enter custom provider URL: ");
        s3.Config("ServiceProviderURL=" + Console.ReadLine());
        break;
      default:
        throw new Exception("Invalid S3 service provider.\n");
    }
  }

  private static void s3_OnBucketList(object sender, S3BucketListEventArgs e)
  {
    Console.WriteLine(e.BucketName);
  }

  private static void s3_OnObjectList(object sender, S3ObjectListEventArgs e)
  {
    Console.WriteLine(e.ObjectName);
  }

  private static void s3_OnSSLServerAuthentication(object sender, S3SSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
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