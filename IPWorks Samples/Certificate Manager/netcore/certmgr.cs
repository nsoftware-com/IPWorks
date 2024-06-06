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
using System.IO;

class certmgrDemo
{
  private static CertMgr certmgr1 = new CertMgr();
  private static string[] certificateList = null;
  
  private static void setStore(string storeType, string storename, string password)
  {
    certmgr1.CertStorePassword = password;
    certmgr1.CertStore = storename;
    switch (storeType.ToLower())
    {
      case "pfx":
        certmgr1.CertStoreType = CertStoreTypes.cstPFXFile;
        break;
      case "user":
        certmgr1.CertStoreType = CertStoreTypes.cstUser;
        break;
      case "machine":
        certmgr1.CertStoreType = CertStoreTypes.cstMachine;
        break;
      default: throw new Exception("Please specify valid store type.");
    }
    if (certmgr1.CertStoreType == CertStoreTypes.cstUser || certmgr1.CertStoreType == CertStoreTypes.cstMachine || File.Exists(storename))
    {
      Console.WriteLine("Listing store certificates...");
      Console.WriteLine("Subject \t | CertIssue \t | CertSerialNumber \t | HasPrivateKey");
      certificateList = (certmgr1.ListStoreCertificates()).Split("\n");
      int i = 0;
      foreach (string line in certificateList)
      {
        if (line.Length > 0)
        {
          Console.WriteLine(i + ". " + line);
          i++;
        }
      }
    }
  }

  private static void create(string subject, int serialNumber, int certNumber)
  {
    if(certNumber < 0 || certNumber >= certificateList.Length)
    {
      certmgr1.CreateCertificate(subject, serialNumber);
    } else if(certmgr1.CertStoreType != CertStoreTypes.cstPFXFile)
    {
      string[] chosenCert = certificateList[certNumber].Split("\t");
      certmgr1.Cert = new Certificate(certmgr1.CertStoreType, certmgr1.CertStore, certmgr1.CertStorePassword, chosenCert[0]);
      certmgr1.IssueCertificate(subject, serialNumber);
    } else
    {
      Console.WriteLine("Unsupported action.");
    }

  }


  private static void promptMenu()
  {
    Console.WriteLine("*****************************************************************************************");
    Console.WriteLine("store  <user|machine|pfx> <filename|store>  [password]   Set and list the store.");
    Console.WriteLine("    EX: store user MY");
    Console.WriteLine("    EX: store pfx test.pfx");
    Console.WriteLine("    EX: store pfx test.pfx password");
    Console.WriteLine("create <subject> <serial number> [certNumber]            Creates certificate in the store. If no certificate is \n" +
                      "                                                         set, then a self-signed certificate is created.");
    Console.WriteLine("    EX: create CN=TestSubject 1111");
    Console.WriteLine("    EX: create CN=TestSubject 1111 2");
    Console.WriteLine("help                                                     Print this menu.");
    Console.WriteLine("quit                                                     Exit the demo.");
	Console.WriteLine("*****************************************************************************************");
  }

  static void Main(string[] args)
  {
    bool quit = false;
    promptMenu();
    do
    {
      Console.Write("\ncertmgr> ");
      string responseFull = Console.ReadLine();
      string[] argument = responseFull.Split(" ");
      switch (argument[0].ToLower())
      {
        case "store":
          if(argument.Length == 3)  // no password specified
          {
            setStore(argument[1], argument[2], "");
          } else if (argument.Length == 4)  // password specified
          {
            setStore(argument[1], argument[2], argument[3]);
          } else {
            Console.WriteLine("Please supply a valid number of arguments.");
          }
          break;
        case "create":
          if (argument.Length == 3) // create self-signed certificate
          {
            create(argument[1], int.Parse(argument[2]), -1);
          } else if (argument.Length == 4)  // create certificate signed by specified certificate
          {
            create(argument[1], int.Parse(argument[2]), int.Parse(argument[3]));
          } else
          {
            Console.WriteLine("Please supply a valid number of arguments.");
          }
          break;
        case "help":
          promptMenu();
          break;
        case "quit":
          quit = true;
          break;
        default:
          Console.WriteLine("Invalid option. Please enter a valid command.\n");
          break;
      }
    } while (!quit);
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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