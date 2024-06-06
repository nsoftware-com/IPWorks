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

﻿using nsoftware.IPWorks;
using System;

class oauth
{
  public static OAuth oauth1 = new OAuth();

  private static void oauth1_OnSSLServerAuthentication(object sender, OAuthSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  public static void oauth1_OnLaunchBrowser(object sender, OAuthLaunchBrowserEventArgs e)
  {
    // Normally, the component will execute the command property to launch the browser for authorization.
    // Setting the command to an empty string will prevent a browser from opening the URL. The following 
    // line can be un-commented to exhibit this behavior.
    //e.command = "";
    Console.WriteLine("Authorization URL: " + e.URL + "");
  }

  static void Main(string[] args)
  {
    if (args.Length < 5)
    {
      Console.WriteLine("usage: oauth clientID clientSecret serverAuthURL serverTokenURL authScope");
      Console.WriteLine("  clientID        the id of the client assigned when registering the application (e.g. 723966830965.apps.googleusercontent.com)");
      Console.WriteLine("  clientSecret    the secret value for the client assigned when registering the application (e.g. _bYMDLuvYkJeT_99Q-vkP1rh)");
      Console.WriteLine("  serverAuthURL   the URL of the authorization (e.g. server.https://accounts.google.com/o/oauth2/auth");
      Console.WriteLine("  serverTokenURL  the URL used to obtain the access token (e.g. https://accounts.google.com/o/oauth2/token)");
      Console.WriteLine("  authScope       the scope request or response parameter used during authorization (e.g. https://www.googleapis.com/auth/userinfo.email)");
      Console.WriteLine("\r\nExample: oauth 723966830965.apps.googleusercontent.com _bYMDLuvYkJeT_99Q-vkP1rh https://accounts.google.com/o/oauth2/auth https://accounts.google.com/o/oauth2/token https://www.googleapis.com/auth/userinfo.email");
      return;
    }
    try
    {
      oauth1.OnSSLServerAuthentication += oauth1_OnSSLServerAuthentication;
      oauth1.OnLaunchBrowser += oauth1_OnLaunchBrowser;

      /*This application demonstrates how to use the OAuth component to authenticate with Google using OAuth 2.0 (Device Profile). 
          It also demonstrates how to use the retrieved Authorization String with the HTTP and JSON components to retrieve user information. 
          It will guide you through the steps to perform authorization using OAuth. 
          Please see the Introduction page within the help for more detailed instructions.

      /*Client ID and Client Secret
          Obtain and set your Client ID and Client Secret. For Google, these values can be found in the API Console:
          https://code.google.com/apis/console#access
          The values that are given as an example are from a Google test account that we have setup for you to easily run this demo. */

      String clientID = args[args.Length - 5];
      String clientSecret = args[args.Length - 4];


      /*Server Auth URL, Server Token URL, and Authorization Scope
          You can also set Server Auth URL, Server Token URL, and Authorization Scope to the values desired.
          These are preset to values for Google's User Info service.*/

      String serverAuthURL = args[args.Length - 3];
      String serverTokenURL = args[args.Length - 2];
      String authScope = args[args.Length - 1];


      // Get Authorize URL for user to authenticate
      oauth1.ClientId = clientID;
      oauth1.ClientSecret = clientSecret;
      oauth1.ServerAuthURL = serverAuthURL;
      oauth1.ServerTokenURL = serverTokenURL;
      oauth1.AuthorizationScope = authScope;

      /*The following URL will open in a web browser to authenticate to the
          service. Upon successfully authenticating and allowing access, the user will
          be redirected back to an embedded web server within the component.
          The Authorization String will then be set to the 'Authorization' property
          of the HTTP component and used to retrieve the user info for the authenticated
          client.*/

      String authString = oauth1.GetAuthorization();

      // Retrieve the user info for the authenticated client.
      Console.WriteLine("\nAuthorization String received:\n");
      Console.WriteLine(authString);
    }
    catch (IPWorksException e)
    {
      Console.WriteLine(e.Message);
    }
    Console.WriteLine("Press any key to exit...");
    Console.ReadKey();
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