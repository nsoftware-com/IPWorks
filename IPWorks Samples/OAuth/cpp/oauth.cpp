/*
 * IPWorks 2022 C++ Edition - Sample Project
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
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworks.h"

#define LINE_LEN 80

class MyOAuth : public OAuth
{

public:
	
	virtual int FireLaunchBrowser(OAuthLaunchBrowserEventParams *e)
	{
		// Normally, the component will execute the command property to launch the browser for authorization.
		// Setting the command to an empty string will prevent a browser from opening the URL. The following 
		// line can be un-commented to exhibit this behavior.
		//e->Command = "";
		printf("\n\nAuthorization URL: %s\n\n", e->URL);
		return 0;
	}

	virtual int FireSSLServerAuthentication(OAuthSSLServerAuthenticationEventParams *e)
	{
		if (e->Accept) return 0;
		printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n",
			e->CertIssuer, e->CertSubject);
		printf("The following problems have been determined for this certificate: %s\n", e->Status);
		printf("Would you like to continue anyways? [y/n] ");
		if (getchar() == 'y') e->Accept = true;
		else exit(0);
		return 0;
	}
};

int main(int argc, char* argv[])
{


	MyOAuth oauth; //OAuth object
	HTTP http;
	JSON json;
	char buffer[LINE_LEN]; // user input

	printf("*****************************************************************\n");
	printf("* This application demonstrates how to use the OAuth component  *\n");
	printf("* to authenticate with Google using OAuth 2.0 (Device Profile). *\n");
	printf("* It also demonstrates how to use the retrieved Authorization   *\n");
	printf("* String with the JSON component to retrieve user information.  *\n");
	printf("* It will guide you through the steps to perform authorization  *\n");
	printf("* using OAuth. Please see the Introduction page within the help *\n");
	printf("* for more detailed instructions.                               *\n");
	printf("*****************************************************************\n\n");

	// Client ID and Client Secret
	//1. Obtain and set your Client ID and Client Secret. For Google, these values
	//can be found in the API Console: https://code.google.com/apis/console#access
	//The example values are from a Google test account that we have setup for you to
	//easily run this demo


	oauth.SetClientId("723966830965.apps.googleusercontent.com");
	oauth.SetClientSecret("_bYMDLuvYkJeT_99Q-vkP1rh");



	// Server Auth URL, Server Token URL, and Authorization Scope
	/*2. You can also set Server Auth URL, Server Token URL, and Authorization
	Scope to the values desired. These are preset to values for Google's User Info
	service.*/

	oauth.SetServerAuthURL("https://accounts.google.com/o/oauth2/auth");
	oauth.SetServerTokenURL("https://accounts.google.com/o/oauth2/token");
	oauth.SetAuthorizationScope("https://www.googleapis.com/auth/userinfo.email");

	// Get Authorize URL for user to authenticate
	/*3. The following URL will open in a web browser to authenticate to the
	service. Upon successfully authenticating and allowing access, the user will
	be redirected back to an embedded web server within the component.
	The Authorization String will then be set to the 'Authorization' property
	of the JSON component and used to retrieve the user info for the authenticated
	client.*/

	// Retrieve the authenticated user's info.
	printf("Getting OAuth Authorization String...\n");
	http.SetAuthorization(oauth.GetAuthorization());
	if (strlen(http.GetAuthorization()) == 0)
	{
		printf("No AuthorizationCode specified.\n");
	}
	else
	{
		printf("\nAuthorization String received. Retrieving user info for the authenticated\n");
		printf("client.\n\n");
		printf("Getting UserInfo...\n");
		int ret_code = http.Get("https://www.googleapis.com/oauth2/v1/userinfo");
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (http.GetLastError())
			{
				printf(" \"%s\"\n", http.GetLastError());
			}
		}
		else
		{
			char* transferredData;
			int transferredDataLen;
			http.GetTransferredData(transferredData, transferredDataLen);
			json.SetInputData(transferredData);
			json.Parse();
			json.SetXPath("/json/email");
			printf("Email: %s\n", json.GetXText());
			json.SetXPath("/json/verified_email");
			printf("Verified: %s\n", json.GetXText());
		}

	}

	printf("\nPress enter to exit...");
	fgets(buffer, LINE_LEN, stdin);

	return 0;

};


