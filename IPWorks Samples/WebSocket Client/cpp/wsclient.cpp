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
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "../../include/ipworks.h"
#define LINE_LEN 80

bool dataInReceived;

class MyWSClient : public WSClient
{
public:
	int FireDisconnected(WSClientDisconnectedEventParams *e)
	{
		printf("Disconnected\n");
		return 0;
	}

	int FireDataIn(WSClientDataInEventParams *e)
	{
		printf("Received '%s' \n", e->Text);
		dataInReceived = true;
		return 0;
	}

	int FireSSLServerAuthentication(WSClientSSLServerAuthenticationEventParams *e)
	{
		if (!e->Accept)
		{
			printf("Server provided the following certificate: \n");
			printf("Subject: %s \n", e->CertSubject);
			printf("Issuer: %s \n", e->CertIssuer);
			printf("The following problems have been determined for this certificate:  %s \n", e->Status);
			printf("Would you like to continue or cancel the connection?  [y/n] ");

			char response[LINE_LEN];

			fgets(response,LINE_LEN,stdin);
			response[strlen(response)-1] = '\0';
			if (strcmp(response,"y") == 0)
				e->Accept = true;
		}

		return 0;
	}
};

int main(int argc, char* argv[])
{
	MyWSClient websocketclient;

	printf("*****************************************************************\n");
	printf("* This is a demo to show how to connect to a web socket echo    *\n");
	printf("* server to send data, and receive the echoed response.         *\n");
	printf("*****************************************************************\n");

	printf("URL (for instance 'ws://localhost:777'): ");
	char url[LINE_LEN];
	fgets(url, LINE_LEN, stdin);
	url[strlen(url)-1] = '\0';

	websocketclient.SetTimeout(10);
	int ret_code = websocketclient.ConnectTo(url);

	if(ret_code)
	{
		printf("Error connecting: %i - %s\n", ret_code,websocketclient.GetLastError());
		goto done;
	}

	char command[LINE_LEN];
	while(true)
	{
		dataInReceived = false;
		printf("\nPlease input command: \r\n- 1 Send Data \r\n- 2 Exit \r\n");
		printf(">");

		fgets(command,LINE_LEN,stdin);
		command[strlen(command)-1] = '\0';

		if (!strcmp(command, "1"))
		{
			char text[LINE_LEN];
			printf("Please enter data to send: ");
			fgets(text,LINE_LEN,stdin);
			text[strlen(text)-1] = '\0';
			ret_code = websocketclient.SetDataToSend(text, strlen(text));
			if (ret_code)
			{
				printf("Sending failed: %i - %s\n", ret_code, websocketclient.GetLastError());
			}
			else
			{
				printf("Waiting for response...\n");
				while(!dataInReceived)
					websocketclient.DoEvents();
			}
		}
		else if (!strcmp(command, "2"))
		{
			goto done;
		}
		else
		{
			printf("Command not recognized.\n");
		}
	}

done:
	if (websocketclient.GetConnected())
	{
		websocketclient.Disconnect();
	}
	printf( "Exiting... (press enter)\n" );
	getchar();

	return 0;
}


