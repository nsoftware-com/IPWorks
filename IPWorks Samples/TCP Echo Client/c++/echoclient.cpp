/*
 * IPWorks 2024 C++ Edition - Sample Project
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

class MyTCPClient : public TCPClient
{
public:
	int FireDisconnected(TCPClientDisconnectedEventParams *e)
	{
		printf("Disconnected from %s.\n", this->GetRemoteHost());
		return 0;
	}

	int FireDataIn(TCPClientDataInEventParams *e)
	{
		printf("Received '%s' from '%s'\n", e->Text, GetRemoteHost());
		dataInReceived = true;
		return 0;
	}

	int FireSSLServerAuthentication(TCPClientSSLServerAuthenticationEventParams *e)
	{
		if (!e->Accept)
		{
			printf("Server provided the following certificate: \n");
			printf("Subject: %s \n", e->CertSubject);
			printf("Issuer: %s \n", e->CertIssuer);
			printf("The following problems have been determined for this certificate:  %s \n", e->Status);
			printf("Would you like to continue or cancel the connection?  [y/n] ");

			char response[LINE_LEN];

			fgets(response, LINE_LEN, stdin);
			response[strlen(response) - 1] = '\0';
			if (strcmp(response, "y") == 0)
				e->Accept = true;
		}

		return 0;
	}

};

int main(int argc, char* argv[])
{
	MyTCPClient tcpclient;

	printf("*****************************************************************\n");
	printf("* This is a demo to show how to connect to a remote echo server *\n");
	printf("* to send data, and receive the echoed response.                *\n");
	printf("*****************************************************************\n");


	if (argc < 3) {

		fprintf(stderr, "usage: echoclient server port [ssl]\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  server  the address of the remote host\n");
		fprintf(stderr, "  port    the TCP port in the remote host\n");
		fprintf(stderr, "\nExample (Plaintext): echoclient localhost 777\n");
		fprintf(stderr, "\nExample (SSL):       echoclient localhost 777 ssl\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		tcpclient.SetTimeout(10);

		if (argc > 3)
		{
			if (!strcmp(argv[3], "ssl"))
			{
				tcpclient.SetSSLEnabled(true);
			}
		}

		int ret_code = tcpclient.ConnectTo(argv[1], atoi(argv[2]));

		if (ret_code)
		{
			printf("Error connecting: %i - %s\n", ret_code, tcpclient.GetLastError());
			goto done;
		}

		char command[LINE_LEN];
		while (true)
		{
			dataInReceived = false;
			printf("\nPlease input command: \r\n- 1 Send Data \r\n- 2 Exit \r\n");
			printf(">");

			fgets(command, LINE_LEN, stdin);
			command[strlen(command) - 1] = '\0';

			if (!strcmp(command, "1"))
			{
				char text[LINE_LEN];
				printf("Please enter data to send: ");
				fgets(text, LINE_LEN, stdin);
				text[strlen(text) - 1] = '\0';
				ret_code = tcpclient.SendText(text);
				if (ret_code)
				{
					printf("Sending failed: %i - %s\n", ret_code, tcpclient.GetLastError());
				}
				else
				{
					printf("Waiting for response...\n");
					while (!dataInReceived)
						tcpclient.DoEvents();
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
		if (tcpclient.GetConnected())
		{
			tcpclient.Disconnect();
		}
		printf("Exiting... (press enter)\n");
		getchar();

		return 0;
	}

}






