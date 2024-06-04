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
#define LINE_LEN 100

class MyXMPP : public XMPP
{
public:
	virtual int FireConnected(XMPPConnectedEventParams *e)
	{
		printf("Welcome! Connection established!\n");
		return 0;
	}
	virtual int FireDisconnected(XMPPDisconnectedEventParams *e)
	{
		printf("Disconnected\n");
		return 0;
	}
	virtual int FireMessageIn(XMPPMessageInEventParams *e)
	{
		printf("%s said : %s\n\n", e->From, e->MessageText);
		return 0;
	}

	virtual int FireSSLServerAuthentication(XMPPSSLServerAuthenticationEventParams *e)
	{
		e->Accept = true;
		return 0;
	}
};

int main(int argc, char* argv[])
{

	if (argc != 4) {

		fprintf(stderr, "usage: jabber server username password\n\n");
		fprintf(stderr, "  server    the name or address of the jabber server (XMPP server)\n");
		fprintf(stderr, "  username  the user name used to authenticate to the jaber server\n");
		fprintf(stderr, "  password  the password used to authenticate to the jaber server\n");
		fprintf(stderr, "\nExample: jabber talk.google.com username password\n\n");
		printf("Press enter to continue.");
		getchar();
	}
	else{

		MyXMPP jabber;
		char buddy[80]; //users input
		char buffer[LINE_LEN + 1];
		int ret_code = 0;
		int buddyCount = 0;


		jabber.SetIMServer(argv[1]);
		jabber.SetSSLStartMode(SSL_AUTOMATIC);
		
		printf("Connecting...\n\n");
		ret_code = jabber.ConnectTo(argv[2], argv[3]);
		if (ret_code) goto done;

		buddyCount = jabber.GetBuddyCount();
		printf("Buddy list:\n");

		if (buddyCount != 0){

			for (int i = 0; i < buddyCount; i++)
			{
				printf("%d""%s""%s\n", i + 1, ") ", jabber.GetBuddyId(i));
			}

			printf("\nSelect a buddy : ");
			fgets(buddy, 80, stdin);
			buddy[strlen(buddy) - 1] = '\0';

			printf("\nMessage: ");
			fgets(buffer, 80, stdin);
			buffer[strlen(buffer) - 1] = '\0';

			jabber.SetMessageText(buffer);

			jabber.SendMessage(jabber.GetBuddyId(atoi(buddy) - 1));

			printf("\n\nReceiving responses, Ctrl-C to exit.\n");
			while (1)
			{
				jabber.DoEvents();
			}

			printf("Disconnecting...\n\n");
			ret_code = jabber.Disconnect();
			if (ret_code) goto done;

		}
		else
		{
			printf("No buddies found!");
		}

	done:

		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (jabber.GetLastError())
			{
				printf(" \"%s\"\n", jabber.GetLastError());
			}
		}
		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		exit(ret_code);
		return 0;


	}

}



