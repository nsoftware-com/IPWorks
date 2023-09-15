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

class MySMPP : public SMPP
{
public:
	virtual int FireConnected(SMPPConnectedEventParams *e)
	{
		printf("Connected\n");
		return 0;
	}
	virtual int FireDisconnected(SMPPDisconnectedEventParams *e)
	{
		printf("Disconnected\n");
		return 0;
	}
	virtual int FireSSLServerAuthentication(SMPPSSLServerAuthenticationEventParams *e)
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
	if (argc != 6) {

		fprintf(stderr, "usage: smpp server user password buddy message\n\n");		
		fprintf(stderr, "  server    the SMPP entity to which the component will connect\n");
		fprintf(stderr, "  user      used for identification with the SMPP service\n");
		fprintf(stderr, "  password  the user's password\n");
		fprintf(stderr, "  buddy     the ID or phone number of the recipient\n");
		fprintf(stderr, "  message   the message content\n");
		fprintf(stderr, "\nExample: smpp SmppServer username password recipient \"test message\"\n\n");
		printf("Press enter to continue.");
		getchar();
		return 0;
	}
	else{
		MySMPP smpp;
		
		int ret_code = 0;
		char * str_ret_code;
	
		smpp.SetSMPPServer(argv[1]);
		
		printf("Connecting...\n");

		ret_code = smpp.ConnectTo(argv[2], argv[3]);
		if (ret_code) goto done;
		
		smpp.AddRecipient(0, argv[4]);
		printf("Sending Message...");
		str_ret_code = smpp.SendMessage(argv[5]); //server-assigned id of the message.
		if (ret_code) goto done;
		else
			printf(" Done!\n");
		
		ret_code = smpp.Disconnect();
		if (ret_code) goto done;
	done:
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (smpp.GetLastError())
			{
				printf(" \"%s\"\n", smpp.GetLastError());
			}
		}
		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		return 0;
	}
	
}



