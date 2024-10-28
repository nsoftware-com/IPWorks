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

class MyWSServer : public WSServer
{
	int FireConnected(WSServerConnectedEventParams *e)
	{
		printf("%s has connected.\n", this->GetWSConnectionRemoteHost(e->ConnectionId));
		return 0;
	}

	int FireDisconnected(WSServerDisconnectedEventParams *e)
	{
		printf("Disconnected from %s.\n", this->GetWSConnectionRemoteHost(e->ConnectionId));
		return 0;
	}

	int FireDataIn(WSServerDataInEventParams *e)
	{
		printf("%s - Echoing '%s' back to client.\n", this->GetWSConnectionRemoteHost(e->ConnectionId), e->Text);
		char *text = (char*)e->Text;
		this->Send(e->ConnectionId, text, strlen(text));
		return 0;
	}
};

int main(int argc, char* argv[])
{
	MyWSServer wsserver;

	char buffer[LINE_LEN];

	printf("*****************************************************************\n");
	printf("* This demo shows how to set up an echo server using WSServer.  *\n");
	printf("*****************************************************************\n");

	printf("Local Port: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer)-1] = '\0';
	wsserver.SetLocalPort(atoi(buffer));
	
	printf("Use SSL? [y/n]");
	char response[LINE_LEN];

	fgets(response,LINE_LEN,stdin);
	response[strlen(response)-1] = '\0';
	if (strcmp(response,"y") == 0)
		wsserver.SetUseSSL(true);
		
	if (wsserver.GetUseSSL())
	{
		//This demo will assume a PFX file is being used for the server certificate but other formats can be used.
		wsserver.SetSSLCertStoreType(CST_PFXFILE);

		printf("PFX File to use for server certificate: ");
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';
		wsserver.SetSSLCertStore(buffer,strlen(buffer));

		printf("PFX File Password: ");
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';
		wsserver.SetSSLCertStorePassword(buffer);

		//The default value of "*" picks the first private key in the certificate. For simplicity this demo will use that value.
		wsserver.SetSSLCertSubject("*");
	}

	int ret_code = wsserver.StartListening();

	if(ret_code)
	{
		printf("Error: %i - %s\n", ret_code,wsserver.GetLastError());
		goto done;
	}

	printf("Listening...\n");

	while(true)
	{
		wsserver.DoEvents();
	}

done:
	if (wsserver.GetListening())
	{
		wsserver.StopListening();
	}

	printf( "Exiting... (press enter)\n" );
	getchar();

	return 0;
}

