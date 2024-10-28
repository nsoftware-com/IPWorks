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

class UDPServer : public UDP
{	
	virtual int FireDataIn(UDPDataInEventParams* e)
	{
		printf("Echoing '%s' back to client %s:%d.\n", e->Datagram, e->SourceAddress, e->SourcePort);
		SetRemoteHost(e->SourceAddress);
		SetRemotePort(e->SourcePort);		
		SendText(e->Datagram);
		return 0;
	}
};

int main(int argc, char* argv[])
{
	UDPServer udp;

	char buffer[LINE_LEN];

	printf("***************************************************************\n");
	printf("*   This demo shows how to set up an echo server using UDP.   *\n");
	printf("***************************************************************\n");

	if (argc < 2) {
		fprintf(stderr, "usage: udp port\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  port      the UDP port in the local host where the component listens\n");		
		fprintf(stderr, "\nExample : udp 777\n");
		
		printf("Press enter to continue.");
		getchar();
	}
	else{
		udp.SetLocalPort(atoi(argv[1]));
		int ret_code = udp.Activate();

		if (ret_code)
		{
			printf("Error: %i - %s\n", ret_code, udp.GetLastError());
			goto done;
		}

		printf("Listening on port %d... press Ctrl-C to shutdown.\n", udp.GetLocalPort());
		
		while (true)
		{
			udp.DoEvents();
		}
	done:
		
		printf("Exiting... (press enter)\n");
		getchar();

		return 0;
	}
}

