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
#include <time.h>
#include <string.h>
#include "../../include/ipworks.h"
#define LINE_LEN 100
bool dataInReceived;
class udpclient : public UDP
{
	virtual int FireDataIn(UDPDataInEventParams *e)
	{
		//then concatenate and show the time
		printf("Received '%s' from %s:%d.\n", e->Datagram, e->SourceAddress, e->SourcePort);
		dataInReceived = true;
		return 0;
	}
};

int main (int argc, char* argv[])
{
	udpclient p;
	int ret_code = 0;
	int limit= 0;

	if (argc < 3) {

		fprintf(stderr, "usage: udp host port\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  host    the address of the remote host\n");
		fprintf(stderr, "  port    the UDP port in the remote host\n\n");
		fprintf(stderr, "Example:	udp 192.168.1.2 777\n");
		fprintf(stderr, "Example: udp 255.255.255.255 777 (broadcast)\n");
		printf("Press enter to continue.");
		getchar();

	}
	else {
		ret_code = p.SetRemoteHost(argv[1]);
		ret_code = p.SetRemotePort(atoi(argv[2]));
		ret_code = p.Activate();
		if (ret_code)	
			goto done;
		
		fprintf(stdout, "Type and press enter to send. Press Ctrl-C to exit the application.\n");

		while (true)
		{
			dataInReceived = false;
			
			char text[LINE_LEN];
			fgets(text, LINE_LEN, stdin);
			text[strlen(text) - 1] = '\0';
			if (strlen(text) > 0) {
				ret_code = p.SendText(text);
				if (ret_code)
				{
					printf("Sending failed: %i - %s\n", ret_code, p.GetLastError());
				}
				else
				{
					while (!dataInReceived)
					{
						ret_code = p.DoEvents();
					}
				}
			}
		}

	done:
		if (ret_code)
		{
			printf("error %d (%s)\n", ret_code, p.GetLastError());
		}
		return 0;
	}

}


