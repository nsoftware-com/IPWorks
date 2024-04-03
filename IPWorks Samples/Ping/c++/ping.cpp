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

int main(int argc, char *argv[])
{

	Ping ping;
	int ret_code;
	int i;

	if (argc != 2)
	{
		fprintf(stderr, "usage:  ping hostname\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  hostname the IP address or Domain Name of the remote host\n");
		fprintf(stderr, "\nExample ping www.yahoo.com\n\n");
		printf("Press enter to continue.");
		getchar();
	}
	if(ret_code = ping.SetRemoteHost(argv[1]) )  goto done;

	ping.SetPacketSize(32);
	printf("\nPinging %s with %i bytes of data:\n\n", argv[1], ping.GetPacketSize());

	for (i=0; i<4; i++)
	{
		ping.SetTimeToLive(255);
		if (ret_code = ping.PingHost(argv[1])) goto done;
		printf("Reply from %s: bytes=%i time=%ims\n", ping.GetRemoteHost(), ping.GetPacketSize(), ping.GetResponseTime());
	}

done:
	if (ret_code)     // Got an error.  The user is done.
	{
		printf( "\nError: %d", ret_code );
		if (ping.GetLastError())
		{
			printf( " \"%s\"\n", ping.GetLastError() );
		}
	}
	printf("Press enter to continue.");
	getchar();  
	return 0;
}









