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
#include <string.h>
#include "../../include/ipworks.h"
#define LINE_LEN 100

class MyTraceRoute : public TraceRoute
{
	virtual int FireHop(TraceRouteHopEventParams * e)
	{
		if (e->Duration == -1)
		{
			printf("%i\tTIMEOUT\n");
		}
		else
		{
		printf("%i\t%s\t%i\n", e->HopNumber, e->HostAddress, e->Duration);
		}
		return 0;
	}
};

int main(int argc, char *argv[])
{
	MyTraceRoute trace;
	int ret_code = 0;

	if (argc != 2) {

		fprintf(stderr, "usage: tracert domain\n\n");
		fprintf(stderr, "  domain     the name or address of the host to trace to\n");		
		fprintf(stderr, "\nExample: tracert www.google.com\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		trace.SetHopLimit(100);                 // default = 64

		printf("Performing trace...\n");

		ret_code = trace.SetRemoteHost(argv[1]); // Traces route if host exists, returns nonzero if it doesn't

		if (!ret_code)      // If the host exists
		{
			printf("\nTrace Complete.\n");
		}
		else
		{
			printf("\nError %d: (%s)\n", ret_code, trace.GetLastError());
		}

		printf("\nPress ENTER to continue...");
		getchar();
		printf("\n");
		return 0;
	}	
}


