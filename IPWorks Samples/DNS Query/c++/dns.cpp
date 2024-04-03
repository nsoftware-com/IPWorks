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

class MyDNS : public DNS
{
public:
	char *recordFieldVal;
	int rf_val_len;
	virtual int FireResponse(DNSResponseEventParams *e)
	{
		//record, type, name, value
		if (e->StatusCode == 100)
		{
			//No records were found, output a message and return
			printf("No %d records found.\n", GetQueryType());
			return 0;
		}
		//if we make it this far, loop through all the records returned and
		//output the recordtype and field values.
		for (int i = 0; i < GetRecordCount(); i++)
		{
			for (int j = 0; j < GetRecordFieldCount(i); j++)
			{
				SetRecordFieldIndex(i, j);
				GetRecordFieldValue(i, recordFieldVal, rf_val_len);
				if (j == 0)
					printf("%s\t", GetRecordTypeName(i));
				else
					printf("\t");
				printf("%s\t%s\n", GetRecordFieldName(i), recordFieldVal);
			}
		}
		return 0;
	}
};

int main(int argc, char* argv[])
{
	int ret_code;
	
	MyDNS dns1;

	if (argc != 3) {

		fprintf(stderr, "usage: dns server hostname\n");
		fprintf(stderr,"\n");
		fprintf(stderr,"  server    the address of the DNS server.\n");
		fprintf(stderr,"  hostname  the host domain to query\n");
		fprintf(stderr,"\nExample: dns 8.8.8.8 www.yahoo.com\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		printf("\nType\tField\tValue\r\n-----------------------\n");

		dns1.SetDNSServer(argv[1]);

		for (int i = 1; i <= 22; i++)
		{
			dns1.SetQueryType(i);
			ret_code = dns1.Query(argv[2]);
			if (ret_code)     // Got an error.  The user is done.
			{
				fprintf(stderr, "\nError: %d", ret_code);
				if (dns1.GetLastError())
				{
					fprintf(stderr, " \"%s\"\n", dns1.GetLastError());
					exit(1);
				}
			}
		}
	}
	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();
	exit(ret_code);
	return 0;
}



