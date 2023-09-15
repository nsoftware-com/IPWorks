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

#define LINE_LEN 200

class MyOData : public OData
{
public:


	virtual int FireError(ODataErrorEventParams *e)
	{
		printf("ERROR: %s\n", e->Description);
		return 0;
	}

	virtual int FireSSLServerAuthentication(ODataSSLServerAuthenticationEventParams *e)
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

int main(int args, char **argv)
{
	printf("*****************************************************************\n");
	printf("* This demo demonstrates how to use the OData component         *\n");
	printf("* to query OData's TripPin service.                             *\n");
	printf("*****************************************************************\n");
	
	MyOData odata1; //OData object
	char input[LINE_LEN]; //user input

	printf("\nEnter Last Name to query (leave blank to get all entries): ");
	fgets(input, LINE_LEN, stdin);
	input[strlen(input) - 1] = '\0';

	printf("Searching for '%s'...\n\n", input);

	

	odata1.SetServiceRootURI("http://services.odata.org/TripPinRESTierService/");
	odata1.SetResourcePath("People");

	char filter[LINE_LEN];
	filter[0] = '\0';

	strcat(filter, "contains(LastName, '");
	strcat(filter, input);
	strcat(filter, "')");

	odata1.SetQueryFilter(filter);
	odata1.SetQueryTop("10"); // Only grab top 10 entries so as not to flood the console.
	odata1.QueryService();

	for (int i = 0; i <= odata1.GetEntryCount() - 1; i++)
	{
		char firstName[LINE_LEN];
		firstName[0] = '\0';
		char lastName[LINE_LEN];
		lastName[0] = '\0';
		char email[LINE_LEN];
		email[0] = '\0';
		char gender[LINE_LEN];
		gender[0] = '\0';
		char city[LINE_LEN];
		city[0] = '\0';
		char country[LINE_LEN];
		country[0] = '\0';

		odata1.SetEntryIndex(i);
		for (int j = 0; j <= odata1.GetEntryPropertiesCount()-1; j++)
		{
			if (strcmp(odata1.GetEntryPropertiesName(j), "FirstName")==0)
			{				
				strcat(firstName, odata1.GetEntryPropertiesValue(j));
				
			}
			if (strcmp(odata1.GetEntryPropertiesName(j), "LastName")==0)
			{
				strcat(lastName, odata1.GetEntryPropertiesValue(j));
				
			}
			if (strcmp(odata1.GetEntryPropertiesName(j), "Emails/[1]")==0)
			{
				strcat(email, odata1.GetEntryPropertiesValue(j));
				
			}
			if (strcmp(odata1.GetEntryPropertiesName(j), "Gender")==0)
			{
				strcat(gender, odata1.GetEntryPropertiesValue(j));
				
			}
			if (strcmp(odata1.GetEntryPropertiesName(j), "AddressInfo/[1]/City/Name")==0)
			{
				strcat(city, odata1.GetEntryPropertiesValue(j));
				
			}
			if (strcmp(odata1.GetEntryPropertiesName(j), "AddressInfo/[1]/City/CountryRegion")==0)
			{
				strcat(country, odata1.GetEntryPropertiesValue(j));				
			}
		}

		if (strlen(firstName) != 0) printf(strcat(firstName, "\n"));		 
		else printf("First Name not specified\n");		
		
		printf(strcat(lastName, "\n"));
		
		if (strlen(email) != 0) printf(strcat(email, "\n"));		
		else printf("Email not specified\n");
		
		if (strlen(gender) != 0) printf(strcat(gender, "\n"));
		else printf("Gender not specified\n");
		
		if (strlen(city) != 0) printf(strcat(city, "\n"));		
		else printf("City not specified\n");		
		
		if (strlen(country) != 0) printf(strcat(country, "\n\n"));		
		else printf("Country not specified\n\n");
		
	}

	odata1.SetQuerySkipToken("");
	odata1.SetQueryFilter("");
	odata1.SetQuerySelect("");
	odata1.SetQueryTop("");
	
	printf("Press enter to continue.");
	getchar();
	return 0;
};





