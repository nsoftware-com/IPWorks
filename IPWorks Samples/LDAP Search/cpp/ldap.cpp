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

#define LINE_LEN 80

class MyLDAP : public LDAP
{
public:

	virtual int FireError(LDAPErrorEventParams *e)
	{
		printf( "%d  %s\n", e->ErrorCode, e->Description );
		return 0;
	}


	virtual int FireResult(LDAPResultEventParams *e)
	{
		printf( "%d  %s\n", e->ResultCode, e->ResultDescription );
		return 0;
	}

	virtual int FireSearchComplete(LDAPSearchCompleteEventParams *e)
	{
		printf( "%d  %s\n", e->ResultCode, e->ResultDescription );
		return 0;
	}

	virtual int FireSearchResult(LDAPSearchResultEventParams *e)
	{
		printf( "%s\n", e->DN );
		return 0;
	}

	virtual int FireSSLServerAuthentication(LDAPSSLServerAuthenticationEventParams *e)
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

int main(int argc, char * argv[])
{

	MyLDAP ldap;
	int ret_code = 0;
	char buffer[LINE_LEN];
	char filter[LINE_LEN];

	printf("Server [ldap-master.itd.umich.edu]: ");
	fgets(buffer,LINE_LEN,stdin);
	buffer[strlen(buffer)-1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "ldap-master.itd.umich.edu");
	if ( ret_code = ldap.SetServerName( buffer ) ) goto done;

	printf("Bind (auth) DN: ");
	fgets(buffer,LINE_LEN,stdin);
	buffer[strlen(buffer)-1] = '\0';
	ldap.SetDN(buffer);
	
	printf("Bind (auth) Password: ");
	fgets(buffer,LINE_LEN,stdin);
	buffer[strlen(buffer)-1] = '\0';
	ldap.SetPassword(buffer);
	ldap.Bind();
	if (ldap.GetResultCode() != 0) goto done;

	printf("Search DN [ou=Security,dc=umich,dc=edu]: ");
	fgets(buffer,LINE_LEN,stdin);
	buffer[strlen(buffer)-1] = '\0';
	if (strlen(buffer) == 0) strcat(buffer, "ou=Security,dc=umich,dc=edu");
	ldap.SetDN(buffer);

	do
	{
		printf("Search for [cn=*]:");
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';
		if (strlen(buffer)==0) strcat(buffer, "cn=*");
		strcpy(filter, "cn=");
		strcat(filter, buffer);

		//to only return entries with certain attributes, specify those attrs first:
		//if ( ret_code = ldap.SetAttrCount(1) ) goto done;
		//if ( ret_code = ldap.SetAttrType(0, "email") ) goto done;  // email for addresses etc...

		printf("Sending search request...\n");
		ldap.SetTimeout(10);  //puts component in synchronous mode
		ldap.Search(buffer);
		if (ldap.GetResultCode() != 0) goto done;

		printf( "Another search? (y/n) " );
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';

	}
	while( buffer[0] != 'n' );

done:
	if (ret_code)     // Got an error.  The user is done.
	{
		printf( "\nError: %d", ret_code );
		if (ldap.GetLastError())
		{
			printf( " \"%s\"\n", ldap.GetLastError() );
		}
		else if ( ret_code == 10060) printf( " \"[10060] Request timed out\"\n" );
	}
	printf("\nPress enter to continue...");
	getchar();
	exit(ret_code);
	return 0;
}



