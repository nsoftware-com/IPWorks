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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../../include/ipworks.h"

#include <ctype.h>

#define LINE_LEN 80


class MyNNTP: public NNTP
{
public:
	MyNNTP()
	{
		lines = 0;
	}
	virtual int FireGroupList(NNTPGroupListEventParams *e)
	{
		printf( "%s\n", e->Group );
		lines++;
		if ( lines == 22 )
		{
			printf("Press ENTER to continue...");
			getchar();
			lines = 0;
		}
		return 0;
	}

	virtual int FireGroupOverview(NNTPGroupOverviewEventParams *e)
	{
		printf( "%d  %s\n", e->ArticleNumber, e->Subject );
		lines++;
		if ( lines == 23 )
		{
			getchar();
			lines = 0;
		}
		return 0;
	}

	virtual int FireHeader(NNTPHeaderEventParams *e)
	{
		printf( "%s: %s\n", e->Field, e->Value );
		lines++;
		if ( lines == 23 )
		{
			getchar();
			lines = 0;
		}
		return 0;
	}

	virtual int FireTransfer(NNTPTransferEventParams *e)
	{
		printf( "%s\n", e->Text );
		lines++;
		if ( lines == 23 )
		{
			getchar();
			lines = 0;
		}
		return 0;
	}


	virtual int FireError(NNTPErrorEventParams *e)
	{
		printf("Error #%i: %s\n", e->ErrorCode, e->Description);
		exit(0);
	}

	virtual int FireSSLServerAuthentication(NNTPSSLServerAuthenticationEventParams *e)
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
	int lines;
	char bkey;

};

int main(int argc, char * argv[])
{

	if (argc != 2) {

		fprintf(stderr, "usage: nntpread server\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  server  the name or address of the NNTP server\n");
		fprintf(stderr, "\nExample: nntpread nntp.aioe.org\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		MyNNTP nntp;                // NNTP object
		int ret_code = 0;           // Return code:
		// = 0 when there is no error
		// = error code when there is an error

		char command[LINE_LEN];     // user's command
		char buffer[LINE_LEN];      // text buffer
		char *argument;             // arguments following command

		int msgnum = 0;             // current message number for next command

		if (ret_code = nntp.SetNewsServer(argv[1])) goto done;

		/*
		printf( "login\t: " );
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';
		nntp.SetUser( buffer );

		printf( "password\t: " );
		fgets(buffer,LINE_LEN,stdin);
		buffer[strlen(buffer)-1] = '\0';
		nntp.SetPassword( buffer );
		*/
		if (ret_code = nntp.Connect()) goto done;

		printf("Type \"?\" for a list of commands.\n\n");
		while (1)
		{

			nntp.lines = 0;
			printf("> ");
			fgets(command, LINE_LEN, stdin);
			command[strlen(command) - 1] = '\0';
			argument = strtok(command, " \t\n");

			if (!strcmp(command, "?"))
			{

				printf("Readnews Commands\n"
					"l                               list all available newsgroups\n"
					"c <newsgroup>                   select newsgroup\n"
					"t <message number>              type messages\n"
					"n                               goto and type next message\n"
					"f                               give head lines of messages\n"
					"q                               quit, saving unresolved messages in mbox\n"
					"h                               print out active message headers\n");

			}

			else if (!strcmp(command, "c"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					ret_code = nntp.SetCurrentGroup(argument);
				}
				else
				{
					printf("No newsgroup specified.\n");
				}

			}

			else if (!strcmp(command, "f"))
			{

				nntp.SetOverviewRange("0-");
				ret_code = nntp.GroupOverview();

			}

			else if (!strcmp(command, "h"))
			{

				nntp.SetOverviewRange("0-");
				ret_code = nntp.GroupOverview();

			}

			else if (!strcmp(command, "l"))
			{

				ret_code = nntp.ListGroups();

			}

			else if (!strcmp(command, "n"))
			{

				msgnum++;
				sprintf(buffer, "%i", msgnum);
				nntp.SetCurrentArticle(buffer);
				ret_code = nntp.FetchArticle();

			}

			else if (!strcmp(command, "q"))
			{

				ret_code = nntp.Disconnect();
				exit(0);

			}

			else if (!strcmp(command, "t"))
			{

				argument = strtok(NULL, " \t\n");
				msgnum = atoi(argument);
				nntp.SetCurrentArticle(argument);
				ret_code = nntp.FetchArticle();

			}

			else if (!strcmp(command, ""))
			{
				// do nothing
			}

			else
			{

				if (isdigit((int)command[0]))
				{
					// allow user to enter only the number
					//  of the message they want to view
					nntp.SetCurrentArticle(command);
					msgnum = atoi(command);
					ret_code = nntp.FetchArticle();
				}
				else
				{
					printf("Bad command / Not implemented in demo.\n");
				}

			} // end of command checking

			if (ret_code)     // Got an error.  The user is done.
			{
				printf("\nError: %d", ret_code);
				if (nntp.GetLastError())
				{
					printf(" \"%s\"\n", nntp.GetLastError());
				}
			}

			ret_code = 0;  // flush out the error

		}  // end of main while loop

	done:
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (nntp.GetLastError())
			{
				printf(" \"%s\"\n", nntp.GetLastError());
			}
		}
		printf("Exiting... (press enter)\n");
		getchar();
		exit(ret_code);
		return 0;
	}
}


