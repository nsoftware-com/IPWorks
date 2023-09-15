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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworks.h"

#define LINE_LEN 80

class MyIMAP : public IMAP
{

public:

	MyIMAP()
	{
		lines = 0;
	}

	virtual int FireMailboxList(IMAPMailboxListEventParams *e)
	{
		printf("%s\n", e->Mailbox);
		lines++;
		if (lines == 22)
		{
			printf("Press enter to continue...");
			getchar();
			lines = 0;
		}
		return 0;
	}

	virtual int FireMessageInfo(IMAPMessageInfoEventParams *e)
	{

		// Truncate fields
		printf("%4s  ", e->MessageId);
		printf("%-25.23s", e->Subject);
		printf("%-18.16s", e->MessageDate);
		printf("%-30.30s\n", e->From);

		lines++;
		if (lines == 22)
		{
			printf("Press enter to continue...");
			getchar();
			lines = 0;
		}
		return 0;

	}
	virtual int FireTransfer(IMAPTransferEventParams *e)
	{
		printf("%s\n", e->Text);
		lines++;
		if (lines == 22)
		{
			printf("Press enter to continue...");
			getchar();
			lines = 0;
		}
		return 0;
	}
	virtual int FireSSLServerAuthentication(IMAPSSLServerAuthenticationEventParams *e)
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

};


int main(int argc, char * argv[])
{

	if (argc < 4) {

		fprintf(stderr, "usage: imap server username password [sslmode]\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  server    the name or address of the mail server (IMAP server)\n");
		fprintf(stderr, "  username  the user name used to authenticate to the MailServer\n");
		fprintf(stderr, "  password  the password used to authenticate to the MailServer\n");
		fprintf(stderr, "  sslmode   The SSL mode. Possible values are: none, auto, explicit, implicit.\n");
		fprintf(stderr, "\nExample (Plaintext):    imap 127.0.0.1 username password\n");
		fprintf(stderr, "Example (SSL Auto):     imap 127.0.0.1 username password auto\n");
		fprintf(stderr, "Example (SSL Explicit): imap 127.0.0.1 username password explicit\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		MyIMAP imap;

		if (argc > 4)
		{
			if (!strcmp(argv[4], "auto"))
			{
				imap.SetSSLStartMode(SSL_AUTOMATIC);
			}
			else if (!strcmp(argv[4], "explicit"))
			{
				imap.SetSSLStartMode(SSL_EXPLICIT);
			}
			else if (!strcmp(argv[4], "implicit"))
			{
				imap.SetSSLStartMode(SSL_IMPLICIT);
			}
		}

		char command[LINE_LEN];     // user's command
		char buffer[LINE_LEN];      // text buffer
		char *argument;             // arguments following command

		int msgnum = 0;             // current message number for next command
		char msg_range[LINE_LEN];
		char msg_limit[10];
		int ret_code;
	
		imap.SetMailServer(argv[1]);		
		imap.SetUser(argv[2]);		
		imap.SetPassword(argv[3]);

		ret_code = imap.Connect();

		if (ret_code) goto done;

		printf("Type \"?\" for a list of commands.\n\n");
		while (1)
		{

			imap.lines = 0;
			printf("> ");
			fgets(command, LINE_LEN, stdin);
			buffer[strlen(command) - 1] = '\0';
			argument = strtok(command, " \n\t");

			if (!strcmp(command, "?"))
			{

				printf("IMAP Commands\n"
					"l                               list mailboxes\n"
					"s <mailbox>                     select mailbox\n"
					"t <message number>              type messages\n"
					"n                               goto and type next message\n"
					"f                               give head lines of messages\n"
					"q                               quit, saving unresolved messages in mbox\n"
					"h                               print out active message headers\n");

			}

			else if (!strcmp(command, "s"))
			{

				argument = strtok(NULL, " \t\n");
				imap.SetMailbox(argument);
				ret_code = imap.SelectMailbox();

			}

			else if (!strcmp(command, "f"))
			{

				if (imap.GetMessageCount())
				{
					strcpy(msg_range, "1:");
					sprintf(msg_limit, "%i", imap.GetMessageCount());
					strcat(msg_range, msg_limit);
					imap.SetMessageSet(msg_range);
					ret_code = imap.FetchMessageInfo();
				}
				else
				{
					printf("No messages in this mailbox.\n");
				}

			}

			else if (!strcmp(command, "h"))
			{

				if (imap.GetMessageCount())
				{
					strcpy(msg_range, "0:");
					sprintf(msg_limit, "%i", imap.GetMessageCount());
					strcat(msg_range, msg_limit);
					imap.SetMessageSet(msg_range);
					ret_code = imap.FetchMessageInfo();
				}
				else
				{
					printf("No messages in this mailbox.\n");
				}

			}

			else if (!strcmp(command, "l"))
			{

				if (!(argument = strtok(NULL, " \t\n")))
				{
					strcpy(buffer, "*");
					ret_code = imap.SetMailbox(buffer);
				}
				else
				{
					imap.SetMailbox(argument);
				}
				imap.ListMailboxes();

			}

			else if (!strcmp(command, "n"))
			{

				msgnum++;
				sprintf(buffer, "%i", msgnum);
				imap.SetMessageSet(buffer);
				ret_code = imap.FetchMessageText();

			}

			else if (!strcmp(command, "q"))
			{

				ret_code = imap.Disconnect();
				exit(0);

			}

			else if (!strcmp(command, "t"))
			{
				argument = strtok(NULL, " \t\n");
				imap.SetMessageSet(argument);
				ret_code = imap.FetchMessageText();
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
					imap.SetMessageSet(command);
					msgnum = atoi(command);
					ret_code = imap.FetchMessageText();
				}
				else
				{
					printf("Bad command / Not implemented in demo.\n");
				}

			} // end of command checking

			if (ret_code)     // Got an error.  The user is done.
			{
				printf("\nError: %d", ret_code);
				if (imap.GetLastError())
				{
					printf(" \"%s\"\n", imap.GetLastError());
				}
			}
			ret_code = 0;   // flush out error
		}  // end of main while loop

	done:
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (imap.GetLastError())
			{
				printf(" \"%s\"\n", imap.GetLastError());
			}
		}
		printf("Exiting... (press enter)\n");
		getchar();
		exit(ret_code);
		return 0;
	}
}






