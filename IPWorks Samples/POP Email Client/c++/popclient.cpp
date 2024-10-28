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
#include <stdlib.h>
#include "../../include/ipworks.h"

#include <ctype.h>

#define LINE_LEN 80

enum { HEADERS, FULL, REPLY };

class MyPOP: public POP
{
public:
	MyPOP()
	{
		state = HEADERS;
		lines = 0;
		strcpy(reply,"");
	}

	virtual int FireHeader(POPHeaderEventParams *e)
	{

		if (state == FULL)
		{
			printf("%s: %s\n", e->Field, e->Value);
		}

		else if ( !strcmp(e->Field, "From") )
		{
			sprintf(msg_from, e->Value);
		}

		else if ( !strcmp(e->Field, "Date") )
		{
			sprintf(msg_date, e->Value);
		}

		else if ( !strcmp(e->Field, "Subject") )
		{
			sprintf(msg_subject, e->Value);
			if ( state == REPLY )      //  change the subject to Re:
			{
				char temp[LINE_LEN];
				sprintf(temp, msg_subject);
				strcpy(msg_subject, "Re: ");
				strcat(msg_subject, temp);
			}
		}
		return 0;
	}

	virtual int FireTransfer(POPTransferEventParams *e)
	{
		if (state == FULL)
		{
			printf("%s\n", e->Text);
			lines++;
			if ( lines == 22 )
			{
				printf("Press ENTER to continue...");
				getchar();
				lines = 0;
			}
		}
		else if (state == REPLY)     // if replying, add original message text to reply text
		{
			strcat(reply,e->Text);
			strcat(reply,"\n");
		}
		return 0;
	}

	virtual int FireSSLServerAuthentication(POPSSLServerAuthenticationEventParams *e)
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

	int state;
	char reply[10000];
	char msg_from[LINE_LEN];
	char msg_date[LINE_LEN];
	char msg_subject[LINE_LEN];
	int lines;

	void clearHeadlineData()
	{
		msg_from[0] = '\0';
		msg_date[0] = '\0';
		msg_subject[0] = '\0';
	}

	void printHeadline()
	{
		printf("%-4d%-21.19s%-21.19s%-21.19s\n", GetMessageNumber(), msg_from, msg_date, msg_subject);
	}
};

class MyFileMailer: public FileMailer
{

public:
	MyFileMailer() { }

	virtual int FireTransfer(FileMailerTransferEventParams *e)
	{
		//printf("Transfer: %ld", e->BytesTransferred);
		return 0;
	}

};

int main(int argc, char * argv[])
{

	if (argc != 6) {
		fprintf(stderr, "* This demo shows how to use POP component to view messages in the mailbox and reply to an email with the Filemailer component. *\n");
		fprintf(stderr, "usage: popclient popserver user password smtpserver from\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  popserver   the name or address of a mail server (internet post office server)\n");
		fprintf(stderr, "  user        the user identifier for the mailbox\n");
		fprintf(stderr, "  password    the password for the mailbox user\n");
		fprintf(stderr, "  smtpserver  the name or address of a mail server (mail relay)\n");
		fprintf(stderr, "  from        the email address of the sender\n");
		fprintf(stderr, "\nExample: popclient mail.local username password mail.local user@service.com\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		MyPOP pop;                  // POP object
		MyFileMailer smtp;                  // SMTP object
		int ret_code = 0;           // Return code:
		// = 0 when there is no error
		// = error code when there is an error

		char command[LINE_LEN];     // user's command
		char buffer[LINE_LEN];      // text buffer
		char *argument;            // arguments to the user's command

		int k = 0;         // counter for use in for() loops
		char msg[10000];            // storage for mail message
		int msgnum = 0;             // current message number for next command
		
		if (ret_code = pop.SetMailServer(argv[1])) goto done;		
		pop.SetUser(argv[2]);		
		pop.SetPassword(argv[3]);
		if (ret_code = pop.Connect()) goto done;		
		smtp.SetMailServer(argv[4]);		
		smtp.SetFrom(argv[5]);

		printf("Type \"?\" for a list of commands.\n\n");

		while (1)
		{

			strcpy(msg, "");
			printf("> ");
			fgets(command, LINE_LEN, stdin);
			command[strlen(command) - 1] = '\0';
			argument = strtok(command, " \t\n");

			if (!strcmp(command, "?"))
			{

				printf("Mail Commands\n"
					"t <message number>              type messages\n"
					"n                               goto and type next message\n"
					"f                               give head lines of messages\n"
					"d <message number>              delete messages\n"
					"m <user>                        mail to specific users\n"
					"q                               quit, saving unresolved messages in mbox\n"
					"h                               print out active message headers\n"
					"r <message number>              reply to message\n");

			}

			else if (!strcmp(command, "d"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					msgnum = atoi(argument);
					pop.SetMessageNumber(msgnum);
					ret_code = pop.Delete();
				}

				else
				{
					printf("No message specified.\n");
				}

			}

			else if (!strcmp(command, "f"))
			{

				pop.state = HEADERS;
				pop.SetMaxLines(1);
				for (k = 1; k <= pop.GetMessageCount(); k++)     // get header info for every message
				{
					pop.SetMessageNumber(k);
					pop.clearHeadlineData();
					ret_code = pop.Retrieve();
					pop.printHeadline();
				}
				pop.SetMaxLines(0);

			}

			else if (!strcmp(command, "h"))     // get header info for every message
			{

				pop.state = HEADERS;
				pop.SetMaxLines(1);
				for (k = 1; k <= pop.GetMessageCount(); k++)
				{
					pop.SetMessageNumber(k);
					pop.clearHeadlineData();
					ret_code = pop.Retrieve();
					pop.printHeadline();
				}
				pop.SetMaxLines(0);

			}

			else if (!strcmp(command, "m"))
			{


				if (!(argument = strtok(NULL, " \t\n")))    // if no recipient specified
				{
					printf("To: ");
					fgets(buffer, LINE_LEN, stdin);
					buffer[strlen(buffer) - 1] = '\0';
					smtp.SetSendTo(buffer);
				}
				else
				{
					smtp.SetSendTo(argument);
				}

				printf("Subject: ");
				fgets(buffer, LINE_LEN, stdin);
				buffer[strlen(buffer) - 1] = '\0';
				smtp.SetSubject(buffer);

				printf("Type \".\" on a line by itself to end message.\n");
				strcpy(msg, "\n");                          // initialize the message string
				fgets(buffer, LINE_LEN, stdin);
				buffer[strlen(buffer) - 1] = '\0';                             // get first line
				while (strcmp(buffer, "."))                // until period on a line by itself
				{
					strcat(msg, buffer);                    // add a line
					strcat(msg, "\n");                       // with an endline
					fgets(buffer, LINE_LEN, stdin);
					buffer[strlen(buffer) - 1] = '\0';                         // get next line
				}

				if (ret_code = smtp.SetMessageText(msg)) goto done;

				printf("Cc: ");
				fgets(buffer, LINE_LEN, stdin);
				buffer[strlen(buffer) - 1] = '\0';
				smtp.SetCc(buffer);

				ret_code = smtp.Send();

			}

			else if (!strcmp(command, "n"))
			{

				msgnum++;
				pop.state = FULL;
				pop.SetMessageNumber(msgnum);
				ret_code = pop.Retrieve();

			}

			else if (!strcmp(command, "q"))
			{

				printf("Save changes to inbox? (y/n): ");
				if (getchar() == 'n')
				{
					ret_code = pop.Reset();
				}
				ret_code = pop.Disconnect();
				exit(0);

			}

			else if (!strcmp(command, "r"))
			{

				if (argument = strtok(NULL, " \t\n"))    // get number of message to reply to
				{
					msgnum = atoi(argument);
					printf("Type your message here. Type \".\" on a line by itself to continue.\n");
				}
				if (!msgnum)
				{
					printf("No message specified.\n");
				}
				else
				{
					pop.state = REPLY;
					pop.SetMessageNumber(msgnum);
					ret_code = pop.Retrieve();
					smtp.SetSendTo(pop.msg_from);
					smtp.SetSubject(pop.msg_subject);

					fgets(buffer, LINE_LEN, stdin);
					buffer[strlen(buffer) - 1] = '\0';
					while (strcmp(buffer, "."))
					{
						strcpy(msg, buffer);
						strcat(msg, "\n");
						fgets(buffer, LINE_LEN, stdin);
						buffer[strlen(buffer) - 1] = '\0';
					}

					strcat(msg, pop.msg_from);  // insert name of previous message author
					strcat(msg, " wrote: \n");
					strcat(msg, pop.reply);     // insert previous message
					strcpy(pop.reply, "");      // reset reply field for future use

					smtp.SetMessageText(msg);
					printf("Cc: ");
					fgets(buffer, LINE_LEN, stdin);
					buffer[strlen(buffer) - 1] = '\0';
					smtp.SetCc(buffer);
					ret_code = smtp.Send();
				}
			}

			else if (!strcmp(command, "t"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					msgnum = atoi(argument);
					pop.state = FULL;
					pop.SetMessageNumber(msgnum);
					ret_code = pop.Retrieve();
				}

				else
				{
					printf("No message specified.\n");
				}

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
					msgnum = atoi(command);
					pop.state = FULL;
					pop.SetMessageNumber(msgnum);
					ret_code = pop.Retrieve();
				}
				else
				{
					printf("Bad command / Not implemented in demo.\n");
				}

			} // end of command checking

			if (ret_code)     // Got an error.  The user is done.
			{
				printf("\nError: %d", ret_code);
				if (pop.GetLastError())
				{
					printf(" \"%s\"\n", pop.GetLastError());
				}
				if (smtp.GetLastError())
				{
					printf(" \"%s\"\n", smtp.GetLastError());
				}
			}

			ret_code = 0;  // flush out the error

		}  // end of main while loop

	done:
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (pop.GetLastError())
			{
				printf(" \"%s\"\n", pop.GetLastError());
			}
			if (smtp.GetLastError())
			{
				printf(" \"%s\"\n", smtp.GetLastError());
			}
			printf("Exiting... (press enter)\n");
			getchar();
			exit(1);
		}
		return 0;
	}
	
}








