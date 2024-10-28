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

#define LINE_LEN 80
#define MESSAGE_LEN 1024

class MyHTMLMailer : public HTMLMailer
{
public:

	virtual int FireSSLServerAuthentication(HTMLMailerSSLServerAuthenticationEventParams *e)
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

int main(int argc, char* argv[])
{
	if (argc < 4) {

		fprintf(stderr, "usage: htmlmailer [options] server from to\n\n");
		fprintf(stderr, "Options: \n");
		fprintf(stderr, "  -s      the subject of the mail message\n");		
		fprintf(stderr, "  -a      the path of file to attach to the message\n");
		fprintf(stderr, "  server  the name or address of a mail server (mail relay)\n");
		fprintf(stderr, "  from    the email address of the sender\n");
		fprintf(stderr, "  to      a comma separated list of addresses for destinations\n");
		fprintf(stderr, "\nExample: htmlmailer -s test -a FileToAttach mail.local sender@mail.com recipient@mail.local\n\n");
		printf("Press enter to continue.");
		getchar();
		
	}
	else{
		MyHTMLMailer htmlmailer;
		char command[LINE_LEN];     // user's command
		for (int i = 0; i < argc; i++) {

			if (strncmp(argv[i], "-", strlen("-")) == 0) { //check if argument starts with "-"
				if (strcmp(argv[i], "-s") == 0) {
					// args[i+1] corresponds to the value of argument [i]					
					htmlmailer.SetSubject(argv[i + 1]);
				}
				
				if (strcmp(argv[i], "-a") == 0) {
					// args[i+1] corresponds to the value of argument [i]					
					htmlmailer.AddAttachment(argv[i + 1]);
				}
			}
		}
		
		htmlmailer.SetMailServer(argv[argc-3]);
		
		htmlmailer.SetFrom(argv[argc-2]);

		htmlmailer.SetSendTo(argv[argc-1]);
		
		printf("Enter the message. To end the message, enter \".\" on a single line by itself.\n");
		printf("Message:\n");

		char message[MESSAGE_LEN];
		message[0] = '\0';
		while (fgets(command, LINE_LEN, stdin))
		{
			command[strlen(command) - 1] = '\0';
			strcat(message, command);
			if (strcmp(command, ".") == 0)
				break;
		}

		htmlmailer.SetMessageHTML(message);
		
		printf("Sending message ...\n");
		int ret_code = htmlmailer.Send();

		if (ret_code)     // Got an error.  The user is done.
		{
			printf("Error: %d", ret_code);
			if (htmlmailer.GetLastError())
			{
				printf(" \"%s\"\n", htmlmailer.GetLastError());
			}
		}
		else
		{
			printf("Message sent successfully\n");
		}

		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		exit(ret_code);
		return 0;
	}
}











