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

class MyFTP : public FTP
{
public:

	int verbose;

	MyFTP()
	{
		verbose = 1;
	}

	virtual int FireSSLServerAuthentication(FTPSSLServerAuthenticationEventParams *e)
	{
		if (e->Accept) return 0;

		printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n",
			e->CertIssuer, e->CertSubject);
		printf("The following problems have been determined for this certificate: %s\n", e->Status);
		printf("Would you like to continue anyways? [y/n] ");
		if (getchar() == 'y') 
			e->Accept = true;
		return 0;
	}

	virtual int FireDirList(FTPDirListEventParams *e)
	{
		printf("%s\n", e->DirEntry);
		return 0;
	}

	void toggle_verbose()
	{
		verbose = 1 - verbose;
		printf("Verbose mode ");
		if (verbose)
		{
			printf("ON.\n");
		}
		else
		{
			printf("OFF.\n");
		}
	}

	virtual int FirePITrail(FTPPITrailEventParams *e)
	{
		if (verbose)
		{
			printf("%s\n", e->Message);
		}
		return 0;
	}

};
int logon(MyFTP &ftp, char server[LINE_LEN], char user[LINE_LEN], char password[LINE_LEN])
{
	ftp.SetRemoteHost(server);
	ftp.SetUser(user);
	ftp.SetPassword(password);
	return ftp.Logon();
}


int main(int argc, char* argv[])
{

	if (argc < 4) {

		fprintf(stderr, "usage: ftp server username password [sslmode]\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "  server    the domain name or IP address of the FTP server\n");
		fprintf(stderr, "  username  the user identifier to use for login\n");
		fprintf(stderr, "  password  the password to log in.\n");
		fprintf(stderr, "  sslmode   The SSL mode. Possible values are: none, auto, explicit, implicit.\n");
		fprintf(stderr, "\nExample (Plaintext):    ftp 127.0.0.1 username password\n");
		fprintf(stderr, "Example (SSL Auto):     ftp 127.0.0.1 username password auto\n");
		fprintf(stderr, "Example (SSL Explicit): ftp 127.0.0.1 username password explicit\n\n");
		printf("Press enter to continue.");
		getchar();

	}
	else{
		MyFTP ftp;                  // FTP object
		char command[LINE_LEN];     // user's command
		char *argument;             // arguments to the user's command
		char pathname[LINE_LEN];    // for use with the ls command

		if (argc > 4)
		{
			if (!strcmp(argv[4], "auto"))
			{
				ftp.SetSSLStartMode(SSL_AUTOMATIC);
			}
			else if (!strcmp(argv[4], "explicit"))
			{
				ftp.SetSSLStartMode(SSL_EXPLICIT);
			}
			else if (!strcmp(argv[4], "implicit"))
			{
				ftp.SetSSLStartMode(SSL_IMPLICIT);
			}
		}

		int ret_code = logon(ftp, argv[1], argv[2], argv[3]);
		if (ret_code) goto done;

		while (1)
		{

			ftp.SetRemoteFile("");
			printf("ftp> ");
			fgets(command, LINE_LEN, stdin);
			command[strlen(command) - 1] = '\0';
			argument = strtok(command, " \t\n");

			if (!strcmp(command, "?"))
			{
				printf("?        bye     help     put     rmdir\n"
					"append   cd      ls       pwd     verbose\n"
					"ascii    close   mkdir    quit\n"
					"binary   get     open     rm\n");
			}

			else if (!strcmp(command, "append"))
			{

				argument = strtok(NULL, " \t\n");
				ftp.SetLocalFile(argument);
				argument = strtok(NULL, " \t\n");
				ftp.SetRemoteFile(argument);
				ret_code = ftp.Append();

			}

			else if (!strcmp(command, "ascii"))
			{

				ftp.ChangeTransferMode(TM_ASCII);

			}

			else if (!strcmp(command, "binary"))
			{

				ftp.ChangeTransferMode(TM_BINARY);

			}

			else if (!strcmp(command, "bye"))
			{

				ret_code = ftp.Logoff();
				exit(0);

			}

			else if (!strcmp(command, "cd"))
			{

				if (argument = strtok(NULL, "\t\n"))
				{
					ftp.ChangeRemotePath(argument);
				}

			}

			else if (!strcmp(command, "close"))
			{

				ret_code = ftp.Logoff();

			}

			else if (!strcmp(command, "get"))
			{

				argument = strtok(NULL, " \t\n");
				ftp.SetRemoteFile(argument);
				ftp.SetLocalFile(argument);
				ret_code = ftp.Download();
				printf("File downloaded successfully.\n");

			}

			else if (!strcmp(command, "help"))
			{

				printf("?        bye     help     put     rmdir\n"
					"append   cd      ls       pwd     verbose\n"
					"ascii    close   mkdir    quit\n"
					"binary   get     open     rm  \n");

			}

			else if (!strcmp(command, "ls"))
			{

				if (argument = strtok(NULL, "\t\n"))
				{
					strcpy(pathname, ftp.QueryRemotePath());
					int ret_code = ftp.ChangeRemotePath(argument);
					if (!ret_code)
					{
						ret_code = ftp.ListDirectoryLong();
					}
					if (!ret_code)
					{
						ret_code = ftp.ChangeRemotePath(pathname);
					}
				}
				else
				{
					ret_code = ftp.ListDirectoryLong();
				}

			}

			else if (!strcmp(command, "mkdir"))
			{

				if (argument = strtok(NULL, "\t\n"))
				{
					ret_code = ftp.MakeDirectory(argument);
				}

			}

			else if (!strcmp(command, "mv"))
			{

				argument = strtok(NULL, " \t\n");
				ftp.SetRemoteFile(argument);
				argument = strtok(NULL, " \t\n");
				ret_code = ftp.RenameFile(argument);

			}

			else if (!strcmp(command, "open"))
			{

				ftp.Logoff();
				argument = strtok(NULL, " \t\n");
				ftp.SetRemoteHost(argument);
				ret_code = logon(ftp, argv[1], argv[2], argv[3]);
			}

			else if (!strcmp(command, "passive"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					if ((!strcmp(argument, "on")) && !ftp.GetPassive())
					{
						ret_code = ftp.SetPassive(1);
						printf("Passive mode ON.\n");
					}
					else if ((!strcmp(argument, "off")) && ftp.GetPassive())
					{
						ret_code = ftp.SetPassive(0);
						printf("Passive mode OFF.\n");
					}
				}

			}

			else if (!strcmp(command, "put"))
			{

				argument = strtok(NULL, " \t\n");
				ftp.SetRemoteFile(argument);
				ftp.SetLocalFile(argument);
				ret_code = ftp.Upload();
				printf("File uploaded successfully.\n");

			}

			else if (!strcmp(command, "pwd"))
			{

				printf("%s\n", ftp.QueryRemotePath());

			}

			else if (!strcmp(command, "quit"))
			{

				ret_code = ftp.Logoff();
				exit(0);

			}

			else if (!strcmp(command, "rm"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					ret_code = ftp.DeleteFile(argument);
				}

			}

			else if (!strcmp(command, "rmdir"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					ret_code = ftp.RemoveDirectory(argument);
				}

			}

			else if (!strcmp(command, "verbose"))
			{

				if (argument = strtok(NULL, " \t\n"))
				{
					if ((!strcmp(argument, "on")) && !ftp.verbose)
					{
						ftp.toggle_verbose();
					}
					else if ((!strcmp(argument, "off")) && ftp.verbose)
					{
						ftp.toggle_verbose();
					}
				}
				else
				{
					ftp.toggle_verbose();
				}

			}
			else if (!strcmp(command, ""))
			{
				// Do nothing
			}
			else
			{

				printf("Bad command / Not implemented in demo.\n");

			} // end of command checking
			if (ret_code)     // Got an error.  The user is done.
			{
				printf("\nError: %d", ret_code);
				if (ftp.GetLastError())
				{
					printf(" \"%s\"\n", ftp.GetLastError());
				}
			}
			ret_code = 0;   // flush out error
		}  // end of main while loop

	done:
		if (ret_code)     // Got an error.  The user is done.
		{
			printf("\nError: %d", ret_code);
			if (ftp.GetLastError())
			{
				printf(" \"%s\"\n", ftp.GetLastError());
			}
		}
		fprintf(stderr, "\npress <return> to continue...\n");
		exit(ret_code);
		return 0;
	}
}





