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
#include <string>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <vector>
#include "../../include/ipworks.h"

using namespace std;

#define LINE_LEN 256

class MyWebDAV : public WebDAV
{
public:
	bool exportingEvent;
	int eventNumber;
	vector<string> resourceURIs;

	MyWebDAV()
	{
		eventNumber = 1;
		exportingEvent = false;
	}

	virtual int FireSSLServerAuthentication(WebDAVSSLServerAuthenticationEventParams* e)
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
	virtual int FireConnected(WebDAVConnectedEventParams* e)
	{
		printf("Server connected\n");
		return 0;
	}
	virtual int FireConnectionStatus(WebDAVConnectionStatusEventParams* e)
	{
		printf("Status code %i: %s\n", e->StatusCode, e->Description);
		return 0;
	}
	virtual int FireDisconnected(WebDAVDisconnectedEventParams* e)
	{
		printf("Server Disconnected\n");
		return 0;
	}
	virtual int FireTransfer(WebDAVTransferEventParams* e)
	{
		printf("Resource being received from server (in full text): \n========================================= \n%s\n", e->Text);
		return 0;
	}

};

void prompt(char* writeStr, const char* message) {
	printf("%s", message);
	scanf("%s", writeStr, LINE_LEN);
}


int main(int argc, char* argv[])
{
	MyWebDAV webdav;
	char command[LINE_LEN];               // user's command
	char argument[LINE_LEN];           // arguments to the user's command
	char buffer[LINE_LEN];
	int ret_code;

	if (argc < 2) {

		fprintf(stderr, "usage: webdav username password\n");
		fprintf(stderr, "username  the username to login\n");
		fprintf(stderr, "password  the password to login\n");
		fprintf(stderr, "\r\nExample: webdav username password\n");
		exit(1);
	}
	printf("******************************************************************************\n");
	printf("* This demo shows how to use the WebDAV component to make directories, move  *\n");
	printf("* resources, delete resource, and to get resources.                          *\n");
	printf("******************************************************************************\n\n");



	while (true) {
		prompt(argument, "Choose an option:\n1) Make Directory\n2) Move Resource\n3) Get Resource\n4) Delete Resource\n5) Put Resource\nQ) Quit.\n> ");

		if (!strcmp(argument, "1")) { // Make Directory
			webdav.Reset();
			webdav.SetUser(argv[argc - 2]);
			webdav.SetPassword(argv[argc - 1]);
			char server[LINE_LEN];
			char dir[LINE_LEN];
			prompt(server, "Name server where you wish to create a directory (ex. http://localhost:443): ");
			prompt(dir, "Name directory to create (ex. directory/folder): ");
			sprintf(buffer, "%s/%s", server, dir);
			ret_code = webdav.MakeDirectory(buffer);

		}
		else if (!strcmp(argument, "2")) {  // Move Resource
			webdav.Reset();
			webdav.SetUser(argv[argc - 2]);
			webdav.SetPassword(argv[argc - 1]);
			char server[LINE_LEN];
			char src[LINE_LEN];
			char dest[LINE_LEN];
			char path[LINE_LEN] = "/\0";
			prompt(server, "Name server (ex. http://localhost:443): ");
			prompt(src, "Name source of the resource (ex. myoldfolder/myfile.txt): ");
			prompt(dest, "Name destination of the resource (ex. mynewfolder/myfile.txt): ");
			strcat(path, dest);
			sprintf(buffer, "%s/%s", server, src);
			ret_code = webdav.MoveResource(buffer, dest);
		}
		else if (!strcmp(argument, "3")) { // Get Resource
			webdav.Reset();
			webdav.SetUser(argv[argc - 2]);
			webdav.SetPassword(argv[argc - 1]);
			char get[LINE_LEN];
			prompt(get, "Name URI of resource you wish to get: ");
			ret_code = webdav.GetResource(get);

		}
		else if (!strcmp(argument, "4")) { // Delete Resource
			webdav.Reset();
			webdav.SetUser(argv[argc - 2]);
			webdav.SetPassword(argv[argc - 1]);
			char del[LINE_LEN];
			prompt(del, "Name URI of resource you wish to delete: ");
			ret_code = webdav.DeleteResource(del);

		}
		else if (!strcmp(argument, "5")) { // Put Resource on server, can create resource on server or replace old resource
			webdav.Reset();
			webdav.SetUser(argv[argc - 2]);
			webdav.SetPassword(argv[argc - 1]);
			char file[LINE_LEN];
			char put[LINE_LEN];
			prompt(file, "Name path of file you wish to put on server: ");
			webdav.SetLocalFile(file);
			prompt(put, "Name URI of resource you wish to put on server: ");
			ret_code = webdav.PutResource(put);

		}
		else if (!strcmp(argument, "q") || !strcmp(argument, "Q")) {
			exit(0);
		}
		else if (!strcmp(argument, "")) {
			// Do nothing
		}
		else {
			fprintf(stderr, "Bad command / Not implemented in demo.");
		} // end of command checking
		if (ret_code)    // Got an error.
		{
			printf("\nError: %d", ret_code);
			if (webdav.GetLastError())
			{
				printf(" \"%s\"\n", webdav.GetLastError());
			}
		}
		ret_code = 0; //flush out error

	}
}


