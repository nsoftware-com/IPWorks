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

bool exportingEvent = false;

int parseDateTime(const char* str, const char* format, tm* timeinfo)
{
	if (!strcmp(format, "YYYYMMDDThhmmss"))
	{
		if (strlen(str) > 16)
			return 1;

		char buffer[LINE_LEN + 1];
		char* tmp;
		tmp = (char*)str;
		strncpy(buffer, tmp, 4);
		buffer[4] = '\0';
		timeinfo->tm_year = atoi(buffer) - 1900;

		tmp += 4;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_mon = atoi(buffer) - 1;

		tmp += 2;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_mday = atoi(buffer);

		tmp += 3;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_hour = atoi(buffer);

		tmp += 2;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_min = atoi(buffer);

		tmp += 2;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_sec = atoi(buffer);
	}
	else if (!strcmp(format, "MM/dd/yyyy HH:mm:ss"))
	{
		if (strlen(str) > 19)
			return 1;

		char buffer[LINE_LEN + 1];
		char* tmp;
		tmp = (char*)str;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_mon = atoi(buffer) - 1;

		tmp += 3;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_mday = atoi(buffer);

		tmp += 3;
		strncpy(buffer, tmp, 4);
		buffer[4] = '\0';
		timeinfo->tm_year = atoi(buffer) - 1900;

		tmp += 5;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_hour = atoi(buffer);

		tmp += 3;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_min = atoi(buffer);

		tmp += 3;
		strncpy(buffer, tmp, 2);
		buffer[2] = '\0';
		timeinfo->tm_sec = atoi(buffer);
	}
	else
	{
		//Not supported format string
		return 1;
	}
	return 0;
}

void writeToFile(const char* path, const char* data)
{
	FILE* file;
	file = fopen(path, "wb");
	if (file != NULL)
	{
		fputs(data, file);
		fclose(file);
	}
	else
	{
		printf("Can't create file: %s to write.\n", path);
	}
}

class MyCalDAV : public CalDAV
{
public:
	bool exportingEvent;
	int eventNumber;
	vector<string> resourceURIs;

	MyCalDAV()
	{
		eventNumber = 1;
		exportingEvent = false;
	}

	virtual int FireEventDetails(CalDAVEventDetailsEventParams *e)
	{
		if (!exportingEvent)
		{
			resourceURIs.push_back(e->ResourceURI);
			char friendlyStartDate[LINE_LEN];
			char* startDate = this->GetStartDate();
			tm timeinfo;
			if (!parseDateTime(startDate, "YYYYMMDDThhmmss", &timeinfo))
			{
				strftime(friendlyStartDate, LINE_LEN, "%m/%d %H:%M:%S", &timeinfo);
			}
			else
			{
				strcpy(friendlyStartDate, startDate);
			}
			printf("%2d) %-50.50s %-20s%-30.30s %-100s\n", eventNumber, this->GetSummary(), friendlyStartDate, this->GetLocation(), e->ResourceURI);
			eventNumber++;
		}
		return 0;
	}

	virtual int FireSSLServerAuthentication(CalDAVSSLServerAuthenticationEventParams *e)
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
	MyCalDAV caldav;
	OAuth *oauth = new OAuth();
	char command[LINE_LEN];     // user's command
	char* argument;             // arguments to the user's command	
	char calendarName[LINE_LEN];
	char clientID[LINE_LEN];
	char clientSecret[LINE_LEN];
	int ret_code = 0;

	printf("******************************************************************************\n");
	printf("* This demo shows how to use the CalDAV component to list upcoming events    *\n");
	printf("* from an existing Google calendar. You can also create a new event,*\n");
	printf("* delete an event, or export an event to an ics file.                        *\n");
	printf("******************************************************************************\n\n");
	
	if (argc < 4)
	{
		fprintf(stderr,"usage: caldav [options] provider username password\n");
		fprintf(stderr, "Options\n");
		fprintf(stderr, "  -i        OAuth Client ID\n");
		fprintf(stderr, "  -s        OAuth Client Secret\n");
		fprintf(stderr, "  provider  the provider name: Google \n");
		fprintf(stderr, "  username  the username to login\n");
		fprintf(stderr, "  password  the password to login\n");
		fprintf(stderr, "\nExample: caldav -i \"Client ID\" -s \"Client Secret\" google username password\n\n");
		printf("Press enter to continue.");
		getchar();
	}
	else
	{
		printf("\nType \"?\" for a list of commands.\n");

		while (true)
		{
			for (int i = 0; i < argc; i++) {

				if (strncmp(argv[i], "-", strlen("-")) == 0) { //check if argument starts with "-"
					if (strcmp(argv[i], "-i") == 0) {
						// args[i+1] corresponds to the value of argument [i]					
						strcpy(clientID, argv[i + 1]);					
					}
					if (strcmp(argv[i], "-s") == 0) {
						// args[i+1] corresponds to the value of argument [i]					
						strcpy(clientSecret, argv[i + 1]);
					}
				}
			}

			caldav.exportingEvent = false; //This is used so output from EventDetails is not displayed when exporting an even to an ics file
			printf("> ");
			fgets(command, LINE_LEN, stdin);
			argument = strtok(command, " \t\n");
			caldav.SetUser(argv[argc - 2]);
			oauth->SetClientId(clientID);
			oauth->SetClientSecret(clientSecret);
			oauth->SetServerAuthURL("https://accounts.google.com/o/oauth2/auth");
			oauth->SetServerTokenURL("https://oauth2.googleapis.com/token");
			caldav.SetAuthScheme(AUTH_OAUTH);
			
			if (!strcmp(argument, "?"))
			{
				printf("1) List Events\n");
				printf("2) Add Event\n");
				printf("3) Delete Event\n");
				printf("4) Export ICS File\n");
				printf("?) This help menu.\n");
				printf("Q) Quit.\n");
			}
			else if (!strcmp(argument, "1"))   //List Events
			{
				caldav.Reset();
				caldav.resourceURIs.clear();
				caldav.eventNumber = 1;
				caldav.SetUser(argv[argc-2]);
				caldav.SetPassword(argv[argc-1]);
				caldav.SetReportFilterEventType(VT_EVENT);
				oauth->SetAuthorizationScope("https://www.googleapis.com/auth/calendar");
				caldav.SetAuthorization(oauth->GetAuthorization());

				

				time_t rawtime;
				time(&rawtime);
				char buffer[LINE_LEN];

				struct tm* timeinfo1;
				timeinfo1 = gmtime(&rawtime);
				strftime(buffer, LINE_LEN, "%Y%m%dT%H%M%SZ", timeinfo1);
				caldav.SetReportFilterStartDate(buffer);

				struct tm* timeinfo2;
				timeinfo2 = gmtime(&rawtime);
				timeinfo2->tm_mon += 1;
				mktime(timeinfo2);
				strftime(buffer, LINE_LEN, "%Y%m%dT%H%M%SZ", timeinfo2);
				caldav.SetReportFilterEndDate(buffer);

				printf("    %-50.50s %-20s%-30.30s %-100s\n\n", "Summary", "Start Date", "Location", "ResourceURI");
				char calBuffer[LINE_LEN];
				sprintf(calBuffer, "https://apidata.googleusercontent.com/caldav/v2/%s/events", caldav.GetUser());
				caldav.GetCalendarReport(calBuffer);

			}
			else if (!strcmp(argument, "2"))     //Add Event
			{
				caldav.Reset();			
				caldav.SetUser(argv[argc - 2]);
				caldav.SetPassword(argv[argc - 1]);
				caldav.SetReportFilterEventType(VT_EVENT);
				oauth->SetAuthorizationScope("https://www.googleapis.com/auth/calendar");
				caldav.SetAuthorization(oauth->GetAuthorization());

				tm timeStart;
				tm timeEnd;
				char buffer[LINE_LEN];

				printf("Start Date (MM/dd/yyyy HH:mm:ss): ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				if (parseDateTime(command, "MM/dd/yyyy HH:mm:ss", &timeStart))
				{
					printf("Bad start date format.");
					continue;
				}
				strftime(buffer, LINE_LEN, "%Y%m%dT%H%M%S", &timeStart);
				caldav.SetStartDate(buffer);

				printf("End Date (MM/dd/yyyy HH:mm:ss): ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				if (parseDateTime(command, "MM/dd/yyyy HH:mm:ss", &timeEnd))
				{
					printf("Bad end date format.");
					continue;
				}
				strftime(buffer, LINE_LEN, "%Y%m%dT%H%M%S", &timeEnd);
				caldav.SetEndDate(buffer);

				printf("Location: ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				caldav.SetLocation(command);

				printf("Summary: ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				caldav.SetSummary(command);

				printf("Description: ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				caldav.SetDescription(command);

				strcpy(buffer, caldav.GetStartDate());
				strcat(buffer, "Z");
				caldav.SetUID(caldav.GetStartDate());
				string temp = argv[argc - 3];
				sprintf(buffer, "https://apidata.googleusercontent.com/caldav/v2/%s/events/%s.ics", caldav.GetUser(), caldav.GetUID());
				ret_code = caldav.CreateEvent(buffer);
				
				if (!ret_code)
					printf("Event Successfully Added.\n");
			}
			else if (!strcmp(argument, "3"))     //Delete Event
			{
				caldav.Reset();
				caldav.SetUser(argv[argc - 2]);
				caldav.SetPassword(argv[argc - 1]);
				caldav.SetReportFilterEventType(VT_EVENT);
				oauth->SetAuthorizationScope("https://www.googleapis.com/auth/calendar");
				caldav.SetAuthorization(oauth->GetAuthorization());

				printf("Specify the number of the event to delete.\n");
				printf("Event Number: ");
				fgets(command, LINE_LEN, stdin);
				command[strlen(command) - 1] = '\0';
				int index = atoi(command);
				char buffer[LINE_LEN];

				if (index > caldav.resourceURIs.capacity())
				{
					printf("Please run 1) List Events command first.\n");
					continue;
				}

				sprintf(buffer, "%s", caldav.resourceURIs[index - 1].c_str());
				ret_code = caldav.DeleteEvent(buffer);

				if (!ret_code)
					printf("Event Successfully deleted.\n");
			}
			
			else if (!strcmp(argument, "q") || !strcmp(argument, "Q"))
			{
				exit(0);
				return 0;
			}
			else if (!strcmp(argument, ""))
			{
				// Do nothing
			}
			else
			{
				printf("Bad command / Not implemented in demo.\n");
			}
			if (ret_code)    // Got an error.
			{
				printf("\nError: %d", ret_code);
				if (caldav.GetLastError())
				{
					printf(" \"%s\"\n", caldav.GetLastError());
				}
			}
			ret_code = 0; //flush out error
		}
	}

	exit(ret_code);
	return 0;
}

