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
#define LINE_LEN 100

class MySyslog : public SysLog
{
public:

	virtual int FirePacketIn(SysLogPacketInEventParams *e)
	{
		printf("Host: %s\nFacility: %s\nSeverify: %s\nTime: %s\nMessage: %s\n\n", e->Hostname, e->Facility, e->Severity, e->Timestamp, e->Message);
		return 0;
	}

};

int main(int argc, char **argv)
{

	MySyslog syslog;
	SysLog syslogsender;
	int ret_code = 0;

	printf("Activating Syslog Listener.\n");
	printf("Sending test messages.\n");
	
	syslog.Activate();

	syslogsender.SetLocalPort(0);//system chosen free port
	syslogsender.SetRemoteHost("localhost");
	syslogsender.SetRemotePort(syslog.GetLocalPort());

	ret_code = syslogsender.Activate();
	if (ret_code) goto done;

	ret_code = syslogsender.SendPacket(1, 2, "Test 1");
	if (ret_code) goto done;
	syslogsender.SendPacket(1, 2, "Test 2");
	syslogsender.SendPacket(1, 2, "Test 3");
	syslogsender.Deactivate();

	printf("Test messages sent. To stop listener, press Ctrl - C.\n\n");

	while(true)
	{
		syslog.DoEvents();
	}

done:
	if (ret_code)
		printf("Error: %s\n",syslogsender.GetLastError());

	syslog.Deactivate();
	syslogsender.Deactivate();

	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();
	return 0;
}





