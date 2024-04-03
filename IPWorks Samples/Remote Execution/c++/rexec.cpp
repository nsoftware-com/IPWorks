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
#define LINE_LEN 100

class MyRexec : public Rexec
{
public:
	int waiting;

	int FireStderr(RexecStderrEventParams *e)
	{
		fwrite(e->Text, 1, e->lenText, stderr);
		if (e->EOL) fprintf(stderr, "\n");
		return 0;
	}
	int FireStdout(RexecStdoutEventParams *e)
	{
		fwrite(e->Text, 1, e->lenText, stdout);
		if (e->EOL) fprintf(stdout, "\n");
		return 0;
	}
	int FireDisconnected(RexecDisconnectedEventParams *e)
	{
		waiting = 0;
		return 0;
	}
};

int main(int argc, char **argv)
{

	if (argc < 2)
	{
		fprintf(stderr, "usage: %s <host> <user> <password> <command>\n", argv[0]);
		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		exit(1);
	}

	MyRexec rexec;

	int ret_code = rexec.SetRemoteHost(argv[1]);
	if (ret_code) goto done;

	ret_code = rexec.SetRemoteUser(argv[2]);
	if (ret_code) goto done;

	ret_code = rexec.SetRemotePassword(argv[3]);
	if (ret_code) goto done;

	ret_code = rexec.SetCommand(argv[4]);
	if (ret_code) goto done;

	//now wait for command completion
	rexec.waiting = 1;
	while (rexec.waiting) rexec.DoEvents();

done:
	if (ret_code)
	{
		fprintf(stderr, "error: %d", ret_code);
		if (rexec.GetLastError())
			fprintf(stderr, " (%s)", rexec.GetLastError());
		fprintf(stderr, "\nexiting...\n");
		exit(1);
	}
	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();
	return 0;
}





