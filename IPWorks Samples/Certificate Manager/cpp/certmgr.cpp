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

/**
 * README
 * If the options are selected sequentially no certificates or keys
 * are required everything used will be created by the demo.
 **/


class MyCertMgr : public CertMgr
{
private :
	static int serialNumber;
	char subjectCA[1024];
	char subjectCert[1024];
public:
	MyCertMgr()
	{
		Config("CertValidityTime=10");
		Config("CertKeyLength=1024");
		strncpy(subjectCA,"C=US,ST=North Carolina,L=Durham,O=/n Software,OU=CA",1023);
		strncpy(subjectCert,"C=US,ST=North Carolina,L=Durham,O=/n Software,OU=Development",1023);
	}

	virtual int FireError(CertMgrErrorEventParams *e)
	{
		printf("Error Code :%d\n",e->ErrorCode);
		printf("Error Description :%s\n",e->Description);
		return 0;
	}

	void exportToFile()
	{
		char filename[1024];
		char password[1024];
		printf("Export certificate,key to file : [store.pfx] :");
		fgets(filename,1024,stdin);
		if( strlen(filename) == 1 ) strcpy(filename,"store.pfx");
		else filename[strlen(filename)-1] = '\0';
		printf("Password for private key encryption :");
		fgets(password,1024,stdin);
		password[strlen(password)-1] = '\0';
		if ( !ExportCertificate(filename,password))
		{
			printf("Certificate & key exported successfully.\n");
		}
		else
		{
			printf("Error :%s\n",GetLastError());
		}
	}

	void setStore()
	{
		SetCertStoreType(CST_PFXFILE);
		char filename[1024];
		char password[1024];
		printf("Store file : [store.pfx] :");
		fgets(filename,1024,stdin);
		if( strlen(filename) == 1 ) strcpy(filename,"store.pfx");
		else filename[strlen(filename)-1] = '\0';
		printf("Store password :");
		fgets(password,1024,stdin);
		password[strlen(password)-1] = '\0';
		SetCertStorePassword(password);
		if ( !SetCertStore(filename,strlen(filename)))
		{
			printf("Store set successfully.\n");
		}
		else
		{
			printf("Error :%s\n",GetLastError());
		}
	}

	void readCert()
	{
		char filename[1024];
		printf("Read Certificate file : [cert.pem] :");
		fgets(filename,1024,stdin);
		if( strlen(filename) == 1 ) strcpy(filename,"cert.pem");
		else filename[strlen(filename)-1] = '\0';
		if(! ReadCertificate(filename))
		{
			printf("Certificate read successfully.\n");
		}
		else
		{
			printf("Error :%s\n",GetLastError());
		}
	}

	void printCert()
	{
		printf("**** Certificate Details *****\n");
		printf("Certificate Effective Date :%s\n",GetCertEffectiveDate());
		printf("Certificate Expiration Date :%s\n",GetCertExpirationDate());
		printf("Certificate Issuer :%s\n",GetCertIssuer());
		printf("Certificate Subject :%s\n",GetCertSubject());
		printf("Certificate Usage :%s\n",GetCertUsage());
		printf("Certificate Public Key :%s\n",GetCertPublicKey());
		printf("Certificate Public Key Algorithm :%s\n",GetCertPublicKeyAlgorithm());
		printf("Certificate Public Key Length :%d\n",GetCertPublicKeyLength());
		printf("Certificate Serial Number :%s\n",GetCertSerialNumber());
		printf("Certificate Signature Algorithm :%s\n",GetCertSignatureAlgorithm());
		printf("Certificate Thumbprint SHA1 :%s\n",GetCertThumbprintSHA1());
		printf("Certificate Thumbprint MD5 :%s\n",GetCertThumbprintMD5());
	}

	void saveCert()
	{
		char filename[1024];
		printf("Save certificate to file : [cert.pem] :");
		fgets(filename,1024,stdin);
		if( strlen(filename) == 1 ) strcpy(filename,"cert.pem");
		else filename[strlen(filename)-1] = '\0';
		if( !SaveCertificate(filename))
		{
			printf("Certificate saved successfully.\n");
		}
		else
		{
			printf("Error :%s\n",GetLastError());
		}
	}

	void createCert(int selfsigned)
	{
		char *store = NULL;
		int length = 0;
		if(selfsigned)
		{
			if (!CreateCertificate(subjectCA,-1) )
			{
				printf("Certificate created successfully.\n");
			}
			else
			{
				printf("Error :%s\n",GetLastError());
			}
		}
		else
		{
			GetCertStore(store,length);
			if( !store )
			{
				printf("There is no certificate in the store, try a self signed certificate.\n");
				return;
			}
			if ( !IssueCertificate(subjectCert,serialNumber++) )
			{
				printf("Certificate created successfully.\n");
			}
			else
			{
				printf("Error :%s\n",GetLastError());
			}
		}
	}
};

int MyCertMgr::serialNumber = 1;

char get_selection()
{
	char choice[20];
	do
	{
		printf("\n********************************\n");
		printf("[a] Create a self signed certificate.\n");
		printf("[b] Export the certificate and the private key to a file.\n");
		printf("[c] Assign a certificate to the store.\n");
		printf("[d] Create a certificate signed with the certificate in the store.\n");
		printf("[e] Save the certificate to a file.\n");
		printf("[f] Read certificate from a file.\n");
		printf("[g] Print certificate details.\n");
		printf("[q] Quit.\n");
		printf("What would you like to do? ");
		fgets(choice,20,stdin);
	}
	while (strlen(choice) != 2);
	return choice[0];
}

int main(int argc, char **argv)
{
	MyCertMgr certmgr;
	int quit = 0;
	do
	{
		switch(get_selection())
		{
			case 'a':
			case 'A':
				certmgr.createCert(1);
				break;

			case 'b':
			case 'B':
				certmgr.exportToFile();
				break;

			case 'c':
			case 'C':
				certmgr.setStore();
				break;

			case 'd':
			case 'D':
				certmgr.createCert(0);
				break;

			case 'f':
			case 'F':
				certmgr.readCert();
				break;

			case 'G':
			case 'g':
				certmgr.printCert();
				break;

			case 'e':
			case 'E':
				certmgr.saveCert();
				break;

			case 'q':
			case 'Q':
				quit = 1;
				break;

			default:
				printf("Invalid option. Please select a letter from the menu below.\n");
		}
	}
	while(!quit);
	return 0;
}











