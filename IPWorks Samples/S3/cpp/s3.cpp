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
#define LINE_LEN 150

class MyS3 : public S3 {
public:
    virtual int FireObjectList(S3ObjectListEventParams* e) {
        printf("%s\n", e->ObjectName);
        return 0;
    }

    virtual int FireBucketList(S3BucketListEventParams* e) {
        printf("%s\n", e->BucketName);
        return 0;
    }

    virtual int FireError(S3ErrorEventParams* e) {
        printf("Error Code: %d, Description: %s", e->ErrorCode, e->Description);
        return 0;
    }
};

void printoptions() {
    printf("\nType ? to see this menu, or a number 1-6 to perform one of the following actions:\n"
        "1. List Buckets\n"
        "2. Select Bucket\n"
        "3. List Objects\n"
        "4. Get Object\n"
        "5. Put Object\n"
        "6. Quit\n");
}

int main(int argc, char** argv) {
    MyS3 s3;
    char buffer[LINE_LEN + 1];
    int ret_code = 0;

    printf("\nWelcome to the IPWorks S3 Demo.\n");
    printf("This demo shows how to use the IPWorks S3 component to interact with S3 compatible storage providers.\n");
    printf("It is assumed that you already signed up for a service (for example, at aws.amazon.com/s3) and have an Access Key and Secret Key which are required to use the service.\n");
    printf("------------------------------------------------------------ \n");

    printf("\n0     -   Amazon S3\n");
    printf("1     -   Digital Ocean\n");
    printf("2     -   Google Storage\n");
    printf("3     -   Wasabi\n");
    printf("4     -   Backblaze B2\n");
    printf("5     -   Huawei\n");
    printf("6     -   Alibaba\n");
    printf("7     -   IBM\n");
    printf("9     -   Linode\n");
    printf("255   -   Custom\n\n");

    printf("Select Service Provider (enter number): ");
    scanf("%150s", buffer);
    if (ret_code = s3.SetServiceProvider(atoi(buffer))) goto error;
    if (s3.GetServiceProvider() == SP_CUSTOM) {
        printf("Enter custom provider URL: ");
        buffer[0] = 'U'; buffer[1] = 'R'; buffer[2] = 'L'; buffer[3] = '=';
        scanf("%146s", (buffer + 4));
        s3.Config(buffer);
        if (s3.GetLastErrorCode()) goto error;
    }
    printf("Access Key: ");
    scanf("%150s", buffer);
    s3.SetAccessKey(buffer);
    printf("Secret Key: ");
    scanf("%150s", buffer);
    s3.SetSecretKey(buffer);
    printf("\nBucket List:\n\n");
    s3.ListBuckets();

    printoptions();
    while (1) {
        printf("s3> ");
        scanf("%150s", buffer);
        if (!strcmp(buffer, "?")) {
            printoptions();
        }
        else if (!strcmp(buffer, "1")) {
            if (ret_code = s3.ListBuckets()) goto error;
        }
        else if (!strcmp(buffer, "2")) {
        bucket:
            printf("Enter Bucket: ");
            scanf("%150s", buffer);
            if (!strcmp(buffer, "")) goto bucket;
            if (ret_code = s3.SetBucket(buffer)) goto error;
        }
        else if (!strcmp(buffer, "3")) {
            if (ret_code = s3.ListObjects()) goto error;
        }
        else if (!strcmp(buffer, "4")) {
        obj:
            printf("Which object: ");
            scanf("%150s", buffer);
            if (!strcmp(buffer, "")) goto obj;
            s3.SetLocalFile("");
            if (ret_code = s3.GetObject(buffer)) goto error;
            char* data; int size;
            if (ret_code = s3.GetObjectData(data, size)) goto error;
            printf("Contents of object %s :\n", buffer);
            for (int i = 0; i < size; i++) {
                printf("%c", data[i]);
            }
            printf("\n");
        }
        else if (!strcmp(buffer, "5")) {
        locfile:
            printf("Local file to upload: ");
            scanf("%150s", buffer);
            if (!strcmp(buffer, "")) goto locfile;
            s3.SetLocalFile(buffer);
        newobj:
            printf("Name of new object: ");
            scanf("%150s", buffer);
            if (!strcmp(buffer, "")) goto newobj;
            if (ret_code = s3.CreateObject(buffer)) goto error;
            printf("Object created.\n");
        }
        else if (!strcmp(buffer, "6")) {
            exit(0);
        }
        else {
            printf("Please select a number from [1-6].\n");
        }
    }
    return ret_code;

error:
    printf("\nError: %d", ret_code);
    if (s3.GetLastError()) {
        printf(" \"%s\"\n", s3.GetLastError());
    }
    return ret_code;
}

