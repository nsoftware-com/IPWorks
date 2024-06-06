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
#include <string>
#include <vector>
#include <iostream>
#include <iomanip> 
#include <sstream>
#include <chrono>

#include "../../include/ipworks.h"
#define LINE_LEN 100

using namespace std;

bool exportingContact = false;

void getAuthorization(CardDAV* carddav, OAuth* oauth, string password, string clientID, string clientSecret) {
  if (clientID.empty() && clientSecret.empty()) {
    carddav->SetPassword(password.c_str());
  }
  else {
    oauth->SetClientId(clientID.c_str());
    oauth->SetClientSecret(clientSecret.c_str());
    oauth->SetServerAuthURL("https://accounts.google.com/o/oauth2/auth");
    oauth->SetServerTokenURL("https://oauth2.googleapis.com/token");
    oauth->SetAuthorizationScope("https://www.googleapis.com/auth/carddav");
    carddav->SetAuthScheme(AUTH_OAUTH);
    carddav->SetAuthorization(oauth->GetAuthorization());
  }
}

class MyCardDAV : public CardDAV {
public:
  int contactNumber = 1;

  MyCardDAV() {
    contactNumber = 1;
  }

  virtual int FireContactDetails(CardDAVContactDetailsEventParams* e) {
    string firstPhoneNumber = " - ";
    if (this->GetPhoneNumbers()->GetCount() > 0) {
      firstPhoneNumber = this->GetPhoneNumbers()->Get(0)->GetValue();
    }
    string firstEmail = " - ";
    if (this->GetEMails()->GetCount() > 0) {
      firstEmail = this->GetEMails()->Get(0)->GetValue();
    }
    cout << left << contactNumber
                << setw(4) << ")"
                << setw(40) << this->GetFormattedName()
                << setw(30) << firstPhoneNumber
                << setw(30) << firstEmail
                << setw(30) << e->ResourceURI << "\n\n";
    contactNumber++;
    return 0;
  }

  virtual int FireSSLServerAuthentication(CardDAVSSLServerAuthenticationEventParams* e) {
    e->Accept = true;  //this will trust all certificates and it is not recommended for production use
    return 0;
  }

  virtual int FireLog(CardDAVLogEventParams* e) {
    //cout << e->Message;
    return 0;
  }

};

int main(int argc, char* argv[]) {

  string command;     // user's command
  string argument;             // arguments to the user's command	
  string clientID;
  string clientSecret;
  int ret_code = 0;
  MyCardDAV carddav;
  OAuth* oauth = new OAuth();

  cout << "********************************************************************************\n";
  cout << "* This demo shows how to use the CardDAV component to list contacts from an    *\n";
  cout << "* existing Google or NextCloud address book. You can also create a new         *\n";
  cout << "* contact, delete a contact, or export a contact to a vcf file.                *\n";
  cout << "********************************************************************************\n\n";

  if (argc < 4) {
    cerr << "usage: carddav [options] uri username password\n";
    cerr << "Options\n";
    cerr << "  -i        OAuth Client ID\n";
    cerr << "  -s        OAuth Client Secret\n";
    cerr << "  URI       the URI of the address book \n";
    cerr << "  username  the username to login\n";
    cerr << "  password  the password to login\n";
    cerr << "\nExample: carddav -i OAuthClientID -s OAuthClientSecret URI username password\n\n";
    cout << "Press enter to continue.";
  }
  else {
    cout << "\nType \"?\" for a list of commands.\n";

    while (true) {
      for (int i = 0; i < argc; i++) {

        if (strncmp(argv[i], "-", 1) == 0) { //check if argument starts with "-"
          if (strcmp(argv[i], "-i") == 0) {
            clientID = argv[i + 1];
          }
          if (strcmp(argv[i], "-s") == 0) {
            clientSecret = argv[i + 1];
          }
        }
      }

      exportingContact = false; //This is used so output from ContactDetails is not displayed when exporting an even to an ics file
      cout << "> ";
      cin >> command;
      istringstream iss(command);

      iss >> argument;

      string user = argv[argc - 2];
      string password = argv[argc - 1];
      string mainAddressbookURL = argv[argc - 3];

      if (argument.compare("?") == 0) {
        cout << "1) List Events\n";
        cout << "2) Add Event\n";
        cout << "3) Delete Event\n";
        cout << "4) Export ICS File\n";
        cout << "?) This help menu.\n";
        cout << "Q) Quit.\n";
      }
      else if (argument.compare("1") == 0) {  //List Events
        carddav.contactNumber = 1;
        cout << left << setfill(' ') << ""
          << setw(5) << ""
          << setw(40) << "Name"
          << setw(30) << "PhoneNumber"
          << setw(30) << "Email"
          << setw(30) << "ResourceURI" << "\n\n";
        carddav.SetUser(user.c_str());
        getAuthorization(&carddav, oauth, password, clientID, clientSecret);
        carddav.GetAddressbookReport(mainAddressbookURL.c_str());
        carddav.Reset();
      }
      else if (argument.compare("2") == 0) {    //Add Event
        carddav.SetUser(user.c_str());
        getAuthorization(&carddav, oauth, password, clientID, clientSecret); 

        auto now = chrono::system_clock::now();
        auto millis = chrono::time_point_cast<chrono::milliseconds>(now);
        carddav.SetUID(to_string(millis.time_since_epoch().count()).c_str());
        cout << "Name: ";
        string name;
        cin >> name;
        carddav.SetFormattedName(name.c_str());
        cout << "Phone number: ";
        string phoneNumber;
        cin >> phoneNumber;
        carddav.GetPhoneNumbers()->SetCount(1);
        carddav.SetPhoneNumberName(0, "TEL");
        carddav.SetPhoneNumberValue(0, phoneNumber.c_str());
        cout << "Email: ";
        string email;
        cin >> email;
        carddav.GetEMails()->SetCount(1);
        carddav.SetEMailName(0, "EMAIL");
        carddav.SetEMailValue(0, email.c_str());
        cout << "Address: ";
        string address;
        cin >> address;
        carddav.GetAddresses()->SetCount(1);
        carddav.SetAddressName(0, "ADR");
        carddav.SetAddressValue(0, address.c_str());
        carddav.Config("loglevel=3");
        carddav.CreateContact((mainAddressbookURL + "/" + carddav.GetUID() + ".vcf").c_str());
        cout << "Contact created successfully";
      }
      else if (argument.compare("3") == 0) {     //Delete Event
        cout << "Give name of file to delete (find the file names by running option #1): ";
        string filename;
        cin >> filename;
        carddav.SetUser(user.c_str());
        getAuthorization(&carddav, oauth, password, clientID, clientSecret);
        carddav.DeleteContact((mainAddressbookURL + "/" + filename).c_str());
        cout << "Contact successfully deleted\n";
      }
      else if (argument.compare("4") == 0) {     //Export Event
        cout << "Give name of the .vcf file (find the file names by running option #1): ";
        string filename;
        cin >> filename;
        carddav.SetUser(user.c_str());
        getAuthorization(&carddav, oauth, password, clientID, clientSecret);
        cout << left << setfill(' ') << ""
          << setw(5) << ""
          << setw(40) << "Name"
          << setw(30) << "PhoneNumber"
          << setw(30) << "Email"
          << setw(30) << "ResourceURI" << "\n\n";
        carddav.GetContact((mainAddressbookURL + "/" + filename).c_str());
      }
      else if (argument.compare("q") == 0 || argument.compare("Q") == 0) {
        exit(0);
        return 0;
      }
      else if (argument.empty()) {
        // Do nothing
      }
      else {
        cout << "Bad command / Not implemented in demo.\n";
      }
      if (ret_code)    // Got an error.
      {
        cout << "\nError: %d", ret_code;
        if (carddav.GetLastError()) {
          cout << " \"%s\"\n", carddav.GetLastError();
        }
      }
      ret_code = 0; //flush out error
    }
  }

  exit(ret_code);
  return 0;
}

