# 
# IPWorks 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworks
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworks import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

caldav = CalDAV()
oauth = OAuth()

resource_uris = []

def fire_SSL_server_authentication(e):
    e.accept = True
	
def event_details(e):
    resource_uris.append(e.resource_uri)
    s = "%2d) %-50.50s %-20s%-30.30s %-100s\n" % (len(resource_uris), caldav.get_summary(), caldav.get_start_date(), caldav.get_location(),e.resource_uri)
    print(s)

if len(sys.argv) < 3:
    print("usage: caldav [options] provider username password")
    print("Options")
    print("  -i        OAuth Client ID")
    print("  -s        OAuth Client Secret")
    print("  provider  the provider name: Google ")
    print("  username  the username to login")
    print("  password  the password to login")
    print("\r\nExample: python cal_client.py -i \"Client ID\" -s \"Client Secret\" google username password")
else:
    try:
        

        for i in range(0,len(sys.argv)):
            if (sys.argv[i].startswith("-")):
                if (sys.argv[i]=="-i"):
                    oauth.set_client_id(sys.argv[i+1])
                if (sys.argv[i]=="-s"):
                    oauth.set_client_secret(sys.argv[i+1])

        
        caldav.on_ssl_server_authentication = fire_SSL_server_authentication
        # caldav.on_status = fire_Status
        caldav.on_event_details = event_details
        oauth.set_server_auth_url("https://accounts.google.com/o/oauth2/auth")
        oauth.set_server_token_url("https://oauth2.googleapis.com/token")
        caldav.set_auth_scheme(6)
        
        while True:
            command = input("Choose an option:\n1) List Events\n2) Add Event\n3) Delete Event\nQ) Quit.\n> ")
            if command == "1":
                caldav.reset()
                resource_uris[:] = [] # Clear the list
                event_num = 1
                caldav.set_user(sys.argv[len(sys.argv) - 2])
                caldav.set_password(sys.argv[len(sys.argv) - 2])
                oauth.set_authorization_scope("https://www.googleapis.com/auth/calendar")
                auth=oauth.get_authorization()
                caldav.set_authorization(auth)
                print("     %-50.50s %-20s%-30.30s %-100s\n" % ("Summary","Start Date","Location","ResourceURI"))
                s = "https://apidata.googleusercontent.com/caldav/v2/%s/events" % (caldav.get_user())
                caldav.get_calendar_report(s)

            elif command == "2":
                caldav.reset()
                # start = 
                # end = str(input("End Date (YYYYMMDDTHHMMSS): "))
                # location = str(input("Location: "))
                # summary = str(input("Summary: "))
                # desc = str(input("Description: "))
                caldav.set_user(sys.argv[len(sys.argv) - 2])
                caldav.set_password(sys.argv[len(sys.argv) - 2])
                oauth.set_authorization_scope("https://www.googleapis.com/auth/calendar")

                caldav.set_start_date(str(input("Start Date (YYYYMMDDTHHMMSS): ")))
                caldav.set_end_date(str(input("End Date (YYYYMMDDTHHMMSS): ")))
                caldav.set_location(str(input("Location: ")))
                caldav.set_summary(str(input("Summary: ")))
                caldav.set_description(str(input("Description: ")))
                caldav.set_uid(caldav.get_start_date())
                caldav.set_authorization(oauth.get_authorization())
                caldav.put_calendar_event("https://apidata.googleusercontent.com/caldav/v2/" + caldav.get_user() + "/events/" + caldav.get_uid() + ".ics")
                print("Event successfully added.")

            elif command == "3":
                caldav.reset()
                caldav.set_user(sys.argv[len(sys.argv) - 2])
                caldav.set_password(sys.argv[len(sys.argv) - 2])
                oauth.set_authorization_scope("https://www.googleapis.com/auth/calendar")
                index = int(input("Specify the number of the event to delete: "))
                if index > len(resource_uris):
                    print("\nPlease run 1) List Events command first.\n")
                    continue
                caldav.set_authorization(oauth.get_authorization())
                caldav.delete_calendar_event("https://apidata.googleusercontent.com" + resource_uris[index - 1])
                print("Event successfully deleted.")
               


           
            elif command == "q" or command == "Q":
                sys.exit()
        
    except IPWorksError as e:
        print("ERROR %s" %e)


