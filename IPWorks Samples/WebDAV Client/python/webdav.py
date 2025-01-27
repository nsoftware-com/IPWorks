# 
# IPWorks 2024 Python Edition - Sample Project
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

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)


webdav = WebDAV()

def fireSSLServerAuthentication(e):
    print("\nServer provided the following fingerprint:\n%s\n"%e.fingerprint)
    cntue = input("Would you like to continue [y/n]: ")
    if cntue == "Y" or cntue == "y":
      e.accept = True

	
def fireConnected(e):
	print("Server connected\n")

def fireConnectionStatus(e):	
    print("Status Code %d: %s" %(e.status_code, e.description))
	
def fireDisconnected(e):
	print("Server disconnected\n")
	
def fireTransfer(e):
	print("Resource being received from server (in full text): \n========================================= \n%s\n" % e.text)

print("******************************************************************************")
print("* This demo shows how to use the WebDAV component to make directories, move  *")
print("* resources, delete resource, and to get resources.                          *")
print("******************************************************************************\n")


if len(sys.argv) != 3:
    print("usage: python webdav_client.py username password")
    print("Options")
    print("  ")
    print("username  the username to login")
    print("password  the password to login")
    print("\r\nExample: webdav username password")
    sys.exit()
else:
    try:
        webdav.on_ssl_server_authentication = fireSSLServerAuthentication
        webdav.on_connected = fireConnected
        webdav.on_connection_status = fireConnectionStatus
        webdav.on_disconnected = fireDisconnected
        webdav.on_transfer = fireTransfer
        
        while True:
            command = input("Choose an option:\n1) Make Directory\n2) Move Resource\n3) Get Resource\n4) Delete Resource\n5) Put Resource\nQ) Quit.\n> ")
            if command == "1":
                webdav.reset()
                webdav.set_user(sys.argv[1])
                webdav.set_password(sys.argv[2])
                server = input("Name server where you wish to create a directory (ex. http://localhost:443): ")
                dir = input("Name directory to create (ex. directory/folder): ")
                webdav.make_directory(server + "/" + dir)
            elif command == "2":
                webdav.reset()
                webdav.set_user(sys.argv[1])
                webdav.set_password(sys.argv[2])
                server = input("Name server (ex. http://localhost:443): ")
                src = input("Name source of the resource (ex. myoldfolder/myfile.txt): ")
                dest = input("Name destination of the resource (ex. mynewfolder/myfile.txt): ")
                webdav.move_resource(server + "/" + src, "/" + dest)
            elif command == "3":
                webdav.reset()
                webdav.set_user(sys.argv[1])
                webdav.set_password(sys.argv[2])
                file = input("Name URI of resource you wish to get: ")
                webdav.get_resource(file)
            elif command == "4":
                webdav.reset()
                webdav.set_user(sys.argv[1])
                webdav.set_password(sys.argv[2])
                file = input("Name URI of resource you wish to delete: ")
                webdav.delete_resource(file)
            elif command == "5":
                webdav.reset()
                webdav.set_user(sys.argv[1])
                webdav.set_password(sys.argv[2])
                file = input("Name path of file you wish to put on server: ")
                webdav.set_local_file(file)
                put = input("Name URI of resource you wish to put on server: ")
                webdav.put_resource(put)
            elif command == "q" or command == "Q":
                sys.exit()
        
    except IPWorksError as e:
        print(e)



