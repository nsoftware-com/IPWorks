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

messageReceived = False

def fireMessageIn(e):
  print("%s said : %s" %(e.from_,e.message_text))
  global messageReceived
  messageReceived = True

def fireSSLServerAuthentication(e):
  e.accept = True

if len(sys.argv) != 4:
  print("usage: xmpp.py server user password")
  print("")
  print("  server    the address of the XMPP (Jabber) server")
  print("  user      the user associated with the server")
  print("  password  the password to logon")
  print("\r\nExample: xmpp.py talk.google.com username@gmail.com password")
else:
  try:
    print("Connecting...")
  
    xmpp = XMPP()
    xmpp.on_message_in = fireMessageIn
    xmpp.on_ssl_server_authentication = fireSSLServerAuthentication

    xmpp.set_im_server(sys.argv[1])
    xmpp.set_im_port(5222)
    xmpp.ssl_start_mode = 2
    xmpp.connect_to(sys.argv[2],sys.argv[3])
    print("Welcome! Connection established!")
    buddyCount = xmpp.get_buddy_count()
    print("Buddy list:")
    if int(buddyCount) != 0:
      for i in range(0,buddyCount):
        print(str(i+1)+") " + xmpp.get_buddy_id(i))

      buddy = int(input("\nSelect a buddy: "))
      text = input("Message to send to buddy: ")

      xmpp.set_message_text(text)

      print("Sending...")
      xmpp.send_message(xmpp.get_buddy_id(buddy-1))

      flag = input("Enter 'y' to wait for a response [n]: ").lower()
      if flag.startswith('y'):
        print("Waiting for response...")
        while messageReceived==False:
          xmpp.do_events()

    else:
      print("No buddies found!")
    
    print("Disconnecting")
    xmpp.disconnect()
  
  except IPWorksError as e:
    print("ERROR: %s"%e.message)
  




