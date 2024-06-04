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


def fireError(e):
  print("ERROR: %s" %e.description)
  
def fireSSLServerAuthentication(e):
  e.accept = True

try:
  atom = Atom()

  atom.on_error = fireError
  atom.on_ssl_server_authentication = fireSSLServerAuthentication

  print("This demo uses the most recent Google News atom feed. Sometimes Google changes the url so if you get an error try updating the url below")
  feed = "https://news.google.com/atom?hl=en-US&gl=US&ceid=US:en"
  atom.get_feed(feed)

  while True:
    print("Atom Channel :%s\n%s\n"%(atom.get_channel_title(),atom.get_channel_subtitle()))
    for i in range(0,atom.get_entry_count()):
      print("%i, %s\n"%(i+1,atom.get_entry_title(i)))
    print("Q, Quit\n")
    response = (input("news item number: ")).lower()
    if response.startswith('q'):
      break
    else:
      selectedindex = int(response)-1
      print("\n\n[%d] %s\n\n" %(int(response),atom.get_entry_content(selectedindex)))
      print("Find full article at %s\n\n" %atom.get_entry_link_href(selectedindex))

except IPWorksError as e:
  print("ERROR: %s" %e.message)

except KeyboardInterrupt:
  print("Shutdown requested...exiting")





