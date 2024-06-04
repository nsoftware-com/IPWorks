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

def fireSSLServerAuthentication(e):
  e.accept = True

print("Feed is: https://news.google.com/rss?topic=h&hl=en-US&gl=US&ceid=US:en")
feed = "https://news.google.com/rss?topic=h&hl=en-US&gl=US&ceid=US:en"

try:
  rss = RSS()
  rss.on_ssl_server_authentication = fireSSLServerAuthentication
  print("Connecting...")
  rss.get_feed(feed)
  count = int(rss.get_item_count())
  command = input("Welcome! Type ? for a list of commands.> ").lower()
  while True:
    if command.startswith("?"):
      print("Read RSS feed")
      print("l                               title list")
      print("<num>                           show news description")
      print("q                               quit")
    elif command.startswith("q"):
      break
    elif command.startswith("l"):
      for i in range(0,count):
        print("%s) %s" %(str(i+1),rss.get_item_title(i)))
    elif command == "" :
      print("Type ? for a list of commands.> ")
    else:
      num=int(command)
      print("%s) %s\n"%(num,rss.get_item_description(num-1)))
      print("Find full article at: %s\n\n"%rss.get_item_link(num-1))

    command = input("> ").lower()

except IPWorksError as e:
  print("ERROR %s"%e.message)





