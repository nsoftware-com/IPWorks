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



if len(sys.argv) != 2:
  print("usage: ping.py host")
  print("")
  print("  host  the IP address (IP number in dotted internet format) or Domain Name of the remote host")
  print("\r\nExample: ping.py www.google.com")
else:
  ping = Ping()

  try:
    ping.set_remote_host(sys.argv[1])
    print("\nPinging %s.\n" %(sys.argv[1]))
    for i in range(1,5):
      ping.set_timeout(15)
      ping.ping_host(sys.argv[1])
      if ping.get_response_time() == 0:
        print("Reply from %s: bytes=%i time=0ms" %(ping.get_remote_host(),ping.get_packet_size()))
      else:
        print("Reply from %s: bytes=%i time=%sms" %(ping.get_remote_host(),ping.get_packet_size(),ping.get_response_time()))

  except IPWorksError as e:
    print("ERROR %s" %e.message)



