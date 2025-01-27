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


import time
if len(sys.argv) != 2:
  print("usage: time_synchronization.py server")
  print("")
  print("  server  the time server from which to request the time")
  print("\r\nExample: time_synchronization.py time.nist.gov")
else:
  myclock = NetClock()
  myclock.set_time_server(sys.argv[1])
  myclock.get_time()
  print("System date and time: " +time.strftime("%m/%d/%Y %H:%M:%S GMT", time.gmtime()))
  print("Internet date and time: "+myclock.get_local_time())




