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


def fireHop(e):
  print((str(e.hop_number) + ")").ljust(5) + e.host_address.ljust(20) + str(e.duration))
  #If a hop times out the e.Duration of the fireHop event will be -1

# This demo prints out the path IP packets take from your machine to a specified domain

domain=input("Enter a domain (www.google.com): ")
if domain == "" : domain = "www.google.com"
trace = TraceRoute()
trace.on_hop = fireHop
trace.set_hop_limit(10)

try:
  print("Hop".ljust(5) + "Hop Address".ljust(20) + "Hop Time (ms)")
  trace.trace_to(domain)

except IPWorksError as e:
  print("ERROR %s" %e.message)

