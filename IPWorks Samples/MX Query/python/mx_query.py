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

def fireResponse(e):
  print("Use Mail Server: %s "%(e.mail_server))

def fireError(e):
  print("Error [%s]: %s "%(e.error_code,e.description))

if len(sys.argv) != 3:
  print("usage: mx_query.py server email")
  print("")
  print("  server  the name or address of the DNS server")
  print("  email   the email address to resolve")
  print("\r\nExample: mx_query.py 4.2.2.1 billg@microsoft.com")
else:
  try:
    mx = MX()
    mx.on_response = fireResponse
    mx.on_error = fireError
    mx.set_timeout(10)
    mx.set_dns_server(sys.argv[1])
    mx.resolve(sys.argv[2])

  except IPWorksError as e:
    print("ERROR %s" %e.message)




