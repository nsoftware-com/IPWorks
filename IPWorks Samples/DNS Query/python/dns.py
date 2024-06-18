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

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)



if len(sys.argv) != 3:
  print("usage: dns_query.py server hostname")
  print("")
  print("  server    the address of the DNS server.")
  print("  hostname  the host domain to query")
  print("\r\nExample: dns_query.py 8.8.8.8 www.yahoo.com")
else:
  dns = DNS()
  dns.set_dns_server(sys.argv[1])
  print("Type".ljust(10)+" Field".ljust(15)+"  Value".ljust(50))
  for i in range(1,22):
    dns.set_query_type(i)
    try:
      dns.query(sys.argv[2])
    except IPWorksError as e:
      print("ERROR: %s"%e.message) 
      sys.exit()
    if dns.get_record_count() == None:
      continue
    for j in range(0,dns.get_record_count()):
      for k in range(0,dns.get_record_field_count(j)):
        dns.set_record_field_index(j, k)
        recordFieldVal = dns.get_record_field_value(j)
        if k==0:
          print(dns.get_record_type_name(j).ljust(10) + " " + dns.get_record_field_name(j).ljust(15) + " " + bytes.decode(recordFieldVal.ljust(50)))
        else:
          print(" ".ljust(10) + " " + dns.get_record_field_name(j).ljust(15) + " " + bytes.decode(recordFieldVal.ljust(50)))



