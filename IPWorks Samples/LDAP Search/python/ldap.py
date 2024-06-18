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


def fireSearchResult(e):
  print("Result DN: " + e.dn)

if len(sys.argv) != 3:
  print("usage: ldap_search.py server search")
  print("")
  print("  server  the name or address of the LDAP server")
  print("  search  the parameters to search for")
  print("\r\nExample: ldap_search.py ldap.umich.edu cn=* ")
else:
  try:
    ldap = LDAP()
    ldap.on_search_result = fireSearchResult

    ldap.set_server_name(sys.argv[1])
    ldap.set_dn("dc=umich, dc=edu")

    ldap.bind()
    if ldap.get_result_code() == 0:
      print("Connected!")
      ldap.set_timeout(10)
      ldap.search(sys.argv[2])
    else:
      print("Bind Failed: " + str(ldap.get_result_code()) + " : " + ldap.get_result_description())

  except IPWorksError as e:
    print("ERROR %s" %e.message)  




