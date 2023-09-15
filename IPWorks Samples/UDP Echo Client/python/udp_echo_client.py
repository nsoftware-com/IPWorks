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

flag = True

def fireDataIn(e):
  global flag
  print(str(e.source_address).ljust(15)+" "+(bytes.decode(e.datagram)))
  flag = False

try:
  udptime = UDP()
  udptime.on_data_in = fireDataIn
  udptime.set_remote_host("255.255.255.255")
  udptime.set_remote_port(7)
  udptime.set_local_host(udptime.get_local_host())
  udptime.set_active(1)

  print("Source".ljust(15)+" Data")
  udptime.send(bytes("hello!", 'utf-8'))

  while flag:
    udptime.do_events()

except IPWorksError as e:
  print("ERROR %s" %e.message)

