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

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

def firePacketIn(e):
  print("Host: %s\nFacility: %s\nSeverity: %s\nTime: %s\nMessage: %s\n\n"%(e.hostname,e.facility,e.severity, e.timestamp, e.message))

try:
  syslog = SysLog()
  syslog.on_packet_in = firePacketIn
  syslog.set_remote_host("127.0.0.1")
  syslog.set_active(True)
  print("Send a test message...")
  syslog.send_packet(1, 1, "hello!")
  print("Activating Syslog Listener.  To stop, press Ctrl-C.\n\n")
  while True:
    syslog.do_events()

except KeyboardInterrupt:
  print("\nexiting")
  syslog.set_active(False)


