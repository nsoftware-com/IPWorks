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


dataReceived = False

def fireError(e):
  print("ERROR: " + e.description)

def fireDataIn(e):
  global dataReceived
  dataReceived = True
  print ("Received '" + bytes.decode(e.datagram) + "' from " + e.source_address + ":" + str(e.source_port) + ".")

if len(sys.argv) != 3:
  print("usage: py udpclient.py server port")
  print("  server      the address of the remote host")
  print("  port        the port of the remote host")
  print("Example: py udpclient.py localhost 777")
  print("         py udpclient.py 255.255.255.255 777 (broadcast)")
else:
  try:
    udpclient = UDP()
    udpclient.on_data_in = fireDataIn
    udpclient.on_error = fireError
    udpclient.remote_host = sys.argv[1]
    udpclient.remote_port = int(sys.argv[2])
    udpclient.activate()
    print("Type and press enter to send. Press Ctrl-C to exit the application.")
    while True:
      data = input("")
      if (len(data) > 0):
        udpclient.send_text(data)    
        dataReceived = False
        while dataReceived == False:
          udpclient.do_events()

  except IPWorksError as e:
    print("ERROR: " + e.message)

  except KeyboardInterrupt:
    print("Exiting...")
    udpclient.deactivate()

