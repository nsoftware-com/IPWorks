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



def fireError(e):
  print("ERROR: " + e.description)

def fireDataIn(e):
  global udpserver
  print("Echoing '" + bytes.decode(e.datagram) + "' back to client " + e.source_address + ":" + str(e.source_port) + ".")
  udpserver.remote_host = e.source_address
  udpserver.remote_port = e.source_port
  udpserver.send_text(bytes.decode(e.datagram))

if len(sys.argv) != 2:
  print("usage: py udpserver.py port")
  print("  port    the port on which the server will listen")
  print("Example: py udpserver.py 777")
  sys.exit(0)

print("*****************************************************************")
print("* This demo shows how to set up an echo server using UDP.       *")
print("*****************************************************************")

try:
  udpserver = UDP()
  udpserver.set_local_port(int(sys.argv[1]))
  udpserver.activate()
  print("Listening on port " + str(udpserver.local_port) + "... press Ctrl-C to shutdown.")

  udpserver.on_error = fireError
  udpserver.on_data_in = fireDataIn

  while True:
    udpserver.do_events()

except IPWorksError as e:
  print("ERROR: %s" % e.message)

except KeyboardInterrupt:
  print("Exiting...")
  udpserver.deactivate()




