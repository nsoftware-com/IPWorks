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

print("********************************************************************************\n")
print("* This demo shows how to set up a TFTP server using the TFTPServer component.  *\n")
print("********************************************************************************\n")

try:
  server = TFTPServer()

  def fireError(e):
    print(e.description)

  def fireLog(e):
    print("Log: " + e.message)

  def fireConnected(e):
    print("%d: connected." % e.connection_id)

  def fireConnectionRequest(e):
    print(e.remote_host + ": attempting to connect.")

  def fireDisconnected(e):
    print("%d: disconnected." % e.connection_id)

  def fireStartTransfer(e):
    print("%d: started a transfer." % e.connection_id)

  def fireEndTransfer(e):
    print("%d: transfer complete." % e.connection_id)

  def fireTransfer(e):
    print("%d: transferring data." % e.connection_id)

  server.on_connected = fireConnected
  server.on_connection_request = fireConnectionRequest
  server.on_disconnected = fireDisconnected
  server.on_start_transfer = fireStartTransfer
  server.on_transfer = fireTransfer
  server.on_end_transfer = fireEndTransfer
  server.on_error = fireError

  server.set_local_dir(input("Local Directory: "))

  server.start_listening()

  print("Listening...")
  print("Press Ctrl-C to shutdown")

  while True:
    server.do_events()

except IPWorksTftpserverError as e:
  print("ERROR: " + e.message)

except KeyboardInterrupt:
  print("Shutdown requested...exiting")
  server.stop_listening()


