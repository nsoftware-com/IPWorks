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

def fireConnected(e):
  global wsserver
  print(wsserver.get_ws_connection_remote_host(e.connection_id) + " has connected.")

def fireDisconnected(e):
  global wsserver
  print("Disconnected from %s" % wsserver.get_ws_connection_remote_host(e.connection_id))

def fireDataIn(e):
  global wsserver
  print("Received a message from %s and the message is: %s" % (wsserver.get_ws_connection_remote_host(e.connection_id), e.text))
  print("Echoing the message back...")
  wsserver.send(e.connection_id, e.text)

print("*****************************************************************\n")
print("* This demo shows how to set up an echo server using WSServer.  *\n")
print("*****************************************************************\n")

port = int(input("Local Port: "))

try:
  wsserver = WSServer()
  wsserver.on_connected = fireConnected
  wsserver.on_disconnected = fireDisconnected
  wsserver.on_data_in = fireDataIn

  buffer = ""
  while buffer == "":
    buffer = input("Use SSL? [y/n]")

  if buffer == "y" or buffer == "Y":
    buffer = ""
    while buffer == "":
      print("Please choose a certificate type:")
      print("1: PFX File")
      print("2: PEMKey File")
      buffer = input("Selection: ")

    if buffer == "1" or buffer == "": #by default use PFX
      wsserver.set_ssl_cert_store_type(2)
    else:
      wsserver.set_ssl_cert_store_type(6)

    buffer = ""
    buffer = input("Enter Certificate File Location (.\sslcert.pfx): ")
    if buffer == "":
      buffer = ".\sslcert.pfx"

    wsserver.set_ssl_cert_store(buffer)

    buffer = ""
    buffer = input("Enter Certificate Password (password): ")
    if buffer == "":
      buffer = "password"

    wsserver.set_ssl_cert_store_password(buffer)
    wsserver.set_ssl_cert_subject("*")
    wsserver.set_use_ssl(True)
  
  wsserver.set_local_port(port)
  wsserver.set_local_host('localhost')
  wsserver.start_listening()
  print("Listening... press Ctrl-C to shutdown")
  while True:
    wsserver.do_events()

except IPWorksError as e:
  print(e)
  print("ERROR: %s" % e.message)

except KeyboardInterrupt:
  print("Shutdown requested...exiting")
  wsserver.shutdown()


