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


print("*****************************************************************\n")
print("* This demo shows how to set up an echo server using TCPServer.  *\n")
print("*****************************************************************\n")

port = int(input("Local Port: "))

try:
  tcpserver = TCPServer()
  tcpserver.set_local_port(port)
  tcpserver.start_listening()
  print("Listening...")
  print("Press Ctrl-C to shutdown")

  def fireConnected(e):
    global tcpserver
    print(tcpserver.get_remote_host(e.connection_id) + " has connected.")

  def fireDisconnected(e):
    global tcpserver
    print("Disconnected from %s" % tcpserver.get_remote_host(e.connection_id))

  def fireDataIn(e):
    global tcpserver
    print("Received a message from %s and the message is: %s" % (tcpserver.get_remote_host(e.connection_id), e.text.decode("utf-8")))
    print("Echoing the message back...")
    tcpserver.send(e.connection_id, e.text)

  tcpserver.on_connected = fireConnected
  tcpserver.on_disconnected = fireDisconnected
  tcpserver.on_data_in = fireDataIn

  while True:
    tcpserver.do_events()

except IPWorksError as e:
  print("ERROR: %s" % e.message)

except KeyboardInterrupt:
  print("Shutdown requested...exiting")
  tcpserver.shutdown()




