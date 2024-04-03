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

smtp = SMTP()

def fireSSLServerAuthentication(e):
  e.accept = True


if len(sys.argv) < 9 or len(sys.argv) > 14:
  print("Usage: python email_to_list.py  [-option parameter] server  from  to")
  print("Options: ")
  print("  -ssl    a switch parameter to enable SSL")
  print("  -u      the user to authenticate")
  print("  -p      the password of the user")
  print("  -s      the subject of the mail message")
  print("  -m      the raw message content")
  print("  server  the name or address of a mail server (mail relay)")
  print("  from    the email address of the sender")
  print("  to      a comma separated list of addresses for destinations")
  print(
    'Example: python email_to_list -ssl -u myself@me.com -p mypassword -s test -m "test message" my.smtpserver.com sender@mail.com recipient1@gmail.com,recipient2@gmail.com'
  )
  sys.exit()

try:
  
  smtp.on_ssl_server_authentication = fireSSLServerAuthentication

  for i in range(0,len(sys.argv)):
    if (sys.argv[i].startswith("-")):
      if (sys.argv[i] == "-ssl"): smtp.set_ssl_enabled(True)
      if (sys.argv[i] == "-s"): smtp.set_subject(sys.argv[i + 1])
      if (sys.argv[i] == "-u"): smtp.set_user(sys.argv[i + 1])
      if (sys.argv[i] == "-p"): smtp.set_password(sys.argv[i + 1])
      if (sys.argv[i] == "-m"): smtp.set_message_text(sys.argv[i + 1])


  smtp.set_timeout(60)
  smtp.set_mail_server(sys.argv[len(sys.argv) - 3])
  smtp.set_from(sys.argv[len(sys.argv) - 2])
  smtp.set_send_to(sys.argv[len(sys.argv) - 1])
  
  print("Sending message...")
  try:
    smtp.send()
    print("Message sent successfully")
    smtp.disconnect()
    sys.exit()
  except Exception as e:
    print(e)

except IPWorksError as e:
  print(e)



