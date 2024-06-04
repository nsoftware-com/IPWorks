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


def fireSSLServerAuthentication(e):
  e.accept = True

if len(sys.argv) < 4:
  print("usage: html_mailer.py [options] server from to")
  print("Options: ")
  print("  -s      the subject of the mail message")
  print("  -m      the HTML version of the message content")
  print("  -a      the path of file to attach to the message")
  print("  server  the name or address of a mail server (mail relay)");		
  print("  from    the email address of the sender")
  print("  to      a comma separated list of addresses for destinations")
  print("\r\nExample: html_mailer.py -s test -m \"<b>Hello</b>, my name is <i>Tom</i>\" -a FileToAttach mail.local sender@mail.com recipient@mail.local")
else:

  html_mailer = HTMLMailer()

  html_mailer.set_mail_server(sys.argv[len(sys.argv)-3])
  html_mailer.set_from(sys.argv[len(sys.argv)-2])
  html_mailer.set_send_to(sys.argv[len(sys.argv)-1])

  for i in range(0,len(sys.argv)):
    if (sys.argv[i].startswith("-")):
      if (sys.argv[i]=="-s"):
        html_mailer.set_subject(sys.argv[i+1]); #args[i+1] corresponds to the value of argument [i]
      if (sys.argv[i]=="-m"):
        html_mailer.set_message_html(sys.argv[i+1])
      if (sys.argv[i]=="-a"):
        html_mailer.add_attachment(sys.argv[i+1])

  print("Now sending...")
  try:
    html_mailer.send()

  except IPWorksHtmlmailerError as e:
    print("ERROR: %s"%e.message)

  print("Message sent!")




