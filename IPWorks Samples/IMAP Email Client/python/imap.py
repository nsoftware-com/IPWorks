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



count = 0
mailboxes=[]

def fireMailBoxList(e):
  global count
  global mailboxes
  count+=1
  mailboxes.append(e.mailbox)

def fireMessageInfo(e):
  print(e.message_id + ") " + e.subject)

def fireTransfer(e):
  print(e.text)

def displayMailBoxList():
  global count
  global mailboxes
  for i in range(1,count+1):
    print(str(i)+") <%s>"%mailboxes[i-1])

def fireSSLServerAuthentication(e):
  e.accept = True

print("************************************************************************")
print("* This demo shows how to use the IMAP protocol to visit a mail server. *")
print("************************************************************************")

if len(sys.argv) != 4:
  print("usage: imap_email_client.py server username password\r\n")
  print("  server    the name or address of the mail server (IMAP server)")
  print("  username  the user name used to authenticate to the MailServer ")
  print("  password  the password used to authenticate to the MailServer ")
  print("\r\nExample: imap_email_client.py 127.0.0.1 username password")
else:
  try:
    imap = IMAP()

    imap.on_ssl_server_authentication = fireSSLServerAuthentication
    imap.set_mail_server(sys.argv[1])
    imap.set_user(sys.argv[2])
    imap.set_password(sys.argv[3])
    imap.on_mailbox_list = fireMailBoxList
    imap.on_message_info = fireMessageInfo
    imap.on_transfer = fireTransfer
    imap.connect()
    imap.set_mailbox("*")
    imap.list_mailboxes()

    command = input("Welcome! Type ? for a list of commands.> ")
  
    while True:
      if command == '?':
        print("IMAP Demo Commands")
        print("l                 list mailboxes")
        print("s mailbox_num     show messages in mailbox by index (e.g. 's 1')")
        print("r message_num     read the message by index (e.g. 'r 1')")
        print("q                 quit")
      
      elif str(command).startswith('l'):
        print("Mailboxes for " + sys.argv[2] + ":")
        count = 0
        mailboxes=[]
        imap.set_mailbox("*")
        imap.list_mailboxes()
        displayMailBoxList()
        print("Total: " + str(count))
      elif str(command).startswith('s'):
        index=str(command).split()[1]
        imap.set_mailbox(mailboxes[int(index)-1])
        imap.select_mailbox()
        messageCount=imap.get_message_count()
        if messageCount!=0:
          imap.set_message_set("1:" + str(messageCount))
          print("Messages in current mailbox:")
          imap.retrieve_message_info()
        else:
          print("No messages found in the current mailbox.")
      elif str(command).startswith('r'):
        print("Message text:")
        index=str(command).split()[1]
        imap.set_message_set(index)
        imap.retrieve_message_text()
      elif str(command).startswith("q"):
        imap.disconnect()
        print("Goodbye!")
        sys.exit(0)

      command = input("> ").lower()

  except IPWorksError as e:
    print("ERROR: %s"%e.message)



