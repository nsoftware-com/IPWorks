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


def fireSSLServerAuthentication(e):
  e.accept = True

msg_num=0
count = 0

pop = POP()
htmlmailer = HTMLMailer()

pop.on_ssl_server_authentication = fireSSLServerAuthentication
htmlmailer.on_ssl_server_authentication = fireSSLServerAuthentication

if len(sys.argv) != 6:
  print("* This demo shows how to use POP component to view messages in the mailbox and reply to an email with the HTMLmailer component. *")
  print("usage: pop_email_client.py popserver user password mailserver from")
  print("")
  print("  popserver   the name or address of a mail server (internet post office server)")
  print("  user        the user identifier for the mailbox")
  print("  password    the password for the mailbox user")
  print("  mailserver  the name or address of a mail server (mail relay)")
  print("  from        the email address of the sender ")
  print("\r\nExample: pop_email_client.py mail.local username password mail.local user@service.com")
else:

  try:
    pop.set_mail_server(sys.argv[1])
    pop.set_mail_port(110)
    pop.set_user(sys.argv[2])
    pop.set_password(sys.argv[3])
    pop.connect()

    htmlmailer.set_mail_server(sys.argv[4])
    htmlmailer.set_from(sys.argv[5])
    htmlmailer.set_user(sys.argv[2])
    htmlmailer.set_password(sys.argv[3])

    command = input("Welcome! Type ? for a list of commands.> ").lower()

    while True:
      if str(command).startswith("?"):
        print("POP client & htmlmailer client demo")
        print("h                       print out active message headers")
        print("n                       goto and type next message")
        print("t <message number>      type message")
        print("d <message number>      delete message")
        print("m <user@host.com>       mail to specific users")
        print("r <message number>      reply to message")
        print("q                       quit")
      elif str(command).startswith("h"):
        #if already connected, reconnect to establish a new session and get updated messagecount
        if pop.get_connected():
          pop.disconnect()
          pop.connect()
        else:
          pop.connect()

        pop.set_max_lines(1)
        mailcount=pop.get_message_count()
        if mailcount != 0:
          print("\nMsg ".ljust(5) + " " +  "Sender".ljust(50) + " " + "Subject".ljust(50) + " Size\n")
          for i in range(0,int(mailcount)):
            pop.set_message_number(i+1)
            pop.retrieve()
            print((str(i+1) + ") ").ljust(5) + pop.get_message_from().ljust(50)[:50] + " " + pop.get_message_subject().ljust(50)[:50] + " " + str(pop.query_message_size()))
        else:
          print("No messages in inbox")
        pop.set_max_lines(0)  #reset MaxLines so future actions are not limited
      elif str(command).startswith("n"):
        msg_num += 1
        pop.set_message_number(msg_num)
        pop.retrieve()
        print(pop.get_message_text())
      elif str(command).startswith("t"):
        index=int(str(command).split()[1])
        pop.set_message_number(index)
        pop.retrieve()
        print(pop.get_message_text())
      elif str(command).startswith("d"):
        index=int(str(command).split()[1])
        pop.set_message_number(index)
        pop.delete()
        print("Delete successful!")
      elif str(command).startswith("q"):
        pop.disconnect()
        sys.exit()
      elif str(command).startswith("m"):
        target = str(command).split()[1]
        htmlmailer.set_send_to(target)
        subject=input("Subject: ")
        htmlmailer.set_subject(subject)
        print("Input your message")
        print("Type '.' on a line by itself to end message.")
        temptext=input()
        alltext=temptext

        while True:
          temptext=input()
          if temptext == '.':
            break
          alltext=alltext+"\n"+temptext

        htmlmailer.set_message_text(alltext)
        htmlmailer.send()
        print("Send Successful!")
      elif str(command).startswith("r"):
        index=int(str(command).split()[1])
        pop.set_message_number(index)
        pop.retrieve()
        htmlmailer.set_send_to(pop.get_message_from())
        htmlmailer.set_subject("Re: " + pop.get_message_subject())
        print("Input your reply message")
        print("Type '.' on a line by itself to end message.")
        temptext=input()
        alltext=temptext
        while True:
          temptext=input()
          if temptext == '.':
            break
          alltext=alltext+"\n"+temptext
        alltext += "\n-------------\nRe:\n"
        alltext += pop.get_message_text()
        htmlmailer.set_message_text(alltext)
        htmlmailer.send()
        print("Send Successful!")
      command = input("> ").lower()
  
  except IPWorksError as e:
    print("ERROR %s"%e.message)
    sys.exit()  






