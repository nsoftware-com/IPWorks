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

lines = 0
msgnum = 0

def fireGroupList(e):
  global lines
  print(e.group)
  lines += 1
  if lines == 20:
    input("Press ENTER to continue...")
    lines = 0

def fireGroupOverview(e):
  global lines
  print("%s %s"%(e.article_number,e.subject))
  lines +=1
  if lines == 20:
    input("Press ENTER to continue...")
    lines = 0

def fireHeader(e):
  global lines
  print("%s: %s"%(e.field,e.value))
  lines +=1
  if lines == 20:
    input("Press ENTER to continue...")
    lines = 0

def fireTransfer(e):
  global lines
  print(e.text)
  lines += 1
  if lines == 20:
    input("Press ENTER to continue...")
    lines = 0

if len(sys.argv) != 2:
  print("usage: news_client.py server ")
  print("")
  print("  server  the name or address of the NNTP server (e.g. nntp.aioe.org)")
  print("\r\nExample: news_client.py nntp.aioe.org")
else:

  try:
    print("Using news server: " + sys.argv[1])

    nntp = NNTP()
    nntp.on_group_list = fireGroupList
    nntp.on_group_overview = fireGroupOverview
    nntp.on_header = fireHeader
    nntp.on_transfer = fireTransfer
    nntp.set_news_server(sys.argv[1])
    nntp.set_news_port(119)
    nntp.connect()

    command = str(input("Welcome! Type ? for a list of commands.> ")).lower()
    while True:

      if command.startswith('?'):
        print("Readnews Commands")
        print("l                               list all available newsgroups")
        print("c <newsgroup>                   select newsgroup")
        print("t <message number>              type messages")
        print("n                               goto and type next message")
        print("f                               give head lines of messages")
        print("q                               quit")

      elif command.startswith('l'):
        nntp.list_groups()
      elif command.startswith('c'):
        newsgroup = command
        newsgroup = newsgroup[2:newsgroup.__len__()]
        nntp.set_current_group(newsgroup)
      elif command.startswith('f'):
        print("Listing news in %s ..."%nntp.get_current_group())
        nntp.set_overview_range("0-")
        nntp.group_overview()
      elif command.startswith('n'):
        msgnum+=1
        nntp.set_current_article(str(msgnum))
        nntp.fetch_article()
      elif str(command).startswith('t'):
        article = str(command)
        article=article[2:article.__len__()]    
        nntp.set_current_article(article)
        nntp.fetch_article()
      elif str(command).startswith('q'):
        break
      command = str(input("> ")).lower()
  except IPWorksError as e:
    print("ERROR %s"%e.message)




