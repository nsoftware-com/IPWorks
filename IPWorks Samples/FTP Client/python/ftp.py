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


verbose = 1

def toggleVerbose():
  global verbose
  verbose = 1 - verbose
  print("Verbose mode " + (verbose and "ON." or "OFF."))

def fireDirList(e):
  print("%s"%e.dir_entry)

def firePITrail(e):
  if verbose: print("%s"%e.message)

def fireError(e):
  print("Error %i: %s\n"%(e.error_code, e.description))

def fireSSLServerAuthentication(e):
  e.accept = True

ftp = FTP()

ftp.on_dir_list = fireDirList
ftp.on_pi_trail = firePITrail
ftp.on_error = fireError
ftp.on_ssl_server_authentication = fireSSLServerAuthentication

def logon():
  user = input("User: ")
  ftp.set_user(user)

  pwd = input("Password: ")
  ftp.set_password(pwd)
  ftp.logon()
  
def printMenu():
  print("Available Commands:\n?        bye     help     put     rmdir\n"
    "append   cd      ls       pwd     verbose\n"
    "ascii    close   mkdir    quit\n"
    "binary   get     open     rm")

try:
  if len(sys.argv) >= 4:
    ftp.set_remote_host(sys.argv[1])
    ftp.set_user(sys.argv[2])
    ftp.set_password(sys.argv[3])
    ftp.logon()

  if len(sys.argv) == 2:
    ftp.set_remote_host(sys.argv[1])
    logon()

  if len(sys.argv) < 2:
    ftp.set_remote_host( input("Enter FTP Host: ") )
    logon()

  printMenu()

  while(1):
    ftp.set_remote_file("")
    command = input("ftp> ")
    if len(command) <= 0: continue
    argument = command.split()
    command = argument[0]
    if command == "?" or command == "help":
      printMenu()
    elif command == "quit" or command == "bye":
      ftp.logoff()
      sys.exit(0)
    elif command == "append":
      ensureArg(argument,"Local file: ",1)
      ensureArg(argument,"Remote file: ",2)
      ftp.set_local_file(argument[1])
      ftp.set_remote_file(argument[2])
      ftp.append()
    elif command == "ascii":
      ftp.set_transfer_mode(1)
    elif command == "binary":
      ftp.set_transfer_mode(2)
    elif command == "cd":
      ensureArg(argument,"Remote directory: ",1)
      ftp.change_remote_path(argument[1])
    elif command == "close":
      ftp.logoff()
    elif command == "get":
      ensureArg(argument,"Remote file: ",1)
      ensureArg(argument,"Local file: ",2)
      ftp.set_remote_file(argument[1])
      ftp.set_local_file(argument[2])
      ftp.download()
      print("Download complete.")
    elif command == "ls":
      if len(argument) < 2:
        ftp.list_directory_long()
      else:
        cur_path = ftp.query_remote_path()
        ftp.change_remote_path(argument[1])
        ftp.list_directory_long()
        ftp.change_remote_path(cur_path)
    elif command == "mkdir":
      ensureArg(argument,"Directory name: ",1)
      ftp.make_directory(argument[1])
    elif command == "mv":
      ensureArg(argument,"Source file: ",1)
      ensureArg(argument,"Destination file: ",2)
      ftp.set_remote_file(argument[1])
      ftp.rename_file(argument[2])
    elif command == "open":
      ftp.logoff()
      ensureArg(argument,"Remote host: ",1)
      ftp.set_remote_host(argument[1])
      logon()
    elif command == "passive":
      ensureArg(argument,"on/off: ",1)
      if argument[1] == "on" and not ftp.get_passive():
        ftp.set_passive(True)
        print("Passive mode ON.")
      elif argument[1] == "off" and ftp.get_passive():
        ftp.set_passive(False)
        print("Passive mode OFF.")
    elif command == "put":
      ensureArg(argument,"Local file: ",1)
      ensureArg(argument,"Remote file: ",2)
      ftp.set_local_file(argument[1])
      ftp.set_remote_file(argument[2])
      ftp.upload()
      print("Upload complete.")
    elif command == "pwd":
      print(ftp.query_remote_path())
    elif command == "rm":
      ensureArg(argument,"Remote file: ",1)
      ftp.delete_file(argument[1])
    elif command == "rmdir":
      ensureArg(argument,"Remote file: ",1)
      ftp.remove_directory(argument[1])
    elif command == "verbose":
      if len(argument) < 2:
        toggleVerbose()
      else:
        if argument[1] == "on" and not verbose:
          toggleVerbose()
        elif argument[1] == "off" and verbose:
          toggleVerbose()
    else:
      print("Bad command / Not implemented in demo.")

except IPWorksError as e:
  print("ERROR: %s"%e.message)




