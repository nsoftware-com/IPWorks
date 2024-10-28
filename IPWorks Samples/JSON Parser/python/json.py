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


def save_and_reparse(json):
  json.save()
  json.parse()

def get_element_type_str(element_type):
  if element_type == 0:
    return "object"
  elif element_type == 1:
    return "array"
  elif element_type == 2:
    return "string"
  elif element_type == 3:
    return "number"
  elif element_type == 4:
    return "bool"
  elif element_type == 5:
    return "null"
  elif element_type == 6:
    return "raw"

json = JSON()

ensureArg(sys.argv, "Provide a JSON File: ", 1)

json.set_input_file(sys.argv[1])  # Read from this file
json.set_output_file(sys.argv[1]) # Write to the same file

try:
  json.parse()
  
  json.config("PrettyPrint=true")
  json.set_overwrite(True)
  json.set_xpath("/json")

  print("Parsing complete. Navigate, read, and edit using the available commands.")
  print("Type \"?\" or \"help\" for a list of commands.")

  while True:
    command = input("> ")

    if len(command) > 0:
      arguments = command.split()
      command = arguments.pop(0).strip().lower()

    if command == "?" or command == "help":
      print("Commands:")
      print("  ?                                  display the list of valid commands")
      print("  help                               display the list of valid commands")
      print("  children                           display the children of the currently selected element (if named)")
      print("  insert <new name> <new value> <value type> <position>")
      print("                                     insert the specified name and value at the specified position")
      print("                                       Value Types:")
      print("                                         0        object")
      print("                                         1        array")
      print("                                         2        string")
      print("                                         3        number")
      print("                                         4        bool")
      print("                                         5        null")
      print("                                         6        raw")
      print("                                       Positions:")
      print("                                         0        Before the current element")
      print("                                         1        After the current element")
      print("                                         2        The first child of the current element")
      print("                                         3        The last child of the current element")
      print("  parent                             display the parent of the currently selected element (if named)")
      print("  path                               display the full path of the currently selected element")
      print("  remove                             remove the currently selected element")
      print("  rename <new name>                  set a new name for the currently selected element")
      print("  revalue <new value> <value type>   set a new value for the currently selected element")
      print("  select <element>                   navigate to the specified element by providing either an absolute or relative path")
      print("  subtree                            display a snapshot of the currently selected element in the document")
      print("  text                               display the text of the currently selected element")
      print("  quit                               exit the application")
      

    elif command == "children":
      print("Currently selected element contains " + str(json.get_xchild_count()) + " children.")
      for i in range(json.get_xchild_count()):
        elem_name = json.get_xchild_name(i)
        if len(elem_name) > 32:
          elem_name = elem_name[:29] + "..."
        elem_type = get_element_type_str(json.get_xchild_element_type(i))
        print(" {:32} [{}]".format(elem_name, elem_type))

    elif command == "insert":
      print("Inserting element...", end = '')
      name     = arguments[0]
      val      = arguments[1]
      val_type = int(arguments[2])
      position = int(arguments[3])
      json.insert_property(name, val, val_type, position)
      save_and_reparse(json)
      print("finished!")

    elif command == "parent":
      parent = json.get_xparent()
      if len(parent) > 0:
        print(parent)
      else:
        print("Currently selected element either contains no parent, or the parent is unnamed.")

    elif command == "path":
      print(json.get_xpath())

    elif command == "remove":
      print("Removing current element \"{}\" ...".format(json.get_xelement()), end = '')
      json.remove()
      save_and_reparse(json)
      print("finished!")
    
    elif command == "rename":
      print("Renaming current element \"{}\" to \"{}\"...".format(json.get_xelement(), arguments[0]), end = '')
      json.set_name(arguments[0])
      save_and_reparse(json)
      print("finished!")

    elif command == "revalue":
      print("Changing value of current element \"{}\"...".format(json.get_xelement()), end = '')
      json.set_value(arguments[0], int(arguments[1]))
      save_and_reparse(json)
      print("finished!")

    elif command == "select":
      if json.has_xpath(arguments[0]):
        json.set_xpath(arguments[0])
        print("Currently selected element path is now \"" + json.get_xpath() + "\".")
      else:
        print("Invalid path supplied.")

    elif command == "subtree":
      print(json.get_xsub_tree())

    elif command == "text":
      text = json.get_xtext()
      if len(text) > 0:
        print(text)
      else:
        print("Currently selected element contains no text.")

    elif command == "quit":
      print("Goodbye.")
      break

    else:
      print("Invalid command.")

except IPWorksError as e:
  print("ERROR [JSON]: %s" % e.message)

