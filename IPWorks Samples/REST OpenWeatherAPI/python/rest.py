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


def fireSSLServerAuthentication(e):
    if e.accept:
      return
    print("Server provided the following certificate:\nIssuer: " + e.cert_issuer + "\nSubject: " + e.cert_subject + "\n")
    print("The following problems have been determined for this certificate: " + e.status + "\n")
    print("Would you like to continue anyways? [y/n] ")
    response = input("Would you like to continue anyways? [y/n]")
    if response == 'y':
      e.accept = True

rest = REST()

if len(sys.argv) < 4:
  print("usage: rest_openweatherapi.py key city state country")
  print("  key      API key required for authentication (free and available at https://home.openweathermap.org/users/sign_up)")
  print("  city  the city of the address for which to get weather data (underscore spaces in city names)")
  print("  state    the state of the address for which to get weather data (only for the US)")
  print("  country  the country of the address for which to get weather data (use ISO 3166 country codes)")
  print("\r\nExample: rest_openweatherapi.py da9bd73746219f432ddb52abf6b3b087 Chapel_Hill NC US")
  print("Example: rest_openweatherapi.py da9bd73746219f432ddb52abf6b3b087 London GB")
else:
  rest.on_ssl_server_authentication = fireSSLServerAuthentication

  try:
    # Parse arguments.
    apiKey = sys.argv[1]
    address = ""

    # Create and URL-encode the address based on the input received.
    if (len(sys.argv) == 5 and sys.argv[len(sys.argv) - 1] == "US"):
      address = sys.argv[len(sys.argv) - 3].replace("_", "%20") + "%2c" + sys.argv[len(sys.argv) - 2] + "%2cUS"
    elif (len(sys.argv) == 4 and sys.argv[len(sys.argv) - 1] != "US"):
      address = sys.argv[len(sys.argv) - 2].replace("_", "%20") + "%2c" + sys.argv[len(sys.argv) - 1]
    else:
      raise Exception("Invalid address provided.  Input documentation can be found at https://openweathermap.org/api/geocoding-api.")
    
    # Geocode the address to retrieve its latitude and longitude.
    rest.get("http://api.openweathermap.org/geo/1.0/direct?q=" + address + "&appid=" + apiKey)

    rest.xpath = "/json/[1]/lat"
    latitude = rest.xtext
    rest.xpath = "../lon"
    longitude = rest.xtext

    # If you wish to see the entire geocoding REST response, uncomment the line below.
    #print(rest.transferred_data)

    # Retrieve the weather at those coordinates.
    rest.get("http://api.openweathermap.org/data/2.5/weather?lat=" + latitude + "&lon=" + longitude + "&appid=" + apiKey)

    # If you wish to see the entire weather REST response, uncomment the line below.
    #print(rest.transferred_data)

    # Process user commands.
    print("Type \"?\" or \"?\" for a list of commands.")

    while(True):
      command = input("rest> ")
      arguments = command.split()

      if (arguments[0] == '?' or arguments[0] == "help"):
        print("Commands: ")
        print(" ?             display the list of valid commands")
        print(" help          display the list of valid commands")
        print(" clouds        display information relating to the cloud cover")
        print(" conditions    display the main weather conditions and a description")
        print(" temperature   display information relating to the current temperature")
        print(" visibility    display information relating to the visibility")
        print(" wind          display information relating to the wind conditions")
        print(" quit          exit the application")
      elif (arguments[0] == "clouds"):
        rest.xpath = "/json/clouds/all"
        print("\tCloud Cover:  " + rest.xtext + "%")
      elif (arguments[0] == "conditions"):
        rest.xpath = "/json/weather/[1]/main"
        print("\tMain Weather Conditions:  " + rest.xtext)
        rest.xpath = "../description"
        print("\tDescription:  " + rest.xtext)
      elif (arguments[0] == "temperature"):
        rest.xpath = "/json/main/temp"
        print("\tCurrent Temperature:  " + rest.xtext + " K")
        rest.xpath = "../feels_like"
        print("\tFeels Like:  " + rest.xtext + " K")
        rest.xpath = "../temp_min"
        print("\tMinimum Temperature:  " + rest.xtext + " K")
        rest.xpath = "../temp_max"
        print("\tMaximum Temperature:  " +  rest.xtext + " K")
        rest.xpath = "../pressure"
        print("\tPressure:  " + rest.xtext + " hPa")
        rest.xpath = "../humidity"
        print("\tHumidity:  " + rest.xtext + "%")
      elif (arguments[0] == "visibility"):
        rest.xpath = "/json/visibility"
        print("\tVisibility:  " + rest.xtext)
      elif (arguments[0] == "wind"):
        rest.xpath = "/json/wind/speed"
        print("\tWind Speed:  " + rest.xtext + " m/s")
        rest.xpath = "../deg"
        print("\tDegrees:  " + rest.xtext + " degrees")
        rest.xpath = "../gust"
        print("\tGusts:  " + rest.xtext + " m/s")
      elif (arguments[0] == "quit"):
        break
      elif (arguments[0] == ""):
        # Do nothing.
        pass
      else:
        print("Invalid command.")
  except IPWorksError as e:
    print("%s" %e.message)
  
  input("Press any key to exit...")

