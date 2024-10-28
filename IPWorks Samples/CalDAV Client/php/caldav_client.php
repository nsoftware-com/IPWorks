<?php
/*
 * IPWorks 2024 PHP Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworks
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */
require_once('../include/ipworks_caldav.php');
require_once('../include/ipworks_oauth.php');
require_once('../include/ipworks_const.php');
?>
<?php
require_once "../include/ipworks_caldav.php";
require_once "../include/ipworks_oauth.php";

class MyDAV extends IPWorks_CalDAV
{
  public function fireSSLServerAuthentication($param){
      $param["accept"] = true;
      return $param;
  }
  public function fireEventDetails($param){
    $startDateFormatted = date("m/d H:i:s", strtotime($this->getStartDate()));
    echo sprintf("%-40s %-20s %-30s %-100s\n",
      $this->getSummary(),
      $startDateFormatted,
      $this->getLocation(),
      $param["resourceuri"]
    );
  }
}

function prompt($message) {
  echo $message . ": ";
  return trim(fgets(STDIN));
}

function authorize(){
  global $caldav, $oauth, $authString, $user;
  $caldav->setUser($user);
  if(empty($authString)){
    $authString = $oauth->doGetAuthorization();
  }
  $caldav->setAuthorization($authString);
}

if (count($argv) < 4) {
  echo "usage: php caldav.php [options] provider username password\n";
  echo "Options:\n";
  echo "  -i        OAuth Client ID\n";
  echo "  -s        OAuth Client Secret\n";
  echo "  provider  the provider name: Google\n";
  echo "  username  the username to login\n";
  echo "  password  the password to login\n";
  echo "\nExample: php caldav.php -i \"clientID\" -s \"clientSecret\" google username password\n";
  exit(1);
}

echo "******************************************************************************\n";
echo "* This demo shows how to use the CalDAV component to list upcoming events    *\n";
echo "* from an existing Google calendar. You can also create a new event,         *\n";
echo "* delete an event, or export an event to an ics file.                        *\n";
echo "******************************************************************************\n\n";

$caldav = new MyDAV();
$oauth = new IPWorks_OAuth();

$clientID = "";
$clientSecret = "";
$user = $argv[count($argv) - 2];
$authString = "";

for ($i = 0; $i < $argc; $i++) {
  if ($argv[$i][0] === '-') {
    if ($argv[$i] === '-i') $clientID = $argv[$i + 1];
    if ($argv[$i] === '-s') $clientSecret = $argv[$i + 1];
  }
}
$oauth->setClientId($clientID);
$oauth->setClientSecret($clientSecret);
$oauth->setServerAuthURL("https://accounts.google.com/o/oauth2/auth");
$oauth->setServerTokenURL("https://accounts.google.com/o/oauth2/token");
$oauth->setAuthorizationScope("https://www.googleapis.com/auth/calendar");

echo "Type \"?\" for a list of commands.\n";

while (true) {
  echo "> ";
  $command = trim(fgets(STDIN));
  $args = explode(' ', $command);

  switch ($args[0]) {
    case '?':
      echo "1) List Events\n";
      echo "2) Add Event\n";
      echo "3) Delete Event\n";
      echo "4) Export ICS File\n";
      echo "?) This help menu.\n";
      echo "Q) Quit.\n";
      break;

    case '1': // List Calendar Events
      authorize();
      echo sprintf("%-40s %-20s %-30s %-100s\n",
          "Summary",
          "Start date",
          "Location",
          "ResourceUri"
      );
      $caldav->setReportFilterEventType(0);
      $caldav->doGetCalendarReport("https://apidata.googleusercontent.com/caldav/v2/" . $user . "/events");
      break;
      
    case '2': // Add Event
      $caldav->doReset();
      authorize();

      $startDate = prompt("Start Date (MM/dd/yyyy HH:mm:ss)");
      $endDate = prompt("End Date (MM/dd/yyyy HH:mm:ss)");
      $location = prompt("Location");
      $summary = prompt("Summary");
      $description = prompt("Description");
      $caldav->setStartDate(date("Ymd\THis", strtotime($startDate)));
      $caldav->setEndDate(date("Ymd\THis", strtotime($endDate)));
      $caldav->setLocation($location);
      $caldav->setSummary($summary);
      $caldav->setDescription($description);
      $caldav->setUID($caldav->getStartDate());
      $caldav->doCreateEvent("https://apidata.googleusercontent.com/caldav/v2/" . $user . "/events/" . $caldav->getUID() . ".ics");

      echo "Event Successfully Added.\n";
      break;

    case "3": //Delete event
      $caldav->doReset();
      $file = prompt("Give name of file to delete (find the file names by running option #1)");
      authorize();
      $caldav->doDeleteEvent("https://apidata.googleusercontent.com/caldav/v2/" . $user . "/events/" . $file);
      echo "Event Successfully Deleted.\n";
      break;

    case "4": //Export ICS
      $caldav->doReset();
      $file = prompt("Give name of .ICS file");
      authorize();
      $caldav->doGetEvent("https://apidata.googleusercontent.com/caldav/v2/" . $user . "/events/" . $file);
      $icsContent = $caldav->doExportICS();
      file_put_contents($file . '.ics', $icsContent);
      echo "ICS file saved.\n";
      break;

    case 'q':
    case 'Q':
      exit(0);

    default:
      echo "Bad command / Not implemented in demo.\n";
      break;
  }
}