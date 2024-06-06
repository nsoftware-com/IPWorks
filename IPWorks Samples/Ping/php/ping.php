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
require_once('../include/ipworks_ping.php');
require_once('../include/ipworks_const.php');
?>
<?php

if ($argc < 2) {
  echo "Usage: php ping.php url\n";
  echo "  url: the url to ping\n";
  echo "Example: php ping.php www.nsoftware.com";
} else {

$url = $argv[1];

try {
  
  $ping = new IPWorks_Ping();
  $ping->setTimeout (10);
  
  echo "Pinging $url with " . $ping->getPacketSize() . " bytes of data:\n";

  for ($i = 0; $i < 5; $i++) {
    $ping->doPingHost ($url);
    echo "Reply from " . $ping->getResponseSource() . ": bytes=" . $ping->getPacketSize() . " time=" . $ping->getResponseTime() . "ms\n";
  }

} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}

}
 ?>