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
require_once('../include/ipworks_ipinfo.php');
require_once('../include/ipworks_const.php');
?>
<?php

if ($argc < 2) {
  echo "Usage: php resolve_ip.php host\n";
  echo "  host: the host address OR host name to resolve\n";
  echo "Example: php resolve_ip.php www.google.com\n";
  echo "Example: php resolve_ip.php 8.8.8.8\n";
  
} else {
  try {
  
  $ipinfo = new IPWorks_IPInfo();
  $host = $argv[1];

  // Check if IP address or hostname
  if ((bool)ip2long($host) ) { // Read as IP address
    $ipinfo->doResolveAddress($host);
  } else { // Read as hostname
    $ipinfo->doResolveName($host);
  }
  
  // wait for resolution
  while($ipinfo->getPendingRequests() > 0) {
    $ipinfo->doEvents();
  }
  
  echo "Real Name:       " . $ipinfo->getHostName() . "\n";
  echo "Primary Address: " . $ipinfo->getHostAddress() . "\n";
  echo "Other Addresses: " . $ipinfo->getOtherAddresses() . "\n";

  } catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
  }
}
?>