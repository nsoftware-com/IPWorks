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
require_once('../include/ipworks_traceroute.php');
require_once('../include/ipworks_const.php');
?>
<?php

class MyTraceroute extends IPWorks_TraceRoute{
  function fireHop($param){
    echo $param['hopnumber'] . "\t";
    echo $param['hostaddress']. "\t\t";    
    echo $param['duration']."\n";
  }
   
}
if ($argc < 2) {
  echo "Usage: php traceroute.php url\n";
  echo "  url: the url to trace\n";
  echo "Example: php traceroute.php www.nsoftware.com";
  return;
} else {
  $host = $argv[1];
}

$timeout = 20;

// Create a new Traceroute object
$traceroute = new MyTraceroute();

// Configure Traceroute
$traceroute->doConfig("UseICMPDLL=true");
$traceroute->setTimeout($timeout);

// Display results
echo "Traceroute to $host\n\n";
echo "Hop\tHop Host Address\tHop Time (ms)\n";

// Perform the traceroute
try {
    $traceroute->doTraceTo($host);
} catch (Exception $e) {
    echo 'Error: ' . $e->getMessage() . "\n";
    exit(1);
}
?>