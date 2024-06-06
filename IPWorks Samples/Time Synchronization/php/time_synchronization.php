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
require_once('../include/ipworks_netclock.php');
require_once('../include/ipworks_const.php');
?>
<?php

require_once('../include/ipworks_netclock.php');
require_once('../include/ipworks_const.php');

$clock = new IPWorks_NetClock();

if ($argc < 2){
    echo "Usage: php time_synchronization.php server\n\n";
    echo "server: the time server from which to request the time\n\n";
    echo "Example: php time_synchronization.php time.nist.gov\n";
} else{

    $url = $argv[1];

    try{
             
        $clock->setTimeServer($url);
        $clock->doGetTime();
        echo "System date and time: " . date("m/d/y") . " " . date("h:i:s") . "\n";
        echo "Internet date and time: " . $clock->getLocalTime();

    }
    catch(Exception $e){
        echo "Error: " . $e->getMessage() . "\n";
    }
}