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
require_once('../include/ipworks_udp.php');
require_once('../include/ipworks_const.php');
?>
<?php
global $udpserver;
class MyUDPServer extends IPWorks_UDP
{
  function FireDataIn($param) {
    global $udpserver;
    echo "Echoing '" . $param['datagram'] . "' back to client " . $param['sourceaddress'] . ":" . $param['sourceport'] . ".\n";
    $udpserver->setRemoteHost($param['sourceaddress']);
    $udpserver->setRemotePort($param['sourceport']);
    $udpserver->doSendText($param['datagram']);
  }
  function FireError($param) {
    echo "Error: " . $param['description'] . "\n";
  }
}

$udpserver = new MyUDPServer();
if ($argc < 2) {
  echo "Usage php udp_echo_server.php port\n\n";
  echo "  port    the port on which the server will listen\n\n";
  echo "Example: php udp_echo_server.php 777\n";
  return;
} else {
  echo "*****************************************************************\n";
  echo "* This demo shows how to set up an echo server using UDP.       *\n";
  echo "*****************************************************************\n";
  $udpserver->setLocalPort($argv[1]);
}

try {
  $udpserver->doActivate();
  echo "Listening on port " . $udpserver->getLocalPort() . "... press Ctrl-C to shutdown.\n";

  while (true) {
    $udpserver->doEvents();
  }
} catch (Exception $ex) {
  echo 'Error: ',  $ex->getMessage(), "\n";
}
?>