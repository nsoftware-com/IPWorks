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
global $receivedDatagram;
$receivedDatagram = "";

class MyUDPClient extends IPWorks_UDP
{
  function FireDataIn($param) {
    global $receivedDatagram;
    echo "Received: '" . $param['datagram'] . "' from " . $param['sourceaddress'] . ":" . $param['sourceport'] . ".\n";
    $receivedDatagram = $param['datagram'];
  }
  function FireError($param) {
    echo "Error: " . $param['description'] . "\n";
  }
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

$udpclient = new MyUDPClient();
if ($argc < 3) {
  echo "Usage: php udp_echo_client.php host port\n\n";
  echo "  host:   the address of the remote host\n";
  echo "  port:   the UDP port of the remote host\n\n";
  echo "Example: php udp_echo_client.php 192.168.1.2 777\n";
  echo "         php udp_echo_client.php 255.255.255.255 777 (broadcast)\n";
  return;
} else {
  $udpclient->setRemoteHost($argv[1]);
  $udpclient->setRemotePort($argv[2]);
}

try {
  $udpclient->doActivate();
  echo "Type and press enter to send. Press Ctrl-C to exit the application.\n";
  while (true) {
    try {
      $data = input("");
      global $receivedDatagram;
      $receivedDatagram = "";
      $udpclient->doSendText($data);
      while($receivedDatagram == "")
      {
        $udpclient->doEvents();
      }
    } catch (Exception $ex) {
      echo "Error: " . $ex->getMessage() . "\n";
    }
  }
} catch (Exeception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}
?>