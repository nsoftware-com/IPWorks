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
require_once('../include/ipworks_dns.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyDNS extends IPWorks_DNS
{
  function fireError($param) {
    echo "Error: " . $param['errorcode'] . "[" . $param['description'] . "].\n";
  }

  function fireResponse($param) {
    try {
      if ($param['statuscode'] == 0) {
        for ($j = 0; $j < $this->getRecordCount(); $j++){
          for ($k = 0; $k < $this->getRecordFieldCount($j); $k++) {
            $this->setRecordFieldIndex($j, $k);
            if ($k == 0) {
              echo $this->getRecordTypeName($j) . "\t";
            } else {
              echo "\t";
            }
            echo $this->getRecordFieldName($j) . "\t" . $this->getRecordFieldValue($j) . "\n";
          }
        }
      }
    } catch (Exception $e) {
      echo "Response Error: " . $e->getMessage() . "\n";
    }
  }
}

if ($argc < 3) {
  echo "Usage: php dns_query.php server hostname\n";
  echo "\n";
  echo "  server:   the address of the DNS server.\n";
  echo "  hostname: the host domain to query\n";
  echo "Example: php dns_query.php 8.8.8.8 www.google.com\n";
  return;
}

$server = $argv[1];
$hostname = $argv[2];

$dns = new MyDNS();

try {
  echo "Type\tField\tValue\n";
  echo "---------------------------------\n";

  $dns->setDNSServer($server);
  for ($i = 1; $i <= 22; $i++) {
    $dns->setQueryType($i);
    $dns->doQuery($hostname);
  }
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>
