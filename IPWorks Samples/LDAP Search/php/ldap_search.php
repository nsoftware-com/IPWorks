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
require_once('../include/ipworks_ldap.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyLDAP extends IPWorks_LDAP
{
  function fireError($param) {
    echo "Error: " . $param['errorcode'] . "[" . $param['description'] . "].\n";
  }

  function fireSearchResult($param) {
    echo "Result DN: " . $param['dn'] . "\n";
  }
}

if ($argc < 4) {
  echo "Usage: php ldap_search.php server dn search\n";
  echo "\n";
  echo "  server:   the name or address of the LDAP server.\n";
  echo "  dn:       the distinguished name used as the base for LDAP operations.\n";
  echo "  search:   the parameters to search for.\n";
  echo "Example: php ldap_search.php ldap-master.itd.umich.edu dc=umich,dc=edu cn=*\n";
  echo "Example: php ldap_search.php ldap-master.itd.umich.edu:389 dc=umich,dc=edu cn=*\n";
  return;
}

$server = $argv[1];
$dn = $argv[2];
$search = $argv[3];

$ldap = new MyLDAP();

try {
  if (str_contains($server, ":")) {
    $serverPort = explode(":", $server, 2);
    $ldap->setServerName($serverPort[0]);
    $ldap->setServerPort($serverPort[1]);
  } else {
    $ldap->setServerName($server);
  }
  $ldap->setDN($dn);
  
  $ldap->doBind();

  if ($ldap->getResultCode() == 0) {
    echo "Connected!\n";
    $ldap->setTimeout(10);
    $ldap->doSearch($search);
  } else {
    echo "Bind failed: " . $ldap->getResultCode() . ": " . $ldap->getResultDescription() . "\n";
  }
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>
