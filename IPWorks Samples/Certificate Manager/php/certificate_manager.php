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
require_once('../include/ipworks_certmgr.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyCertMgr extends IPWorks_CertMgr
{
  function fireCertList($param) {
    echo $param['certsubject'] . "\n";
  }
  function fireError($param) {
    echo "Error: " . $param['description'] . "\n";
  }
}

if ($argc < 2) {
  echo "Usage: php certificate_manager.php filename [password]\n\n";
  echo "  filename: the path to the file containing certificates and optional private keys\n";
  echo "  password: the password for the certificate store file. If test file is used, set the password to \"test\"\n\n";
  echo "Example: php certificate_manager.php test.pfx test\n";
  return;
} else {
  try {
    $certmgr = new MyCertMgr();
    $certmgr->setCertStoreType(99); // auto
    $certmgr->setCertStore($argv[1]);
    if ($argc > 2) { $certmgr->setCertStorePassword($argv[2]);}
    echo "Listing certificates in " . $argv[1] . "\n";
    $certmgr->doListStoreCertificates();
  } catch (Exception $e) {
    echo "Cannot open certificate store!\n" . $e->getMessage();
  }
}
?>