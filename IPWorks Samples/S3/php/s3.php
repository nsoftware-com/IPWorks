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
require_once('../include/ipworks_s3.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyS3 extends IPWorks_S3
{
  function fireBucketList($param) {
    echo $param['bucketname']."\n";
  }
  function fireObjectList($param) {
    echo $param['objectname']."\n";			
  }
  function fireError($param){
    echo $param['errorcode']." ". $param['description'];
  }
}

$s3 = new MyS3();
function print_menu()
{
    echo "Commands:\n";
    echo "?                     display the list of valid commands\n";
    echo "help                  display the list of valid commands\n";
    echo "cd <bucket>           change to the specified bucket\n";
    echo "lb                    list all buckets\n";
    echo "ls (or lo)            list all objects in the currently selected bucket\n";
    echo "get <object>          get the specified object\n";
    echo "put <name> <file>     create a new object in the currently selected bucket\n";
    echo "rm <object>           delete the specified object\n";
    echo "quit                  exit the application\n";
}

$buffer = "";
echo "\n0     -   Amazon S3\n";
echo "1     -   Digital Ocean\n";
echo "2     -   Google Storage\n";
echo "3     -   Wasabi\n";
echo "4     -   Backblaze B2\n";
echo "5     -   Huawei\n";
echo "6     -   Alibaba\n";
echo "7     -   IBM\n";
echo "8     -   Oracle\n";
echo "9     -   Linode\n";
echo "255   -   Custom\n\n";

try {    
  $s3->setServiceProvider((int)readline("Select Service Provider (enter number): "));
  if ($s3->getServiceProvider() == 255) {  // Custom service Provider
      $s3->config("URL=" . readline("Custom URL: "));
  } elseif ($s3->getServiceProvider() == 8) {  // Custom service Provider
      $s3->config("OracleNamespace" . readline("Oracle Namespace: "));
  }   
  $s3->setAccessKey(readline("Access Key: "));
  $s3->setSecretKey(readline("Secret Key: "));    

  echo "\nBucket List:\n";
  $s3->doListBuckets();
  print_menu();
  while (true) {
    try {
      $buffer = readline("\ns3> ");
      $arguments = explode(" ", $buffer);
      $command = strtolower($arguments[0]);

      if ($command == "q" || $command == "exit" || $command == "quit") {
        return;
      } elseif ($command == "?" || $command == "help") {
        print_menu();
      } elseif ($command == "cd") {
        if (count($arguments) > 1) {
          $s3->setBucket($arguments[1]);
        }
      } elseif ($command == "lb") {
        $s3->doListBuckets();
      } elseif ($command == "lo" || $command == "ls") {
        $s3->doListObjects();
      } elseif ($command == "get") {
        if (count($arguments) > 1) {
          $s3->setLocalFile("");
          $s3->doGetObject($arguments[1]);
          echo "The content of the object:\n" . ($s3->getObjectData()) . "\n";
        }
      } elseif ($command == "put") {
        if (count($arguments) > 2) {
          $s3->setLocalFile($arguments[2]);
          $s3->doCreateObject($arguments[1]);
          echo "Object Created.\n";
        }
      } elseif ($command == "rm") {
        if (count($arguments) > 1) {
          $s3->doDeleteObject($arguments[1]);
          echo "Object Deleted.\n";
        }
      } else {
        echo "Command Not Recognized.\n";
      }
    } catch (Exception $e) {
      echo "Error: " . $e->getMessage() . "\n";
    }
  }
} catch (IPWorksError $e) {
    fireError($e);
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}

?>