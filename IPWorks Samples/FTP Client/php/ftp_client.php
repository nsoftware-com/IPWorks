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
require_once('../include/ipworks_ftp.php');
require_once('../include/ipworks_const.php');
?>
<?php

class MyFTP extends IPWorks_FTP
{
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function FireDirList($param) {
    echo $param['direntry'] . "\n";
  }
  function FireTransfer($param) {
    // echo $param['percentdone'] . "%\n";
  }
  function FireError($param) {
    echo $param['description'] . "\n";
  }
  function FirePITrail($param) {
    // echo $param['direction'] . ": " . $param['message'] . "\n";
  }
}

if ($argc < 4) {
  echo "Usage: php ftp_client.php host user pass\n\n";
  echo "  host: the host to connect to\n";
  echo "  user: the username to use for authentication\n";
  echo "  pass: the password to use for authentication\n\n";
  echo "Example: php ftp_client.php 192.168.1.2 myusername mypassword\n";
  return;
} else {
  $remoteHost = $argv[1];
  $user = $argv[2];
  $pass = $argv[3];
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

try {

  $ftp = new MyFTP();
  $ftp->setRemoteHost($remoteHost);
  $ftp->setUser($user);
  $ftp->setPassword($pass);

  $ftp->doLogon();
  echo "Type \"?\" for a list of commands.\n";

  while (true) {
    try {
      $ftp->setRemoteFile("");

      $data = input("ftp> ");
      $arguments = explode(" ", $data);
      $command = $arguments[0];

      if ($command == "?" || $command == "help") {
        echo "?       cd      ls      pwd\n";
        echo "get     put     rm      passive\n";
        echo "mkdir   rmdir   mv      exit\n";
      } elseif ($command == "bye" || $command == "quit" || $command == "exit") {
        $ftp->doLogoff();
        break;
      } elseif ($command == "cd") {
        if (count($arguments) > 1) {
          $ftp->doChangeRemotePath($arguments[1]);
        } else {
          echo "Usage: cd <path>\n";
        }
      } elseif ($command == "get") {
        // get <remotefile> [<localfile>]
        if (count($arguments) < 2) {
          $ftp->setRemoteFile(input("Remote File: "));
          $ftp->setLocalFile(input("Local File: "));
        } elseif (count($arguments) == 2) {
          $ftp->setRemoteFile($arguments[1]);
          $ftp->setLocalFile($arguments[1]);
        } else {
          $ftp->setRemoteFile($arguments[1]);
          $ftp->setLocalFile($arguments[2]);
        }
        $ftp->doDownload();
        echo "File downloaded\n";
      } elseif ($command == "put") {
        // put <localfile> <remotefile>
        if (count($arguments) < 2) {
          $ftp->setLocalFile(input("Local File: "));
          $ftp->setRemoteFile(input("Remote File: "));
        } elseif (count($arguments) == 3) {
          $ftp->setLocalFile($arguments[1]);
          $ftp->setRemoteFile($arguments[2]);
        } else {
          echo "Usage: put <localFile> <remoteFile>\n";
          continue;
        }
        $ftp->doUpload();
        echo "File uploaded\n";
      } elseif ($command == "ls") {
        if (count($arguments) > 1) {
          $pathname = $ftp->doQueryRemotePath();
          $ftp->doChangeRemotePath($arguments[1]);
          $ftp->doListDirectoryLong();
          $ftp->doChangeRemotePath($pathname);
        } else {
          $ftp->doListDirectoryLong();
        }
      } elseif ($command == "pwd") {
        echo $ftp->doQueryRemotePath() . "\n";
      } elseif ($command == "mkdir") {
        if (count($arguments) > 1) {
          $ftp->doMakeDirectory($arguments[1]);
        } else {
          echo "Usage: mkdir <directory>\n";
        }
      } elseif ($command == "rmdir") {
        if (count($arguments) > 1) {
          $ftp->doRemoveDirectory($arguments[1]);
        } else {
          echo "Usage: rmdir <directory>\n";
        }
      } elseif ($command == "rm" || $command == "delete") {
        if (count($arguments) > 1) {
          $ftp->doDeleteFile($arguments[1]);
        } else {
          echo "Usage: rm <remoteFile>\n";
        }
      } elseif ($command == "mv") {
        if (count($arguments) > 2) {
          $ftp->setRemoteFile($arguments[1]);
          $ftp->doRenameFile($arguments[2]);
        } else {
          echo "Usage: mv <filename> <newFileName>\n";
        }
      } elseif ($command == "passive") {
        if (count($arguments) > 1) {
          if (($arguments[1] == "on") && !$ftp->getPassive()) {
            $ftp->setPassive(true);
            echo "Passive mode ON\n";
          } elseif (($arguments[1] == "off") && $ftp->getPassive()) {
            $ftp->setPassive(false);
            echo "Passive mode OFF\n";
          }
        } else {
          echo "Usage: passive <on/off>\n";
        }
      } elseif ($command == "") {
        // do nothing
      } else {
        echo "Bad command / Not implemented in demo.\n";
      }
    } catch (Exception $e) {
      echo "Error: " . $e->getMessage() . "\n";
    }
  }

} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}

input("\npress <return> to continue...");

?>