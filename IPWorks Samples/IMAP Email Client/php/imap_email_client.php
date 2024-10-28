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
require_once('../include/ipworks_imap.php');
require_once('../include/ipworks_htmlmailer.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyIMAP extends IPWorks_IMAP
{
  function fireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function firePITrail($param) {
    // echo $param['direction'] . ": " . $param['message'] . "\n";
  }
  function fireLog($param) {
    // echo $param['message'] . "\n";
  }
  function fireMailboxList($param) {
    echo $param['mailbox'] . "\n";
  }
  function fireMessageInfo($param) {
    echo $param['messageid'] . "  " . $param['subject'] . "  " . $param['messagedate'] . "  " . $param['from'] . "\n";
  }
  function fireTransfer($param) {
    echo $param['text'] . "\n";
  }
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

function displayMenu()
{
  echo "IMAP Commands:\n";
  echo "  l                   list mailboxes\n";
  echo "  s <mailbox>         select mailbox\n";
  echo "  v <message number>  view the content of selected message\n";
  echo "  n                   goto and view next message\n";
  echo "  h                   print out active message headers\n";
  echo "  ?                   display options\n";
  echo "  q                   quit\n";
}

try {

  $imap = new MyIMAP();

  if ($argc < 4)
  {

    echo "usage: imap [options] server username password\n\n";
    echo "Options: \n";
    echo "  -ssl      the SSL start mode (auto/implicit/explicit/none)\n";
    echo "  -port     the mail server port\n";
    echo "  server    the name or address of the mail server (IMAP server)\n";
    echo "  username  the user name used to authenticate to the MailServer \n";
    echo "  password  the password used to authenticate to the MailServer \n\n";
    echo "Example: imap 127.0.0.1 username password\n";
    return;
  } else {
    $imap->setMailServer($argv[$argc-3]);
    $imap->setUser($argv[$argc-2]);
    $imap->setPassword($argv[$argc-1]);
  }

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-port") {$imap->setMailPort($argv[$i + 1]);}
      if ($argv[$i] == "-ssl") {
        switch ($argv[$i + 1]) {
          case "auto":
            $imap->setSSLStartMode(0);
            break;
          case "implicit":
            $imap->setSSLStartMode(1);
            break;
          case "explicit":
            $imap->setSSLStartMode(2);
            break;
          case "none":
          default:
            $imap->setSSLStartMode(3);
            break;
        }
      }
    }
  }
  echo "Connecting...\n";
  $imap->doConnect();
  echo "Type \"?\" for a list of commands.\n";
  $msgNum = 1;

  while (true) {
    try {
      $data = input("imap> ");
      $arguments = explode(" ", $data);
      $command = $arguments[0];

      if ($command == "?" || $command == "help") {
        displayMenu();
      } elseif ($command == "q" || $command == "quit" || $command == "exit") {
        break; // fall through to disonnect
      } else if ($command == "l") {
        $imap->setMailbox("*");
        $imap->doListMailboxes();
      } else if ($command == "s") {
        if (count($arguments) > 1 ) {
          $imap->setMessageSet("");
          $imap->setMailbox($arguments[1]);
          $imap->doSelectMailbox();
        } else {
          echo "Usage: s <mailbox>\n";
        }
      } else if ($command == "v") {
        if (count($arguments) > 1 ) {
         $msgNum = $arguments[1];
         $imap->setMessageSet($arguments[1]);
         $imap->doRetrieveMessageText();
        } else {
          echo "Usage: v <message number>\n";
        }
      } else if ($command == "n") {
        $msgNum++;
        $imap->setMessageSet($msgNum);
        $imap->doRetrieveMessageText();
      } else if ($command == "h") {
        if ($imap->getMessageCount() > 0) {
          if ($imap->getMessageSet() == "") {$imap->setMessageSet("1:" . $imap->getMessageCount());}
          $imap->doRetrieveMessageInfo();
        } else {
          echo "No messages in this mailbox.\n";
        }
      } else if ($command == "") {
        // do nothing
      } else {
        echo "Command not recognized by this demo.\n";
      }
    } catch (Exception $ex) {
      echo "Error: " . $ex->getMessage() . "\n";
    }
  }
  $imap->doDisconnect();
} catch (Exception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}

?>