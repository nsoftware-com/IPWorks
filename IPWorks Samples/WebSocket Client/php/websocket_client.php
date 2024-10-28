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
require_once('../include/ipworks_wsclient.php');
require_once('../include/ipworks_const.php');
?>
<?php
global $receivedData;
$receivedData = "";

class MyWSClient extends IPWorks_WSClient
{
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function FireDataIn($param) {
    global $receivedData;
    echo "Received: '" . $param['text'] . "'\n";
    $receivedData = $param['text'];
  }
  function FireLog($param) {
    echo $param['message'] . "\n";
  }
}

if ($argc < 2) {
  echo "Usage: php websocket_client.php url\n\n";
  echo "  url: the server's URL in the format \"ws[s]://host:[port]/[URI]\"\n\n";
  echo "Example: php websocket_client.php ws://echo.websocket.events/.ws\n";
  echo "         php websocket_client.php wss://localhost:777\n";
  return;
} else {
  $url = $argv[1];
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

try {
  $websocketclient = new MyWSClient();
  $websocketclient->doConnectTo($url);
  $websocketclient->doEvents();
  echo "Type \"?\" for a list of commands.\n";
  while (true) {
    try {
      $data = input("wsclient> ");
      $arguments = explode(" ", $data);
      $command = $arguments[0];

      if ($command == "?" || $command == "help") {
        echo "Commands: \n";
        echo "  ?             display the list of valid commands\n";
        echo "  help          display the list of valid commands\n";
        echo "  send <text>   send text data to the server\n";
        echo "  quit          exit the application\n";
      } elseif ($command == "quit" || $command == "exit" ) {
        $websocketclient->doDisconnect();
        return;
      } elseif ($command == "send") {
        if (count($arguments) > 1) {
          global $receivedData;
          $receivedData = "";
          $websocketclient->doSendText(trim(substr($data, strpos($data, " "))));
          while($receivedData == "")
          {
            $websocketclient->doEvents();
          }
        } else {
          echo "Usage: send <text>\n";
        }
      }
    } catch (Exception $ex) {
      echo 'Error: ' . $ex->getMessage() . "\n";  
    }
    $websocketclient->doEvents();
  }
} catch (Exception $ex) {
  echo 'Error: ' . $ex->getMessage() . "\n";
}
?>