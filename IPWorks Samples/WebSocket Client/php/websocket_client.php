<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - WebSocket Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - WebSocket Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>WebSocket Client</h2>
<p>Shows how to use the WebSocketClient component to send and receive data.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_wsclient.php');
require_once('../include/ipworks_const.php');

?>

<?php
  global $receivedData;
  $receivedData = "";
  class MyWsclient extends IPWorks_WSClient
  {
    function fireDataIn($param) {
			global $receivedData;
			$receivedData = $param['text'];
		}
  }
  
  $websocketclient = new MyWsclient();
  
	$url= (empty($_POST['url'])) ? 'ws://echo.websocket.events/.ws' : $_POST['url'];
	$dataToSend = (empty($_POST['dataToSend'])) ? 'Hello World!' : $_POST['dataToSend'];
?>

	Echoes text back from the specified WebSocket Server:
	<form method="post">
	<table>
	<tr><td>Server URL</td><td><input type="text" name="url" value="<?php echo $url; ?>" size="35"></td></tr>
	<tr><td>Data To Send</td><td><input type="text" name="dataToSend" value="<?php echo $dataToSend; ?>" size="35"></td></tr>
	<tr><td></td><td><input type="submit" value="Send Data"></td></tr>
	</table>
	</form>

<?php

if ($_SERVER['REQUEST_METHOD'] == "POST") {

  try{
	  $websocketclient->doConnectTo($url);
	  $websocketclient->doSendText($dataToSend);
	  
	  global $receivedData;
	  while($receivedData == "")
	  {
		$websocketclient->doEvents();
	  }
	  echo "<hr><b>Received: " . htmlspecialchars($receivedData) . "</b>";
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
}
?>
<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
