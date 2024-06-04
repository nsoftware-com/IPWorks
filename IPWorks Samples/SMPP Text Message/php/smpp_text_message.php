<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - SMPP Text Message</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - SMPP Text Message">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>SMPP Text Message</h2>
<p>A short demo showing how to use the SMPP component to send text messages to an SMS-compliant device (e.g. cell phone).</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_smpp.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MySMPP extends IPWorks_SMPP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$smpp = new MySMPP();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Server:   <td><input type=text name=smppserver value="" size=30>
 <tr><td>SMPP Port:   <td><input type=text name=smppport value="2775" size=30>
 <tr><td>User Id:   <td><input type=text name=userid value="" size=30>
 <tr><td>Password:   <td><input type=text name=password value="" size=30>
 <tr><td>System Type (optional):   <td><input type=text name=systemtype value="" size=30>
 <tr><td>Recipient:   <td><input type=text name=recipient value="" size=30>

 <tr><td>Message:     <td><input type=text name=message value="hello from asp!" size=60>

 <tr><td><td><input type=submit value="  Send Page!  ">

</table>
</center>
</form>

<?php

if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $smpp->setSMPPServer($_POST["smppserver"]);
  $smpp->setSMPPPort($_POST["smppport"]);
  $smpp->setUserId($_POST["userid"]);
  $smpp->setPassword($_POST["password"]);
  $smpp->setSystemType($_POST["systemtype"]);
  $smpp->doAddRecipient(0, $_POST["recipient"]);

  try{
    $smpp->doConnect($smpp->getUserId(), $smpp->getPassword());
    $id = $smpp->doSendMessage($_POST["message"]);
    echo "<b>Message sent successfully.  Message Id:" . $id . "</b><hr>";
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
