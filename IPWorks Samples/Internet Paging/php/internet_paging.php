<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Internet Paging</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Internet Paging">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Internet Paging</h2>
<p>A simple SNPP client showing how to send a message to an SNPP server.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_snpp.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MySNPP extends IPWorks_SNPP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$snpp = new MySNPP();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Paging Server: <td>
   <select name=server>
     <option>snpp.airtouch.com</option>
     <option>snpp.metrocall.com</option>
     <option>pecos.nextel.com</option>
     <option>pagemart.net</option>
     <option>airmessage.net</option>
     <option>epage.porta-phone.com</option>
     <option>page.propage.net</option>
     <option selected>snpp.skytel.com:7777</option>
     <option>alphanow.net</option>
   </select>
 <tr><td>Recipient:   <td><input type=text name=recipient value="" size=30>
 <tr><td>Caller Id:   <td><input type=text name=callerid value="" size=30> (the server may require this)
 <tr><td>Message:     <td><input type=text name=message value="" size=60>

 <tr><td><td><input type=submit value="  Send Page!  ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $data = explode(":",$_POST["server"]);
  $snpp->setServerName($data[0]);
  if (count($data) > 1) { $snpp->setServerPort($data[1]);}

  $snpp->setPagerId($_POST["recipient"]);
  $snpp->setCallerId($_POST["callerid"]);
  $snpp->setMessage($_POST["message"]);

  try{
    $snpp->doSend();
    echo "<b>Server Response</b>:   " . $snpp->getLastReply() . "<hr>";
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
