<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - HTML Mailer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - HTML Mailer">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>HTML Mailer</h2>
<p>Shows how to use HTMLMailer to send HTML Mail with inline images.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_htmlmailer.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyHTMLMailer extends IPWorks_HTMLMailer
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$smtp = new MyHTMLMailer();
?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $smtp->setMailServer($_POST["mailserver"]);
  $smtp->setFrom($_POST["from"]);
  $smtp->setSendTo($_POST["sendto"]);
  $smtp->setSubject($_POST["subject"]);
  $smtp->setMessageHTML($_POST["message"]);

  try{
    $smtp->doSend();
    echo "Message Sent!<hr>";
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
}
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Mail Server: <td><input type=text name=mailserver value="mail"  size=50>
 <tr><td>Sender:      <td><input type=text name=from value="me@myaddress.com" size=50>
 <tr><td>Recipient:   <td><input type=text name=sendto value="santa@north.pole" size=50>
 <tr><td>Subject:     <td><input type=text name=subject value="A Christmas Wish" size=50>

 <tr><td><td><textarea name=message cols=55 rows=15>

Dear Santa...

 </textarea>

 <tr><td><td><br><input type=submit value="Send Message!">

</table>
</center>
</form>

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
