<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - MX Query</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - MX Query">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>MX Query</h2>
<p>Shows how to query a DNS server for a list of servers accepting email for a specific email address.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_mx.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $mx = new IPWorks_MX();
?>

<?php
$email = '';
if (isset($_POST['email'])) {
  $email = $_POST["email"];
}
if ($email == "") { $email = "billg@microsoft.com";}

$dnsserver = '';
if (isset($_POST['dnsserver'])) {
  $dnsserver = $_POST["dnsserver"];
}
if ($dnsserver == "") { $dnsserver = $mx->getDNSServer();}
?>

<p>This sends a request to a DNS server to check an email address.

<form method=POST>
<center>
<table width="90%">
   <tr><td>Email Address:           <td><input type=text name=email value="<?php echo $email; ?>" size=40>
   <tr><td>DNS Server:              <td><input type=text name=dnsserver value="<?php echo $dnsserver; ?>" size=25>
   <tr><td><td><input type=submit value="  Verify!  ">
</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $mx->setDNSServer($_POST["dnsserver"]);

  //this verifies the domain
  $mx->doResolve($_POST["email"]);

?>
<dd>Verification Status: <?php echo $mx->getStatus(); ?><p>
<dd>Default Mail Server: <?php echo $mx->getMailServer(); ?><p>

<?php
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
