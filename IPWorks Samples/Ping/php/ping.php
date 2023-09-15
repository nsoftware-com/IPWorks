<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Ping</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Ping">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Ping</h2>
<p>Shows how to use the Ping component to check whether a host is accessible.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_ping.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $ping = new IPWorks_Ping();
?>

<form method=POST>

<center>
<table width="90%">
<tr><td>Host Name/Address: <td><input type=text name=host value="www.yahoo.com" size=50>
<tr><td>Timeout (seconds): <td><input type=text name=timeout value="10" size=5>
<tr><td><td><input type=submit value="   Ping Now!   ">
</table>
</center>

</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $ping->doConfig("UseICMPDLL=true");
  $ping->setTimeout($_POST["timeout"]);

?>

<hr>
<center>
<table width="90%">
  <tr>
    <th>Response Source</th>
    <th>Response Time (ms)</th>
  </tr>

<?php
  for($i = 0; $i < 5; $i++){
  	$ping->doPingHost($_POST["host"]);
?>

  <tr>
    <td><?php echo $ping->getResponseSource(); ?></td>
    <td><?php echo $ping->getResponseTime(); ?></td>
  </tr>

<?php
  } //for loop
?>

</table>
</center>

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
