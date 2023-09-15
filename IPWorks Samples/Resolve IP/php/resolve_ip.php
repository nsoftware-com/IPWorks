<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Resolve IP</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Resolve IP">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Resolve IP</h2>
<p>Shows how to use the IPInfo component to resolve hostnames.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_ipinfo.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $ipinfo = new IPWorks_IPInfo();
?>

<?php
$host = '';
if (isset($_POST['host'])) {
  $host = $_POST["host"];
}
if ($host == "") { $host = "www.yahoo.com";}
?>

<form method=POST>
<p><dd>Host Name:
       <input type=text name=host value="<?php echo $host; ?>" size=35>
       <input type=submit value="  Resolve!  ">
</form>
<br>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $ipinfo->doResolveName($host);
  //wait for resolution
  while($ipinfo->getPendingRequests() > 0)
  	$ipinfo->doEvents();
?>

<center>
<table width="90%">
  <tr>
    <th>Real Name</th>
    <th>Primary Address</th>
    <th>Other Addresses</th>
  </tr>
  <tr>
    <td valign=top><?php echo $ipinfo->getHostName(); ?></td>
    <td valign=top><?php echo $ipinfo->getHostAddress(); ?></td>
    <td valign=top><?php echo $ipinfo->getOtherAddresses(); ?></td>
  </tr>
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
