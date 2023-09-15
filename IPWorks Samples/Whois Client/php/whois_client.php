<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Whois Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Whois Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Whois Client</h2>
<p>Demonstrates the use of the Whois component.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_whois.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $whois = new IPWorks_Whois();
?>

<form method=POST>
<p><dd>Domain Name:

  <input type=text name=domain value="microsoft.com" size=30>
  <input type=submit value="Search!">

</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $whois->doQuery($_POST["domain"]);

  echo "<hr size=1><pre>" . $whois->getDomainInfo() . "</pre>";

  if ($whois->getServer() != "") {
    echo "<font color=blue><i>Information for this domain is provided by " . $whois->getServer() . "</i></font>";
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
