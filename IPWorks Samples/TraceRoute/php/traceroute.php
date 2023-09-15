<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - TraceRoute</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - TraceRoute">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>TraceRoute</h2>
<p>Demonstrates the use of the TraceRoute component.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_traceroute.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $traceroute = new IPWorks_TraceRoute();
?>

<?php
$host = '';
if (isset($_POST['host'])) {
  $host = $_POST["host"];
}
if ($host == "") { $host = "www.yahoo.com"; }
?>

<form method=POST>

<center>
<table width="90%">
<tr><td>Host Name/Address: <td><input type=text name=host value="<?php echo $host; ?>" size=50>
<tr><td>Timeout (seconds): <td><input type=text name=timeout value="10" size=5>
<tr><td>Resolve Names:     <td><input type=checkbox name=resolve> (This could take a long time - a larger timeout may be needed.)
<tr><td><td><input type=submit value="   Trace Now!   ">
</table>
</center>

</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $traceroute->doConfig("UseICMPDLL=true");
  $traceroute->setTimeout($_POST["timeout"]);

  if ($_POST["resolve"] == "on") { $traceroute->setResolveNames(true);}

  try{
    $traceroute->doTraceTo($host);
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
?>

<hr>
<center>
<table width="90%">
  <tr>
    <th>Hop</th>
    <th>Hop Address</th>
    <th>Hop Host Name</th>
    <th>Hop Time (ms)</th>
  </tr>

<?php for($i=0; $i < $traceroute->getHopCount(); $i++) { ?>

  <tr>
    <td><?php echo $i ?></td>
    <td><?php echo $traceroute->getHopHostAddress($i) ?></td>
    <td><?php echo $traceroute->getHopHostName($i) ?></td>
    <td><?php echo $traceroute->getHopTime($i) ?></td>
  </tr>

<?php } ?>

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
