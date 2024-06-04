<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Time Synchronization</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Time Synchronization">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Time Synchronization</h2>
<p>Shows how to get the time from a network time server and synchronize the PC clock with it.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_netclock.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $netclock = new IPWorks_NetClock();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Time Server: <td>
   <select name=server>
      <option>time.nist.gov</option>
      <option>clock-1.cs.cmu.edu</option>
      <option>clock-1.cs.cmu.edu</option>
      <option>ntp.lth.se</option>
      <option>ntp0.cornell.edu</option>
      <option>ntp2d.mcc.ac.uk</option>
      <option>ntp2c.mcc.ac.uk</option>
      <option>timelord.uregina.ca</option>
   </select>
 <tr><td><td><input type=submit value=" Get Time ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $netclock->setTimeServer($_POST["server"]);
  try{
    $netclock->doGetTime();
    echo "<hr><p><dd><b>Time Reported By Server:   " . $netclock->getLocalTime() . "</b>";
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
