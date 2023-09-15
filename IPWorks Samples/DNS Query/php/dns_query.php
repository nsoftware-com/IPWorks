<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - DNS Query</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - DNS Query">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>DNS Query</h2>
<p>Shows how to use the DNS component to perform queries for a wide range of DNS record types.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_dns.php');
require_once('../include/ipworks_const.php');

?>

<?php
  class MyDns extends IPWorks_DNS
  {
    function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
  }
    
?>

<form method=POST>
<center>
<table width="90%">
<tr><td>DNS Server: <td><input type=text name=server value="8.8.8.8" size=50>
<tr><td>Host Name/Address: <td><input type=text name=domain value="yahoo.com" size=50>
<tr><td>Query Type:<td>
   <select name=querytype>
     <option>A</option>
     <option>MX</option>
     <option>NS</option>
     <option>SOA</option>
   </select>
<tr><td><td><input type=submit value="Query">
</table>
</center>

</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
  $dns1 = new MyDns();
  switch($_POST["querytype"]){
  	case "A":
  	  $dns1->setQueryType(1);
  	  break;
  	case "NS":
  	  $dns1->setQueryType(2);
  	  break;
  	case "SOA":
  	  $dns1->setQueryType(6);
  	  break;
  	case "MX":
  	  $dns1->setQueryType(15);
  	  break;
  }

  $dns1->setDNSServer($_POST["server"]);
  $dns1->doQuery($_POST["domain"]);

  $dns1->setRecordSource(0); //only the Answer section:
?>

<hr>
<center>
<table width="90%">
<?php
for($i = 0;$i < $dns1->getRecordCount(); $i++){
  echo "<tr>";
  for($j = 0;$j < $dns1->getRecordFieldCount($i); $j++){
    $dns1->setRecordFieldIndex($i,$j);
    $myVar = 0;
    echo "<td>" . $dns1->getRecordFieldName($i) . "</td><td>" . $dns1->getRecordFieldValue($i) . "</td>";
  }
  echo "</tr>";
}
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
