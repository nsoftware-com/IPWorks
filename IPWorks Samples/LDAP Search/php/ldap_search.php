<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - LDAP Search</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - LDAP Search">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>LDAP Search</h2>
<p>Shows how to contact an LDAP server and lookup a name.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_ldap.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyLDAP extends IPWorks_LDAP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$ldap1 = new MyLDAP();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>LDAP Server: <td><input type=text name=server value="ldap-master.itd.umich.edu" size=30>
 <tr><td>Search DN: <td><input type=text name=basedn value="ou=security,dc=umich,dc=edu" size=30>
 <tr><td>Query:       <td><input type=text name=query value="cn=Manager" size=50>

 <tr><td><td><input type=submit value="Search!">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

	$ldap1->setServerName($_POST["server"]);

	//Some servers require you to bind first:
  //$ldap1->setDN("yourdn");
	//$ldap1->setPassword("password");
	//$ldap1->doBind();

  //Here you would check the value of ResultCode to determine if Bind was successful. A non-zero value indicates failure and ResultDescription will hold the error text.

  //set the required attributes, no attributes will result in the server returning all attributes
  // $ldap1->setAttrCount = 1;
  // $ldap1->setAttrType(0,"mail"); //email for addresses etc...

	$ldap1->setDN($_POST["basedn"]);
	$ldap1->doConfig("SingleResultMode=true");
	$ldap1->doSearch($_POST["query"]);

	echo "<hr><pre>";

  $result = $ldap1->getResultDN();
  while($result != "") {
    	echo $result . "<br>";
    	//The attributes of each entry are in the AttrType and AttrValue property arrays
      for($i =0;$i<$ldap1->getAttrCount();$i++){
				echo "&nbsp;&nbsp;&nbsp;" . $ldap1->getAttrType($i) . ": " . $ldap1->getAttrValue($i) . "<br>";
			}
    $result = $ldap1->getResultDN();
	}
  echo "</pre>";

  //After you find the DN you want, use it to bind with:
  //$ldap1->setDN($ldap1->getResultDN());
  //$ldap1->setPassword("yourpassword");
  //$ldap1->doBind();
  //echo "Bind Result: " . $ldap1->getResultCode() . "<br>";
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
