<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - OData People Search</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - OData People Search">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>OData People Search</h2>
<p>Shows how to use the OData component to query a test OData V4 service.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_odata.php');
require_once('../include/ipworks_const.php');

?>


<?php
	class MyOData extends IPWorks_OData
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
   	
?>

<br />
<form method=POST>
<p>
	Filter by Last Name &nbsp; <input type = "text" name = "txtSearch" size = "20">
	<input type = "submit" name = "btnSearch" value = "Search">
</p>
			
<p />
<br />

<table>

<?php

if ($_SERVER['REQUEST_METHOD'] == "POST")
{
  $odata1 = new MyOData();
  
  $odata1->setServiceRootURI("http://services.odata.org/TripPinRESTierService/");
  $odata1->setResourcePath("People");
  $odata1->setQueryFilter("contains(LastName, '" . $_POST["txtSearch"] . "')");
  
  try
  {
  	$odata1->doQueryService();
  	
  	
  	for ($i = 0; $i <= $odata1->getEntryCount()-1; $i++)
  	{
  		$odata1->setEntryIndex($i);
		for ($j = 0; $j <= $odata1->getEntryPropertiesCount()-1; $j++)
		{
			if ($odata1->getEntryPropertiesName($j)=="FirstName") 
			{
				$firstName=$odata1->getEntryPropertiesValue($j);
			}
			if ($odata1->getEntryPropertiesName($j)=="LastName") 
			{
				$lastName=$odata1->getEntryPropertiesValue($j);
			}
			if ($odata1->getEntryPropertiesName($j)=="Emails/[1]") 
			{
				$email=$odata1->getEntryPropertiesValue($j);
			}
			if ($odata1->getEntryPropertiesName($j)=="Gender") 
			{
				$gender=$odata1->getEntryPropertiesValue($j);
			}
			if ($odata1->getEntryPropertiesName($j)=="AddressInfo/[1]/City/Name") 
			{
				$city=$odata1->getEntryPropertiesValue($j);
			}
			if ($odata1->getEntryPropertiesName($j)=="AddressInfo/[1]/City/CountryRegion") 
			{
				$country=$odata1->getEntryPropertiesValue($j);
			}
				
		
		
		}	
		
  		
  		echo "<tr>";
  		echo "<td valign=\"top\" style=\"border-style:none\">";
  		echo "<b>" . $firstName . "</b>&nbsp;&nbsp;" . "<b>" . $lastName . "</b>";
  		echo "<br />";
  		echo $email;
		echo "<br />";
  		echo $gender;
		echo "<br />";  		
  		echo $city;
		echo "<br />";
		echo $country;
		echo "<br />";
  		echo "</td>";
  		echo "</tr>";
  	}
  	
  }
  catch (Exception $ex)
  {
  	echo "<font color=\"red\">" . $ex->getMessage() . "<//font><//b><br //><br />";
  }
}
?>

</table>          

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
