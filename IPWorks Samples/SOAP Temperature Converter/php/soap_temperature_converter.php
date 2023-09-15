<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - SOAP Temperature Converter</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - SOAP Temperature Converter">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>SOAP Temperature Converter</h2>
<p>Uses the SOAP component and a freely available Web Service to do temperature conversion.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_soap.php');
require_once('../include/ipworks_const.php');

?>

<?php
require_once('../include/ipworks_soap.php');
?>

<?php
	class MySOAP extends IPWorks_SOAP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$soap1 = new MySOAP();
	$fahrenheitValue="";
	$celsiusValue="";
	
	$soap1->doReset();            
	$soap1->doConfig("MethodNamespacePrefix=");
	$soap1->setURL("https://www.w3schools.com/xml/tempconvert.asmx");
	$soap1->setMethodURI("https://www.w3schools.com/xml/");
	
	if (isset($_POST['btnFahrenheit'])) {
        # Fahrenheit-button was clicked
		$soap1->setMethod("CelsiusToFahrenheit");
		$soap1->doAddParam("Celsius", $_POST["txtCelsius"]);

		$soap1->setActionURI($soap1->getMethodURI() . $soap1->getMethod());

		try
		{       
			$soap1->doSendRequest();
		}
		catch (Exception $e)
		{
			echo 'Error: ',  $e->getMessage(), "\n";
		}
    
		$soap1->setXPath("/Envelope/Body/CelsiusToFahrenheitResponse/CelsiusToFahrenheitResult");
		$fahrenheitValue = $soap1->getXText();
		$celsiusValue=$_POST["txtCelsius"];
    }
    elseif (isset($_POST['btnCelsius'])) {
        # Celsius-button was clicked
		$soap1->setMethod("FahrenheitToCelsius");
		$soap1->doAddParam("Fahrenheit", $_POST["txtFahrenheit"]);

		$soap1->setActionURI($soap1->getMethodURI() . $soap1->getMethod());

		try
		{       
			$soap1->doSendRequest();
		}
		catch (Exception $e)
		{
			echo 'Error: ',  $e->getMessage(), "\n";
		}
    
		$soap1->setXPath("/Envelope/Body/FahrenheitToCelsiusResponse/FahrenheitToCelsiusResult");
		$celsiusValue = $soap1->getXText();
		$fahrenheitValue=$_POST["txtFahrenheit"];
    }
	
?>
<br/>
<br/>
<FORM method='post'>
	Fahrenheit: &nbsp;
	<INPUT name='txtFahrenheit' size=20 value='<?php echo $fahrenheitValue;?>'> &nbsp;
	<INPUT type=submit name='btnFahrenheit' value="<">
	<INPUT type=submit name='btnCelsius' value=">">&nbsp;
	Celsius: &nbsp;
	<INPUT name='txtCelsius' size=20 value='<?php echo $celsiusValue;?>'>
</FORM>

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
