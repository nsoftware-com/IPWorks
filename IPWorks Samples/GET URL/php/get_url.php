<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - GET URL</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - GET URL">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>GET URL</h2>
<p>Shows how to download the content of a URL from a Web server.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_http.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyHTTP extends IPWorks_HTTP
	{
		function fireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$http = new MyHTTP();
?>

<?php
  $url = '';
  if (isset($_POST['url'])) {
    $url = $_POST["url"];
  }
	if ($url == "") {
    $url= "http://www.google.com";
  }
?>

	Displays the HTML of the web page you specify:
	<form method="post">
	Web Page: <input type="text" name="url" value="<?php echo $url ?>" size="35">
	        <input type="submit" value="  Get HTML   ">
	</form>

<?php
	if ($_SERVER['REQUEST_METHOD'] == "POST") {
		$http->setFollowRedirects(1);
		$http->setTransferredDataLimit(0);
		$http->doGet($url);
		echo "<hr> <h1> Contents of " . $url . "</h1>";
		echo $http->getTransferredData();
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
