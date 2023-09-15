<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - RSS Aggregator</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - RSS Aggregator">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>RSS Aggregator</h2>
<p>Shows how to use the RSS component to read RSS feeds.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_rss.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyRSS extends IPWorks_RSS
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$rss1 = new MyRSS();
?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
  $rss1->setFollowRedirects(1);
  $rss1->doGetFeed($_POST["url"]);
  echo $rss1->getChannelTitle() . " - " . $rss1->getChannelDescription();
?>

  <table>

<?php
  for($i = 0; $i < $rss1->getItemCount();$i++){
        echo "<tr><td><a href=\"" . $rss1->getItemLink($i) . "\">" . $rss1->getItemTitle($i) . "</a></td>";
        echo "<td>" . $rss1->getItemPubDate($i) . "</td></tr>";
  }
?>

  </table>

<?php
}
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Feed: <td><input type=text name=url value="http://www.nsoftware.com/rss"  size=50>

 <tr><td><td><br><input type=submit value="Get Feed!">

</table>
</center>
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
