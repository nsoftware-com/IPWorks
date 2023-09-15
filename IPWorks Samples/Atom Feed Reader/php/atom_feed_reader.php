<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Atom Feed Reader</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Atom Feed Reader">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Atom Feed Reader</h2>
<p>Shows how to use the Atom component to read Atom feeds.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_atom.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyAtom extends IPWorks_Atom
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
   
	$atom1 = new MyAtom();
?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
  $atom1->setFollowRedirects(1);
  $atom1->doGetFeed($_POST["url"]);
  echo $atom1->getChannelTitle() . " - " . $atom1->getChannelSubtitle();
?>

  <table>

<?php
  for($i = 0; $i < $atom1->getEntryCount();$i++){
        echo "<tr><td><a href=\"" . $atom1->getEntryLinkHref($i) . "\">" . $atom1->getEntryTitle($i) . "</a></td>";
        echo "<td>" . $atom1->getEntryPublished($i) . "</td></tr>";
  }
?>

  </table>

<?php
}
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Feed: <td><input type=text name=url value="https://news.google.com/news?ned=us&topic=h&output=atom"  size=60>

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
