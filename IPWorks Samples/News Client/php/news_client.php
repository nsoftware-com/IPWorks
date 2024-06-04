<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - News Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - News Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>News Client</h2>
<p>A simple news client using the NNTP component.  The news client can be used to connect to Usenet News servers to view, post and reply to messages.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_nntp.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyNNTP extends IPWorks_NNTP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
  $nntp = new MyNNTP();
?>

Please select a newsgroup to list from the list box below:

<p><i>Retrieving newsgroup list from nntp.aioe.org...</i><p>

<form method=POST>
<select name=newsgroup>
<?php

$nntp->setNewsServer("nntp.aioe.org");
$nntp->setNewsPort(119);
$nntp->setTimeout(60);
try{
  $nntp->doListGroups();
} catch (Exception $e) {
  echo 'Error: ',  $e->getMessage(), "\n";
}

for($i = 0;$i < $nntp->getGroupListCount(); $i++){
	if ($i==0) {
		echo "<option selected>" . $nntp->getGroupListGroup($i) . "</option>";
	} else {
		echo "<option>" . $nntp->getGroupListGroup($i) . "</option>";
	}
}

?>
</select>

<input type=submit value="List Articles">
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
?>

<p><i>Retrieving article list for <?=$_POST["newsgroup"]?>...</i><p>

<center>
<table width="90%">
  <tr>
    <th>Article</th>
    <th>Date</th>
    <th>Subject</th>
  </tr>

<?php
	$nntp->setCurrentGroup($_POST["newsgroup"]);
	$nntp->setOverviewRange($nntp->getLastArticle() - 100 . "-" . $nntp->getLastArticle());

  try{
    $nntp->doGroupOverview();
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }

	for($i = 0; $i < $nntp->getGroupOverviewCount(); $i++) {
		$subject = $nntp->getGroupOverviewSubject($i);
    if(strlen($subject) > 30)
    {
      $subject = substr($subject,0,30) . "...";
    }
?>

  <tr>
    <td nowrap><?php echo $nntp->getGroupOverviewArticleNumber($i); ?></td>
    <td nowrap><?php echo htmlspecialchars($nntp->getGroupOverviewDate($i)); ?></td>
    <td nowrap><?php echo htmlspecialchars($subject); ?></td>
  </tr>

<?php
	} //for loop
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
