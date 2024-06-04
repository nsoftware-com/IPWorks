<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - IMAP Email Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - IMAP Email Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>IMAP Email Client</h2>
<p>Shows how to use the IMAP component to access IMAP servers (e.g. MS Exchange).</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_imap.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyIMAP extends IPWorks_IMAP
	{
		function fireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
  $imap = new MyIMAP();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Server:      <td><input type=text name=server value="<?php echo isset($_POST["server"])?$_POST["server"]:""; ?>" size=40>
 <tr><td>User:        <td><input type=text name=user value="<?php echo isset($_POST["user"])?$_POST["user"]:""; ?>" size=20>
 <tr><td>Password:    <td><input type=password name=password value="<?php echo isset($_POST["password"])?$_POST["password"]:""; ?>" size=20>

 <tr><td><td><input type=submit value="  Go!  ">&emsp13;&emsp13;<label><input type="checkbox" name="useSSL" value="1" size="25"/> Use SSL </label>

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
  if (!empty($_REQUEST["useSSL"])) {
    $imap->SetSSLStartMode(0);
    $imap->setMailPort(993);
  }
  $imap->setMailServer($_POST["server"]);
  $imap->setUser($_POST["user"]);
  $imap->setPassword($_POST["password"]);

  try{
     $imap->doSelectMailbox(); //"Inbox" is always the default mailbox
  } catch (Exception $e) {
     echo 'Error: ',  $e->getMessage(), "\n";
  }
?>

<p><i>Retrieving the Inbox messages from the IMAP server...</i><p>

<center>
<table width="90%">
  <tr>
    <th>Index</th>
    <th>Subject</th>
    <th>From</th>
    <th>Date</th>
  </tr>

<?php
  $imap->setMessageSet("1:" . $imap->getMessageCount());
  $imap->doFetchMessageInfo();
  echo "Message Info Count: " . $imap->getMessageInfoCount();

  for($i= 0; $i < $imap->getMessageInfoCount();$i++){
?>
  <tr>
    <td nowrap><?php echo $i ?></td>
    <td nowrap><?php echo htmlspecialchars($imap->getMessageInfoSubject($i)) ?></td>
    <td nowrap><?php echo htmlspecialchars($imap->getMessageInfoFrom($i)) ?></td>
    <td nowrap><?php echo htmlspecialchars($imap->getMessageInfoDate($i)) ?></td>
  </tr>

<?php
  } //end for

  $imap->doDisconnect();
?>

</table>
</center>

<p><i>Disconnected from the IMAP server.</i>

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
