<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - POP Email Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - POP Email Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>POP Email Client</h2>
<p>A simple email client. It shows how to use the SMTP and POP components access POP and SMTP mail servers.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_pop.php');
require_once('../include/ipworks_smtp.php');
require_once('../include/ipworks_mime.php');
require_once('../include/ipworks_htmlmailer.php');
require_once('../include/ipworks_const.php');

?>

<?php
  class MyPOP extends IPWorks_POP{
    function FireSSLServerAuthentication($param) {
      $param['accept'] = true;
      return $param;
		}
  }

  $pop = new MyPOP();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Server:      <td><input type=text name=server value="<?php echo isset($_POST["server"])?$_POST["server"]:""; ?>" size=40>
 <tr><td>User:        <td><input type=text name=user value="<?php echo isset($_POST["user"])?$_POST["user"]:""; ?>" size=20>
 <tr><td>Password:    <td><input type=password name=password value="<?php echo isset($_POST["password"])?$_POST["password"]:""; ?>" size=20>
 <tr><td nowrap>Enable SSL   <td><input type="checkbox" name="useSSL" value="1" checked>
 <SMALL>if you enable SSL please make sure you are connecting to a secure server.</SMALL>

 <tr><td><td><input type=submit value="  Go!  ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  if (!empty($_REQUEST["useSSL"])) {
    $pop->setSSLStartMode(0); //automatic
    $pop->setMailPort(995);
  }

  $pop->setMailServer($_POST["server"]);
  $pop->setUser($_POST["user"]);
  $pop->setPassword($_POST["password"]);

  try{
    $pop->doConnect();
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
?>

<p><i>Retrieving message list from POP server...</i><p>

<center>
<table width="90%">
  <tr>
    <th>Index</th>
    <th>Subject</th>
    <th>From</th>
    <th>Date</th>
  </tr>

<?php
  $pop->setMaxLines(20); //only headers

  for($i=1; $i<=$pop->getMessageCount();$i++){
    $pop->setMessageNumber($i);
    $pop->doRetrieve();
?>
  <tr>
    <td nowrap><?php echo $i ?></td>
    <td nowrap><?php echo htmlspecialchars($pop->getMessageSubject()); ?></td>
    <td nowrap><?php echo htmlspecialchars($pop->getMessageFrom()); ?></td>
    <td nowrap><?php echo htmlspecialchars($pop->getMessageDate()); ?></td>
  </tr>

<?php
  } //for loop
?>

</table>
</center>

<p><i>Disconnected from POP server.</i>

<?php
  $pop->doDisconnect();
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
