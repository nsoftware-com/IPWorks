<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - FTP Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - FTP Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>FTP Client</h2>
<p>A full featured FTP client built using the FTP component.  It allows browsing of directories, uploads and downloads of files, and more.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_ftp.php');
require_once('../include/ipworks_const.php');

?>

<?php
	class MyFTP extends IPWorks_FTP
	{
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
	}
	$ftp = new MyFTP();
?>
<form method=POST>
<center>
<table width="90%">

 <tr><td>Server:      <td><input type=text name=server value="<?php echo isset($_POST["server"])?$_POST["server"]:""; ?>" size=40>
 <tr><td>User:        <td><input type=text name=user value="<?php echo isset($_POST["user"])?$_POST["user"]:""; ?>" size=20>
 <tr><td>Password:    <td><input type=password name=password value="<?php echo isset($_POST["password"])?$_POST["password"]:""; ?>" size=20>

 <tr><td><td><input type=submit value="  Go!  ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  $ftp->setRemoteHost($_POST["server"]);

  $ftp->setUser($_POST["user"]);
  $ftp->setPassword($_POST["password"]);

  $ftp->setTimeout(60);
  try{
  $ftp->doListDirectoryLong();
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
?>

<p><i>Retrieving file list from FTP server...</i><p>

<center>
<table width="90%">
  <tr>
    <th>File Name</th>
    <th>File Size</th>
    <th>Modification Time</th>
  </tr>

<?php
  for($i= 0; $i < $ftp->getDirListCount(); $i++)
  {
?>
  <tr>
    <td nowrap><?php echo htmlspecialchars($ftp->getDirListFileName($i)); ?></td>
		<?php if ($ftp->getDirListIsDir($i)) { ?>
    	<td nowrap>&lt;dir&gt;</td>
    <?php } else { ?>
    	<td nowrap><?php echo htmlspecialchars($ftp->getDirListFileSize($i)); ?></td>
    <?php } ?>
    <td nowrap><?php echo htmlspecialchars($ftp->getDirListFileTime($i)); ?></td>
  </tr>

<?php
  }

  $ftp->doLogoff();
?>

</table>
</center>

<p><i>Disconnected from FTP server.</i>

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
