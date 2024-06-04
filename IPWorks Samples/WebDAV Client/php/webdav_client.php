<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - WebDAV Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - WebDAV Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>WebDAV Client</h2>
<p>Allows you to copy files back and forth between a DAV server and localhost.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_webdav.php');
require_once('../include/ipworks_const.php');

?>

<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - WebDAV Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - WebDAV Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>WebDAV Client</h2>
<p>Allows you to view files in a DAV server.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_webdav.php');
require_once('../include/ipworks_const.php');

?>

<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - WEBDAV</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - WebDAV">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>WebDAV</h2>
<p>Shows how to display the directories and files of a WebDAV server at a given URL.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks.php');
require_once('../include/ipworks_webdav.php');

?>

<?php


	class MyDAV extends IPWorks_WebDAV
	{
    public $fileList = array();
		function FireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
    function FireDirList($param) {
      $this->fileList[] = array($param['displayname'],$param['contentlength'],$param['lastmodified'],$param['contenttype']); 
    }
  
	}
  $dav = new MyDAV();
	
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

  $dav->setUser($_POST["user"]);
  $dav->setPassword($_POST["password"]);

  $dav->setTimeout(60);
  $dav->setDepth(2);
  try{
  $dav->doListDirectory($_POST["server"]);
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }
?>

<p><i>Retrieving file list from DAV server...</i><p>

<center>
<table width="90%">
  <tr>
    <th>File Name</th>
    <th>File Size</th>
    <th>Modification Time</th>
  </tr>
  <?php
  for($i= 0; $i < count($dav->fileList); $i++)
  {
  ?>
    <tr>
      <?php if($dav->fileList[$i][3] == null) { ?>
        <td nowrap><?php echo $dav->fileList[$i][0], " &lt;DIR&gt"; ?></td>
      <?php } else { ?>
        <td nowrap><?php echo $dav->fileList[$i][0]; ?></td>
      <?php }   ?>    
      <td nowrap><?php echo $dav->fileList[$i][1]; ?></td>
      <td nowrap><?php echo $dav->fileList[$i][3]; ?></td>
    </tr>

  <?php
    }
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
Copyright (c) 2022 /n software inc. - All rights reserved.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks 2022 - Copyright (c) 2022 /n software inc. - All rights reserved. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>

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
Copyright (c) 2022 /n software inc. - All rights reserved.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks 2022 - Copyright (c) 2022 /n software inc. - All rights reserved. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>

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
