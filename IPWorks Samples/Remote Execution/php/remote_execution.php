<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Remote Execution</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Remote Execution">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Remote Execution</h2>
<p>Shows how to use the Rexec component for remotely executing programs on a unix machine that supports rexec.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_rexec.php');
require_once('../include/ipworks_const.php');

?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Protocol: <td>
   <select name=protocol>
     <option selected>REXEC</option>
     <option>RSHELL</option>
   </select>
 <tr><td>Command:     <td><input type=text name=command value="ls -al /" size=50>
 <tr><td>Remote Host: <td><input type=text name=host value="<?php echo isset($_POST["host"])?$_POST["host"]:""; ?>" size=30>
 <tr><td>User:        <td><input type=text name=user value="<?php echo isset($_POST["user"])?$_POST["user"]:""; ?>" size=20>
 <tr><td>Password:    <td><input type=password name=password value="<?php echo isset($_POST["password"])?$_POST["password"]:""; ?>" size=20>

 <tr><td><td><input type=submit value="  Go!  ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

   switch($_POST["protocol"]) {
    case "REXEC":
      $obj = new IPWorks_Rexec();
      $obj->setRemoteUser ($_POST["user"]);
      $obj->setRemotePassword ($_POST["password"]);
      break;
    case "RSHELL":
      $obj = new IPWorks_Rshell();
      $obj->setRemoteUser($_POST["user"]);
      break;
  }

  $obj->setRemoteHost($_POST["host"]);
  $obj->doExecute($_POST["command"]);

  echo "<hr><pre>";

  while(!$obj->EOF){
    $obj->doEvents();
    echo $obj->getStdout() . "\r\n";
  }

  echo "</pre>";

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
