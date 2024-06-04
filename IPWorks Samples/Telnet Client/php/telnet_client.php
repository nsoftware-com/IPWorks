<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Telnet Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Telnet Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Telnet Client</h2>
<p>A simple demo of the Telnet component.  Shows how to use the component to build a Telnet client.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_telnet.php');
require_once('../include/ipworks_const.php');

?>

<?php
if (isset($_POST["server"])) {
  $server = $_POST["server"];
}
else {
  $server = "";
}
if (isset($_POST["user"])) {
  $user = $_POST["user"];
}
else {
  $user = "";
}
if (isset($_POST["password"])) {
  $password = $_POST["password"];
}
else {
  $password = "";
}
if (isset($_POST["command"])) {
  $command = $_POST["command"];
}
else {
  $command = "ls -l";
}

$ready = false;

class MyTelnet extends IPWorks_Telnet{
  
  function fireDataIn($param) {
    global $ready;
    if (strpos($param['text'], "Terminal type?") !== false) {//you may need to change this to match the expected output for your server
      echo "<pre>" . htmlspecialchars($param['text']) . "vt100" . "</pre><br/>";
      $this->setDataToSend("vt100\r\n");
    }
    elseif (strpos($param['text'], "login:") !== false || strpos($param['text'], "user:") !== false) {//you may need to change this to match the expected output for your server
      echo "<pre>" . htmlspecialchars($param['text']) . $_POST["user"] . "</pre><br/>";
      $this->setDataToSend($_POST["user"] . "\r\n");
    }
    elseif (strpos($param['text'], "Password:") !== false) {//you may need to change this to match the expected output for your server
      echo "<pre>" . htmlspecialchars($param['text']) . $_POST["password"] . "</pre><br/>";
      $this->setDataToSend($_POST["password"] . "\r\n");
    }
    else {
      if (strpos($param['text'], $_POST["user"] . "@" . $_POST["server"] . ":~>") !== false) { //you may need to change this to match the expected output for your server
        $ready = true;
      }
      else {
        $ready = false;
      }
      echo "<pre>" . htmlspecialchars($param['text']) . "</pre><br/>";
    }
  }

  function fireWill($param) {
    echo "SERVER: WILL OPTION " . $param['optioncode'] . "<br/>";
    if ($param['optioncode'] == 38) {
      $this->setDontOption($param['optioncode']);
      echo "CLIENT: DONT OPTION " . $param['optioncode'] . "<br/>";
    }
  }

  function fireWont($param) {
    echo "SERVER: WONT OPTION " . $param['optioncode'] . "<br/>";
  }

  function fireDo($param) {
    echo "SERVER: DO OPTION " . $param['optioncode'] . "<br/>";
    $this->setWontOption($param['optioncode']);
    echo "CLIENT: WONT OPTION " . $param['optioncode'] . "<br/>";
  }

  function fireDont($param) {
    echo "SERVER: DONT OPTION " . $param['optioncode'] . "<br/>";
  }

  function fireError($param) {
    echo "Error " . $param['errorcode'] . ": " . $param['description'] . "<br/>";
  }

  function fireConnected($param) {
    echo "Connected <br/>";
  }

  function fireDisconnected($param) {
    echo "Disconnected <br/>";
  }
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
}
$telnet = new MyTelnet();
?>

<form method=POST>
<center>
<table width="90%">


 <tr><td>Server:      <td><input type=text name=server value="<?php echo $server ?>" size=40>
 <tr><td>User:        <td><input type=text name=user value="<?php echo $user ?>" size=20>
 <tr><td>Password:    <td><input type=password name=password value="<?php echo $password ?>" size=20>
 <tr><td>Command:    <td><input type=text name=command value="<?php echo $command ?>" size=20>

 <tr><td><td><input type=submit value="  Go!  ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {
  $timeout = 30; //ten seconds less than PHP
  $end = time() + $timeout;

  try{
    echo "<hr/> <h1>Telnet Output</h1> <p>";
    $telnet->setRemoteHost($_POST["server"]);
    $telnet->doConnect();

    while ((!$telnet->getConnected() || !$ready) && time() < $end) { $telnet->doEvents(); } //wait for connection to be established and for the server to be ready

    if ($command != "" && time() < $end) {
      $ready = false;
      $telnet->doSend($_POST["command"] . "\r\n");
      while ($telnet->getConnected() && !$ready && time() < $end) { $telnet->doEvents(); } //wait for command to be processed
    }

    $telnet->doDisconnect();
  } catch (Exception $e) {
    echo "Error: " .  $e->getMessage() . "\n";
  }
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
