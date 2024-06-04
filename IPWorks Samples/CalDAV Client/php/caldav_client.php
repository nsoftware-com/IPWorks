<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - CalDAV Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - CalDAV Client">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>CalDAV Client</h2>
<p>Shows how to use the CalDAV component to get, add, and delete events from an existing calendar on Yahoo or Google.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_caldav.php');
require_once('../include/ipworks_const.php');

?>

<?php
require_once('../include/ipworks_oauth.php');
?>
<?php

	class MyDAV extends IPWorks_CalDAV
	{
    public $fileList = array();
		function fireSSLServerAuthentication($param) {
			$param['accept'] = true;
			return $param;
		}
    function fireConnected($param) {
      echo "<p><i>Retrieving file list from CalDAV server...</i><p>";
    }
    function fireEventDetails($param) {
      $this->fileList[] = array($this->getSummary(), $this->getStartDate(), $this->getLocation(), $param['resourceuri']); 
    }
  
	}

?>
<form method=POST>
<center>
<table width="90%">
  <tr><td>Username:      </td><td><input type="text" name="txtUser" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtUser']; } else { echo ''; } ?>" /></td>

  <tr><td>Client Profile: </td><td><input type = "text" name = "txtClientId" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtClientId']; } else { echo ''; } ?>" /></td>
  <tr><td>Client Secret: </td><td><input type = "text" name = "txtClientSecret" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtClientSecret']; } else { echo ''; } ?>" /></td>
  <tr><td>Server Auth URL: </td><td><input type = "text" name = "txtServerAuthURL" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtServerAuthURL']; } else { echo 'https://accounts.google.com/o/oauth2/auth'; } ?>" /></td>
  <tr><td>Server Token URL: </td><td><input type = "text" name = "txtServerTokenURL" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtServerTokenURL']; } else { echo 'https://oauth2.googleapis.com/token'; } ?>" /></td>
  <tr><td>Authorization Scope: </td><td><input type = "text" name = "txtAuthScope" size = "50" value = "<?php if(array_key_exists('code', $_GET)){ echo $_COOKIE['txtAuthScope']; } else { echo 'https://www.googleapis.com/auth/calendar'; } ?>" /></td>
  <tr><td><td><input type=submit value="  Go!  ">

</table>
</center>
</form>

<?php
$caldav1 = new MyDAV();
$oauth1 = new IPWorks_OAuth();

// Setup the ReturnURL
if ($_SERVER['HTTPS'] == 'ON') {
  $pageUrl = 'https://';
} else {
  $pageUrl = 'http://';
}
$pageUrl .= $_SERVER['HTTP_HOST'];
$pageUrl .= $_SERVER['PHP_SELF'];

try
{ 
  if ($_SERVER['REQUEST_METHOD'] == 'POST')
  {
    // Authorize button was clicked. Obtain and redirect to authorization URL.
    if (array_key_exists("txtClientId", $_POST))
      {
      // Add these values to cookies
      setcookie('txtUser', $_POST['txtUser'], time() + 900); 
      setcookie('txtClientId', $_POST['txtClientId'], time() + 900);  
      setcookie('txtClientSecret', $_POST['txtClientSecret'], time() + 900);
      setcookie('txtServerAuthURL', $_POST['txtServerAuthURL'], time() + 900);
      setcookie('txtServerTokenURL', $_POST['txtServerTokenURL'], time() + 900);
      setcookie('txtAuthScope', $_POST['txtAuthScope'], time() + 900);
      
      $oauth1->setClientProfile(1);
      $oauth1->setClientId($_POST['txtClientId']);
      $oauth1->setClientSecret($_POST['txtClientSecret']);
      $oauth1->setServerAuthURL($_POST['txtServerAuthURL']);
      $oauth1->setServerTokenURL($_POST['txtServerTokenURL']);
      $oauth1->setAuthorizationScope($_POST['txtAuthScope']);
      $oauth1->setReturnURL($pageUrl);
      $url = $oauth1->doGetAuthorizationURL();
      header('Location: '.$url);
      exit();
    } 

  }
  else
  {
	// Redirected from authorization page. Get authorization string and retrieve user information. 
	if (array_key_exists('code', $_GET))
	{
	  $oauth1->setClientProfile(1);
	  $oauth1->setClientId($_COOKIE['txtClientId']);
	  $oauth1->setClientSecret($_COOKIE['txtClientSecret']);
	  $oauth1->setServerAuthURL($_COOKIE['txtServerAuthURL']);
	  $oauth1->setServerTokenURL($_COOKIE['txtServerTokenURL']);
	  $oauth1->setAuthorizationScope($_COOKIE['txtAuthScope']);
    $oauth1->setAuthorizationCode($_GET['code']); 
	  $oauth1->setReturnURL($pageUrl);

	  $authString = $oauth1->doGetAuthorization();
	
	  // Display user info for the authenticated client.
    $caldav1->setAuthScheme(6);
    $caldav1->setUser($_COOKIE['txtUser']);
	  $caldav1->setAuthorization($authString);
    $caldav1->doGetCalendarReport("https://apidata.googleusercontent.com/caldav/v2/" . $_COOKIE['txtUser'] . "/events");

		
    }
	else if (array_key_exists('error', $_GET))
	{
	  
	  echo '<b>ERROR: ' . $_GET['error'] . '</b>';
	  
	}

  }
}
catch (Exception $ex)
{
  echo '<font color="red">' . $ex->getMessage() . '</font></b><br /><br />';
}

?>

</form>

<center>
<table width="90%">
  <tr>
    <th>Summary</th>
    <th>Start Date</th>
    <th>Location</th>
    <th>ResourceURI</th>
  </tr>
  <?php
  for($i= 0; $i < count($caldav1->fileList); $i++)
  {
  ?>
    <tr>
      <td nowrap><?php echo $caldav1->fileList[$i][0]; ?></td> 
      <td nowrap><?php echo $caldav1->fileList[$i][1]; ?></td>  
      <td nowrap><?php echo $caldav1->fileList[$i][2]; ?></td>
      <td nowrap><?php echo $caldav1->fileList[$i][3]; ?></td>
    </tr>

  <?php
    }
  ?>


</table>
</center>
<br/>
<br/>
<p><i>Disconnected from CalDAV server.</i>

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
