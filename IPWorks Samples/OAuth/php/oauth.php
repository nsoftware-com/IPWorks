<?php
/*
 * IPWorks 2024 PHP Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworks
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */
require_once('../include/ipworks_oauth.php');
require_once('../include/ipworks_json.php');
require_once('../include/ipworks_http.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyOAuth extends IPWorks_OAuth {
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function FireLaunchBrowser($param) {
    // Normally, the component will execute the command property to launch the browser for authorization.
    // Setting the command to an empty string will prevent a browser from opening the URL. The following 
    // line can be un-commented to exhibit this behavior.
    echo "Authorization URL: " . $param['url'] . "\n";
  }
}

if ($argc < 6) {
  echo "usage: php oauth.php clientID clientSecret serverAuthURL serverTokenURL authScope\n\n";
  echo "  clientID        the id of the client assigned when registering the application (e.g. 723966830965.apps.googleusercontent.com)\n";
  echo "  clientSecret    the secret value for the client assigned when registering the application (e.g. _bYMDLuvYkJeT_99Q-vkP1rh)\n";
  echo "  serverAuthURL   the URL of the authorization (e.g. server.https://accounts.google.com/o/oauth2/auth\n";
  echo "  serverTokenURL  the URL used to obtain the access token (e.g. https://accounts.google.com/o/oauth2/token)\n";
  echo "  authScope       the scope request or response parameter used during authorization (e.g. https://www.googleapis.com/auth/userinfo.email)\n";
  echo "\r\nExample: php oauth.php 723966830965.apps.googleusercontent.com _bYMDLuvYkJeT_99Q-vkP1rh https://accounts.google.com/o/oauth2/auth https://accounts.google.com/o/oauth2/token https://www.googleapis.com/auth/userinfo.email\n";
  return;
}

try {
  $oauth = new MyOAuth();

  /*This application demonstrates how to use the OAuth component to authenticate with Google using OAuth 2.0 (Device Profile). 
    It also demonstrates how to use the retrieved Authorization String with the HTTP and JSON components to retrieve user information. 
    It will guide you through the steps to perform authorization using OAuth. 
    Please see the Introduction page within the help for more detailed instructions.

  /*Client ID and Client Secret
    Obtain and set your Client ID and Client Secret. For Google, these values can be found in the API Console:
    https://code.google.com/apis/console#access
    The values that are given as an example are from a Google test account that we have setup for you to easily run this demo. */
  $oauth->setClientId($argv[1]);
  $oauth->setClientSecret($argv[2]);

  /*Server Auth URL, Server Token URL, and Authorization Scope
    You can also set Server Auth URL, Server Token URL, and Authorization Scope to the values desired.
    These are preset to values for Google's User Info service.*/
  $oauth->setServerAuthURL($argv[3]);
  $oauth->setServerTokenURL($argv[4]);
  $oauth->setAuthorizationScope($argv[5]);
  $authString = $oauth->doGetAuthorization();  

  echo "\nAuthorization String received:\n\n";
  echo $authString . "\n";
}
catch (Exception $ex)
{
  echo 'Error: ' . $ex->getMessage() . "\n";
}
?>