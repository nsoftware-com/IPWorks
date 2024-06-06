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
require_once('../include/ipworks_htmlmailer.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyHTMLMailer extends IPWorks_HTMLMailer
{
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
  function FirePITrail($param) {
    echo $param['direction'] . ": " . $param['message'] . "\n";
  }
}

try {

  $htmlmailer = new MyHTMLMailer();
  if ($argc < 5)
  {
    echo "usage: html_mailer.php [options] server port from to\n\n";
    echo "Options: \n";
    echo "  -s      the subject of the mail message\n";
    echo "  -m      the HTML version of the message content\n";
    echo "  -a      the path of file to attach to the message\n";
    echo "  -u      the username for the mail server\n";
    echo "  -p      the password for the mail server\n";
    echo "  -ssl    the SSL start mode (auto/implicit/explicit/none)\n";
    echo "  server  the name or address of a mail server (mail relay)\n";
    echo "  port    the port to connect to on the mail server (mail relay)\n";
    echo "  from    the email address of the sender\n";
    echo "  to      a comma separated list of addresses for destinations\n\n";
    echo "Example: htmlmailer -s test -m \"<b>Hello</b>, my name is <i>Tom</i>\" -a FileToAttach mail.local 587 sender@mail.com recipient@mail.local\n";
    return;
  } else {
    $htmlmailer->setMailServer($argv[$argc-4]);
    $htmlmailer->setMailPort($argv[$argc-3]);
    $htmlmailer->setFrom($argv[$argc-2]);
    $htmlmailer->setSendTo($argv[$argc-1]);
  }

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-s") {$htmlmailer->setSubject($argv[$i + 1]);}
      if ($argv[$i] == "-m") {$htmlmailer->setMessageHTML($argv[$i + 1]);}
      if ($argv[$i] == "-a") {$htmlmailer->doAddAttachment($argv[$i + 1]);}
      if ($argv[$i] == "-u") {$htmlmailer->setUser($argv[$i + 1]);}
      if ($argv[$i] == "-p") {$htmlmailer->setPassword($argv[$i + 1]);}
      if ($argv[$i] == "-ssl") {
        switch ($argv[$i + 1]) {
          case "auto":
            $htmlmailer->setSSLStartMode(0);
            break;
          case "implicit":
            $htmlmailer->setSSLStartMode(1);
            break;
          case "explicit":
            $htmlmailer->setSSLStartMode(2);
            break;
          case "none":
          default:
            $htmlmailer->setSSLStartMode(3);
            break;
        }
      }
    }
  }

  echo "Sending message...\n";
  $htmlmailer->doSend();
  echo "Message sent successfully!\n";

} catch (Exception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}
?>