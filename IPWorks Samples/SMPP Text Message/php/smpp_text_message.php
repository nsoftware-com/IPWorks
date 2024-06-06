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
require_once('../include/ipworks_smpp.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MySMPP extends IPWorks_SMPP
{
    function FireSSLServerAuthentication($param) {
        $param['accept'] = true;
        return $param;
      }
      
}

    if ($argc < 7) {
        echo "usage: smpp [options] server port user password buddy message\n";
        echo "Options:\n";
        echo "  -s        the type of system during a connection(some SMS servers require that a system type be supplied during connection)\n";
        echo "  server    the SMPP entity to which the component will connect\n";
        echo "  port      the server port for secure SMPP (default 2775)\n";
        echo "  user      used for identification with the SMPP service\n";
        echo "  password  the user's password\n";
        echo "  buddy     the ID or phone number of the recipient\n";
        echo "  message   the message content\n";
        echo "\nExample: php smpp_text_message.php SMPPServer 2775 username password recipient \"test message\"\n";
    } else {

        
    
        try {

            $smpp = new MySMPP();
            echo "Connecting to server\n";
            $smpp->setSMPPServer($argv[$argc - 6]);
            
            if ($argv[$argc - 5] !== "") {
                $smpp->setSMPPPort(intval($argv[$argc - 5]));
            }
            
            for ($i = 0; $i < $argc; $i++) {
                if (strpos($argv[$i], '-') === 0) {
                    if ($argv[$i] === "-s") {
                        $smpp->setSystemType($argv[$i + 1]); 
                    }
                }
            }
            
            $smpp->setUserId($argv[$argc - 4]);
            $smpp->setPassword($argv[$argc-3]);
            $smpp->doAddRecipient(0, $argv[$argc - 2]);
            $smpp->doSendMessage($argv[$argc - 1]); 

            echo "Message sent!\n";
        } catch (Exception $e) {
           echo $e->getMessage();
        }
    
        try {
            echo "Disconnecting";
            $smpp->doDisconnect();
        } catch (Exception $e) {
            echo $e->getMessage();
            exit(1);
        }           
    }
