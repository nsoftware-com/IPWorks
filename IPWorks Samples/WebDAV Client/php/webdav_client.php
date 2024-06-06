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
require_once('../include/ipworks_webdav.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MyDav extends IPWorks_WebDAV{

    public $fileList = array();
    function FireSSLServerAuthentication($param){
        $param['accept'] = true;
        return $param;
    }

    function fireTransfer($param){  
        echo $param['text'];
    }

    function fireConnected($param){
        echo "Connected! \n";
    }

}

$webdav = new MyDav();


if ($argc < 3) {
    echo "usage: php webdav.php [options] username password\n";
    echo "Options: \n";
    echo "  username   the username to login\n";
    echo "  password   the password to login\n";
    echo "\nExample: php webdav.php username password\n";
} else {
    try {
        // Parse arguments into component.
        $webdav->setUser($argv[$argc - 2]);
        $webdav->setPassword($argv[$argc - 1]);
        // Process user commands.
        echo "Type \"?\" or \"help\" for a list of commands.\n";
        $command = '';
        $arguments = [];

        while (true) {
            $command = readline("webdav> ");
            $arguments = explode(' ', $command);

            if ($arguments[0] === '?' || $arguments[0] === 'help') {
                echo "Commands: \n";
                echo "  ?                                      display the list of valid commands\n";
                echo "  help                                   display the list of valid commands\n";
                echo "  make <resource uri>                    make a new directory at the specified location (ex. make localhost:443/directoryName)\n";
                echo "  move <source uri> <destination uri>    move a specified resource to a new location (ex. move localhost:443/oldFolder/file.txt localhost:443/newFolder/file.txt)\n";
                echo "  get <resource uri>                     get a specified resource\n";
                echo "  delete <resource uri>                  delete a specified resource\n";
                echo "  put <local file> <resource uri>        send data to the server\n";
                echo "  quit                                   exit the application\n";
            } elseif ($arguments[0] === "make") {

                if (count($arguments) > 1) $webdav->doMakeDirectory($arguments[1]);
                else echo "Please specify a resource URI.\n";

            } elseif ($arguments[0] === "move") {

                if (count($arguments) > 2) $webdav->doMoveResource($arguments[1], $arguments[2]);
                else echo "Please specify a source and destination URI.\n";

            } elseif ($arguments[0] === "get") {

                if (count($arguments) > 1)  $webdav->doGetResource($arguments[1]);
                else echo "Please specify a resource URI.\n";

            } elseif ($arguments[0] === "delete") {

                if (count($arguments) > 1) $webdav->doDeleteResource($arguments[1]);
                else echo "Please specify a resource URI.\n";

            } elseif ($arguments[0] === "put") {

                if (count($arguments) > 2) {
                    $webdav->setLocalFile($arguments[1]);
                    $webdav->doPutResource($arguments[2]);

                } else {
                    echo "Please specify a local file and resource URI.\n";
                }
            } elseif ($arguments[0] === "quit") {
                break;
            } elseif ($arguments[0] === "") {
                // Do nothing.
            } else {
                echo "Invalid command.\n";
            }
        }
    } catch (Exception $e) {
        echo $e->getMessage() . "\n";
    }
    echo "Press any key to exit...";
    readline();
}
?>