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
require_once('../include/ipworks_soap.php');
require_once('../include/ipworks_const.php');
?>
<?php
class MySOAP extends IPWorks_SOAP
{
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
}

function input($prompt) {
  echo $prompt;
  $handle = fopen("php://stdin", "r");
  $data = trim(fgets($handle));
  fclose($handle);
  return $data;
}

function printCommands() {
  echo "?                     display the list of valid commands\n";
  echo "help                  display the list of valid commands\n";
  echo "f2c <temperature>     convert Fahrenheit value to Celsius\n";
  echo "c2f <temperature>     convert Celsius value to Fahrenheit\n";
  echo "quit                  exit the application\n";
}

$soap1 = new MySOAP();

echo "This sample console application converts temperature units using SOAP calls.\n";
// echo "Type \"?\" or \"help\" for a list of commands.\n";
printCommands();

while (true) {
  $soap1->doReset();
  $soap1->doConfig("MethodNamespacePrefix=");
  $soap1->setURL("https://www.w3schools.com/xml/tempconvert.asmx");
  $soap1->setMethodURI("https://www.w3schools.com/xml/");

  $data = input("soap>");
  $arguments = explode(" ", $data);
  $command = $arguments[0];

  if ($command == "?" || $command == "help") {
    printCommands();
  } else if ($command == "f2c") {

    $soap1->setMethod("FahrenheitToCelsius");
    $soap1->doAddParam("Fahrenheit", $arguments[1]);
    $soap1->setActionURI($soap1->getMethodURI() . $soap1->getMethod());
    try {
      $soap1->doSendRequest();
    } catch (Exception $e) {
      echo 'Error: ' . $e->getMessage() . "\n";
    }
    
    $soap1->setXPath("/Envelope/Body/FahrenheitToCelsiusResponse/FahrenheitToCelsiusResult");
    echo "Celsius value: " . $soap1->getXText() . "\n";
  } else if ($command == "c2f") {
    $soap1->setMethod("CelsiusToFahrenheit");
    $soap1->doAddParam("Celsius", $arguments[1]);
    $soap1->setActionURI($soap1->getMethodURI() . $soap1->getMethod());
    try {
      $soap1->doSendRequest();
    } catch (Exception $e) {
      echo 'Error: ' . $e->getMessage() . "\n";
    }
    $soap1->setXPath("/Envelope/Body/CelsiusToFahrenheitResponse/CelsiusToFahrenheitResult");
    echo "Fahrenheit value: " . $soap1->getXText() . "\n";
  } elseif ($command == "") {
    // do nothing
  } elseif ($command == "quit" || $command == "exit") {
    break;
  } else {
    echo "Invalid command\n";
  }
}
?>