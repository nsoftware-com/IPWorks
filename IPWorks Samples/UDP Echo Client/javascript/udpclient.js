/*
 * IPWorks 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworks = require("@nsoftware/ipworks");

if(!ipworks) {
  console.error("Cannot find ipworks.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();
async function main() {
  const argv = process.argv;
  if (argv.length < 4) {
    console.log("Usage: node udpclient.js server port");
    console.log("  server    the address of the remote host");
    console.log("  port      the port of the remote host.");
    console.log("Example: node udpclient.js 192.168.1.2 777");
    console.log("         node udpclient.js 255.255.255.255 777 (broadcast)");
    process.exit();
  }

  const udpclient = new ipworks.udp();
  udpclient.on('DataIn', function (e) {
    console.log('Received: "' + e.datagram + '" from ' + e.sourceAddress + ':' + e.sourcePort);
  });

  udpclient.setRemoteHost(argv[2]);
  udpclient.setRemotePort(parseInt(argv[3]));

  await udpclient.activate().catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  await udpclient.doEvents().catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  console.log("Type and press enter to send. Press Ctrl-C to exit the application.");
  rl.prompt();
  rl.on('line', line => {
    udpclient.sendText(line).catch((err) => {
      if (err) {
        console.log(err);
        process.exit(2);
      }
    });
  });
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
