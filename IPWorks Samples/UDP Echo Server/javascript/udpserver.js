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
async function main () {
  const argv = process.argv;
  if (argv.length !== 3) {
    console.log("Usage: node udpserver.js port");
    console.log("  port   the port on the local host where the component listens");
    console.log("Example: node udpserver.js 777");
    process.exit();
  }

  const udpserver = new ipworks.udp();

  udpserver.on('DataIn', function (e) {
    console.log("Echoing '" + e.datagram + "' back to client " + e.sourceAddress + ":" + e.sourcePort + ".");
    udpserver.setRemoteHost(e.sourceAddress);
    udpserver.setRemotePort(e.sourcePort);
    udpserver.sendText(e.datagram);
  })

  console.log('*****************************************************************')
  console.log('* This demo shows how to set up an echo server using UDP.       *')
  console.log('*****************************************************************')

  udpserver.setLocalPort(parseInt(argv[2]));

  await udpserver.activate()
  console.log("Listening on port " + udpserver.getLocalPort() + "... press Ctrl-C to shutdown.");

  while (true) {
    await udpserver.doEvents(function(err) {
      if (err) {
        console.log(err);
        return;
      }
    });
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
