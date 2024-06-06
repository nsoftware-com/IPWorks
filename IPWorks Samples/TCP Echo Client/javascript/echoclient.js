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
  if (argv.length < 5 || argv.length > 7) {
    console.log("Usage: node echoclient.js -s server -p port [-ssl]");
    console.log("Options: ");
    console.log("  -ssl  a switch parameter to enable SSL");
    console.log("  -s    the address of the remote host.");
    console.log("  -p    the TCP port of the remote host");
    console.log("Example: node echoclient.js -s localhost -p 777 -ssl");
    process.exit();
  }

  const tcpclient = new ipworks.tcpclient();
  tcpclient.config("AcceptAnyServerCert=true");
  let server, port;

  for (i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("-")) {
      if (argv[i] === "-ssl") { tcpclient.setSSLEnabled(true); }
      if (argv[i] === "-s") { server = argv[i + 1]; }
      if (argv[i] === "-p") { port = argv[i + 1]; }
    }
  }

  function clientprompt() {
    process.stdout.write(' ');
  }

  tcpclient.on('SSLServerAuthentication', function (e) {
    e.accept = true;
  })
    .on('Connected', function (e) {
      console.log(tcpclient.getRemoteHost() + " has connected.")
    })
    .on('Disconnected', function (e) {
      console.log("Disconnected " + e.description + " from " + tcpclient.getRemoteHost() + ".");
    });

  await tcpclient.connectTo(server, parseInt(port)).catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  clientprompt();

  await tcpclient.doEvents().catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  if (tcpclient.isConnected()) {
    console.log("> Press 1 to input data \n > Press 2 to quit")
    rl.prompt()
    rl.on('line', command => {
      
      if ("1" === command) {
        rl.question("Type data to send: ", data => {
          tcpclient.sendText(data)
          
          tcpclient.on('DataIn', function (e) {
            console.log("Received " + e.text + " from " + tcpclient.getRemoteHost());
            clientprompt();
            rl.prompt()
          })

        })

      } else if ("2" === command) {
        rl.close()
      } else {
        console.log("\r\nInvalid input!");
        rl.prompt()
      }
    }).on('close', () => {
      tcpclient.disconnect();
    })
  }

}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
