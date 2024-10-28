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
  if (argv.length !== 5) {
    console.log("Usage: node jabber.js server user password");
    console.log("  server    the address of the XMMP (Jabber) server");
    console.log("  user      the user associated with the server");
    console.log("  password  the password to logon");
    console.log("Example: node jabber.js servername username@gmail.com password");
    process.exit();
  }

  var messageReceived = false;
  const xmpp = new ipworks.xmpp();

  xmpp.on("SSLServerAuthentication", function (e) {
    e.accept = true;
  })
	.on("MessageIn", function (e) {
    console.log(e.from + " said : " + e.messageText);
    messageReceived = true;
  });

  try {
    xmpp.setIMServer(argv[2]);
    xmpp.setIMPort(5222);
    xmpp.setSSLEnabled(true);
    xmpp.setSSLStartMode(2);
    xmpp.config("AcceptAnyServerCert=true"); // In most cases certificate will be unauthorized, so we have to add this
    console.log("Connecting...");
    console.error(argv[2], argv[3], argv[4]);
    await xmpp.connectTo(argv[3], argv[4]);
    console.log("Welcome! Connection established!");
  } catch (e) {
    console.log(e);
    process.exit();
  }

  let buddyCount = xmpp.getBuddies().size();
  let buddy = 0;
  console.log("Buddy list:");

  if (parseInt(buddyCount) !== 0) {
    for (i = 0; i < buddyCount; i++) {
      console.log(i + 1 + ") " + xmpp.getBuddies().item(i).getId());
    }

    rl.question("Select a buddy:", (answer) => {
      buddy = parseInt(answer);
      rl.question("Message to send to buddy:", async (answer) => {
        xmpp.setMessageText(answer);
        console.log("Sending...");
        await xmpp.sendMessage(xmpp.getBuddies().item(buddy - 1).getId());
        rl.question("Enter 'y' to wait for a response [n]: ", async (answer) => {
          if (answer.toLowerCase().startsWith("y")) {
            console.log("Waiting for response...");
            while (messageReceived === false) {
              await xmpp.doEvents();
            }
          }
          console.log("Disconnecting...");
          xmpp.disconnect();
          console.log("Bye!");
          rl.close();
        });
      });
    });
  } else {
    console.log("No buddies found!");
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
