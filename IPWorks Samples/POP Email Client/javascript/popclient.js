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
  if (argv.length < 8 || argv.length > 11) {
		console.log("Usage: node popclient.js  -s popserver  -u username  -p password  [-option parameter]");
		console.log("Options: ");
		console.log("  -ssl     a switch parameter to enable SSL");
		console.log("  -s       the name or address of a mail server (internet post office server)");
		console.log("  -port    the port of the mail server");
		console.log("  -u       the user identifier for the mailbox");			
		console.log("  -p       the password for the mailbox user");
		console.log("Example: node popclient.js -s my.popserver.com -u username -p password -port 995 -ssl");
		process.exit();
  }

  let msg_num = 0;
  let count = 0;
  let popserver, port, username, password;

  const pop = new ipworks.pop();

	for (i = 0; i < argv.length; i++) {
		if (argv[i].startsWith("-")) {
			if (argv[i] === "-ssl")  { pop.setSSLEnabled(true); }
			if (argv[i] === "-s")    { popserver = argv[i + 1]; } 
			if (argv[i] === "-port") { port = argv[i + 1]; } 
			if (argv[i] === "-u")    { username = argv[i + 1]; } 
			if (argv[i] === "-p")    { password = argv[i + 1]; } 
		}
	}
  pop.on("SSLServerAuthentication", function (e) {
    e.accept = true;
  });

  pop.setMailServer(popserver);
  pop.setMailPort(parseInt(port));
  pop.setUser(username);
  pop.setPassword(password);
  await pop.connect().catch((err) => {
    console.log("Error: " + err.message);
    process.exit();
  });

  console.log('"Welcome! Type ? for a list of commands.');
  process.stdout.write('pop> ');

  rl.on('line', async function (line) {

    const args = line.split(/\s+/);
    if (args[0] === "?") {
      console.log("POP client demo");
      console.log("h                       print out active message headers");
      console.log("n                       goto and type next message");
      console.log("t <message number>      type message");
      console.log("d <message number>      delete message");
      console.log("q                       quit");
      process.stdout.write('pop> ');
    } else if (args[0] === "h") {
      pop.setMaxLines(1);
      const mailcount = pop.getMessageCount();
      if (mailcount !== 0) {
        console.log("\nMsg " + "\t" + "Sender" + "\t\t\t" + "Subject");
        console.log("========================================");
        for (i = 0; i < mailcount; i++) {
          pop.setMessageNumber(i + 1);
          try {
            await pop.retrieve();
            console.log((i + 1) + ")" + "\t" + pop.getMessageFrom() + "\t" + pop.getMessageSubject());
          } catch (ex) {
            console.log("Error: " + err.message);
          }
        }
      } else {
        console.log("No messages in inbox");
      }
      pop.setMaxLines(0)  //reset MaxLines so future actions are not limited
      process.stdout.write('pop> ');
    } else if (args[0] === "n") {
      msg_num += 1;
      if (msg_num <= pop.getMessageCount()) {
        pop.setMessageNumber(msg_num);
        try {
          await pop.retrieve();
          console.log(pop.getMessageText());
        } catch (ex) {
          console.log("Error: " + err.message);
        }
      } else {
        console.log("Index out of range");
      }
      process.stdout.write('pop> ');
    } else if (args[0] === "t") {
      const index = parseInt(args[1]);
      msg_num = index;
      if (index <= pop.getMessageCount()) {
        pop.setMessageNumber(index);
        try {
          await pop.retrieve();
          console.log(pop.getMessageText());
        } catch (ex) {
          console.log("Error: " + err.message);
        }
      } else {
        console.log("Index out of range");
      }
      process.stdout.write('pop> ');
    } else if (args[0] === "d") {
      pop.setMessageNumber(parseInt(args[1]));
      await pop.delete().catch((err) => {
        console.log("Error: " + err.message);
        process.exit();
      });
      console.log("Delete successful!");
      process.stdout.write('pop> ');
    } else if (args[0] === "q") {
      await pop.disconnect();
      process.exit();
    }
  });
}



function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
