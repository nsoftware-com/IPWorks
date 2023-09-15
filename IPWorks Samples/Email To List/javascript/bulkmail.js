/*
 * IPWorks 2022 JavaScript Edition - Sample Project
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
	if (argv.length < 9 || argv.length > 14) {
		console.log("Usage: node bulkmail.js  [-option parameter] server  from  to");
		console.log("Options: ");
		console.log("  -ssl    a switch parameter to enable SSL");
		console.log("  -u      the user to authenticate");
		console.log("  -p      the password of the user");
		console.log("  -s      the subject of the mail message");
		console.log("  -m      the raw message content");
		console.log("  server  the name or address of a mail server (mail relay)");
		console.log("  from    the email address of the sender");
		console.log("  to      a comma separated list of addresses for destinations");
		console.log("Example: node bulkmail.js -ssl -u myself@me.com -p mypassword -s test -m \"test message\" my.smtpserver.com sender@mail.com recipient1@gmail.com,recipient2@gmail.com");
		process.exit();
	}

	const smtp1 = new ipworks.smtp();

	smtp1.on("SSLServerAuthentication", function (e) {
		e.accept = true;
	});

	for (i = 0; i < argv.length; i++) {
		if (argv[i].startsWith("-")) {
			if (argv[i] === "-ssl") smtp1.setSSLEnabled(true);
			if (argv[i] === "-s")   smtp1.setSubject(argv[i + 1]);
			if (argv[i] === "-u")   smtp1.setUser(argv[i + 1]);
			if (argv[i] === "-p")   smtp1.setPassword(argv[i + 1]);
			if (argv[i] === "-m")   smtp1.setMessageText(argv[i + 1]);
		}
	}

	smtp1.setTimeout(60);
	smtp1.setMailServer(argv[argv.length - 3]);
	smtp1.setFrom(argv[argv.length - 2]);
	smtp1.setSendTo(argv[argv.length - 1]);


	console.log('Sending message...');
	try {
		await smtp1.send();
		console.log("Message sent successfully.");
		smtp1.disconnect();
		process.exit(0);
	} catch (ex) {
		console.log("Error: " + ex);
		return;
	}
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
