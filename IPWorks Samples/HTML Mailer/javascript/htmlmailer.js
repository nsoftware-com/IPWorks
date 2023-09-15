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
	if(argv.length < 9 || argv.length > 16) {
		console.log("Usage: node htmlmailer.js [-option parameter] server from to");
		console.log("Options: ");
		console.log("  -ssl    a switch parameter to enable SSL");
		console.log("  -u      the username used to authenticate to the mail server");
		console.log("  -p      the password of the user");		
		console.log("  -s      the subject of the mail message");
		console.log("  -m      the raw message content");
		console.log("  -a      the path of file to attach to the message");
		console.log("  server  the name or address of a mail server");			
		console.log("  from    the email address of the sender");
		console.log("  to      a comma separated list of addresses for destinations");
		console.log("Example: node htmlmailer.js -ssl -u test@mydomain.com -p mypassword -s test -m \"<b>Hello</b>, my name is <i>Tom</i>\" -a \"C:\\File\\To\\Attach.jpg\" smtp.gmail.com sender@mail.com recipient1@gmail.com,recipient2@gmail.com");
		process.exit();
	} 

	const htmlmailer1 = new ipworks.htmlmailer();

	htmlmailer1.on("SSLServerAuthentication", function(e) {
		e.accept = true;
	});

	for (i=0; i<argv.length; i++) {
		if (argv[i].startsWith("-")) {
			if (argv[i] === "-ssl") htmlmailer1.setSSLEnabled(true);
			if (argv[i] === "-s")   htmlmailer1.setSubject(argv[i+1]);
			if (argv[i] === "-m")   htmlmailer1.setMessageHTML(argv[i+1]);
			if (argv[i] === "-a")   htmlmailer1.addAttachment(argv[i+1]);
			if (argv[i] === "-u")   htmlmailer1.setUser(argv[i+1]);
			if (argv[i] === "-p")   htmlmailer1.setPassword(argv[i+1]);
		}
	}

	htmlmailer1.setTimeout(60);
	htmlmailer1.setMailServer(argv[argv.length-3]);
	htmlmailer1.setFrom(argv[argv.length-2]);			  
	htmlmailer1.setSendTo(argv[argv.length-1]);


	console.log('Sending message...');

	try {
		await htmlmailer1.send();
		console.log("Message sent successfully.");
	} catch (ex) {
		console.log("Error: " + ex); 
	}
	process.exit();
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
