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
	if(argv.length < 8 || argv.length > 11) {
		console.log("Usage: node imap.js -s server -u username -p password [-option parameter]]");
		console.log("Options: ");
		console.log("  -ssl     a switch parameter to enable SSL");
		console.log("  -s       the name or address of the mail server (IMAP server)");
		console.log("  -port    the port of the mail server");
		console.log("  -u       the username used to authenticate to the mail server");			
		console.log("  -p       the password used to authenticate to the mail server ");
		console.log("Example: node imap.js -s my.imapserver.com -port 993 -u username -p password -ssl");
		process.exit();
	}

	let count = 0;
	let mailboxes=[];
	let messageCount=0;
	const imap = new ipworks.imap();
  let server, port, username, password;

	for (i = 0; i < argv.length; i++) {
		if (argv[i].startsWith("-")) {
			if (argv[i] === "-ssl") {  imap.setSSLEnabled(true); } 
			if (argv[i] === "-s") {    server = argv[i + 1]; }
			if (argv[i] === "-port") { port = argv[i + 1]; }
			if (argv[i] === "-u") {    username = argv[i + 1]; } 
			if (argv[i] === "-p") {    password = argv[i + 1]; }
		}
	}

	imap.on("MailboxList", function(e){
		count+=1;
		mailboxes.push(e.mailbox);
	})
	.on("MessageInfo", function(e){
		console.log(e.messageId + ") " + e.subject);
	})
	.on("Transfer", function(e){
		console.log(e.text.toString());
	})
	.on("SSLServerAuthentication", function(e) {
		e.accept = true;
	});

	function displayMailBoxList(){
		for (i=1; i<count+1; i++){
			console.log(i + ")" + mailboxes[i-1]);
		}   
	}

	try {
		imap.setMailServer(server);
		if (port) { imap.setMailPort(parseInt(port)); }
		imap.setUser(username);
		imap.setPassword(password);
		await imap.connect();
		imap.setMailbox("*");
		await imap.listMailboxes();
	} catch (ex) {
		console.log("Error: "+ex);
		return;
	}

	console.log('Welcome! Type ? for a list of commands.');
	process.stdout.write('imap> ');



	rl.on('line', async function(line) {
		var index=line.toLowerCase().split(/\s+/)[1];
		if(line.toLowerCase() === '?') {
			console.log("IMAP Demo Commands");
			console.log("l                 list mailboxes");
			console.log("s mailbox_num     show messages in mailbox by index eg:s 1");
			console.log("r message_num     read the message by index eg:r 1");
			console.log("q                 quit");
		} else if (line.toLowerCase().startsWith('l')){
			console.log("Mailboxes for " + argv[2] + ":");
			count = 0;
			mailboxes=[];
			imap.setMailbox("*");
			try {
				await imap.listMailboxes();
				displayMailBoxList();
				console.log("Total: " + count.toString());
			} catch (ex) {
				console.log("Error: "+ex);
			}
		} else if (line.toLowerCase().startsWith('s')){		
			imap.setMailbox(mailboxes[parseInt(index)-1]);
			try {
				await imap.selectMailbox();
				messageCount=parseInt(imap.getMessageCount());
				if (messageCount!==0) {
					imap.setMessageSet("1:" + messageCount.toString());
					console.log("Messages in current mailbox:");
					await imap.fetchMessageInfo();
				} else{
					console.log("No messages found in the current mailbox.");
				}
			} catch(ex) {
				console.log("Error: "+ex);
			}
		} else if (line.toLowerCase().startsWith('r')){
			var currentMailbox=imap.getMailbox();
			messageCount=parseInt(imap.getMessageCount());
			if (currentMailbox !='*') {		
				if (parseInt(index)<=messageCount) {
					console.log("Message text:");
					imap.setMessageSet(index);
					await imap.fetchMessageText().catch((err) => console.log("Error: "+err.message));;
				} else{
					console.log("Index out of range!");
				}
				
			} else {
				console.log("Select a mailbox first.");
			}	
		} else if (line.toLowerCase().startsWith("q")){
			console.log("Goodbye!");
			await imap.disconnect();
			process.exit();
		} else{
			console.log("Command not known!")
		}
		process.stdout.write('imap> ');
	});
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
