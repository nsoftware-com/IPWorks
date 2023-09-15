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
  if(argv.length !== 5) {
    console.log("Usage: node ftp.js  host  user  password");
    console.log("  host      the domain name or IP address of the FTP server");
    console.log("  user      the user identifier to use for login");
    console.log("  password  the password to log in");
    console.log("Example: node ftp.js  localhost  user  password");
    process.exit();
  }
  
  const ftp1 = new ipworks.ftp();
  
  ftp1.setRemoteHost(argv[2]);
  ftp1.setUser(argv[3]);
  ftp1.setPassword(argv[4]);
  
  verbose = true;
  function toggleVerbose() {
    verbose = !verbose;
    console.log("Verbose mode " + (verbose ? "ON.":"OFF."));
  }
  
  ftp1.on("PITrail", function(e){
    if(verbose) console.log(e.message);
  })
	.on("DirList", function(e){
    console.log(e.fileName);
  })
	.on("Transfer", function(e){
    //console.log(e.bytesTransferred + ", " + e.percentDone + "%");
  })
	.on("Error", function(e){
    console.log(e.description);
  })
	.on("SSLServerAuthentication", function(e) {
    e.accept = true;
  });
  
  await ftp1.logon().catch((err) => { 
    console.log("Error: "+err.message); 
    process.exit();
  });
  ftp1.setPassive(true);
  console.log("Succesfully logged on.");
  printMenu();
  ftpprompt();
  
  rl.on('line', async function(line) {
  
    var args = line.split(/\s+/);
    var cmd = args[0];
  
    switch (cmd) {
      case "?":
      case "help":
      {
        printMenu();
        ftpprompt();
        break;
      }
      case "close":
      case "quit":
      case "bye":
      {
        await ftp1.logoff();
        process.exit(0);
        break;
      }
      case "put":{
        if(args.length<3) {
          console.log('Usage: put localfile remotefile');
          ftpprompt();
          break;
        }
        ftp1.setLocalFile(args[1]);
        ftp1.setRemoteFile(args[2]);
        try {
          await ftp1.upload();
          console.log("upload finished.");
        } catch (ex) {
          console.log("Error: "+ex);
        }
        ftp1.setLocalFile('');
        ftp1.setRemoteFile('');
        ftpprompt();
        break;
      }
      case "ls" : {
        try {
          if (args.length <=1 ) {
            await ftp1.listDirectoryLong();
          } else {
            var curPath = await ftp1.getRemotePath();
            await ftp1.setRemotePath(args[1]);
            await ftp1.listDirectoryLong();
            await ftp1.setRemotePath(curPath); 
          }
        } catch (ex) {
          console.log("Error: " + ex);
        }
        ftpprompt();
        break;
      }

      case "pwd" : {
        console.log(await ftp1.getRemotePath());
        ftpprompt();
        break;
                
      }
      case "cd" : {
        if(args.length <=1) {
          console.log("Usage: cd remote path");
          ftpprompt();
        } else {
          await ftp1.setRemotePath(args[1]).catch((err) => { console.log("Error: "+err.message); });
          ftpprompt();
        }
        break;
                
      }
      case "rmdir" : {
        if(args.length <=1) {
          console.log("Usags: rmdir remote folder");
          ftpprompt();
        } else {
          await ftp1.removeDirectory(args[1]).catch((err) => { console.log("Error: "+err.message); });
          ftpprompt();
        }
        break;
      }
      case "append" : {
        if(args.length<3) {
          console.log('Usage: append localfile remotefile');
          ftpprompt();
          break;
        }
  
        ftp1.setLocalFile(args[1]);        
        ftp1.setRemoteFile(args[2]);  
        try {
          await ftp1.append();
          console.log("append finished.")
        } catch (ex) {
          console.log("Error: "+ex);
        }
        ftp1.setLocalFile('');
        ftp1.setRemoteFile('');
        ftpprompt();
        break;
      }
      case "verbose" : {
        if(args.length <=1) {
          console.log("Usage: verbose on|off");
        } else {
          if(!verbose && args[1] === 'on') {
            toggleVerbose();
          } else if(verbose && args[1] === 'off') {
            toggleVerbose();
          }
        }
        ftpprompt();
        break;
      }
      case "ascii" : {
        ftp1.setTransferMode(1);
        ftpprompt();
        break;
      }
      case "binary" : {
        ftp1.setTransferMode(2);
        ftpprompt();
        break;
      }
      case "mkdir" : {
        if(args.length <=1) {
          console.log("Usage: mkdir folder");
          ftpprompt();
        } else {
           await ftp1.makeDirectory(args[1]).catch((err) => { console.log("Error: "+err.message); });
           ftp1.setRemoteFile('');
           ftpprompt();
        }
        break;
      }
      case "get" : {
        if(args.length<3) {
          console.log('Usage: get remotefile localfile');
          ftpprompt();
          break;
        }
        ftp1.setRemoteFile(args[1]);
        ftp1.setLocalFile(args[2]);
        try {
          await ftp1.download();
          console.log("download finished.");
        } catch (ex) {
          console.log("Error: "+ex);
        }
        ftp1.setLocalFile('');
        ftp1.setRemoteFile('');
        ftpprompt(); 
        break;
      }
      case "rm" : {
        if(args.length <=1) {
          console.log("Usage: rm file");
          ftpprompt();
        } else {
          await ftp1.deleteFile(args[1]).catch((err) => { console.log("Error: "+err.message); });
          ftp1.setRemoteFile('');
          ftpprompt();
        }
        break;
      }
  
    }
  });
}

function ftpprompt(){
  process.stdout.write('ftp> ');  
}

function printMenu(){
  console.log("Available Commands:\n?        bye     help     put     rmdir");
  console.log("append   cd      ls       pwd     verbose");
  console.log("ascii    close   mkdir    quit");
  console.log("binary   get     rm");
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
