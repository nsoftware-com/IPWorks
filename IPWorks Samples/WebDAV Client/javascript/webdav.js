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

main()
async function main() {

    const webdav1 = new ipworksdav.webdav();
    let argument;            // arguments to the user's command 
    const argv = process.argv;

    if (argv.length < 2) {

        console.log("usage: node webdav.js username password");
        console.log("Options");
        console.log("  ");
        console.log("username  the username to login");
        console.log("password  the password to login");
        console.log("\r\nExample: webdav username password");
        process.exit();
    } else {
        webdav1.setUser(argv[argv.length - 2]);
        webdav1.setPassword(argv[argv.length - 1]);
        webdav1.on('SSLServerAuthentication', function (e) {
            e.accept = true; //this will trust all certificates and it is not recommended for production use
        })
            .on('Connected', function (e) {
                console.log("Server connected")
            })
            .on('ConnectionStatus', function (e) {
                console.log("Status code " + e.statusCode + ": " + e.description)
            })
            .on('Disconnected', function (e) {
                console.log("Server disconnected")
            })
            .on('Transfer', function (e) {
                console.log("Resource being received from server (in full text): \n" +
                    "========================================= \n" + e.text)
            })

        console.log("******************************************************************************");
        console.log("* This demo shows how to use the WebDAV component to make directories, move *");
        console.log("* resources, delete resource, and to get resources.                          *");
        console.log("******************************************************************************\n");

        rl.setPrompt("Choose an option:\n1) Make Directory\n2) Move Resource\n3) Get Resource\n4) Delete Resource\n5) Put Resource\nQ) Quit.\n> ")
        rl.prompt()

        rl.on('line', async line => {
            argument = line.split(/\s+/);
            var cmd = argument[0];
            if (cmd == "1") { // Make Directory

                rl.question('Name server where you wish to create a directory (ex. localhost:443): ', (server) => {
                    rl.question("Name directory to create (ex. directory/folder): ", async dir => {
                        await webdav1.makeDirectory(server + "/" + dir).catch(ex => console.log("Error: " + ex ))
                        rl.prompt()
                    })
                })

            } else if (cmd == "2") {  // Move Resource
                await webdav1.reset();
                rl.question('Name server (ex. localhost:443): ', (server) => {
                    rl.question("Name source of the resource (ex. myoldfolder/myfile.txt): ", async src => {
                        rl.question("Name destination of the resource (ex. mynewfolder/myfile.txt): ", async dest => {
                            await webdav1.moveResource(server + "/" + src, "/" + dest).catch(ex => console.log("Error: " + ex ))
                            rl.prompt()
                        })
                    })
                })

            } else if (cmd == "3") { // Get Resource
                await webdav1.reset();

                rl.question('Name URI of resource you wish to get: ', async (get) => {
                    await webdav1.getResource(get).catch(ex => console.log("Error: " + ex ))
                    rl.prompt()
                })

            } else if (cmd == "4") { // Delete Resource
                await webdav1.reset();

                rl.question('Name URI of resource you wish to delete: ', async (del) => {
                    await webdav1.deleteResource(del).catch(ex => console.log("Error: " + ex ))
                    rl.prompt()
                })

            } else if (cmd == "5") { // Put Resource on server, can create resource on server or replace old resource
                await webdav1.reset();

                rl.question('Name path of file you wish to put on server: ', (file) => {
                    webdav1.setLocalFile(file);
                    rl.question("Name URI of resource you wish to put on server: ", async put => {
                        await webdav1.putResource(put).catch(ex => console.log("Error: " + ex ))
                        rl.prompt()
                    })
                })

            } else if (cmd == "q") {
                process.exit(0);
            } else if (cmd == "") {
                rl.prompt()
            } else {
                console.log("Bad command / Not implemented in demo.");
                rl.prompt()
            } // end of command checking
        })
    }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
