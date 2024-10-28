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

main()
async function main() {

    const caldav1 = new ipworks.caldav();
    const oauth1 = new ipworks.oauth();
    let argument;            // arguments to the user's command 
    const argv = process.argv;

    let resourceURIs = [];

    let eventNumber;

    let clientID;

    let clientSecret;

    if (argv.length < 4) {

        console.log("usage: node caldav.js [options] provider username");
        console.log("Options");
        console.log("  -i        OAuth Client ID");
        console.log("  -s        OAuth Client Secret");
        console.log("username  the username to login");
        console.log("\r\nExample: caldav -i \"Client ID\"  -s \"Client Secret\" google username password");
        process.exit();
    } else {
        caldav1.setUser(argv[argv.length - 2]);
        caldav1.setPassword(argv[argv.length - 1]);
        caldav1.on('SSLServerAuthentication', function (e) {
            e.accept = true; //this will trust all certificates and it is not recommended for production use
        })
            .on('EventDetails', function (e) {
                resourceURIs.push(e.resourceURI);
                let mStartDate = caldav1.getStartDate();

                //console.log(`{eventNumber}) %2$-50.50s %3$-20s%4$-30.30s %5$-100s\n`, eventNumber, caldav1.getSummary(), mStartDate, caldav1.getLocation(), e.resourceURI);
                console.log(`${eventNumber})  ${caldav1.getSummary().padEnd(50, " ")} ${mStartDate.padEnd(20, " ")}  ${caldav1.getLocation().padEnd(30, " ").substring(0, 30)}  ${e.resourceURI.padEnd(100, " ")}\n`);
                eventNumber++;

            })

        for (let i = 0; i < argv.length; i++) {
            if (argv[i].startsWith("-")) {
                if (argv[i] === "-i") clientID = argv[i + 1];// args[i+1] corresponds to the value of argument [i]
                if (argv[i] === "-s") clientSecret = argv[i + 1];
            }
        }


        // Google CalDAV requires OAuth 2
        oauth1.setClientId(clientID);
        oauth1.setClientSecret(clientSecret);
        oauth1.setServerAuthURL("https://accounts.google.com/o/oauth2/auth");
        oauth1.setServerTokenURL("https://oauth2.googleapis.com/token");
        caldav1.setAuthScheme(ipworks.CalDAVAuthSchemes.authOAuth);
        oauth1.setAuthorizationScope("https://www.googleapis.com/auth/calendar");
        "Example: node oauth.js 723966830965.apps.googleusercontent.com _bYMDLuvYkJeT_99Q-vkP1rh https://accounts.google.com/o/oauth2/auth https://accounts.google.com/o/oauth2/token https://www.googleapis.com/auth/userinfo.email"



        console.log("******************************************************************************");
        console.log("* This demo shows how to use the CalDAV component to list upcoming events    *");
        console.log("* from an existing Google calendar. You can also create a new event,         *");
        console.log("* delete an event, or export an event to an ics file.                        *");
        console.log("******************************************************************************\n");

        rl.setPrompt("Choose an option:\n1) List Events\n2) Add Event\n3) Delete Event\n?) This help menu.\nQ) Quit.\n> ")
        rl.prompt()

        rl.on('line', async line => {
            argument = line.split(/\s+/);
            var cmd = argument[0];
            if (cmd == "1") { // Make Directory
                resourceURIs = [];
                eventNumber = 1;
                console.log(`   ${"Summary".padEnd(50, " ")}  ${"Start Date".padEnd(20, " ")}  ${"Location".padEnd(30, " ")}  ${"ResourceURI".padEnd(100, " ")}\n`);
                caldav1.setAuthorization(await oauth1.getAuthorization().then((e) => {
                    return e;
                }).catch((e) => console.log(e)));

                await caldav1.getCalendarReport("https://apidata.googleusercontent.com/caldav/v2/" + caldav1.getUser() + "/events").catch((e) => console.log(e));
                rl.prompt()

            } else if (cmd == "2") {  // Add event
                await caldav1.reset();
                rl.question("Start Date (YYYYMMDDTHHMMSS): ", (start) => {
                    rl.question("End Date (YYYYMMDDTHHMMSS): ", async end => {
                        rl.question("Location: ", async location => {
                            rl.question("Summary: ", async summary => {
                                rl.question("Description: ", async desc => {
                                    caldav1.setStartDate(start);
                                    caldav1.setEndDate(end);

                                    caldav1.setLocation(location);
                                    caldav1.setSummary(summary);
                                    caldav1.setDescription(desc);
                                    caldav1.setEventType(0);

                                    caldav1.setUID(caldav1.getStartDate());
                                    caldav1.setAuthorization(await oauth1.getAuthorization().then((e) => {
                                        return e;
                                    }).catch((e) => console.log(e)));
                                    await caldav1.createEvent("https://apidata.googleusercontent.com/caldav/v2/" + caldav1.getUser() + "/events/" + caldav1.getUID() + ".ics");
                                    console.log("Event successfully added")
                                    rl.prompt()
                                })
                            })
                        })
                    })
                })

            } else if (cmd == "3") { // Delete Resource
                await caldav1.reset();

                rl.question('Specify the number of the event to delete: ', async (index) => {
                    if (index > resourceURIs.length) {
                        console.log("Please run 1) List Events command first.");
                        rl.prompt();
                        return;
                    }
                    caldav1.setAuthorization(await oauth1.getAuthorization().then((e) => {
                        return e;
                    }).catch((e) => console.log(e)));
                    await caldav1.deleteEvent(resourceURIs[index - 1]).catch(e => console.log(e))
                    console.log("Event successfully deleted")
                    rl.prompt();
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
