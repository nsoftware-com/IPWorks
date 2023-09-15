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
  if (argv.length !== 7) {
    console.log("Usage: node oauth.js clientID clientSecret serverAuthURL serverTokenURL authScope");
    console.log(
      "  clientID          the id of the client assigned when registering the application (e.g. 723966830965.apps.googleusercontent.com)"
    );
    console.log(
      "  clientSecret      the secret value for the client assigned when registering the application (e.g. _bYMDLuvYkJeT_99Q-vkP1rh)"
    );
    console.log(
      "  serverAuthURL     the URL of the authorization (e.g. server.https://accounts.google.com/o/oauth2/auth"
    );
    console.log(
      "  serverTokenURL    the URL used to obtain the access token (e.g. https://accounts.google.com/o/oauth2/token)"
    );
    console.log(
      "  authScope         the scope request or response parameter used during authorization (e.g. https://www.googleapis.com/auth/userinfo.email)"
    );
    console.log(
      "Example: node oauth.js 723966830965.apps.googleusercontent.com _bYMDLuvYkJeT_99Q-vkP1rh https://accounts.google.com/o/oauth2/auth https://accounts.google.com/o/oauth2/token https://www.googleapis.com/auth/userinfo.email"
    );
    process.exit();
  }

  const oauth1 = new ipworks.oauth();
  const json1 = new ipworks.json();
  const rest = new ipworks.rest();

  oauth1.on("LaunchBrowser", function(e) {
    //e.command="cmd.exe /C start";
    console.log("Authorization URL: " + e.URL + "");
  })
	.on("SSLServerAuthentication", function(e) {
    e.accept = true;
  });

  /*This application demonstrates how to use the OAuth component to authenticate with Google using OAuth 2.0 (Device Profile). 
	It also demonstrates how to use the retrieved Authorization String with the Json component to retrieve user information. 
	It will guide you through the steps to perform authorization using OAuth. 
	Please see the Introduction page within the help for more detailed instructions.
							
	/*Client ID and Client Secret
	Obtain and set your Client ID and Client Secret. For Google, these values can be found in the API Console:
	https://code.google.com/apis/console#access
	The values that are given as an example are from a Google test account that we have setup for you to easily run this demo. */

  const clientID = argv[2];
  const clientSecret = argv[3];

  /*Server Auth URL, Server Token URL, and Authorization Scope
	You can also set Server Auth URL, Server Token URL, and Authorization Scope to the values desired.
	These are preset to values for Google's User Info service.*/

  const serverAuthURL = argv[4];
  const serverTokenURL = argv[5];
  const authScope = argv[6];

  // Get Authorize URL for user to authenticate
  try {
    oauth1.setClientId(clientID);
    oauth1.setClientSecret(clientSecret);
    oauth1.setServerAuthURL(serverAuthURL);
    oauth1.setServerTokenURL(serverTokenURL);
    oauth1.setAuthorizationScope(authScope);

    let authString = await oauth1.getAuthorization();

    // Retrieve the user info for the authenticated client.
    console.log("Authorization String received. Retrieving user info for the authenticated client...");
    rest.setAuthScheme(6);
    rest.setAuthorization(authString);
    await rest.get("https://www.googleapis.com/oauth2/v1/userinfo");
    rest.setXPath("/json/email");
    console.log("Email: " + rest.getXText().replace('"', ""));
    rest.setXPath("/json/verified_email");
    console.log("Verified: " + rest.getXText().replace('"', ""));
	process.exit();
  } catch (ex) {
    console.log(ex.message);
    process.exit();
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
