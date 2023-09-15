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
  if (argv.length !== 4) {
    console.log("Usage: node ldap.js server search");
    console.log("  server  the name or address of the LDAP server.");
    console.log("  search  the parameters to search for.");
    console.log("Example: node ldap.js ldap-master.itd.umich.edu cn=*");
    process.exit();
  }

  const ldap1 = new ipworks.ldap();

  ldap1.on("SearchResult", function (e) {
    console.log("Result DN :" + e.DN);
  });

  try {
    if (argv[2].includes(":")) {
      ldap1.setServerName(argv[2].split(":")[0]);
      ldap1.setServerPort(parseInt(argv[2].split(":")[1]));
    } else {
      ldap1.setServerName(argv[2]);
    }
    ldap1.setDN("ou=Security,dc=umich,dc=edu");
    await ldap1.bind();
    if (ldap1.getResultCode() === 0) {
      console.log("Connected!");
      ldap1.setTimeout(10);
      await ldap1.search(argv[3]);
    } else {
      console.log("Bind failed: " + ldap1.getResultCode().toString() + " : " + ldap1.getResultDescription());
    }
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
