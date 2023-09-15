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
    console.log("Usage: node dns.js server hostname");
    console.log("");
    console.log("  server    the address of the DNS server.");
    console.log("  hostname  the host domain to query");
    console.log("Example: node dns.js 8.8.8.8 www.yahoo.com");
    return;
  }

  const dns1 = new ipworks.dns();

  dns1.on("Error", function (e) {
    console.log("Error:" + e.errorCode + "[" + e.description + "].");
  })
	.on("Response", function (e) {
    if (e.statusCode === 0) {
      //there was a record in the response
      for (let i = 0; i < dns1.getRecords().size(); i++) {
        var curRecord = dns1.getRecords().item(i);
        for (let j = 0; j < curRecord.getFieldCount(); j++) {
          curRecord.setFieldIndex(j);
          if (j === 0) {
            console.log(
              curRecord.getRecordTypeName() + "\t" + curRecord.getFieldName(i) + "\t" + curRecord.getFieldValue(i)
            );
          } else {
            console.log("\t");
            console.log(curRecord.getFieldName(i) + "\t" + curRecord.getFieldValue(i));
          }
        }
      }
    }
  });

  console.log("Type\tField\tValue");
  console.log("--------------------");
  for (let i = 1; i <= 22; i++) {
    dns1.setQueryType(i);
    await dns1.setDNSServer(argv[2]);
    await dns1.query(argv[3]);
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
