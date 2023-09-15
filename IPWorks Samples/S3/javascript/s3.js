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

async function main(){

  const s3 = new ipworks.s3();
  s3.on("SSLServerAuthentication", (e) => (e.accept = true));

  console.log("\nWelcome to the IPWorks S3 Demo.");
  console.log("This demo shows how to use the IPWorks S3 component to interact with S3 compatible storage providers");
  console.log("It is assumed that you already signed up for a service (for example, at aws.amazon.com/s3) and have an Access Key and Secret Key which are required to use the service \n");
  console.log("------------------------------------------------------------ \n");
  
  printProviders()
  
  prompt("serviceprovider", "Select Service Provider (enter number)", ":", "");
  
  rl.on('line', async (line)=> {
    switch (lastPrompt) {
      //Set the service provider
      case "serviceprovider":  
        try{
          s3.setServiceProvider(parseInt(line));
        } catch (error) {
          console.log(error.message);
          process.exit(1);
        }
        //Ask for oracle namespace, send to case "oraclenamespace"
        if (s3.getServiceProvider() == 8){
          prompt("oraclenamespace", "Enter Oracle Namespace", ":", "");
        //Ask for custom URL, send to case "customurl"
        } else if (s3.getServiceProvider() == 255){
          prompt("customurl", "Enter custom provider URL", ":", "");
        //Ask for access key, send to case "accesskey"
        } else {
          prompt("accesskey", "Access Key", ":", "");
        }       
      break;
      case "customurl":
        s3.config("URL="+line)
        //Ask for access key, send to case "accesskey"
        prompt("accesskey", "Access Key", ":", "");
      break;
      case "oraclenamespace":
        s3.config("OracleNamespace="+line)
        //Ask for access key, send to case "accesskey"
        prompt("accesskey", "Access Key", ":", "");
      break;
      case "accesskey":
        s3.setAccessKey(line);
        //Ask for secret key, send to case "secretkey"
        prompt("secretkey", "Secret Key", ":", "");
      break;
      case "secretkey":
        s3.setSecretKey(line);
        //List all the buckets
        console.log("\nBucket List:\n");
        try{
          //Async function, captured by "BucketList" event
          await s3.listBuckets();
        } catch (error){
          console.log(error.message);      
          process.exit(1);
        }
        //Sends to no case, which will allow default case in following switch case to print menus
        lastPrompt = ""		
      break;
      //Used for moving into a bucket.
      case "bucket" :
        //Wait for confirmation that the bucket was set
        await s3.setBucket(line);
        console.log(`Changed to bucket: ${s3.getBucket()}`)
        //Print menus
        lastPrompt = "";
      break;
      //Used to get a file from a bucket
      case "file":
        try { 
          //Wait for its return
          await s3.getObject(line);
        } catch (error) {
          console.log(error.message);
          process.exit(1);
        }  
        objectdata = s3.getObjectData();
        console.log(`The content of the file: \r\n ${objectdata}`);
        //Print menus
        lastPrompt = ""		
      break;
      //Used to put a file in a bucket, sets the local file name
      case "local_file" :
        try{
          //Sets where the local file is
          s3.setLocalFile(line);
        } catch (error) {
          console.log(error.message);
          process.exit(1);
        }          
        //Asks what the name of the new object will be, sets start to "rem_object"
        prompt("rem_object", "Name of new object?", ":", ""); 
      break;
      //Used to put a file in a bucket, sets the remote file name
      case "rem_object" :
        try{
          //Wait to finish
          await s3.createObject(line);
        } catch (error){
          console.log(error.message);
          process.exit(1);
        }
        console.log("Object Created");
        s3.setLocalFile("");
        //print menus
        lastPrompt = "";
      break;    
    }
    switch (line) {
      case "?":
        printMenu();
        menuPrompt();
      break;
      case "cd":
        prompt("bucket", "Which Bucket?", ":", "");
      break;
      case "ls": 
        try{
          console.log("Objects: ");
          //Async function, captured by the "ObjectList" event
          await s3.listObjects();
        } catch (error){
          console.log(error.message);
          process.exit();
        }
        printMenu();
        menuPrompt();       
      break;
      case "get" :
        prompt("file", "Which File?", ":", "");
      break;
      case "put" :
        prompt("local_file", "Local file to upload", ":", "");
      break;
      case "q":
        process.exit();
      break;
      default:
      if (lastPrompt === ""){
        printMenu();
        menuPrompt();
      }
      break;
    }
  });
  s3.on("BucketList", async (e)=>(console.log(e.bucketName)))
  .on("ObjectList", async (e) => (console.log(e.objectName)));
}

function printMenu() {
  console.log("\n?     -   Help");
  console.log("cd    -   Change to bucket");
  console.log("ls    -   List objects");
  console.log("get   -   Get remoteFile");
  console.log("put   -   Put localFile");
  console.log("q     -   Quit\n");	
  lastPrompt = "";
}
function printProviders() {
  console.log("\n0     -   Amazon S3");
  console.log("1     -   Digital Ocean");
  console.log("2     -   Google Storage");
  console.log("3     -   Wasabi");
  console.log("4     -   Backblaze B2");
  console.log("5     -   Huawei");
  console.log("6     -   Alibaba");
  console.log("7     -   IBM");
  console.log("8     -   Oracle");
  console.log("9     -   Linode");
  console.log("255   -   Custom\n");
}
function menuPrompt(){
  process.stdout.write('Enter command: ');
  lastPrompt = "";  
}



function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
