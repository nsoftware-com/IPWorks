<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - S3</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - S3">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>S3</h2>
<p>Uses the S3 component to allow you to interact with Amazon S3 and other S3-compatible services.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_s3.php');
require_once('../include/ipworks_const.php');

?>

<?php
$buckets = array();
$objects = array();
?>

<?php
class MyS3 extends IPWorks_S3
	{
		function fireBucketList($param) {
			$GLOBALS['buckets'][] = $param['bucketname'];
			return $param;
		}

		function fireObjectList($param) {
            $GLOBALS['objects'][] = $param['objectname'];
			return $param;
		}
	}

$s3 = new MyS3();

?>

<?php
if ($_SERVER['REQUEST_METHOD'] == 'POST') {
  $s3->setAccessKey($_POST['accessKey']);
  $s3->setSecretKey($_POST['secretKey']);
  $s3->setServiceProvider($_POST['serviceProvider']);
  if (isset($_POST['bucket']) && $_POST['button']!="Connect"){
    $s3->setBucket($_POST['bucket']);
  }
  try{
    switch($_POST['button']){
        case 'Add Bucket':
            $s3->setBucket($_POST['newbucketname']);
            $_POST['bucket'] = $_POST['newbucketname'];
            $s3->doCreateBucket();
            break;
        case 'Delete Bucket':
            $s3->doDeleteBucket($_POST['bucket']);
            $s3->setBucket('');
            break;
        case 'Add Object':
            $s3->setLocalFile($_POST['newobjectpath']);
            $s3->doCreateObject($_POST['newobjectname']);
            $s3->setLocalFile('');
            break;
        case 'Remove Object':
            $s3->doGetObject($_POST['object']);
            $s3->doDeleteObject($_POST['object']);
            break;
    }
    $s3->doListBuckets();
    if($s3->getBucket() != '')
        $s3->doListObjects();
  } catch (Exception $e) {
    echo 'Error: ',  $e->getMessage(), "\n";
  }

}
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td width="20%">Access Key:        <td><input type=text name=accessKey value="<?php echo isset($_POST["accessKey"])?$_POST["accessKey"]:""; ?>" size=40>
 <tr><td width="20%">Secret Key:        <td><input type=password name=secretKey value="<?php echo isset($_POST["secretKey"])?$_POST["secretKey"]:""; ?>" size=40>
 <tr><td width="20%">Service Provider:  <td><select name=serviceProvider>
                                <option value=0 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '0') ? 'selected' : ''; ?>>Amazon S3</option>}
                                <option value=1 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '1') ? 'selected' : ''; ?>>Digital Ocean Spaces</option>
                                <option value=2 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '2') ? 'selected' : ''; ?>>Google Cloud Storage</option>
                                <option value=3 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '3') ? 'selected' : ''; ?>>Wasabi</option>
                                <option value=4 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '4') ? 'selected' : ''; ?>>Backblaze B2</option>
                                <option value=5 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '5') ? 'selected' : ''; ?>>Huawei Cloud Object Storage</option>
                                <option value=6 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '6') ? 'selected' : ''; ?>>Alibaba Cloud Object Storage</option>
                                <option value=7 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '7') ? 'selected' : ''; ?>>IBM Cloud Object Storage</option>
                                <option value=9 <?php echo (isset($_POST['serviceProvider']) && $_POST['serviceProvider'] === '9') ? 'selected' : ''; ?>>Linode Object Storage</option>
                            </select>

 <tr><td><td><input name=button type=submit value="List Buckets">

</table>


<table width="90%">
  <tr><td width="20%">Bucket:<td style="width:400px" style="text-align:right">
   <select name=bucket size=10 style="width: 400px">
   <?php
     for($i= 0; $i < count($buckets); $i++)
     {
   ?>
        <option value=<?php echo $buckets[$i]; ?> <?php echo (isset($_POST['bucket']) && $_POST['bucket'] === $buckets[$i]) ? 'selected' : ''; ?>><?php echo $buckets[$i]; ?></option>
<?php
     }
?>

    </select><td style="width:550px">
    <table>
    <tr><td><input name=button type=submit value="List Objects"></tr>
    <tr><td><input name=button type=submit value="Add Bucket"><td> New Bucket Name: <td>
    <input type=text name=newbucketname value="new-bucket-name" size=20> </tr>
    <tr><td><input name=button type=submit value="Delete Bucket"></tr>
    </table><td><td>

  </tr>
</table>
</center>


<center>
<table width="90%">
  <tr><td width="20%">Object:<td style="width:400px" style="text-align:right">
   <select name=object size=12 style="width: 400px">
   <?php
     for($i= 0; $i < count($objects); $i++)
     {
   ?>
        <option value=<?php echo $objects[$i]; ?> <?php echo (isset($_POST['object']) && $_POST['object'] === $objects[$i]) ? 'selected' : ''; ?>><?php echo $objects[$i]; ?></option>
<?php
     }
?>
    </select><td style="width:550px">
    <table>
    <tr><td><input name=button type=submit value="Get Object"></tr>
    <tr><td><input name=button type=submit value="Add Object"><td> New Object Path: <br><br> New Object Name: <td>
    <input type=text name=newobjectpath value="C:/objects/object.txt" size=20> <br><br> <input type=text name=newobjectname value="new-object-name" size=20> </tr>
    <tr><td><input name=button type=submit value="Remove Object"></tr>
    </table><td>

  </tr>
</table>
</center>
</form>

<?php
if (isset($_POST['object']) && $_POST['button'] == 'Get Object'){
    $s3->doGetObject($_POST['object']);
    echo 'Object Data: ',  $s3->getObjectData(), "\n";
}
?>
<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-IPPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
