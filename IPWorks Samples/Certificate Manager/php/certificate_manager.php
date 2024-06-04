<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks 2022 Demos - Certificate Manager</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks 2022 Demos - Certificate Manager">
</head>

<body>

<div id="content">
<h1>IPWorks - Demo Pages</h1>
<h2>Certificate Manager</h2>
<p>Full-featured certificate management sample.  Demonstrates how to list, manage, create, and sign certificates.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworks_certmgr.php');
require_once('../include/ipworks_const.php');

?>

<?php
  $certstore = '';
  $certsubject = '';
  $machine = '';

  if (isset($_GET['certstore'])) {
    $certstore = $_GET["certstore"];
  }
  if (isset($_GET['certsubject'])) {
    $certsubject = $_GET["certsubject"];
  }
  if (isset($_GET['machine'])) {
    $machine = $_GET["machine"];
  }
  
  $thispage = $_SERVER["PHP_SELF"];
  $certmgr = new IPWorks_CertMgr();
?>

<ul>
<li><b><a href="<?php echo $thispage; ?>?machine=yes">[List Machine Stores]</a>
<li><b><a href="<?php echo $thispage; ?>">[List User Stores]</a>

<p>
<dl>
<?php
  if ($machine == "yes") {
  	$stores = $certmgr->doListMachineStores();
  	$storeparam = "&machine=yes";
  } else {
  	$stores = $certmgr->doListCertificateStores();
  	$storeparam = "";
  }

  $mystores =  explode("\r\n",$stores);
  for($i = 0; $i < count($mystores) -1; $i++){

    $store = $mystores[$i];

    if (strtoupper($certstore) == strtoupper($store)) {

      //if store is selected then expand it
      echo "<dt><b>[ &nbsp; ] <u>" . $store . "</u></b>";

      echo "<dd><ul>";

      $certmgr->setCertStore($store);
      try {
        $certs = $certmgr->doListStoreCertificates();
      
      
        $mycerts = explode("\r\n",$certs);

        for($j=0;$j<count($mycerts) -1;$j++){
          $subjectList = explode("\t",$mycerts[$j]);
          $subject = $subjectList[0];

          if (strtoupper($certsubject) == strtoupper($subject)) {
          //if certificate is selected then show it
            echo "<li><b><u><a name=selectedCert>" . $subject . "</u></b>";

            $certmgr->setCertSubject($subject);

            echo "<table bgcolor=whitesmoke>";

            echo "<tr><td><i>Issuer:              <td>" . $certmgr->getCertIssuer();
            echo "<tr><td><i>Subject:             <td>" . $certmgr->getCertSubject();
            echo "<tr><td><i>Version:             <td>" . $certmgr->getCertVersion();
            echo "<tr><td><i>SerialNumber:        <td>" . $certmgr->getCertSerialNumber();
            echo "<tr><td><i>SignatureAlgorithm:  <td>" . $certmgr->getCertSignatureAlgorithm();
            echo "<tr><td><i>EffectiveDate:       <td>" . $certmgr->getCertEffectiveDate();
            echo "<tr><td><i>ExpirationDate:      <td>" . $certmgr->getCertExpirationDate();
            echo "<tr><td><i>PublicKeyAlgorithm:  <td>" . $certmgr->getCertPublicKeyAlgorithm();
            echo "<tr><td><i>PublicKeyLength:     <td>" . $certmgr->getCertPublicKeyLength();

            echo "</table>";
          } else {
            echo "<li><a href=" . $thispage . "?";
            echo "certstore=" . urlencode($store);
            echo "&certsubject=" . urlencode($subject) . $storeparam;
            echo "#selectedCert>" . $subject . "</a>";
          }
        } //for loop
      } catch (Exception) {
        echo "<font color=\"RED\">Cannot open certificate store!</font><br/>";
      }


      echo "</ul>";

    } else {

      //if store not selected, just list it
      echo "<dt><b>";
      echo "<a href=" . $thispage . "?certstore=" . urlencode($store) . $storeparam . ">";
      echo "[+]</a> ";
      echo "<a href=" . $thispage . "?certstore=" . urlencode($store) . $storeparam . ">";
      echo $store;
      echo "</a></b>";

    }
  } //for loop
?>

</dl>

</ul>

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
