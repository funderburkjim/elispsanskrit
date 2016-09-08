<?php
// called by 'saveData' Javascript function in update.php
$myFile = "prechange.txt";
$fh = fopen($myFile, 'w') or die("can't open file");
$stringData = $_POST["data"];
$datarr = json_decode($stringData,$assoc=true);

fwrite($fh,"[\n");
$n = count($datarr);
for($i=0;$i<$n;$i++) {
 $data = $datarr[$i];
 $obj = json_encode($data);
 fwrite($fh,"$obj\n");
 if (($i+1)!= $n) {
  fwrite($fh,",\n");
 }
}
fwrite($fh,"]\n");
//fwrite($fh, $stringData);
fclose($fh);
?>