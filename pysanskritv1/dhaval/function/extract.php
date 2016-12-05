<?php
 /* Extract various information from function.php, verbdata.php
 */


$itemname = $argv[1];
$phpmodule = $argv[2];  // function.php, verbdata.php
require_once($phpmodule); 
$filename = "$itemname.txt";
$fp = fopen($filename,"w");
$d = $$itemname;
/*
echo count($d) . "\n";
echo "itemname=$itemname\n";
echo $d[0]," ",$d[1];
echo "verbdata count=".count($verbdata)."\n";
*/
foreach($d as $value) {
 fwrite($fp,"$value\n");
}
fclose($fp);

?>
