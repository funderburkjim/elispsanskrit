<?php
 if (isset($_GET['key'])) {
  $key = $_GET['key'];
 }else if (isset($argv[1])){
  $key = $argv[1];
 }
 if (! isset($key)) { 
  echo  "huet_get.php: key required\n";
  exit();
 }
 $ch = curl_init();
 $url="http://sanskrit.inria.fr/cgi-bin/SKT/sktindex?lex=SH&t=SL&q=$key";
 // set url 
 curl_setopt($ch, CURLOPT_URL, $url);
 //return the transfer as a string 
 curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1); 
 // $output contains the output string 
 $output = curl_exec($ch); 
 //print $output;
 // Now, examine output to find the 'real' href we want
 /* example: we want to pull out tht href attribute
<tr><th><div class="latin12">Entry found: <a class="navy" href="http://sanskrit.inria.fr/DICO/1.html#ak.s"><i>ak&#7779;</i></a>
*/
 // Use regexps
 if (!preg_match('/Entry found: <a class="navy" href="(.*?)">/',$output,$matches)) {
   echo "<p> could not find Sanskrit Heritage entry for $key</p>\n";
   exit();
  }
 $href = $matches[1];
 //echo "<!--href=$href-->\n";
 //echo "<a href=\"$href\">Please click this link for $key</a>";
 echo $href;
 exit(0);
 // Now, access the contents of $href
 curl_setopt($ch, CURLOPT_URL, $href);
 $output = curl_exec($ch); 
// close curl resource to free up system resources 
 curl_close($ch);      
 echo $output;
?>