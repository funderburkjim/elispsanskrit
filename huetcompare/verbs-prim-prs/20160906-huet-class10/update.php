<?php
 $prechange_file='prechange.txt';
 $allrecs = read_prechange($prechange_file);
 write_prechange($allrecs,'prechange_v1.txt');
?>
<!DOCTYPE html>
<html>
<head>
<meta encoding="utf-8">
<script src="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.2.1/mustache.min.js"></script>
<script   src="https://code.jquery.com/jquery-3.1.0.min.js"   integrity="sha256-cCueBR6CsyA4/9szpPfrX3s49M9vUU5BgtiJj06wt/s="   crossorigin="anonymous"></script>
<style>
 #words {color:black;
  margin :5px;
  padding:5px;
  border-style: solid;
  position: absolute;
  left:0px;
  top: 50px;
  height: 500px;
  width:150px;
  overflow:auto;
 }
 .word_TODO {color:black}
 .word_DONE {color:green; font-weight:bold}
 .word_DONE_y {color:green; font-weight:bold}
 .word_DONE_n {color:red; font-weight:bold}
 .word_DONE_q {color:orange; font-weight:bold}
 .helptitle {color:red}
 /*
 .word1_TODO {color:red}
 .word1_DONE {color:green}
 */
 #main {
  margin :5px;
  padding:5px;
  border-style: dotted;
  position: absolute;
  top: 50px;
  left:200px;
  height: 500px;
  width:600px;
  //overflow:auto;
 }
#header {
 width: 100%;
 margin-top:10px;
 position:absolute;
 top:0px;
 height:30px;
 left:0px;
 /*background:grey;*/
}
#wordstitle {
  margin :5px;
  padding:5px;
  /*border-style: dotted; */
  position: absolute;
  left:0px;
  width:150px;
  top:0px;
}
#maintitle {
  margin :5px;
  padding:5px;
  /*border-style: dotted;*/
  position: absolute;
  left:200px;
  width:600px;
}
#message {
   margin :5px;
  padding:5px;
  /*border-style: dotted;*/
  position: absolute;
  top:510px;
  /*left:200px;*/
  height: 50px;
  width:600px;
  color:red;
  font-size:larger;
}
a:link {
    color: #FF0000;
}

/* visited link */
a:visited {
    color: #00FF00;
}

/* mouse over link */
a:hover {
    color: #FF00FF;
    cursor: pointer;
}

/* selected link */
a:active {
    color: #0000FF;
}

#mwDataframe {
 color: black; background-color: white;
 border:solid; 
 /*position:absolute; */
 margin-top:5px;
 margin-bottom:5px;
 left:5px;
 height: 160px;
 width: 600px;
 overflow: auto;
}
#huetDataframe {
 color: black; background-color: white;
 border:solid; 
 /*position:absolute; */
 margin-top:5px;
 margin-bottom:5px;
 left:5px;
 height: 200px;
 width: 600px;
 overflow: auto;
}
</style>

<script type="text/javascript">
var prechange_data=null;
var saveData = function(callback) {
 // ref: http://stackoverflow.com/questions/20948155/simple-save-to-json-file-with-jquery
//console.log('in saveData');
var jsonstring=JSON.stringify(prechange_data);
console.log('length of jsonstring=',jsonstring.length);
$.ajax
    ({
        type: "POST",
        dataType : 'text',
        url: 'update_save_json.php',
        data: { data: jsonstring },
        //
        success: submit_change_2, //function (data,textStatus,jqXHR) {console.log("saveData says Thanks!"); },
        error: function(data,textStatus,jqXHR) {alert("saveData says Error!"+jqXHR);}
    });
};
var submit_change = function() {
 // read type (y,n,q) and case (base 0)
 var thetype=$('#changetype input[name="changetype"]:checked').val();
 var thecase=$('#current_case').attr('value');
 var idx = thecase - 1;
 console.log('thetype=',thetype,'thecase=',thecase);
 // change formatting of word in list to 'DONE'
 // $('#words_'+idx).removeClass('word_TODO').addClass('word_DONE');
 // change formatting of word in list to 'DONE', with a color based
 // on the 'type'
 var classes_to_remove = "word_TODO word_DONE_y word_DONE_n word_DONE_q"
 $('#words_'+idx).removeClass(classes_to_remove).addClass('word_DONE_'+thetype);
 // change underlying data here in Javascript
 data = prechange_data[idx];
 console.log('modifiying idx=',idx,data['key1']);
 data['status']='DONE';
 data['type']=thetype;
 // save the data to server
 saveData(submit_change_2);
};
var submit_change_2 = function(data,textStatus,jqXHR) {
 $('#message').html('<p>Case submitted</p>');

 $('#message').hide(2000,function() {/*$('#form').hide();*/});
 
 //console.log('saveData succeeded');
};
var cancel_change = function() {
 $('#form').hide();
};
var mw_basicDisplay = function (key) {
  var dict = 'mw'; //$('#dict').val();
  var input = 'slp1'; $('#input').val();  // Not used for AE
  var output = 'deva'; //$('#output').val();
  var accent = 'no'; //$('#accent').val();
  var options = '0'; $('#options').val();
  //console.log('key=',key,', dict=',dict,', input=',input,
  //  ', output=',output,' accent=',accent);
  // TODO: check for valid inputs before ajax call
  var urlbase="http://www.sanskrit-lexicon.uni-koeln.de/scans/awork/apidev/getword.php";
  var url =  urlbase +  
   "?key=" +escape(key) + 
   "&output=" +escape(output) +
   "&dict=" + escape(dict) +
   "&accent=" + escape(accent) +
   "&options=" + escape(options) +
   "&input=" + escape(input);
    jQuery("#mwDataframe").attr("src",url);  // for iframe

   //console.log('mw_basicDisplay for',key);
   //console.log('url=',url);

 }; // mw_basicDisplay

var huet_basicDisplay1 = function (data,textStatus,jqXHR) {
/* data is href
<div class="latin12">Entry found: <a class="navy" href="http://sanskrit.inria.fr/DICO/24.html#garva"><i>garva</i></a>
</div>
*/
 var href = data;
 console.log('huet_basicDisplay1: href=',href);
 jQuery("#huetDataframe").attr("src",href);
};
var huet_basicDisplay = function (key) {
/* Example 
http://sanskrit.inria.fr/cgi-bin/SKT/sktindex?lex=SH&t=SL&q=guru
 This returns an intermediate web page, which has a link to the
 page of the Dictionary that we want
<div class="latin12">Entry found: <a class="navy" href="http://sanskrit.inria.fr/DICO/24.html#garva"><i>garva</i></a>
</div>
 We want to extract the href, and load this href into our iframe.
 Cross-side scripting issues make this difficult (Impossible?) using
 Javascript.  So, we have written a php script that uses curl for
 the same purpose.  This script returns the url for the sanskrit-heritage
 dictionary link to the word in question
*/
  var urlbase="huet_get.php";
  var url =  urlbase +  
   "?key=" +escape(key);
 jQuery.ajax ({
  type: "GET",
  dataType:"text",
  url: url,
  success: huet_basicDisplay1,
  error: function(data,textStatus,jqXHR) {
    jQuery("#huetDataframe".attr("src",
     "<p> Error accessing Huet for " + key + "</p>"));
    },
  });
 /*
  jQuery("#huetDataframe").attr("src",url);  // for iframe
 */
 }; // huet_basicDisplay

var update_make_form = function (icase) {
 //console.log("update_make_form: icase=",icase);
 $('#form').show();
 var data = prechange_data[icase]; 
 var datatype=data['type']; // should be n,t, or p
 // check the appropriate radio button
 $('#change_' + datatype).prop('checked',true);
 var html,view,template,record;
 template="<span id='current_case' value='{{casenum}}'>Case {{casenum}}: </span>" +
  "<span>hw={{key1}}</span> " +
  "<span class=\"word1_{{status}}\">{{status}}</span>" +
  "<br/>" +
  "<span>record: {{record}}</span>" +
  "<hr/>";
 record = data['mwkey'] + ' v.' + data['huetkey']
 view = {casenum:data['case'],
  key1:data['key1'],
  status:data['status'],
  record:record};
 html = Mustache.render(template,view);
 $('#formrecord').html(html);
 // Last step, display the underlying data
 mw_basicDisplay(data['mwkey']);
 huet_basicDisplay(data['huetkey']);

};
var update_init_words = function () {
 var i,data,icase,key1;
 var html;
 html = "";
 //$('#words').html(html);
 //var li_template="<li>{{i}}. {{word}}</li>\n";
 var li_template="<a id=\"words_{{icase}}\" onclick=\"update_make_form({{icase}});\" class=\"word_{{status}}_{{type}}\">{{idx}}. {{word}}</a><br/>\n";
 var view,html1
 for (i=0;i<prechange_data.length;i++) {
  data = prechange_data[i];
  view = {icase:i, status:data['status'],idx:i+1,word:data['mwkey'] + ' v. ' + data['huetkey'], type:data['type']};
  html1 = Mustache.render(li_template,view);
  html = html + html1
  //console.log('loadData:',i,data['status'],data['key1']);
 }
 $('#words').html(html);
};
function success_read(data,textStatus,jqXHR) {
 prechange_data=data;
 console.log('read of prechange successful');
 console.log('length of prechange_data is',prechange_data.length);
 update_init_words();
 $('#form').hide();
 //saveData();
}
var loadData = function(filein) {
 console.log('loading data');
 $.ajax({
  dataType: "json",
  url: filein,
  cache: false,  // otherwise, cached data may be loaded!
  /*data: data,*/
  success: success_read
 });

};
var process_data = function() {
 loadData("prechange.txt");
};
$(document).ready(process_data);


</script>
</head>
<body>
<div id="header">
<div id="wordstitle"><span>CASES:  MW v. HUET</span></div>
<div id="maintitle">
 <a href="help.html" target="_help"><span class="helptitle">Instructions</span></a>
</div>
</div> <!-- header-->
<div id="words">

</div>
<div id="main">
<div id="form">
 <div id="formrecord">
  
 </div>
 <div class="formtitle">Does the Huet root Correspond to the MW root?
 </div>
 <div id="changetype">
 <input type="radio" name="changetype" id="change_n" value="n">
 <label for="change_n">No, the roots are different</label>
 <input type="radio" name="changetype" id="change_y" value="y">
 <label for="change_y">Yes, they appear to be the same</label>
 <input type="radio" name="changetype" id="change_q" value="q">
 <label for="change_q">Cannot decide</label>
 </div>
 <div>
  <input type="button" name="submit" id="submit" value="Submit" onclick="submit_change();">
  <input type="button" name="cancel" id="cancel" value="Cancel" onclick="cancel_change();">
 </div>

<iframe id="mwDataframe">
</iframe>
<iframe id="huetDataframe">
</iframe>
</div> <!-- id=form ends -->
<div id="message">
</div>
</div>
</body>
</html>
<?php
function read_prechange($filename) {
 $fp = fopen($filename,"r");
 if (!$fp) {
    print "<n>$n</n>\n";
    print "<p>read_prechange ERROR: Could not open '$filename'</p>\n";
    exit;
 }
 $datarr=array();
 while (!feof($fp)) {
  $line=fgets($fp);
  $line = trim($line);
  if ($line == '') {continue;}
  $data = json_decode($line,$assoc=true); // assoc array
  $datarr[]=$data;
 }
 return $datarr;
}
function write_prechange($allrecs,$filename) {
 $fp = fopen($filename,"w");
 if (!$fp) {
    print "<n>$n</n>\n";
    print "<p>write_prechange ERROR: Could not open '$filename'</p>\n";
    exit;
 }
 foreach($allrecs as $allrec) {
  $s = json_encode($allrec);
  fwrite($fp,"$s\n");
 }
 fclose($fp);
}
?>
