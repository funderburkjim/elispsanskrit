
20160906-huet-class10

Compare certain Huet class '11' (Causal) roots to similarly spelled roots
in MW.  Objective is to determine: are the corresponding Huet/MW roots
"the same".

## comparea_class_pada.txt
This comes from this repository:
https://github.com/funderburkjim/elispsanskrit/tree/master/huetcompare/verbs-prim-prs

Sample line:
```
aMSa:11P # aMS:10P
```

This line means that Huet has a root 'aMSa' (SLP1 spelling), which he
classifies as 11P (In his system, '11' means 'causal').

Similarly, MW has a root 'aMS' (spelled without the final 'a') which he
classifies as class 10.  We suspect that these two roots are to be thought
of as the same, for example when comparing Huet conjugations to those of
pysanskrit. One such comparisoin is the subject of the huetcompare repository 
mentioned above.

One method of comparison is via the entries for `aMSa` in Huet's Sanskrit
Heritage dictionary (in French) and for `aMS` in the Monier-Williams dictionary.
If the senses correspond, we can consider this one strong fact supporting
the correspondence of the roots; we would then expect the inflected forms
of `aMSa` in Huet to correspond to the pysanskrit inflected forms of `aMS`.

Since there are only 36 cases in the comparea_class_pada file, it is feasible
to do this comparison manually.

## web app strategy
The rest of this current directory is devoted to making a web-application
(web form) that facilitates this manual comparison, by 
a. automating the dictionary lookup of corresponding words in the two 
   dictionaries
b. adding fields for indicating:
  - whether the comparison has been made for a given pair of roots
  - the result of the comparison
c. Saving this information as a simple JSON file on the server, and
   having the form update this file.
d. When all comparisons are done, writing a summary based on the JSON file.

## Technical details.

This code is run on Cologne server.  It has also been tested on a local
Windows 10 computer under XAMPP, and seems to run fine there also.

1. The comparea_class_pada.txt file is assumed to be given; it is constructed
  elsewhere, as mentioned above.
2. Change comparea_class_pada.txt to a JSON file, named prechange.txt.
```
python prechange.py comparea_class_pada.txt  prechange.txt
```

  The JSON file is an array, with a JSON object for each line of the input
  The fields of this object are constructed as:
  - case :  a sequence number
  - status : TODO  
  - type : n   
  - mwkey : the mw headword spelling
  - huetkey : the Huet headword spelling
  - line : the line of comparea_class_pada.txt

3. Make a copy of prechange.txt, so we will have the original to fall back
  on while the update program is being developed.
```
 cp prechange.txt  prechange_orig.txt
```
3. Now, the update.php program treates prechange.txt as a database file,
   and provides a user interface for the classification of each case
   as either 
   n - No, the MW root and the Huet root are different
   y - Yes, the two roots are the same
   q - Cannot decide if the two roots are the same or different

4. Here are some programmatic highlights:
  - update.php is the main web application. It makes use of 
    jQuery and Mustache Javascript libraries.  The Mustache library usage
    is not critical, but provides a convenient way to construct the form
    from a template and the JSON objects of prechange.txt.
  - The first thing update.php does is to load prechange.txt into the
    list of cases shown in the left pane.
  - The update_save_json.php is invoked by an Ajax Jquery call whenever
    the user clicks 'submit' for a case. It over-writes the prechange.txt file
    based on the current state of the Application.
  - When the user clicks on one of the cases, three things happen:
   - a form is generated for that case
   - The MW data for the mwkey is retrieved by means of the
    http://www.sanskrit-lexicon.uni-koeln.de/scans/awork/apidev/getword.php
    service, and put into the mw  iframe for the user to see.
   - huet_get.php script is used to, in effect, get the appropriate link
     into the Sanskrit Heritage dictionary from the given huetkey of the case.
     This script does this with curl, and a simple scraping of the curl output.
     Then, jQuery uses this href to fill the huet iframe.

5. Now it's up to the user to choose the radio button that best describes the
  relation between the two roots, based on the displayed information from
  the corresponding dictionaries.  When the user decides, and clicks the
  'Submit' button, her answer is saved, and the process is ready to be
  repeated for another case.   
6. Color coding on the list of cases indicates which have been done, and
  the answer chosen for those which have been done.

