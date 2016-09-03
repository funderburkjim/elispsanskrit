
# ROOTS
Examine root lists derived from SanskritVerb and from MWvlex

## root-class-pada implied by SanskritVerb

In dhaval/function/verbdata.txt we have what is at least close to the
dhatupatha used by SanskritVerb.  One useful feature is the presence of
a spelling of each root *without* anubandha.  This can be used as a first
approximation proxy for the MW root spelling.   

```
aka!:kuwilAyAM gatO:ak:01:0901:pa:sew:अ॑कँ॑:488:512:517:ak1_akaz_BvAxiH+kutilAyAM_gawO:
aki!:lakzaRe:aNk:01:0092:A:sew:अ॑किँ॒:67:66:68:afk1_akiz_BvAxiH+lakRaNe:45
```

Also, in dhaval/conjtab/conj_pre.txt we have information that supplies
the pada (P,A).

The sanverb_cp program generates two lists of roots with class-padas
```

python sanverb_cp.py ../dhaval/function ../dhaval/conjtab/conj_pre.txt conjtab_cp.txt sanverb_cp.txt > sanverb_cp_log.txt
```

sanverb_cp.txt gathers all its information from the verbdata.txt file.
conjtab_cp.txt uses the roots and padas from the extracted present tense
conjugations (conj_pre.txt); it finds the 'root without anubandha' spelling
by reference to verbdata.txt.

The two lists are quite similar, but with a few differences. The next program
compares the two lists.
```
python compare_cp.py sanverb conjtab sanverb_conjtab
```
The output file (sanverb_conjtab.txt) indicates
* 1571 (v. 1581) cases where the root-class-pada information is identical
* 74 (74) cases where roots are present in both sources, but the class-pada 
  information from conj_pre is less inclusive than that in verbdata
* 39 (29) cases where a root is present only in verbdata, but has no present
  tense conjugation according to conj_pre.

Note:  The first number is after revision to verbdata of Sep 2, 2016. The
   second number (in parentheses) is the number in the prior revision of
   verbdata.

## MWvlex root-class-pada

The [MWvlex repository](https://github.com/funderburkjim/MWvlex) deals with
extraction of the root information from the Monier-Williams dictionary.

The file [verb_cp.txt](https://github.com/funderburkjim/MWvlex/blob/master/step1/verb_cp.txt) presents 2150 *simple* roots (i.e., not prefixed, not Denominatives, etc.). Some sample records are:
```
aMS:9:10P,10A
aMs:87:0   no class-pada information
aMh:107:1A,10P
aMh:114:0  no class-pada information
akz:423:1,5 class, but no pada-information
```
The three fields of each record are:
* root - spelled in SLP1 transliteration
* L  - the record identifier in mw.xml
* cplist - a comma-separated list of class pada information. There are three
  forms for each item of this list:
  * cp   class (1-10), pada (A or P)
  * 0    no class pada information. Notice the difference between `aMs` and
   `aMh`. One of the records for `aMh` has class pada information, and
   another record has no such information.
  * c   class information, but no pada information

```
python mwvlex_cp.py ../../../MWvlex/step1/verb_cp.txt mwvlex_cp.txt
```
The output mwvlex_cp.txt is comparable to sanverb_cp.txt and conjtab_cp.txt.
Some details:
* class-pada information will be aggregated for each root spelling
* class will be presented in 2 digits e.g. 
  * `aMh:01A,10P`  
  * `akz:01,05`
  * `aMs`:00

mwvlex_cp.txt has 1954 root spellings.

## compare root-class-pada from SanskritVerb and MWvlex
```
python compare_cp.py sanverb mwvlex sanverb_mwvlex
```
Here are summary statistics from sanverb_mwvlex_cp.txt
* 653 cases of identical class pada information
* 283 cases where mwvlex information *extends* that of sanverb
* 120 cases where sanverb information *extends* that of mwvlex
* 457 cases where sanverb and mwvlex differ, but neither extends the other
* 441 cases where a root appears only in mwvlex
* 171 cases where a root appears only in sanverb

## roots_a

There are several roots in sanverb_cp that end in 'a' (these verbs have a 
class 10 form) which are believed to correspond to MW roots spelled the
same except without that ending 'a'.

```
python roots_a.py sanverb_cp.txt mwvlex_cp.txt roots_a.txt > roots_a_log.txt
```

There are two forms of records in roots_a.txt:

```
In both cases, the last field is the mwvlex form.

1st form: (33 cases) sanverb has the 'a' form, but not the without 'a' form. 
aMsa:10A,10P##aMs:00

2nd form: (30 cases) sanverb has both the 'a' form and the without-'a' form.
aNka:10A,10P#aNk:01A#aNk:01A,10P
```

## sanverb1_cp
This uses the information of roots_a.txt to modify sanverb_cp.  The result
is sanverb1_cp.txt.  The 'a' roots in roots_a have their spelling changed
to 'without-a'; also, if needed, the already existing 'without-a' record
is merged with the 'a' record.


```
python sanverb1_cp.py sanverb_cp.txt roots_a.txt sanverb1_cp.txt

```
One statistic, there are 1684 records in sanverb_cp, and 1654 records in
sanverb1_cp.

## mwvlex1_cp:  Adjustments

This prompted by Dhaval's [comment](https://github.com/funderburkjim/elispsanskrit/issues/34#issuecomment-243677499).  

Probably, there will be further adjustments later.
For now (Aug 31, 2016), we make sure that class 10 roots have both
Parasmai and Atmane pada designations.  We also note in the log file
where these additions were required; this is so that we can back-check
MW to see if the original dictionary text was marked correctly in this detail.

```
python mwvlex1_cp.py mwvlex_cp.txt sanverb1_cp.txt mwvlex1_cp.txt > mwvlex1_cp_log.txt
```

Next, we'll compare the adjusted lists
```
python compare_cp.py sanverb1 mwvlex1 sanverb1_mwvlex1
```
