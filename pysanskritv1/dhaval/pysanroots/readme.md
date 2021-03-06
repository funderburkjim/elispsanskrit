
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

Revision (2016-09-17):
 roots_a1_prep.txt
 This contains 20 roots which, in sanverb, end in 'a'.
 14 of them correspond to Denominatives in MW, with same meaning
  3 of them correspond to Denominatives in MW, with different meaning
  3 of them have no corresponding Denominative in MW.
Addendum (2016-10-02):
 The two sanverb roots nivAsa and saNgrAma are added to roots_a1_prep.txt.
 nivAsa probably corresponds to causal of mw prefixed roots nivas;
 saNgrAma corresponds to mw prefixed root saMgrAm, which may be a Denominative.

In the revision to construction of sanverb1_cp, ALL of these 20 are
roots in Sanverb are made to correspond to a root spelling without the 'a'.
To accomplish this, we first create roots_a1.txt from roots_a1_prep.txt, so 
that roots_a1.txt has the same form as roots_a.txt.

```
python roots_a1.py sanverb_cp.txt roots_a1_prep.txt roots_a1.txt > roots_a1_log.txt
```

Then we modify sanverb_cp.txt using both roots_a and roots_a1.
```

python sanverb1_cp.py sanverb_cp.txt roots_a.txt roots_a1.txt sanverb1_cp.txt

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

## verbdata_map

The conjugation data for present tense (from generated forms) identifes
each (present tense at least) form with
* rootwithanubandha
* gana (class)
* number (sequence number)
* pada

This conjugation data for present tense is in [conj_pre](https://github.com/funderburkjim/elispsanskrit/blob/master/pysanskritv1/dhaval/conjtab/conj_pre.txt).
A sample line is:
```
aka! pre 01.0901P:[akati akataH akanti akasi akaTaH akaTa akAmi akAvaH akAmaH]
```

In order to compare these forms with pysanskrit generated forms, it is 
necessary to get an MW root spelling.  As a first approximation, this 
is likely to be the spelling given by the `rootwithoutanubandha` field of
[verbdata.txt](https://github.com/funderburkjim/elispsanskrit/blob/master/pysanskritv1/dhaval/function/verbdata.txt).

In the example above, in verbdata, there is just one record matching `aka!`:
```
aka!:kuwilAyAM gatO:ak:01:0901:pa:sew:अ॑कँ॑:488:512:517:ak1_akaz_BvAxiH+kutilAyAM_gawO:
```

so the candidate spelling is `ak`.  Indeed MW has a root `ak`.  So this is
the desired answer.

```
python verbdata_map.py verbdata_map.txt > verbdata_map_log.txt
```

The output is a list of 4 colon-separated fields from verbdata:
* verbwithanubandha
* gana
* number
* verbwithoutanubandha

It turns out that no two of the records of verbdata have, as of this
writing (Sep 9, 2016), the same values for all of the first three fields.
In other words, these three fields determine each of the 2210 records of
verbdata.txt.

In particular, there is only one possibility for verbwithoutanubandha, given
the values of the first three fields.  So, we can consider the lines of
this verbdata_map.txt file to be identified by this 3-part key.

It is NOT true that this mapping is one-to-one.  That is, there are
several cases where two lines of verbdata differ in one or more of the three 
fields (rootwithanubandha, gana, number), but nonetheless have the same
value for the rootwithoutanubandha field.

For possible further examination, these duplicates (involving 396 lines of
verbdata, or 18%) are printed to the verbdata_map_log.txt file.


## verbdata_map1.txt

```
python verbdata_map1.py verbdata_map.txt ../pysanroots/roots_a.txt ../pysanroots/roots_a1.txt verbdata_map1.txt > verbdata_map1_log.txt
```

Verbdata_map1 alters the mapping based on the correspondence of roots_a and
roots_a1.

For example, verbdata_map shows the correspondence:
```
Here, rootwithanubandha = aMsa (first field)
and , rootwithoutanubandha is the same, aMsa.
aMsa:10:0460:aMsa
```
and roots_a shows the correspondence:
```
rootwithoutanubandha is aMsa, and corresponding MW root is aMs
aMsa:10A,10P##aMs:00
```
So, the corresponding altered line in verbdata_map1 is
```
aMsa:10:0460:aMs
```

There are 63 instances of the roots_a mapping.
These lead to 78 changes in the mapping between verbdata_map and verbdata_map1.

The reason that there are more changes (78) in the mapping than there are
instances in roots_a is exemplified by these instances in verbdata_map1.
```
SraTa:10:0019:SraT  CHG
SraTa:10:0360:SraT  CHG
```
As indicated, the root SraTa occurs in two places in the verbdata 
dhatupatha, (numbers 19 and 360 in the 10-conjugations). So, there is an 
extra change required for this root.

The changes to the mapping are itemized in verbdata_map1_log.txt.

