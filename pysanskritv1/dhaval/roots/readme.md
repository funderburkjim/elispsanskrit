
dhaval/roots

Various analyses involving dhaval/function/verbdata.txt

## dupnorm

   Identify records of verbdata.txt that have
   verbwithanubandha associated with more than 1 verbwithoutanubandha

```
python verbdata_dupnorm.py verbdata_dupnorm.txt

19 duplicates (original)
 2 duplicates  (Sep 1, 2016)
 3 duplicates  (Sep 3, 2016)
  (These duplicates are in verbdata_dupnorm.txt)
```

## dupsutra
 Identify dhatapatha sutra numbers that appear in multiple records of 
 verbdata.txt
```
 python verbdata_dupsutra.py verbdata_dupsutra.txt
 
43 cases found
43 cases still (Sep 1, 2016)
42 cases (Sep 3, 2016)
```

# verbdata_map

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

