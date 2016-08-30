
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
* 1581 cases where the root-class-pada information is identical
* 74 cases where roots are present in both sources, but the class-pada 
  information from conj_pre is less inclusive than that in verbdata
* 29 cases where a root is present only in verbdata, but has no present
  tense conjugation according to conj_pre.

