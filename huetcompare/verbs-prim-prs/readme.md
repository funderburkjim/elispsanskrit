
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's verb inflected forms

```
python verbs-prim-prs.py ../huet/SL_roots.xml huet_class_pada.txt huet_conj_tables.txt > verbs_log.txt
```

From Huet's SL_roots.xml file, we derive all root-class-pada- variations for
which there is a conjugational form.


Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_class_pada.txt

The data source is the lest of verbs for MW in dcpforms-MW-verb.txt.
This program just collects the information in this file, and changes
format to that of huet_class_pada.txt.
```
python pysan_class_pada.py ../../grammar/prod/inputs/dcpforms-MW-verb.txt pysan_class_pada.txt
```
## compare huet and pysan class_pada data
```
python compare_class_pada.py huet pysan compare
```
The output is compare_class_pada.txt, which provides some analysis of the
similarities and differences between huet_class_pada.txt and
pysan_class_pada.txt

## Generate pysan conjugations

```
python pysan-prim-prs.py huet_conj_tables.txt pysan_conj_tables.txt
```
We read the huet conjugation tables, and for each one use the header
information to compute a pysan conjugation table.  Thus
pysan_conj_tables.txt have the same number of lines and be comparable.


## 
```
python compare_conj_tables.py huet pysan compare
```
We ignore the class-pada differences between huet and pysan.
We ignore about 500 instances where a Huet form (often, optative 3p) ends
in 'r' and the pysan form ends in 'H', and the two forms are otherwise identical.

## first summary

See [issue#7](https://github.com/funderburkjim/elispsanskrit/issues/7) for some discussion of the comparison.

