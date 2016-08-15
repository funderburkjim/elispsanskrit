
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's verb inflected forms, conditional tense

```
python verbs_tp.py ../huet/SL_roots.xml cnd >verbs-tp-log.txt
```
output files are huet_stems_cnd.txt and huet_conj_tables_cnd.txt

From Huet's SL_roots.xml file, we derive all roots for
which there is a form with the conditional tense paradigm. (tp = tense paradigm)

huet_stems_cnd.txt has all the stems that so occur, along with a list of
the padas which occur.



Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_stems_cnd.txt

The data source is the list of verbs for MW in dcpforms-MW-verb.txt.
This program just collects the information in this file, and changes
format to that of huet_stems_cnd.txt.  Namely, it collects stems along
with all padas mentioned.

```
python pysan_stems.py ../../grammar/prod/inputs/dcpforms-MW-verb.txt cnd
```
Output file is pysan_stems_cnd.txt

## compare huet and pysan cnd_stem data
```
python compare_stems.py cnd
```
The output is compare_stems_cnd.txt, which provides some analysis of the
similarities and differences between huet_stems_cnd.txt and
pysan_stems_cnd.txt

## Generate pysan conjugations

```
python pysan_tp.py cnd ../../grammar/prod/outputs/MW-verb-fut.txt

```
We read the huet conjugation tables, and for each one use the header
information to match a conditional tense conjugation precomputed by pysan in 
the MW-verb-fut.txt file.  As currently written, certain details of
 the pysan computation of conditional tense conjugations depends on the underlying
present tense conjugation class of the roots. Since this class is unavailable
in the conditional tense conjugation information from Huet, pysan cannot thus
compute a conditional tense solely on the basis of the huet_conj_tables_cnd.txt file. That's why we're using MW-verb-fut file.

##  compare conjugation tables
```
python compare_conj_tables.py cnd 
```

output is compare_conj_tables_cnd.txt

## first summary

See [issue#10](https://github.com/funderburkjim/elispsanskrit/issues/10) for some discussion of the comparison.

