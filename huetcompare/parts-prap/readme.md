
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's participles, present participle parasmaipada

ppr = present participle
para = parasmaipada

```
python huet_parts.py ../huet/SL_parts.xml ppr para > huet_parts_log.txt
```
output files are huet_stems_prap.txt and huet_decl_tables_prap.txt

For each active present participle form, in addition to the pada (always 
'para' or 'P'), Huet provides the class of the underlying root. Unfortunately,
the SL_parts.xml file does NOT provide the root.  And then there is the
gender.  Note that feminine stems have the same 'stem' as the masculine and
neuter.  Also, there are no case 8 (vocative) forms given in the declensions.


Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_stems_prap.txt

```
python pysan_stems.py ../../grammar/prod/outputs/MW-verb-prap.txt prap
```
Output file is pysan_stems_prap.txt

The data source is the list MW-verb-prap.txt.
This program just collects the information in this file, and changes
format to that of huet_stems_prap.txt.  Namely, it collects stems along
with all padas mentioned.  For comparison with Huet, the underlying root
of the participle is omitted in the output.

The input file has records like:
```
:aMS prap 10a:(aMSApayat aMSayat)
```
which we reformat to be like huet_stems_prap.txt, such as
```
aMSApayat:10P
aMSayat:10P
```


## compare huet and pysan prf_stem data
```
python compare_stems.py prap
```
The output is compare_stems_prap.txt, which provides some analysis of the
similarities and differences between huet_stems_prap.txt and
pysan_stems_prap.txt

## Generate pysan conjugations

```
python pysan_parts.py prap  ../../grammar/prod/outputs/MW-verb-prap-decl.txt

```
This input file of declensions of present active participles is somewhat awkward to compare with the Huet file.  It constructs an output file for each
of the Huet declensions (huet_decl_tables_prap), by matching on the
participle stem and class of the underlying root and the gender.

##  compare declension tables
```
python compare_decl_tables.py prap 
```

output is compare_decl_tables_prap.txt

## first summary

See [issue#16](https://github.com/funderburkjim/elispsanskrit/issues/16) for some discussion of the comparison.

