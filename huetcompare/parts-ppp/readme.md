
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's participles, passive past participle

ppp =  passive past participle

```
python huet_parts.py ../huet/SL_parts.xml ppp > huet_parts_log.txt
```
output files are huet_stems_ppp.txt and huet_decl_tables_ppp.txt

For each passive past participle form,  
the SL_parts.xml file does NOT provide the root.  

Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_stems_ppp.txt

```
python pysan_stems.py ../../grammar/prod/outputs/MW-verb-ppp.txt ppp
```
Output file is pysan_stems_ppp.txt

The data source is the list MW-verb-ppp.txt.
This program just collects the information in this file, and changes
format to that of huet_stems_ppp.txt.  Namely, it collects stems along
with all padas mentioned.  For comparison with Huet, the underlying root
and class of the participle is omitted in the output.

The input file has records like:
```
:akz ppp 1m:(akzamARa)
```
which we reformat to be like huet_stems_ppp.txt, such as
```
akzamARa:1A
```


## compare huet and pysan ppp stem data
```
python compare_stems.py ppp
```
The output is compare_stems_ppp.txt, which provides some analysis of the
similarities and differences between huet_stems_ppp.txt and
pysan_stems_ppp.txt

## Generate pysan declensions

The rest of this readme is a placeholder. Currently, there are not
precomputed declensions for passive past participles.  However, this
is not viewed as a major problem, in terms of comparison with Huet, since
ppp stems are declined with the common paradigms of adjectives ending in 'a'


```
python pysan_parts.py ppp  ../../grammar/prod/outputs/MW-verb-ppp-decl.txt

```
This input file of declensions of passive past participles is somewhat awkward to compare with the Huet file.  It constructs an output file for each
of the Huet declensions (huet_decl_tables_ppp), by matching on the
participle stem and class of the underlying root and the gender.

##  compare declension tables
```
python compare_decl_tables.py ppp 
```

output is compare_decl_tables_ppp.txt

## first summary

See [issue#20](https://github.com/funderburkjim/elispsanskrit/issues/20) for some discussion of the comparison.

