
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's participles, indeclineable past participle 

ab  absolutive in tvaa (Huet)
ipp indeclineable past participle (pysan)

```
python huet_advs.py ../huet/SL_adverbs.xml ab > huet_indecl_ipp_log.txt
```
output file is huet_indecl_ipp.txt 

For each indeclineable past participle form,  
the SL_adverbs.xml file provides the root as stem, and the ipp forms as data.

ipp forms for causal and desiderative conjugations are written to log file
for future reference.

Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_indecl_ipp.txt

```
python pysan_advs.py ../../grammar/prod/outputs/MW-verb-ipp.txt ipp
```
Output file is pysan_indecl_ipp.txt

The data source is the list MW-verb-ipp.txt.
This program just collects the information in this file, and changes
format to that of huet_indecl_ipp.txt.  Namely, it collects stems along
with all padas mentioned.  For comparison with Huet, the underlying root
and class of the participle is omitted in the output.

The input file has records like:
```
:hruq ipp 1a:(hruqitvA hroqitvA)
```
which we reformat to be like huet_indecl_ipp.txt, such as
```
hruq:hruqitvA,hroqitvA
```


## compare huet and pysan ipp stem data
```
python compare_advs.py ipp
```
The output is compare_indecl_ipp.txt, which provides some analysis of the
similarities and differences between huet_indecl_ipp.txt and
pysan_indecl_ipp.txt


## first summary

See [issue#26](https://github.com/funderburkjim/elispsanskrit/issues/26) for some discussion of the comparison.

