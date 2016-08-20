
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## interpret Huet's participles, indeclineable past participle (for roots with prefix)

abs  absolutive in ta (Huet)
ippa indeclineable past participle (pysan)

```
python huet_advs.py ../huet/SL_final.xml abs > huet_indecl_ippa_log.txt
```
output file is huet_indecl_ippa.txt 

For each indeclineable past participle form,  
the SL_final.xml file provides the root as stem, and the abs forms as data.

ippa forms for causal and desiderative conjugations are written to log file
for future reference.

Programming note2: We made a couple of adjustments to huet's data to 
facilitate comparisons.
* Huet uses a convention for homonym representation. For example there 
  are two homonyms for `aja`, and these are distinguished as `aja#1` and
  `aja#2`.   
* We drop the homonym distinction, leaving just `aja`.

## generate pysan_indecl_ippa.txt

```
python pysan_advs.py ../../grammar/prod/outputs/MW-verb-ippa.txt ippa
```
Output file is pysan_indecl_ippa.txt

The data source is the list MW-verb-ippa.txt.
This program just collects the information in this file, and changes
format to that of huet_indecl_ippa.txt.  

The input file has records like:
```
:kal ippa 10a:(kalayya kAlya)
```
which we reformat to be like huet_indecl_ippa.txt, such as
```
kal:kalayya,kAlya
```


## compare huet and pysan ippa data
```
python compare_advs.py ippa
```
The output is compare_indecl_ippa.txt, which provides some analysis of the
similarities and differences between huet_indecl_ippa.txt and
pysan_indecl_ippa.txt


## first summary

See [issue#27](https://github.com/funderburkjim/elispsanskrit/issues/27) for some discussion of the comparison.

