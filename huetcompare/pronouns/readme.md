
The purpose is to compare Huet's inflected forms with those generated by
elispsanskrit/pysanskrit.

## step1: interpret Huet's pronoun declensions

```
python pronouns.py ../huet/SL_pronouns.xml huet_pron_stems_genders.txt huet_pron_tables.txt
```

From Huet's SL_pronouns.xml file, we derive the set of all stems for which
there are declined forms, and for each stem identify the genders for which
there are declined forms.  This summary of coverage is shown in the
 huet_pron_stems_genders.txt file.

We also gather the declension table for each stem-gender form, and print this
in a form similar to that used by the elispsanskrit batch declension 


## Gather pronominal declension from elispsanskrit.

```
python pysan_pronouns.py ../grammar/prod/outputs/MW-pco.txt pysan_pron_stems_genders.txt pysan_pron_tables.txt
```

The source file is (a copy of) [MW-pco.txt](https://github.com/funderburkjim/elispsanskrit/blob/master/grammar/prod/outputs/MW-pco.txt).  This file has declensions for pronouns, cardinal numbers, and ordinal numbers.

The program identifies which declension tables pertain to pronouns, and extracts information for the pronouns to files pysan_pron_stems_genders.txt and pysan_pron_tables.txt.  As the filenames suggest, the formats of these files are comparable to the correspondindly named huet files mentioned above.  

The prefix `pysan` is used because I had just finished a python conversion of 
the elispsanskrit system (present in the [pysanskrit](https://github.com/funderburkjim/elispsanskrit/tree/master/pysanskrit) directory), and so the Python generated versions of the elisp output files was on my mind.


## compare each of the files 
```
python compare_pron_stems.py huet pysan compare
```

The purpose of the comparison is to present points of similarity and difference,
in such a way as to facilitate further investigation.  

The programmatic comparisons are in files
* [compare_pron_stems_genders.txt](https://github.com/funderburkjim/elispsanskrit/blob/master/huetcompare/pronouns/compare_pron_stems_genders.txt)
* [compare_pron_tables.txt](https://github.com/funderburkjim/elispsanskrit/blob/master/huetcompare/pronouns/compare_pron_tables.txt)

See [issue#5](https://github.com/funderburkjim/elispsanskrit/issues/5) for some discussion of the comparison.

* THE END