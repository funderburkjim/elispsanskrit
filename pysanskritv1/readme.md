
# elispsanskrit/pysanskrit

This directory contains a conversion of the elispsanskrit/grammar/lisp
system to Python programs.  The conversion is
* as literal as possible.  Python function names are close to Elisp
  function names.  
* geared toward comparison with Elisp.  The aim is to mimic the Elisp
  results.  Code is thus geared toward generating output files that are
  identical (using the 'diff' file comparison utility) to those
  generated by the Elisp programs described in the readme-batch.org .
* The internals of the Python functions are also as close as possible to
  the internals of the comparable Elisp functions.  Aside from the general
  language differences between Elisp and Python,  the other differences
  relate mainly to the representation of Sanskrit.  Most of the Elisp
  code was written on the assumption that Sanskrit was represented in
  the ITRANS transliteration. At some later point in the history of the
  Elisp code, a thin veneer of SLP1 transliteration was prepared; this
  veneer allowed inputs and outputs to be represented in SLP1, but
  immediately translated to the ITRANS conventions of the primary computations;
  then the results of the ITRANS computations were translated back to SLP1.
  By contrast, the Python conversion uses SLP1 throughout.

## Python modules of pysanskrit.

There are 26 Elisp files in the grammar/lisp/ code; and 9 Python modules
in the pysanskrit code of this directory.

A few of the Elisp code modules are are unrepresented in the pysanskrit,
namely those not involved with the construction of declensions and 
conjugations.

The Python modules are organized into two main groups, aimed at testing
equal functionality with Elisp.

* test_s_file_init_alt1.py  Declension tester, comparable to Elisp 
    s-file-init-alt1
  * test1.py  most declension functions. Also has functionality of
    testing from an input file such as test_declension.txt
  * declension_general_1cons.py  the rest of the declension functions
  * sandhi.py  most sandhi routines used in both declensions and conjugations
  * init.py (see below)
* test_v_file_init_alt1.py  Conjugation tester, comparable to Elisp
    v-file-init-alt1-pre
  * test2.py  most declension functions. Also has functionality of
    testing from an input file such as test_conjugation.txt
  * causal.py
  * sandhi.py
  * util.py  Some utility functions.
  * init.py (see below)

## init.py

The approach to generating declensions and conjugations is based on my
interpretation of Sanskrit grammar as represented primarily by the texts
of Antoine and Kale, and also occasionally by other texts such as that by
Deshpande.  I think of this as a *model-based* approach. In this approach,
the task of inflecting a base form generally is a three step process:
* identify the model that is applicable. For nouns, the model generally
  derives from the spelling of the stem form of the noun.  For verbs, the
  model generally comes from the class and pada (voice), and from the
  particular tense or verb form (present tense, perfect tense, etc.).
* Each model has associated with it a set of endings (a table with 24
  elements for 8 cases and 3 numbers for nouns, a table with 9 elements for
  3 persons and 3 numbers).  
* joining the base form and the endings to form a declension table or 
  conjugation table; this step invariably also involves application of 
  rules of sandhi.

The ending tables and the sandhi rules are constants.  There are in addition
some other constants that are required at various points, such as the
set of semivowels.   These various constants are organized in pysanskrit
in the init.py module.  At some point of further documentation, the details
of the structure of these constants needs to be further described.

## prodchk/outputs

Numerous bash scripts have been written to compare various subsets of 
declension and conjugations.  The general structure of these scripts is:
* use either  test_s_file_init_alt1.py or test_v_file_init_alt1.py  to
* read inputs from a particular file in elispsanskrit/grammar/prod/inputs
  directory (such as MW-adj.txt)
  * limit the inputs processed to a certain range of lines of the input file.
  * write the constructed declensions or conjugations to a particular
    file in prodchk/outputs/ directory
* Compare the resulting output file to the identically named file in
  elispsanskrit/grammar/prod/outputs.  The comparison is done with the
  'diff' file comparision utility, and the differences (if any) are written
  to a temporary file, and the number of differences is noted in stdout.

In this repository, the files in prodchk/outputs are omitted (see .gitignore),
since they are identical to those of elispsanskrit/grammar/prod/outputs.
Similarly, the outputs of the diff file comparisons are omitted, since they
are empty files.

## redo_all_v.sh

This script redoes all the conjugation tests, resulting in 8 files in
prodchk/outputs.  All the files have names of the form MW-verb-X.txt, for some X.
* *MW-verb-pre.txt*  present tense conjugations (present, imperfect,
  optative, imperative)
* *MW-verb-passive.txt* passive voice for all present tense conjugations (present, imperfect, optative, and imperative)
* *MW-verb-fut.txt* simple future, periphrastic future, conditional, and benedictive 
* *MW-verb-prf.txt* perfect tense
* *MW-verb-ppfactn.txt* periphrastic perfect action noun (used in constructing
  periphrastic perfect conjugations with the perfect of roots kf, as)
* *MW-verb-aorvar.txt* a list of the aorist varieties for different roots.
* *MW-verb-aor.txt* the aorist conjugations for each root, using only the
   aorist varieties listed above.
* *MW-verb-inf.txt*  The infinitives of roots.

## redo_all_v_participles.sh

This script computes various participle forms and declensions for roots.

* *MW-verb-prap.txt* present active participle, citation form
* *MW-verb-prap-decl.txt* present active participle, declension tables
* *MW-verb-prmp.txt* present middle participle, citation form
* *MW-verb-prmp-decl.txt* present middle participle, declension tables
* *MW-verb-prpp.txt* present passive participle, citation form
* *MW-verb-prpp-decl.txt* present passive participle, declension tables

* *MW-verb-fap.txt* future active participle, citation form
* *MW-verb-fap-decl.txt* future active participle, declension tables
* *MW-verb-fmp.txt* future middle participle, citation form
* *MW-verb-fmp-decl.txt* future middle participle, declension tables
* *MW-verb-fpp.txt* future middle participle, citation form
* *MW-verb-ipp.txt* indeclineable passive pariticiple (-tvA)
* *MW-verb-ippa.txt* indeclineable passive pariticiple (-ya, used with prefixed verbs)
* *MW-verb-ppp.txt* perfect passive participle, citation form
* *MW-verb-pap.txt* perfect active participle, citation form
* *MW-verb-potp.txt* potential passive participles (gerundives)
* *MW-verb-rpp.txt* reduplicated passive participles

# sh redo_all_s.sh

This computes declension tables.  
It takes several minutes of computation time, due to the large number of nouns and adjectives.

The outputs are typically in batches of 10,000 inputs are are named as 
follows:

* *MW-noun-01.txt* - *MW-noun-11.txt*  nouns, based on input MW-noun.txt 
   from elispsanskrit/grammar/inputs directory
* *MW-adj-01.txt* - *MW-adj-05.txt*  adjectives, based on input MW-adj.txt 
   from elispsanskrit/grammar/inputs directory
* *MW-pco-01.txt* pronouns, cardinal numbers, and ordinal numbers.
