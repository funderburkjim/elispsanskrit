python test_v_file_init_alt1.py dcpforms-MW-verb.txt ../../elispsanskrit/grammar/prod/inputs MW-verb-prf.txt prodchk/outputs 1 3000 prf NONE

 diff -w prodchk/outputs/MW-verb-prf.txt ../../elispsanskrit/grammar/prod/outputs/MW-verb-prf.txt > temp-prf.txt

 wc -l temp-prf.txt
