python test_v_file_init_alt1.py dcpforms-MW-verb.txt ../../elispsanskrit/grammar/prod/inputs MW-verb-fut.txt prodchk/outputs 1 3000 fut NONE

 diff -w prodchk/outputs/MW-verb-fut.txt ../../elispsanskrit/grammar/prod/outputs/MW-verb-fut.txt > temp-fut.txt

 wc -l temp-fut.txt
