python test_v_file_init_alt1.py dcpforms-MW-verb.txt ../../elispsanskrit/grammar/prod/inputs MW-verb-aorvar.txt prodchk/outputs 1 3000 aorvar NONE

 diff -w prodchk/outputs/MW-verb-aorvar.txt ../../elispsanskrit/grammar/prod/outputs/MW-verb-aorvar.txt > temp-aorvar.txt

 wc -l temp-aorvar.txt
