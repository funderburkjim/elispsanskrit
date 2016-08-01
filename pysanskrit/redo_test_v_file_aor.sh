python test_v_file_init_alt1.py dcpforms-MW-verb.txt ../../elispsanskrit/grammar/prod/inputs MW-verb-aor.txt prodchk/outputs 1 3000 aor NONE

 diff -w prodchk/outputs/MW-verb-aor.txt ../../elispsanskrit/grammar/prod/outputs/MW-verb-aor.txt > temp-aor.txt

 wc -l temp-aor.txt
