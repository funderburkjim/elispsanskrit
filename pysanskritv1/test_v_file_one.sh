CASE=$1
BEG=$2
END=$3
python test_s_file_init_alt1.py MW-noun.txt ../../elispsanskrit/grammar/prod/inputs MW-noun-$CASE.txt prodchk/outputs $BEG $END > prodchk/outputs/MW-noun-$CASE-log.txt

diff -w prodchk/outputs/MW-noun-$CASE.txt ../../elispsanskrit/grammar/prod/outputs/MW-noun-$CASE.txt > temp-$CASE.txt
 wc -l temp-$CASE.txt




