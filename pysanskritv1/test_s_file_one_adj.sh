CASE=$1
BEG=$2
END=$3
TYPE="adj"
python test_s_file_init_alt1.py MW-$TYPE.txt ../../elispsanskrit/grammar/prod/inputs MW-$TYPE-$CASE.txt prodchk/outputs $BEG $END > prodchk/outputs/MW-$TYPE-$CASE-log.txt

diff -w prodchk/outputs/MW-$TYPE-$CASE.txt ../../elispsanskrit/grammar/prod/outputs/MW-$TYPE-$CASE.txt > temp-$TYPE-$CASE.txt
 wc -l temp-$TYPE-$CASE.txt




