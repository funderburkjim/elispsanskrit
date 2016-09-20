echo "comparing present tense conjugations"
echo "map sanverb present tense"
cd sanverb
sh redo_pre.sh
cd ../
echo "generating conj_pre_map1"
python pysan_pre.py sanverb/conj_pre_map1.txt data/pysan_conj_pre.txt
echo "comparing present tense conjugations"
python compare_conj_tables.py sanverb/conj_pre_map1.txt data/pysan_conj_pre.txt data/compare_conj_pre.txt
