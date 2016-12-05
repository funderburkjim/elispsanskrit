echo "comparing present tense conjugations"
echo "map sanverb present tense"
cd sanverb
sh redo.sh
cd ../
echo "generating pysan_conj_pre"
python pysan_pre.py sanverb/conj_pre_map1.txt data/pysan_conj_pre.txt
echo "comparing present tense conjugations"
python compare_conj_tables.py sanverb/conj_pre_map1.txt data/pysan_conj_pre.txt data/known_diffs_pre.txt  data/compare_conj_pre.txt
