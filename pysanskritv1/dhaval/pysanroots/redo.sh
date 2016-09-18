echo "recreate sanverb_cp.txt and conjtab_cp.txt"
python sanverb_cp.py ../function ../conjtab/conj_pre.txt conjtab_cp.txt sanverb_cp.txt > sanverb_cp_log.txt
echo "compare sanverb_cp and conjtab_cp"
python compare_cp.py sanverb conjtab sanverb_conjtab
echo "sanverb_conjtab_cp.txt has comparisons"
echo "recreate mwvlex_cp.txt from MWvlex repository file"
python mwvlex_cp.py ../../../../MWvlex/step1/verb_cp.txt mwvlex_cp.txt
echo "compare root-class-pada from SanskritVerb and MWvlex"
python compare_cp.py sanverb mwvlex sanverb_mwvlex
echo "examine sanverb_mwvlex_cp.txt"
echo "roots ending in 'a' in sanverb"
python roots_a.py sanverb_cp.txt mwvlex_cp.txt roots_a.txt > roots_a_log.txt
echo "Additional pseudo-mw roots for sanverb roots ending in 'a'"
python roots_a1.py sanverb_cp.txt roots_a1_prep.txt roots_a1.txt > roots_a1_log.txt
echo "sanverb1_cp: adjust sanverb_cp for some roots ending in 'a'"
python sanverb1_cp.py sanverb_cp.txt roots_a.txt roots_a1.txt sanverb1_cp.txt
echo "mwvlex1 - adjust mwvlex class10 pada information using sanverb"
echo "   Also, adjust missing mwvlex info using sanverb"
python mwvlex1_cp.py mwvlex_cp.txt sanverb1_cp.txt mwvlex1_cp.txt > mwvlex1_cp_log.txt
echo " compare sanverb1 and mwvlex1 cp info. output = sanverb1_mwvlex1_cp.txt"
python compare_cp.py sanverb1 mwvlex1 sanverb1_mwvlex1
echo "make verbdata_map"
python verbdata_map.py verbdata_map.txt > verbdata_map_log.txt
echo "make verbdata_map1"
python verbdata_map1.py verbdata_map.txt roots_a.txt roots_a1.txt verbdata_map1.txt > verbdata_map1_log.txt
