echo "copying generated forms from SanskritVerb"
cp ../../../../SanskritVerb/generatedforms.xml ../generatedforms/
echo "extracting present tense conjugations from SanskritVerb generatedforms"
python extract.py ../generatedforms/generatedforms.xml pre > extract_pre_log.txt
echo "examine conjugation duplicates"
python conjdup.py pre > conjdup_log.txt
