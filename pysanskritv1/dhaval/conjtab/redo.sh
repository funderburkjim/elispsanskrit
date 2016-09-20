echo "extracting present tense conjugations from SanskritVerb generatedforms"
python extract.py ../generatedforms/generatedforms.xml pre 
echo "examine conjugation duplicates"
python conjdup.py pre > conjdup_log.txt
