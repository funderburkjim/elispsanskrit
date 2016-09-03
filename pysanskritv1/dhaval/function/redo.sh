echo "copying function.php"
cp ~/Documents/GitHub/SanskritVerb/scripts/function.php .
echo "extracting verbdata"
php extract.php verbdata
echo "checking AkusmIya, AgarvIya v. verbdata"
python check_class10_pada.py verbdata.txt AkusmIya.txt AgarvIya.txt > check_class10_pada_log.txt
echo "examine check_class10_pada_log.txt"
