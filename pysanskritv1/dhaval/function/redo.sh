echo "copying function.php"
cp ~/Documents/GitHub/SanskritVerb/scripts/function.php .
echo "copying verbdata.php"
cp ~/Documents/GitHub/SanskritVerb/scripts/verbdata.php .
echo "extracting verbdata"
php extract.php verbdata verbdata.php
echo "extracting AkusmIya"
php extract.php AkusmIya function.php
echo "extracting AgarvIya"
php extract.php AgarvIya function.php
echo "checking AkusmIya, AgarvIya v. verbdata"
python check_class10_pada.py verbdata.txt AkusmIya.txt AgarvIya.txt > check_class10_pada_log.txt
echo "examine check_class10_pada_log.txt"
