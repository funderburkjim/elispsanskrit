source temp.txt
number=1
for VALUE1 in "${LIST1[@]}"
do
	echo "$number - processing verb number $VALUE1 "
        ((number++))
done
