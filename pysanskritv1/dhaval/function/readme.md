
Examine https://github.com/drdhaval2785/SanskritVerb/blob/master/scripts/function.php.

## get new copy of function.php
```
cp ~/Documents/GitHub/SanskritVerb/scripts/function.php .
```
## extract
A php function prints contents of various arrays of function.php to text files,
one line per array element. For example,
```
php extract.php verbdata
```
prints the 2213 array elements of `$verbdata` to 2213 lines of the file
verbdata.txt.

first line is:
```
aMsa:samAGAte:aMsa:10:0460:u:sew:अं॑स॑:1420::1475:aMsa1_aMsa_curAxiH+samAGAwe:
```

function.php describes the format as 13 ':' separated fields:

```
verbwithanubandha
meaning
verbwithoutanubandha
gana
number
pada
iDAgama
pureverb
mAdhavIyadhAtuvRtti
kzIrataraGgiNI
dhAtupradIpa
uohydlink
jnulink
```

## verbdata.py
This Python module provides other programs easy access to
verbdata.txt.   Typical usage:
```
import sys
# set relative path to  directory containing verbdata.txt
path='<PATH>/function'
sys.path.append(path) 
import verbdata
recs = verbdata.init_Dhaval_verbdata('%s/verbdata.txt'%path)
# Now recs is a list of Dhaval_verbdata objects.
# A typical element 'rec' of this list has
# attributes rec.verbwithanubandha, etc.  
```


# AkusmIya and AgarvIya

These files are from Dhaval.  Each is a list of roots (spelled
*with* anubandha).  The significance is that these are class 10 roots
which take *only atmanepada* endings  (in contrast to the general case
where class 10 roots are ubhayapada).

We want to check whether verbdata.txt is consistent with these, in the
sense that the records of verbdata for these roots have 'A' in the pada
field.
```
python check_class10_pada.py verbdata.txt AkusmIya.txt AgarvIya.txt > check_class10_pada_log.txt
```
