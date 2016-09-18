

## SanskritVerb conjugations, after mapping

```
python conjmap.py ../../conjtab/conj_pre.txt ../../pysanroots/verbdata_map1.txt conj_pre_map1.txt > conj_pre_map1_log.txt
```

The output conj_pre_map1.txt  differs from the input conj_pre.txt only in the
header of each conjugation table. The two lists have exactly the same 
number of lines, and in the same order.

As example, consider two examples, lines 1 and 4.
```
conj_pre:
aMsa pre 10.0460A:[aMsayate aMsayete aMsayante aMsayase aMsayeTe aMsayaDve aMsaye aMsayAvahe aMsayAmahe]
aki! pre 01.0092A:[aNkate aNkete aNkante aNkase aNkeTe aNkaDve aNke aNkAvahe aNkAmahe]

conj_pre_map1:
aMs pre 10A:[aMsayate aMsayete aMsayante aMsayase aMsayeTe aMsayaDve aMsaye aMsayAvahe aMsayAmahe]
aNk pre 1A:[aNkate aNkete aNkante aNkase aNkeTe aNkaDve aNke aNkAvahe aNkAmahe]

```

The mapping of 'root-with-anubandha' of conj_pre to 'root-without-anubandha' of conj_pre_map1' is done according to [verbdata_map1](https://github.com/funderburkjim/elispsanskrit/blob/master/pysanskritv1/dhaval/roots/verbdata_map1.txt).
The two relevant lines for our examples are:
```
aMsa:10:0460:aMs
aki!:01:0092:aNk
```

