
# extract conjugation tables from SanskritVerb generated forms

For the purpose of finding errors in the pysan conjugation algorithms,
this section prepares conjugation tables implied by the
generated forms in Dhaval Patel's [SanskritVerb](https://github.com/drdhaval2785/SanskritVerb) repository.

The extract.py program takes one of the tenses (based on a naming system in a system devised by Peter Scharf) and constructs all the conjugation tables
implied by Dhaval's generated forms.

```
python extract.py ../generatedforms/generatedforms.xml pre 
```
output in conj_pre.txt.

Samples:

```
aMsa pre 10.0460A:[aMsayate aMsayete aMsayante aMsayase aMsayeTe aMsayaDve aMsaye aMsayAvahe aMsayAmahe]
aka! pre 01.0901P:[akati akataH akanti akasi akaTaH akaTa akAmi akAvaH akAmaH]

```

## comments regarding duplicates in conj_pre
There are several case (180+) where two different present tense conjugation
tables are identical.

The conjdup program does some preliminary examination of this:
```
python conjdup.py pre > conjdup_log.txt
```

In the first part of the program, each of the conj_pre.txt lines is viewed
as having a 'header' and a 'table' part.  The program identifies those cases
where a given 'table' part occurs for more than one header, and prints
the headers thus appearing. For instance,
```
vawa pre 10.0395A :: vawa pre 10.0461A
kawI! pre 01.0330P :: kawe! pre 01.0359P

```
Thus the two forms of 'vawa' have the same conjugation table.

Also, the differently spelled dhatupatha roots `kawI!` and `kawe!` have
the same conjugation table.

There are 181 such duplicates.

The significance of these duplicates is not clear at this time.

In a second step, we examined  the 'sutra-pada' part of the
header (for instance `10.0395A` in the `vawa` example).  We wanted to know
if any two lines in the conj_tab listing had identical sutra-pada fields.
The result is that *there are no duplicate sutra-pada* fields; so this
field may be regarded as a 'key' to the listing of conjugation tables.


