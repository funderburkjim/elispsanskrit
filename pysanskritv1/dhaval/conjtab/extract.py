"""extract.py
   08-26-2016, ejf
   Read Dhaval Patel's generatedforms20062016.xml and generate
   a list of conjugation tables
   for a particular tense.
"""
import sys,re,codecs
from lxml import etree
#import collections

conj_numbers_idx = {'s':0, 'd':1, 'p':2}
conj_persons_idx = {1:2,2:1,3:0}
Huet_padas_prt = {'atma':'A','para':'P'}  
Huet_tenses = ["pr","ip","op","im"]
Dhaval_slp_tense={
  "law":"pre", # present
  "liw":"prf", # reduplicative perfect
  "luw":"pft", # 1st Future (periphrastic future)
  "lfw":"fut", # 2nd Future
  "low":"ipv", # imperative
  "laN":"ipf", # imperfect
  "ASIrliN":"ben", # benedictive
  "viDiliN":"opt", # potential (optative)
  "lfN":"con", # conditional
  "luN":"aor", # aorist
}
Dhaval_sup_pnp = {  # map from symbol to (person,number,pada)
 'tip':(3,'s','P'),
 'tas':(3,'d','P'),
 'Ji':(3,'p','P'),
 'sip':(2,'s','P'),
 'Tas':(2,'d','P'),
 'Ta':(2,'p','P'),
 'mip':(1,'s','P'),
 'vas':(1,'d','P'),
 'mas':(1,'p','P'),

 'ta':(3,'s','A'),
 'AtAm':(3,'d','A'),
 'Ja':(3,'p','A'),
 'TAs':(2,'s','A'),
 'ATAm':(2,'d','A'),
 'Dvam':(2,'p','A'),
 'iw':(1,'s','A'),
 'vahi':(1,'d','A'),
 'mahiN':(1,'p','A'),

}
from sansort import slp_cmp

def person_number_index(person,number):
 # put in order 3s 3d 3p 2s 2d 2p 1s 1d 1p, consistent with Elispsanskrit order
 # for conjugation tables
  iperson = conj_persons_idx[person]
  inumber = conj_numbers_idx[number]
  idx = (iperson*3) + inumber
  return idx

class Dhaval_verb(object):
 def __init__(self,stem):
  self.stem=stem
  # dpid-pada (tuple of 2 strings: dp number (eq 10.0257) and 
  # A (Atmanepada) or P (parasmaipada)
  self.cps = [] 
  self.tenses = {} # key is one of class-padas, value is a list of tense-symbols
  self.tables={}  # key is a triple (class,pada,tense) . 
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def update_form(self,form,theclass,pada,tense,person,number):
  cp = (theclass,pada)
  if cp not in self.cps:
   self.cps.append(cp)
   self.tenses[cp]=[]
  if tense not in self.tenses[cp]:
   self.tenses[cp].append(tense)
   self.tables[(theclass,pada,tense)] = [[] for i in xrange(0,9)]
  t = self.tables[(theclass,pada,tense)]
  idx = person_number_index(person,number)
  t1 = t[idx]
  if form not in t1:
   t1.append(form)

def ending_s_H(x):
 if x.endswith('s'):
  return x[0:-1]+'H'
 else:
  return x
def prettify_one(y):
 """ y is either a list of strings, or the empty list
 """
 if len(y) == 0:
  return 'nil'
 # Where Huet systematically ends inflected forms in 's',
 # pysanskrit uses visarga ('H')
 y = map(ending_s_H,y)
 if len(y) == 1:
  return y[0]
 # list of two or more strings
 z = ' '.join(y)
 return '(' + z + ')'

def prettify(x):
 """ x is a declension table, in list form, as derived from Huet's data 
   We want to generate a string representation,
   following the form used in the pysanskrit declension listings.
   Each of the (24) elements of x is itself either
   (a) a list of strings, OR
   (b) the empty list
 """
 y = map(prettify_one,x)
 z = ' '.join(y)
 w = '[' + z + ']'
 return w

def construct_class_pada(fileout,stemlist,stemdict):
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  cps = rec.cps
  cps.sort()
  cps_str = map(lambda x: x[0]+x[1],cps)
  cpstr = ','.join(cps_str)
  out = "%s:%s"%(stem,cpstr)
  fout.write(out + "\n")
  # check if each tense is represented for a given cp
  # Empirically, this logic generates no output, so we
  # know that each 'class-pada' has all 4 tenses.
  tabkeys = rec.tables.keys() 
  knowntenses = Huet_tenses.sort()
  for (c,p) in cps:
   tenses=[]
   for (c1,p1,tense) in tabkeys:
    if (c1==c) and (p1==p):
     tenses.append(tense)
   if tenses.sort() != knowntenses:
    print "%s:%s: Problem with tenses:%s" %(stem,c+p,tenses)
 fout.close()

def construct_conjtab(fileout1,stemlist,stemdict):
 # make the format like that of the elispsanskrit  MW-verb-pre.txt output file
 fout = codecs.open(fileout1,"w","utf-8") # utf-8 not required
 # translation of Huet tense names to slp tense names
 slp_tenses = {'pr':'pre','ip':'ipv','op':'pop','im':'ipf'}
 # translation of padas P,A to slp voices
 slp_voices = {'P':'a','A':'m'}
 for stem in stemlist:
  rec = stemdict[stem]
  tabs = rec.tables
  tabkeys = rec.tables.keys() 
  tabkeys.sort()
  for (c,p,tense) in tabkeys:
   t = tabs[(c,p,tense)]
   cp = c+p
   tstr = prettify(t)
   out = "%s %s %s:%s" %(stem,tense,cp,tstr)
   fout.write(out + "\n")
 fout.close()

def main(filein,fileout,fileout1,slptensein):
 """ Use lxml """
 recs=[]
 #tree = etree.parse(filein) 
 # better to use iterparse
 # elements can be of two kinds, 'f' or 'pv'.
 # We are interested only in 'f' elements
 # An '<f>' element is an 'inflected form entry'.
 # iterate through all the <f> elements.
 stemdict={}
 stemlist=[]
 n=0
 for _, element in etree.iterparse(filein, tag='f'):
  n = n + 1
  form = element.get("form")
  # "form" is the sole required attribute of an <f> element.  Its value is the inflected form.
  # most of the time, the element has exactly 2 children. However, sometimes there
  # are multiple children. The first instance in file SL_roots.xml is
  # <f form="akaRqayata">
  #  <v><cj><prim/></cj><sys><prs gn="10"><md><im/></md><atma/></prs></sys><np><sg/><trd/></np></v>
  #  <v><cj><prim/></cj><sys><prs gn="10"><md><im/></md><para/></prs></sys><np><pl/><snd/></np></v>
  # <s stem="kaRq"/></f>

  # The rest of the <f> element (its xml 'children') is used to describe the form.
  children = list(element)
  nchildren=len(children)
  assert (nchildren == 3),"Unexpected children @ form=%s"%form
  # The xml '<s>' element is one of the children. "lexicon stem or root generating the form"
  # We assume '<s>' is the 'last' child of '<f>'
  (rootelt,tenseelt,supelt)=children
  # The actual stem is the value of the "stem" attribute of <s>.
  # This stem may have a 'homonym' number, represented as '#1','#2' suffixing the stem value.
  stem= rootelt.get("name")
  assert ((form != None) and (stem != None))
  dpsutra=rootelt.get("num") # 10.0460
  (theclass,sequence) = dpsutra.split('.')
  tensename=tenseelt.tag
  pnpname=supelt.tag
  tense=Dhaval_slp_tense[tensename] # slp name of tense
  (person,number,pada) = Dhaval_sup_pnp[pnpname]
  if tense != slptensein:
   continue
  if stem not in stemdict:
   stemdict[stem]=Dhaval_verb(stem)
   stemlist.append(stem)
  rec = stemdict[stem]
  theclass = dpsutra
  rec.update_form(form,theclass,pada,tense,person,number)
  
 print len(stemlist),"stems found in",filein
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 # skip class_pada file for now (Aug 27, 2016)
 #construct_class_pada(fileout,stemlist,stemdict)
 # generate conjugation tables
 construct_conjtab(fileout1,stemlist,stemdict)

if __name__ == "__main__":
 filein = sys.argv[1]
 slptense=sys.argv[2]
 fileout = "cp_%s.txt" % slptense
 fileout1 = "conj_%s.txt" % slptense
 main(filein,fileout,fileout1,slptense)
