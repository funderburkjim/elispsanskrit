"""verbs-tp.py
   08-14-2016, ejf
   Read huet's SL_roots.xml, and generate 
   (a) a list of stems and
   (b) a list of conjugation tables
   The second input is a Huet tense abbreviation.
   Reference SL_morph.dtd
   Handle only the 'tp' forms with the given tense.
   08-15-2016. adapted to take into account the aorist 'kind', which
   appears as the value of the 'gn' attribute of the <aor> element.
"""
import sys,re,codecs
from lxml import etree
#import collections

Huet_numbers = {'sg':'s', 'du':'d', 'pl':'p'}
Huet_persons = {'fst':1,'snd':2,'trd':3}
Huet_numbers_idx = {'sg':0, 'du':1, 'pl':2}
Huet_persons_idx = {'fst':2,'snd':1,'trd':0}
Huet_padas_prt = {'atma':'A','para':'P','pass':'Q'}  # note odd 'Q' for passive
Huet_tenses = ["pr","ip","op","im"]
from sansort import slp_cmp

def Huet_person_number_index(person,number):
 # put in order 3s 3d 3p 2s 2d 2p 1s 1d 1p, consistent with Elispsanskrit order
 # for conjugation tables
  iperson = Huet_persons_idx[person]
  inumber = Huet_numbers_idx[number]
  idx = (iperson*3) + inumber
  return idx

class Huet_verb_prim_tp(object):
 def __init__(self,stem):
  self.stem=stem
  self.kps = [] # 
  self.tables={}  # key is a kp pair (aorist-kind (1-7) and pada (AP))
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def update_form(self,form,kind,pada,person,number):
  kp = kind+pada
  if kp not in self.kps:
   self.kps.append(kp)
   self.tables[kp] = [[] for i in xrange(0,9)]
  t = self.tables[kp]
  idx = Huet_person_number_index(person,number)
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


def main(filein,htense,fileout,fileout1):
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
 #print "Huet_genders=",Huet_genders
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
  # The xml '<s>' element is one of the children. "lexicon stem or root generating the form"
  # We assume '<s>' is the 'last' child of '<f>'
  s = children[-1]
  # The actual stem is the value of the "stem" attribute of <s>.
  # This stem may have a 'homonym' number, represented as '#1','#2' suffixing the stem value.
  stem= s.get("stem")
  assert ((form != None) and (stem != None))
  # Remove homonym marker from stem, if present
  stem = re.sub(r'#.*$','',stem)
  """
  if stem not in stemdict:
   stemdict[stem]=Huet_verb_prim_prs(stem)
   stemlist.append(stem)
  rec = stemdict[stem]
  """
  # the rest of the children describe details regarding how the inflected form arises from the stem.
  inflected_forms=children[0:-1]
  tags = [inflected_form.tag for inflected_form in inflected_forms]
  assert (set(tags) == set(['v'])),"Unexpected tag for form %s, tags=%s\n" %(form,tags)
  for  inflected_form in inflected_forms:
   (cj,sys,np) = list(inflected_form)
   [cjelt]=list(cj)
   if cjelt.tag != 'prim':
    continue
   [syselt] = list(sys)
   if syselt.tag != 'tp':
    continue
   [tenseelt,padaelt]=list(syselt)
   tense = tenseelt.tag
   if tense != htense:
    continue
   pada = padaelt.tag
   assert (pada in ["para","atma","pass"]),"Unexpected pada tag=%s\n" % pada
   pada = Huet_padas_prt[pada]
   kind = tenseelt.get("gn") # aorist kind 
   assert (kind in ["1","2","3","4","5","6","7"]),"Unexpected aorist kind=%s for stem=%s\n" % (kind,stem)
   [numberelt,personelt]=list(np)
   number = numberelt.tag
   person = personelt.tag
   assert (number in ["sg","du","pl"]),"Unexpected number=%s\n" % number
   assert (person in ["fst","snd","trd"]),"Unexpected person=%s\n" % person
   if stem not in stemdict:
    stemdict[stem]=Huet_verb_prim_tp(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   rec.update_form(form,kind,pada,person,number)
  element.clear() # for efficiency, free memory of this element
 print len(stemlist),"stems found in",filein
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 for stem in stemlist:
  rec = stemdict[stem]
  kps = rec.kps
  kps.sort()
  kpstr = ','.join(kps)
  out = "%s:%s" % (stem,kpstr)
  fout.write(out + "\n")
 fout.close()
 # generate conjugation tables
 # make the format like that of the elispsanskrit  MW-verb-pre.txt output file
 fout = codecs.open(fileout1,"w","utf-8") # utf-8 not required
 # translation of Huet tense names to slp tense names
 #slp_tenses = {'pr':'pre','ip':'ipv','op':'pop','im':'ipf'}
 # translation of padas P,A to slp voices
 #slp_voices = {'P':'a','A':'m'}
 for stem in stemlist:
  rec = stemdict[stem]
  tabs = rec.tables
  tabkeys = rec.tables.keys() 
  tabkeys.sort()
  for kp in tabkeys:
   t = tabs[kp]
   tstr = prettify(t)
   out = "%s %s %s:%s" %(stem,htense,kp,tstr)
   fout.write(out + "\n")
 fout.close()

if __name__ == "__main__":
 filein = sys.argv[1]
 htense = sys.argv[2];
 assert (htense in ["fut","prf","aor","inj","cnd","ben"]),"Unexpected tense abbreviation=%s\n" % htense
 fileout = "huet_stems_%s.txt" % htense
 fileout1 = "huet_conj_tables_%s.txt" % htense
 main(filein,htense,fileout,fileout1)
