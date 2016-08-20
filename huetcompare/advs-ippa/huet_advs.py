"""huet_advs.py
   08-19-2016, ejf
   Read huet's SL_adverbs.xml,
   Reference SL_morph.dtd
   This program is indeclineable  past participle (absolutive in 'ya)
"""
import sys,re,codecs
from lxml import etree
#import collections

Huet_numbers = {'sg':'s', 'du':'d', 'pl':'p'}
Huet_genders = {'fem':'f','mas':'m','neu':'n'} # ,'dei':'d'}
Huet_cases = {'nom':1, 'acc':2, 'ins':3, 'dat':4, 'abl':5, 'gen':6, 'loc':7, 'voc':8}
Huet_numbers_idx = {'sg':0, 'du':1, 'pl':2}
numbers_idx = {'s':0, 'd':1, 'p':2}
Huet_persons_idx = {'fst':2,'snd':1,'trd':0}
Huet_padas_prt = {'atma':'A','para':'P'}  
Huet_tenses = ["pr","ip","op","im"]
from sansort import slp_cmp

def Huet_case_number_index(case,number):
  inumber = number #Huet_numbers_idx[number]
  icase = case - 1
  idx = (icase*3) + inumber
  return idx

class Huet_adverb(object):
 def __init__(self,stem):
  self.stem=stem  
  self.forms = [] # adverbial forms with given stem

 def update_form(self,form):
  if form not in self.forms:
   self.forms.append(form)

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


def main(filein,hadvcode,fileout):
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
 ncaus=0
 ndes=0
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
  #tags = [inflected_form.tag for inflected_form in inflected_forms]
  for  inflected_form in inflected_forms:
   tag = inflected_form.tag
   if tag != 'vu':  # indeclineable verbal form
    continue
   [cjelt,ivelt] = list(inflected_form) # nominal, kridanta
   assert (cjelt.tag == 'cj'),"Unexpected cj tag %s for stem=%s"%(cjelt.tag,stem)
   [primelt]=list(cjelt)
   # can be primary or causal conjugation time
   assert (primelt.tag in['prim','ca','des']),"Unexpected prim tag %s for stem=%s"%(primelt.tag,stem)
   if primelt.tag == 'ca':
    ncaus = ncaus + 1
    print "%s:%s:%s of %s"%(stem,form,hadvcode,primelt.tag)
    continue # don't handle these for now
   elif primelt.tag == 'des':
    ndes = ndes + 1
    print "%s:%s:%s of %s"%(stem,form,hadvcode,primelt.tag)
    continue
   [elt] = list(ivelt)
   if elt.tag != hadvcode:
    continue
   if stem not in stemdict: # stem is a root
    stemdict[stem]=Huet_adverb(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   rec.update_form(form,)
  element.clear() # for efficiency, free memory of this element
 print len(stemlist),"stems found in",filein
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 for stem in stemlist:
  rec = stemdict[stem]
  forms=rec.forms
  forms.sort(cmp=slp_cmp)
  formstr = ','.join(forms)
  out = "%s:%s" % (stem,formstr)
  fout.write(out + "\n")
 fout.close()
 print ncaus,"causal forms skipped"
 print ndes,"desiderative forms skipped"

if __name__ == "__main__":
 filein = sys.argv[1]
 hadvcode = sys.argv[2]
 assert (hadvcode in ["abs"]),"Unexpected adverb abbreviation=%s\n" % hadvcode
 hadv_to_pysan={"abs":"ippa"}
 filesfx = hadv_to_pysan[hadvcode]
 fileout = "huet_indecl_%s.txt" % filesfx
 main(filein,hadvcode,fileout)

