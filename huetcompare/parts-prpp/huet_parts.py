"""huet_parts.py
   08-16-2016, ejf
   Read huet's SL_parts.xml, and generate 
   (a) a list of stems with kridanta information
   (b) a list of declension tables
   The second input is a Huet participle abbreviation.
   Reference SL_morph.dtd
   08-18-2016
   This program is specialized for present passive participles,
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

class Huet_part(object):
 def __init__(self,stem):
  self.stem=stem
  self.cpms = [] # class-pada-gender
  self.tables={}  # key is a class-pada
  # each table is a list of 24 values (1s...8p), and each value is a list of 
  # forms

 def update_form(self,form,theclass,pada,gender,case,number):
  cpm = theclass+pada+gender
  if cpm not in self.cpms:
   self.cpms.append(cpm)
   self.tables[cpm] = [[] for i in xrange(0,24)]
  t = self.tables[cpm]
  idx = Huet_case_number_index(case,number)
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


def main(filein,hpartcode,hpada,fileout,fileout1):
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
  assert (set(tags) == set(['pa'])),"Unexpected tag for form %s, tags=%s\n" %(form,tags)
  for  inflected_form in inflected_forms:
   (naelt,krelt) = list(inflected_form) # nominal, kridanta
   # get gender,case,number from naelt
   (caseelt,numberelt,genderelt) = list(naelt)
   gender = Huet_genders[genderelt.tag]
   case = Huet_cases[caseelt.tag]
   number = Huet_numbers_idx[numberelt.tag]
   [cjelt,noelt]=list(krelt)
   [primelt]=list(cjelt)
   if primelt.tag != 'prim':
    continue
   [partelt]=list(noelt)
   partcode = partelt.tag
   if partcode != hpartcode:
    continue
   """
   [padaelt]=list(partelt)
   pada = padaelt.tag
   if pada != hpada:
    continue
   theclass = partelt.get("gn") # class of underlying root.
   # Note: the root in question is NOT SPECIFIED. UGH!
   assert (pada in ["para","atma"]),"Unexpected pada tag=%s\n" % pada
   pada = Huet_padas_prt[pada]
   """
   # set pada and theclass to be empty strings for present passive participle
   theclass=''
   pada = ''
   if stem not in stemdict:
    stemdict[stem]=Huet_part(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   rec.update_form(form,theclass,pada,gender,case,number)
  element.clear() # for efficiency, free memory of this element
 print len(stemlist),"stems found in",filein
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 for stem in stemlist:
  rec = stemdict[stem]
  cpms = rec.cpms
  # for the stem display, remove the gender (last character of a cpm
  cps = [cpm[0:-1] for cpm in cpms]
  # remove duplicates
  cps = list(set(cps))
  cps.sort()
  cpstr = ','.join(cps)
  out = "%s:%s" % (stem,cpstr)
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
  for cpm in tabkeys:
   t = tabs[cpm]
   tstr = prettify(t) 
   out = "%s %s %s:%s" %(stem,hpartcode,cpm,tstr)
   fout.write(out + "\n")
 fout.close()

if __name__ == "__main__":
 filein = sys.argv[1]
 hpartcode = sys.argv[2]
 hpada = None
 assert (hpartcode in ["ppp","ppa","ppr","pprp","ppft","pfut","pfutp"]),"Unexpected participle abbreviation=%s\n" % hpartcode
 if [hpartcode,hpada] == ['ppr','para']:
  filesfx = 'prap'
 elif [hpartcode,hpada] == ['ppr','atma']:
  filesfx = 'prmp'
 elif [hpartcode,hpada] == ['pprp',None]:
  filesfx = 'prpp'
 else:
  print "Problem with codes:",hpartcode,hpada
  exit(1)
 fileout = "huet_stems_%s.txt" % filesfx
 fileout1 = "huet_decl_tables_%s.txt" % filesfx
 main(filein,hpartcode,hpada,fileout,fileout1)

