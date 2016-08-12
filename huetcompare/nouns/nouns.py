"""pronouns.py
   07-30-2016, ejf
   Read huet's SL_pronouns.xml, and generate 
   (a) a list of stems and
   (b) a list of declension tables
   Reference SL_morph.dtd
   Handle only the 'f'-type records.  This excludes the SL_preverbs.txt file.

"""
import sys,re,codecs
from lxml import etree
#import collections

Huet_genders = {'fem':'f','mas':'m','neu':'n','dei':'d'}
Huet_numbers = {'sg':'s', 'du':'d', 'pl':'p'}
Huet_numbers_idx = {'s':0, 'd':1, 'p':2}
Huet_cases = {'nom':1, 'acc':2, 'ins':3, 'dat':4, 'abl':5, 'gen':6, 'loc':7, 'voc':8}

from sansort import slp_cmp

class Huet_pron(object):
 def __init__(self,stem):
  self.stem=stem
  self.genders = [] # m,f,n,d  (d = deictic, e.g. for asmad)
  self.tables={}  # key is one of genders
 #def update_gender(self,g):
 # if g not in self.genders:
 #  self.genders.append(g)
 def update_form(self,form,gender,case,number):
  if gender not in self.genders:
   self.genders.append(gender)
   # This gives the SAME empty list object in each array
   #self.tables[gender] = [[]]*24
   # this gives a DIFFERENT empty list object in each slot
   # Ref: http://stackoverflow.com/questions/13520876/how-can-i-make-multiple-empty-arrays-in-python
   self.tables[gender] = [[] for _ in xrange(0,24)]
  inumber = Huet_numbers_idx[number]
  icase = case - 1
  idx = (icase*3)+inumber
  t = self.tables[gender]
  t1 = t[idx]
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

def main(filein,fileout,fileout1):
 """ Use lxml """
 recs=[]
 #tree = etree.parse(filein) 
 # better to use iterparse
 # elements can be of two kinds, 'f' or 'pv'. We are interested only in 'f' elements
 # Note: the '<pv>' element appears only at the bottom of SL_final.xml.  It occurs 105
 # times, and corresponds to the 105 lines of SL_preverbs.txt.  It appears to be a list
 # of all single or multiple prefixes for verb forms. 'pv' element == 'preverb sequence'.
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
  if stem not in stemdict:
   stemdict[stem]=Huet_pron(stem)
   stemlist.append(stem)
  rec = stemdict[stem]
  # the rest of the children describe details regarding how the inflected form arises from the stem.
  inflected_forms=children[0:-1]
  tags = [inflected_form.tag for inflected_form in inflected_forms]
  assert (set(tags) == set(['na'])),"Unexpected tag for form %s, tags=%s\n" %(form,tags)
  for  inflected_form in inflected_forms:
   gender=None
   number=None
   case = None
   for elt in list(inflected_form):
    #if stem == 'aDara':
    # print stem,"elt.tag=",elt.tag
    if elt.tag in Huet_genders:
     gender = Huet_genders[elt.tag]
    elif elt.tag in Huet_cases:
     case = Huet_cases[elt.tag]
    elif elt.tag in Huet_numbers:
     number = Huet_numbers[elt.tag]
    else:
     raise NameError("Unexpected tag (%s) for form=%s" %(elt.tag,form))
   #
   assert (not (None in [gender,number,case])), ("form=%s has bad parameters" %(form,' , '.join([elt.tag for elt in list(inflected_form)])))
   #assert (not (None in [gender,number,case]))
   rec.update_form(form,gender,case,number)
  element.clear() # for efficiency, free memory of this element
 print len(stemlist),"stems found in",filein
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 # Adjusted to remove '#n' homonym notation, and
 # relatedly, to remove duplicate stems
 normstemsd = {}
 normstems=[]
 ndupcase=0
 for stem0 in stemlist:
  rec = stemdict[stem0]
  stem = re.sub(r'#.*$','',stem0)
  if stem in normstemsd:
   ndupcase=ndupcase+1
   genders = rec.genders
   genders.sort()
   gstr = ','.join(genders)
   print "NORMDUPLICATE %03d=%s:%s" %(ndupcase,stem0,gstr)
   for g in genders:
    t = rec.tables[g]
    tstr = prettify(t)
    out = "                 %s %s:%s" %(stem,g,tstr)
    print out
   # include extra genders 
   oldrec = normstemsd[stem]
   for g in genders:
    if g not in oldrec.genders:
     oldrec.genders.append(g)
     oldrec.tables[g] = rec.tables[g]
  else:
   normstems.append(stem)
   normstemsd[stem]=rec
 # generate first output file
 for stem in normstems:
  rec = normstemsd[stem]
  genders = rec.genders
  genders.sort()
  gstr = ','.join(genders)
  out = "%s:%s"%(stem,gstr)
  fout.write(out + "\n")
 fout.close()
 fout = codecs.open(fileout1,"w","utf-8") # utf-8 not required
 for stem in normstems:
  rec = normstemsd[stem]
  genders = rec.genders
  assert (len(genders)!=0),"NO INFLECTIONS FOR STEM %s"%stem
  for g in genders:
   t = rec.tables[g]
   tstr = prettify(t)
   out = "%s %s:%s" %(stem,g,tstr)
   fout.write(out + "\n")
 fout.close()

 exit()
 #fout.close()

if __name__ == "__main__":
 filein = sys.argv[1]
 fileout = sys.argv[2]
 fileout1 = sys.argv[3]
 main(filein,fileout,fileout1)
