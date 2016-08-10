"""pysan_pronouns.py
"""

import sys,re,codecs
from sansort import slp_cmp
from pronouns import prettify

def declstr_listify(tabstr):
 """ convert the string representation of a declension to 
     a list form.
 """
 tabstr = tabstr[1:-1] # remove beginning and ending square bracket
 # change (x y) to x,y
 def g(m):
  x = m.group(1)
  return ','.join(x.split(' '))
 tabstr = re.sub(r'\((.*?)\)', g,tabstr)
 tab = tabstr.split(' ')
 # replace x,y by a list [x,y]
 # this also r
 def f(x):
  if ',' in x:
   y = x.split(',')
   y = list(y)
  elif x == 'nil':
   y = []
  else:
   y = [x]
  return y
 tab = map(f,tab)
 return tab

class Pysan_rec(object):
 # parse of MW-pco-01.txt
 def __init__(self,line):
  line = line.rstrip('\r\n')
  (dummy,head,self.key1,self.key2,self.tabstr) = line.split(':')
  (self.code1,self.code2,self.gender) = head.split(' ')
  # parse tabstr into a list
  # tabstr is a peculiar string representation of a list.
  # it has form [a b ... d] (An Elisp array format), with square brackets
  # The hardest part in unpacking this is that some list items look like
  # (x y ...z).
  self.tab = declstr_listify(self.tabstr)

class Pysan_pron(object):
 def __init__(self,stem):
  self.stem=stem
  self.genders = [] # m,f,n,d  (d = deictic, e.g. for asmad)
  self.tables={}  # key is one of genders
 #def update_gender(self,g):
 # if g not in self.genders:
 #  self.genders.append(g)
 def update(self,pyrec):
  gender = pyrec.gender
  if gender not in self.genders:
   self.genders.append(gender)
   # This gives the SAME empty list object in each array
   #self.tables[gender] = [[]]*24
   # this gives a DIFFERENT empty list object in each slot
   # Ref: http://stackoverflow.com/questions/13520876/how-can-i-make-multiple-empty-arrays-in-python
   self.tables[gender] = pyrec.tab
  else:
   print "Duplicate gender (%s) for stem %s" %(gender,self.stem)  

def main(filein,fileout,fileout1):
 recs=[]
 stemdict={}
 stemlist=[]
 f = codecs.open(filein,"r","utf-8")
 for line in f:
  pyrec = Pysan_rec(line)
  stem = pyrec.key1
  # discard cardinals and ordinals
  if pyrec.code2.lower().startswith('ord'):
   continue
  if pyrec.code2.lower().startswith('card'):
   continue
  # some others that are not grammatical pronouns
  if stem in ['azwan','dvi','tri','catur','zaz']:
   print "excluding",stem,"as non-pronoun"
   continue
  # assume it is a pronoun.
  if stem not in stemdict:
   stemdict[stem]=Pysan_pron(stem)
   stemlist.append(stem)
  rec = stemdict[stem]
  rec.update(pyrec)
 f.close()
 #fout = codecs.open(fileout,"w","utf-8")
 print len(stemlist),"stems found in",filein
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 # sort stemlist in Sanskrit alphabetical order
 # must convert the elements to pure ascii (which they are) before sorting
 stemlist = [str(x) for x in stemlist]
 stemlist.sort(cmp=slp_cmp)
 for stem in stemlist:
  rec = stemdict[stem]
  genders = rec.genders
  genders.sort()
  gstr = ','.join(genders)
  out = "%s:%s"%(stem,gstr)
  fout.write(out + "\n")
 fout.close()
 fout = codecs.open(fileout1,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  genders = rec.genders
  assert (len(genders)!=0),"NO INFLECTIONS FOR STEM %s"%stem
  for g in genders:
   t = rec.tables[g]
   tstr = prettify(t)
   out = "%s %s:%s" %(stem,g,tstr)
   fout.write(out + "\n")
 fout.close()

if __name__ == "__main__":
 filein = sys.argv[1]
 fileout = sys.argv[2]
 fileout1 = sys.argv[3]
 main(filein,fileout,fileout1)
