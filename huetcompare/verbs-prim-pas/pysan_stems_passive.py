""" pysan_stems_passive.py
    Aug 13, 2016
"""
import sys,re,codecs
from sansort import slp_cmp


class MWverb(object):
 def __init__(self,stem):
  self.stem=stem
  self.tenses = [] # list of tenses
  self.tables={}  # key is tense
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def unused_update_cp(self,theclass,pada):
  cp = (theclass,pada)
  if cp not in self.cps:
   self.cps.append(cp)
   self.tenses[cp]=[]

 def unused_update_form(self,form,theclass,pada,tense,person,number):
  cp = (theclass,pada)
  if cp not in self.cps:
   self.cps.append(cp)
   self.tenses[cp]=[]
  if tense not in self.tenses[cp]:
   self.tenses[cp].append(tense)
   self.tables[(theclass,pada,tense)] = [[] for i in xrange(0,9)]
  t = self.tables[(theclass,pada,tense)]
  idx = Huet_person_number_index(person,number)
  t1 = t[idx]
  t1.append(form)

def mwparse(line):
 """ parse (aMS 10 P <MW=aMS,17,1>)  into (aMS,10,P), a tuple of strings
 """
 line = line.rstrip('\r\n')
 m = re.search(r'^\((.*?)\)$',line)
 if not m:
  print "mwparse: wrong line form",line
  exit(1)
 x = m.group(1)
 (stem,theclass,pada,extra) = x.split(' ')
 return (stem,theclass,pada)

def main(filein,fileout):
 stemdict={}
 stemlist=[]
 n=0
 with codecs.open(filein,"r") as f:
  for line in f:
   (stem,theclass,pada) = mwparse(line)
   if stem not in stemdict:
    stemdict[stem]=MWverb(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   #rec.update_cp(theclass,pada)
 print len(stemlist),"stems found in filein"
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  #cps = rec.cps
  #cps.sort()
  #cps_str = map(lambda x: x[0]+x[1],cps)
  #cpstr = ','.join(cps_str)
  out = stem # "%s:%s"%(stem,cpstr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] #dcpforms-MW-verb.txt
 fileout = sys.argv[2] # mw_stems_passive.txt
 main(filein,fileout)


