""" pysan_stems.py
    Aug 13, 2016
    Adapted for Aorist. Reads MW-verb-aor.txt tables of conjugations
"""
import sys,re,codecs
from sansort import slp_cmp


class MWverb(object):
 def __init__(self,stem):
  self.stem=stem
  self.kps = [] # list of kps
  self.tables={}  # key is kps
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def update_pada(self,pada,kind):
  kp = "%s%s" %(kind,pada)
  if kp not in self.kps:
   self.kps.append(kp)
   self.tables[kp]=[]

def mwparse(line):
 """ parse 
:avalok aor5 1m:[Avalokizwa AvalokizAtAm Avalokizata AvalokizWAH AvalokizATAm AvalokiDvam Avalokizi Avalokizvahi Avalokizmahi]
 into (avalok,1,m,5)  (stem, class, pada, aorist kind)
 """
 line = line.rstrip('\r\n')
 (empty,header,tabstr)=line.split(':')
 (stem,aorkind,cp) = header.split(' ')
 tense = aorkind[0:-1]  # always aor
 kind =aorkind[-1]
 assert (tense=='aor') and (kind in ["1","2","3","4","5","6","7"]), "mwparse error: %s"%header
 theclass = cp[0:-1]
 voice = cp[-1]
 assert (theclass in ["1","2","3","4","5","6","7","8","9","10"]),"mwparse error: %s"%header
 assert (voice in ["a","m"]),"mwparse error: %s"%header
 voice_to_pada = {'a':'P','m':'A'}
 pada = voice_to_pada[voice]
 return (stem,theclass,pada,kind)

def main(filein,fileout):
 stemdict={}
 stemlist=[]
 n=0
 with codecs.open(filein,"r") as f:
  for line in f:
   (stem,theclass,pada,kind) = mwparse(line)
   if stem not in stemdict:
    stemdict[stem]=MWverb(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   rec.update_pada(pada,kind)
 print len(stemlist),"stems found in filein"
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  kps = rec.kps
  kps.sort()
  kpstr = ','.join(kps)
  out = "%s:%s" % (stem,kpstr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] # MW-verb-aor.txt
 htense = sys.argv[2]
 fileout = "pysan_stems_%s.txt" % htense
 main(filein,fileout)


