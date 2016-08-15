""" pysan_stems.py
    Aug 13, 2016
"""
import sys,re,codecs
from sansort import slp_cmp


class MWverb(object):
 def __init__(self,stem):
  self.stem=stem
  self.padas = [] # list of padas
  self.tables={}  # key is padas
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def update_pada(self,pada):
  if pada not in self.padas:
   self.padas.append(pada)
   self.tables[pada]=[]

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
   rec.update_pada(pada)
 print len(stemlist),"stems found in filein"
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  padas = rec.padas
  padas.sort()
  padastr = ','.join(padas)
  out = "%s:%s" % (stem,padastr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] #dcpforms-MW-verb.txt
 htense = sys.argv[2]
 fileout = "pysan_stems_%s.txt" % htense
 main(filein,fileout)


