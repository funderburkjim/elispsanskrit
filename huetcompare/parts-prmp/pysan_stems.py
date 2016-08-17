""" pysan_stems.py
    Aug 17, 2016
    for present middle participles
"""
import sys,re,codecs
from sansort import slp_cmp


class MWverb(object):
 def __init__(self,stem):
  self.stem=stem
  self.rcps = [] # list of (root,class,pada) tuples

 def update(self,root,theclass,pada):
  rcp = (root,theclass,pada)
  if rcp not in self.rcps:
   self.rcps.append(rcp)

def mwparse(line):
 """ parse 
:aMS prap 10a:(aMSApayat aMSayat)
 into a tuple: (aMS,prap,10,P,[aMSApayat,aMSayat])
 """
 line = line.rstrip('\r\n')
 m = re.search(r'^:(.*?) (.*?) (.*?)(.):\((.*?)\)$',line)
 if not m:
  print "mwparse error",line
  exit(1)
 root=m.group(1)
 partcode = m.group(2)
 theclass = m.group(3)
 voice = m.group(4)
 voice_to_pada = {'a':'P','m':'A'}
 pada = voice_to_pada[voice]
 valuestr = m.group(5)
 values = valuestr.split(' ')
 return (root,partcode,theclass,pada,values)

def main(filein,fileout):
 stemdict={}
 stemlist=[]
 n=0
 with codecs.open(filein,"r") as f:
  for line in f:
   (root,partcode,theclass,pada,stems) = mwparse(line)
   for stem in stems:
    if stem not in stemdict:
     stemdict[stem]=MWverb(stem)
     stemlist.append(stem)
    rec = stemdict[stem]
    rec.update(root,theclass,pada)
 print len(stemlist),"stems found in filein"
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  rcps = rec.rcps
  # for the main output, remove the root
  cps = [(c,p) for (r,c,p) in rcps]
  # remove duplicates
  cps = list(set(cps))
  cpstrs = map(lambda x: x[0]+x[1],cps)
  cpstrs.sort()
  cpstr = ','.join(cpstrs)
  out = "%s:%s" % (stem,cpstr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] #MW-verb-prmp.txt
 sfx = sys.argv[2]
 fileout = "pysan_stems_%s.txt" % sfx
 main(filein,fileout)


