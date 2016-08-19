""" pysan_stems.py
    Aug 16, 2016
    for future middle participles
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
:ak rpp 1a:(Akivas)
 into a tuple: (aMS,rpp,10,P,[Akivas])
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

def main(filein,fileout,voicematch):
 voice_to_pada = {'a':'P','m':'A'}
 padamatch=voice_to_pada[voicematch]
 stemdict={}
 stemlist=[]
 n=0
 with codecs.open(filein,"r") as f:
  for line in f:
   (root,partcode,theclass,pada,stems) = mwparse(line)
   if padamatch != pada:
    continue
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
  # for the main output, remove the root and set theclass to ''
  # this is for comparison with Huet
  cps = [('',p) for (r,c,p) in rcps]
  # remove duplicates
  cps = list(set(cps))
  cpstrs = map(lambda x: x[0]+x[1],cps)
  cpstrs.sort()
  cpstr = ','.join(cpstrs)
  out = "%s:%s" % (stem,cpstr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] #MW-verb-prap.txt
 sfx = sys.argv[2]
 sfx_to_voice={'ppfta':'a','ppftm':'m'}
 voice = sfx_to_voice[sfx]
  
 fileout = "pysan_stems_%s.txt" % sfx
 main(filein,fileout,voice)


