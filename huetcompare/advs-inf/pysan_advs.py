""" pysan_stems.py
    Aug 20, 2016
    for infinitive
"""
import sys,re,codecs
from sansort import slp_cmp


class MWverb(object):
 def __init__(self,stem):
  self.stem=stem
  self.forms = [] # list of adverbial forms

 def update(self,form):
  if form not in self.forms:
   self.forms.append(form)

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
   if root not in stemdict:
     stemdict[root]=MWverb(root)
     stemlist.append(root)
   rec = stemdict[root]
   for stem in stems:
    rec.update(stem)
 print len(stemlist),"stems found in filein"
 # sort stemlist in Sanskrit alphabetical order
 stemlist.sort(cmp=slp_cmp)
 fout = codecs.open(fileout,"w","utf-8") # utf-8 not required
 for stem in stemlist:
  rec = stemdict[stem]
  forms = rec.forms
  forms.sort(cmp=slp_cmp)
  formstr = ','.join(forms)
  out = "%s:%s" % (stem,formstr)
  fout.write(out + "\n")
 fout.close()
if __name__ == "__main__":
 filein = sys.argv[1] #MW-verb-inf.txt,etc
 sfx = sys.argv[2] # ipp
 fileout = "pysan_indecl_%s.txt" % sfx
 main(filein,fileout)


