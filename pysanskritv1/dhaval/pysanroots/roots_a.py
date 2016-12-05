""" roots_a.py
    Sep 1, 2016
    Some roots in verbdata (SanskritVerb) end in 'a', and should 
    correspond to MW roots spelled without the 'a'.
    
"""
import sys,codecs,re
from sansort import slp_cmp
#from pysan_pronouns import declstr_listify

class Stem(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.stem,self.cpstr) = line.split(':')
  self.cps = self.cpstr.split(',')

class Merge(object):
 def __init__(self,stem):
  self.stem = stem
  self.cases = [None,None]

huetadjust={"ayam":"idam","uttara#1":"uttara","uttara#2":"uttara"}

class Decl(object):
 def __init__(self,line,icase):
  line = line.rstrip('\r\n')
  (a,self.tabstr) = line.split(':')
  (self.stem,self.gender) = a.split(' ')
  if (icase == 0):
   if (self.stem in huetadjust):
    newstem = huetadjust[self.stem]
    self.stem = newstem
   elif (self.stem in ['asmad','yuzmad']) and (self.gender == 'd'):
    # change 'deictic' to some gender, say 'm'
    self.gender = 'm'
  self.key = "%s %s" %(self.stem,self.gender)
 
def merge(recs1,recs2,name1,name2):
 """Assume both are sorted via sanskrit order of stem
 """
 mergedict={}
 bothrecs = [recs1,recs2]
 names = [name1,name2]
 for icase in xrange(0,len(bothrecs)):  
  # icase = 0 or 1
  recs = bothrecs[icase] # recs1 if icase=0, recs2 if icase=1
  for rec in recs:
   stem = rec.stem
   if stem not in mergedict:
    mergedict[stem]=Merge(stem)
   mergerec = mergedict[stem]
   if mergerec.cases[icase] != None:
    print "Discarding duplicate stem %s from %s" %(stem,names[icase])
    continue
   mergerec.cases[icase]=rec
 return mergedict

def main(filein1,filein2,fileout):
 """ filein1 is verbdata
     filein2 is mwvlex

 """ 
 # phase 1. load stem-gender information 
 with open(filein1,"r") as f:
  stems1 = [Stem(x) for x in f]
 with open(filein2,"r") as f:
  stems2 = [Stem(x) for x in f]
 name1 = filein1
 name2 = filein2
 names=[name1,name2]
 print name1,"has",len(stems1),"stems"
 print name2,"has",len(stems2),"stems"
 mergedict = merge(stems1,stems2,name1,name2)
 mergestems = mergedict.keys()
 mergestems.sort(cmp=slp_cmp)
 print len(mergestems)," merged stems"
 
 outarr=[]
 ncase=0
 nboth=0
 for stem in mergestems:
  mergerec = mergedict[stem]
  (rec1,rec2) = mergerec.cases
  # rec1 is the sanverb record for stem
  # rec2 is the mwvlex record for stem
  if not (rec1 and (not rec2)):
   continue
  # so stem is in sanverb, but not in MW
  if not stem.endswith('a'):
   continue
  # Examine stem without the final a
  stema = stem[0:-1]
  if stema not in mergedict:
   continue
  mergereca = mergedict[stema]
  (reca1,reca2) = mergereca.cases
  # reca1 is the record for stema in sanverb (or None if absent)
  # reca2 is the record for stema in mwvlex (or None if absent)
  if not reca2:
   continue
  if reca1:
   # stem with both 'a' and not 'a' are in sanverb
   # make a note of this, but also write it to output
   print "both in sanverb: %s and %s" %(rec1.line, reca1.line)
   nboth = nboth+1
  # so reca2 is present, meaning it is in MW. This is the case we want
  if reca1 and reca2:
   try:
    out = "%s#%s#%s" %(rec1.line,reca1.line,reca2.line)
   except:
    print "pysanroots/roots_a.py ERROR:",stem,rec1,reca1,reca2
    #print "pysanroots/roots_a.py ERROR:",rec1.line,reca1.line,reca2.line
    continue
  else:
   out = "%s##%s" %(rec1.line,reca2.line)
  outarr.append(out)
  ncase = ncase+1
 fout=open(fileout,"w")
 for out in outarr:
  fout.write(out + "\n")
 print ncase,"cases written to",fileout
 print nboth,"cases where",filein1,"has root both with 'a' and without 'a'"
 exit()

if __name__ == "__main__":
 filein1 = sys.argv[1]
 filein2 = sys.argv[2]
 fileout = sys.argv[3]
 main(filein1,filein2,fileout)
