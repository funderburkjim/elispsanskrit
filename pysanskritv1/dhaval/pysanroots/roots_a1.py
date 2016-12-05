""" roots_a1.py
    Sep 17, 2016
    See readme.md for discussion
    
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

class PseudoStem(Stem):
 def __init__(self,linein):
  linein = linein.rstrip('\r\n')
  self.linein = linein
  # line = 15:pAra:karmasamAptO:10:0453:u:sew:pAraya:Q
  (tempid,rootwithoutanubandha,meaning,gana,number,pada,sewcode,mwdenomspelling,code)=linein.split(':')
  # generate a Stem record, 
  # spell the root without the ending 'a'
  assert rootwithoutanubandha.endswith('a'),"Pseudostem problem 1, line=:%s" % linein
  self.stem = rootwithoutanubandha[0:-1] # drop the last char, an 'a'
  # generate class pada information from gana and pada
  assert pada in ['pa','u','A'],"Pseudostem problem 2, pada='%s', line=:%s" % (pada,linein)
  pada_to_padas={'pa':['P'],'A':['A'],'u':['P','A']}
  padas = pada_to_padas[pada]
  self.cps = map(lambda x: gana+x,padas)
  self.cpstr = ','.join(self.cps)
  self.line = '%s:%s'%(self.stem,self.cpstr)
class Merge(object):
 def __init__(self,stem):
  self.stem = stem
  # self.cases[0] =  sanverb record for stem (or None)
  # self.cases[1] =  'pseudoMW' record for stem (or None)
  self.cases = [None,None]

#huetadjust={"ayam":"idam","uttara#1":"uttara","uttara#2":"uttara"}


def merge(recs1,recs2,name1,name2):
 """Assume both are sorted via sanskrit order of stem (?)
 """
 mergedict={}
 bothrecs = [recs1,recs2]
 names = [name1,name2]
 for icase in xrange(0,len(bothrecs)):
  recs = bothrecs[icase]
  for rec in recs:
   stem = rec.stem
   if stem not in mergedict: 
    # initialize Merge record for this stem
    mergedict[stem]=Merge(stem)
   mergerec = mergedict[stem]
   if mergerec.cases[icase] != None:
    print "Discarding duplicate stem %s from %s" %(stem,names[icase])
    continue
   mergerec.cases[icase]=rec
 return mergedict

def main(filein1,filein2,fileout):
 """ filein1 is verbdata
     filein2 is roots_a1_prep

 """ 
 # phase 1. load stem-gender information 
 with open(filein1,"r") as f:
  stems1 = [Stem(x) for x in f]
 with open(filein2,"r") as f:
  stems2 = [PseudoStem(x) for x in f if not x.startswith(';')]
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
 ntemp = 0
 for stem in mergestems:
  mergerec = mergedict[stem]
  (rec1,rec2) = mergerec.cases
  if not (rec1 and (not rec2)):
   continue
  # so in sanverb, but not in MW
  if not stem.endswith('a'):
   continue
  # Examine stem without the final a
  stema = stem[0:-1]
  if stema not in mergedict:
   continue
  mergereca = mergedict[stema]
  (reca1,reca2) = mergereca.cases
  if not reca2:
   continue
  ntemp = ntemp+1
  print ntemp,stem,stema,reca1,reca2
  if reca1:
   # stem with both 'a' and not 'a' are in sanverb
   # make a note of this, but also write it to output
   print "both in sanverb: %s and %s" %(rec1.line, reca1.line)
   nboth = nboth+1
  # so reca2 is present, meaning it is in MW. This is the case we want
  if reca1:
   #assert rec1.line!=None,"check rec1.line"
   #assert reca1.line!=None,"check reca1.line"
   #assert reca2.line!=None,"check reca2.line"
   out = "%s#%s#%s" %(rec1.line,reca1.line,reca2.line)
  else:
   out = "%s##%s" %(rec1.line,reca2.line)
   #continue
  outarr.append(out)
  ncase = ncase+1
 fout=open(fileout,"w")
 for out in outarr:
  fout.write(out + "\n")
 print ncase,"cases written to",fileout
 print nboth,"cases where",filein1,"has root both with 'a' and without 'a'"
 exit()

if __name__ == "__main__":
 filein1 = sys.argv[1] # sanverb_cp.txt
 filein2 = sys.argv[2] #  roots_a1_prep.txt
 fileout = sys.argv[3]
 main(filein1,filein2,fileout)
