""" compare_stems.py
    Aug 14, 2016
"""
import sys
from sansort import slp_cmp

class Stem(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.stem,self.padastr) = line.split(':')
  self.padas = self.padastr.split(',')

class Merge(object):
 def __init__(self,stem):
  self.stem = stem
  self.cases = [None,None]

huetadjust={"ayam":"idam","uttara#1":"uttara","uttara#2":"uttara"}


def merge(recs1,recs2,name1,name2):
 """Assume both are sorted via sanskrit order of stem
 """
 mergedict={}
 bothrecs = [recs1,recs2]
 names = [name1,name2]
 for icase in xrange(0,len(bothrecs)):
  recs = bothrecs[icase]
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

def main(tense,name1,name2,nameout):
 # phase 1. Compare stem information 
 filesfx = "_stems_%s.txt"%tense
 filein1 = name1 + filesfx
 with open(filein1,"r") as f:
  stems1 = [Stem(x) for x in f]
 filein2 = name2 + filesfx
 with open(filein2,"r") as f:
  stems2 = [Stem(x) for x in f]
 names=[name1,name2]
 print name1,"has",len(stems1),"stems"
 print name2,"has",len(stems2),"stems"
 mergedict = merge(stems1,stems2,name1,name2)
 mergestems = mergedict.keys()
 mergestems.sort(cmp=slp_cmp)
 print len(mergestems)," merged stems"
 
 statusnames = ["IDENTICAL PADAS",
  "%s PADAS < %s PADAS"%(name1.upper(),name2.upper()),
  "%s PADAS > %s PADAS"%(name1.upper(),name2.upper()),
  "DIFFERENT PADAS",
  "STEM IN %s ONLY" % name2.upper(),
  "STEM IN %s ONLY" % name1.upper()
 ]
 outrecs = []
 istem = 0
 counts = [0]*len(statusnames)  # four status values
 for stem in mergestems:
  istem = istem + 1
  mergerec = mergedict[stem]
  (rec1,rec2) = mergerec.cases
  if rec1 and (not rec2):
   out = "%s in %s" %(rec1.line,name1)
   istatus = 5
  elif (not rec1) and rec2:
   out = "%s in %s" %(rec2.line,name2)
   istatus = 4
  elif rec1 and rec2:
   padas1 = set(rec1.padas)
   padas2 = set(rec2.padas)
   if padas1 == padas2:
    out = "%s identical in BOTH" % rec1.line
    istatus = 0
   elif padas1.issubset(padas2):
    istatus = 1
    diffset = padas2 - padas1
    diff = list(diffset)
    diff.sort()
    diffstr = ','.join(diff)
    out = "%s has %s(%s) < %s + %s(%s)" %(stem,rec1.padastr,name1,rec1.padastr,diffstr,name2)
   elif padas2.issubset(padas1):
    istatus = 2
    diffset = padas1 - padas2
    diff = list(diffset)
    diff.sort()
    diffstr = ','.join(diff)
    out = "%s has %s + %s(%s) > %s (%s)" %(stem,rec2.padastr,diffstr,name1,rec2.padastr,name2)    
   else:
    out = "%s has %s(%s) != %s(%s)" %(stem,rec1.padastr,name1,rec2.padastr,name2)
    istatus = 3
  outrec = (istatus,istem,out)
  outrecs.append(outrec)
  counts[istatus]= counts[istatus]+1
 outrecs.sort(key = lambda outrec: ((10000*outrec[0]) + outrec[1]))
 fileout = nameout + filesfx
 fout=open(fileout,"w")
 prevstatus = None
 for (istatus,istem,out) in outrecs:
  if istatus != prevstatus:
   outarr= []
   outarr.append("-"*50)
   outarr.append("%02d cases of %s" %(counts[istatus],statusnames[istatus]))
   outarr.append("-"*50)
   for out1 in outarr:
    fout.write(out1 + "\n")
   prevstatus = istatus
   icase = 0
  # write next case
  icase = icase + 1
  out1 = "Case %02d: %s" %(icase,out)
  fout.write(out1 + "\n")
 fout.close() 

if __name__ == "__main__":
 tense = sys.argv[1]
 name1 = "huet"
 name2 = "pysan"
 nameout = "compare"

 main(tense,name1,name2,nameout)
