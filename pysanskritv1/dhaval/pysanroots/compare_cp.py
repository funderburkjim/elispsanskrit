""" compare_cp.py
    Aug 29, 2016
    Adapted from elispsanskrit/huetcompare/verbs-prim-prs/compare_class_pada.py
"""
import sys
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

def main(name1,name2,nameout):
 # phase 1. Compare stem-gender information 
 filesfx = "_cp.txt"
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
 
 outrecs = []
 istem = 0
 statusnames = ["IDENTICAL CPS",
  "%s CPS < %s CPS"%(name1.upper(),name2.upper()),
  "%s CPS > %s CPS"%(name1.upper(),name2.upper()),
  "DIFFERENT CPS",
  "STEM IN %s ONLY" % name2.upper(),
  "STEM IN %s ONLY" % name1.upper()
 ]
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
   cps1 = set(rec1.cps)
   cps2 = set(rec2.cps)
   if cps1 == cps2:
    out = "%s identical in BOTH" % rec1.line
    istatus = 0
   elif cps1.issubset(cps2):
    istatus = 1
    diffset = cps2 - cps1
    diff = list(diffset)
    diff.sort()
    diffstr = ','.join(diff)
    out = "%s has %s(%s) < %s + %s(%s)" %(stem,rec1.cpstr,name1,rec1.cpstr,diffstr,name2)
   elif cps2.issubset(cps1):
    istatus = 2
    diffset = cps1 - cps2
    diff = list(diffset)
    diff.sort()
    diffstr = ','.join(diff)
    out = "%s has %s + %s(%s) > %s (%s)" %(stem,rec2.cpstr,diffstr,name1,rec2.cpstr,name2)    
   else:
    out = "%s has %s(%s) != %s(%s)" %(stem,rec1.cpstr,name1,rec2.cpstr,name2)
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
 exit(1)
 # phase 2.  Compare declension tables which appear in both
 # read in declension tables
 filesfx = "_pron_tables.txt"
 fileins = [x+filesfx for x in names]
 decldicts = [None,None]
 for icase in xrange(0,2):
  decldicts[icase]={}
  decldict = decldicts[icase]
  filein = fileins[icase]
  with open(filein,"r") as f:  
   decls = [Decl(x,icase) for x in f]
  for decl in decls:
   decldict[decl.key] = decl
 # prepare outarr
 outrecs = []
 istem = 0
 (decldict1,decldict2) = decldicts
 for stem in mergestems:
  istem = istem + 1
  mergerec = mergedict[stem]
  (rec1,rec2) = mergerec.cases
  if rec1 and (not rec2):
   #out = "%s in %s" %(rec1.line,name1)
   #istatus = 3
   continue
  elif (not rec1) and rec2:
   #out = "%s in %s" %(rec2.line,name2)
   #istatus = 2
   continue
  elif rec1 and rec2:
   if rec1.cpstr == rec2.cpstr:
    istatus = 0  
   else:
    istatus = 1
   #print stem,rec1.cpstr,rec2.cpstr, " (",rec1.cps,rec2.cps,")"
   for g in rec1.cps:
    if g == 'd':
     # change to 'm' (see Decl.__init__ also)
     g = 'm'
    if g not in rec2.cps:
     continue
    key = "%s %s" %(stem,g)
    tab1str = decldict1[key].tabstr
    tab2str = decldict2[key].tabstr
    if tab1str == tab2str:
     out = "%s identical declensions" % key
    else:
     outarr = []
     outarr.append("%s declension differences" % key)
     tab1 = declstr_listify(tab1str)
     tab2 = declstr_listify(tab2str)
     #print len(tab1),len(tab2)
     for jcase in xrange(0,8):
      for jnumber in xrange(0,3):
       j = (jcase*3) + jnumber
       #print jcase,jnumber,j
       if tab1[j] == tab2[j]:
        continue
       d1 = ','.join(tab1[j])
       d2 = ','.join(tab2[j])
       if d1 == '':
        d1 = '_'
       if d2 == '':
        d2 = '_'
       ident = "%s%s" %(jcase+1,['s','d','p'][jnumber])
       outarr.append('     %s (%s) %s != %s (%s)' %(ident,name1,d1,d2,name2))
     #out1 = "     %s declension in %5s = %s" %(key,name1,tab1str)
     #out2 = "     %s declension in %5s = %s" %(key,name2,tab2str)
     out = "\n" .join(outarr)
    outrec = (istatus,istem,out)
    outrecs.append(outrec)
  counts[istatus]= counts[istatus]+1
 outrecs.sort(key = lambda outrec: ((10000*outrec[0]) + outrec[1]))

 # generate output to file
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
 name1 = sys.argv[1]
 name2 = sys.argv[2]
 nameout = sys.argv[3]

 main(name1,name2,nameout)
