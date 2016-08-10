""" compare_pron_stems.py
"""
import sys
from sansort import slp_cmp
from pysan_pronouns import declstr_listify

class Stem(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.stem,self.genderstr) = line.split(':')
  self.genders = self.genderstr.split(',')

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
   if (icase == 0):
    if stem in huetadjust:
     newstem = huetadjust[stem]
     print "Adjusting %s stem from %s to %s" %(name1,stem,newstem)
     stem = newstem
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
 filesfx = "_pron_stems_genders.txt"
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
 outrecs = []
 istem = 0
 statusnames = ["IDENTICAL GENDERS",
  "DIFFERENT GENDERS",
  "STEM IN %s ONLY" % name2.upper(),
  "STEM IN %s ONLY" % name1.upper()
 ]
 counts = [0]*4  # four status values
 for stem in mergestems:
  istem = istem + 1
  mergerec = mergedict[stem]
  (rec1,rec2) = mergerec.cases
  if rec1 and (not rec2):
   out = "%s in %s" %(rec1.line,name1)
   istatus = 3
  elif (not rec1) and rec2:
   out = "%s in %s" %(rec2.line,name2)
   istatus = 2
  elif rec1 and rec2:
   if rec1.genderstr == rec2.genderstr:
    out = "%s identical in BOTH" % rec1.line
    istatus = 0
   else:
    out = "%s has %s(%s) != %s(%s)" %(stem,rec1.genderstr,name1,rec2.genderstr,name2)
    istatus = 1
  outrec = (istatus,istem,out)
  outrecs.append(outrec)
  counts[istatus]= counts[istatus]+1
 outrecs.sort(key = lambda outrec: ((1000*outrec[0]) + outrec[1]))
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
   if rec1.genderstr == rec2.genderstr:
    istatus = 0  
   else:
    istatus = 1
   #print stem,rec1.genderstr,rec2.genderstr, " (",rec1.genders,rec2.genders,")"
   for g in rec1.genders:
    if g == 'd':
     # change to 'm' (see Decl.__init__ also)
     g = 'm'
    if g not in rec2.genders:
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
 outrecs.sort(key = lambda outrec: ((1000*outrec[0]) + outrec[1]))

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
