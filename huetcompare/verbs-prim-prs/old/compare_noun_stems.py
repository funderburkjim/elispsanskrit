""" compare_noun_stems.py
"""
import sys,re
#from sansort import slp_cmp
#from pysan_nouns import declstr_listify

def declstr_listify(tabstr):
 """ convert the string representation of a declension to 
     a list form.
 """
 tabstr = tabstr[1:-1] # remove beginning and ending square bracket
 # change (x y) to x,y
 def g(m):
  x = m.group(1)
  return ','.join(x.split(' '))
 tabstr = re.sub(r'\((.*?)\)', g,tabstr)
 tab = tabstr.split(' ')
 # replace x,y by a list [x,y]
 # this also r
 def f(x):
  if ',' in x:
   y = x.split(',')
   y = list(y)
   y.sort() 
  elif x == 'nil':
   y = []
  else:
   y = [x]
  return y
 tab = map(f,tab)
 return tab

class unused_Stem(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.stem,self.genderstr) = line.split(':')
  self.genders = self.genderstr.split(',')

class unused_Merge(object):
 def __init__(self,stem):
  self.stem = stem
  self.cases = [None,None]

#huetadjust={"ayam":"idam","uttara#1":"uttara","uttara#2":"uttara"}

class Decl(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  (a,self.tabstr) = line.split(':')
  (self.stem,self.gender) = a.split(' ')
  self.key = "%s %s" %(self.stem,self.gender)
 
def unused_merge(recs1,recs2,name1,name2):
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
 """ The files huet_noun_tables.txt and pysan_noun_tables.txt have been
  constructed in such a way that they have the same number of lines and
  the lines are in the same order.  Thus, we compare corresponding
  lines from the two files, one by one, to generate our results.
 """
 # Compare declension tables which appear in both
 # read in declension tables
 filesfx = "_noun_tables.txt"
 names=[name1,name2]
 fileins = [x+filesfx for x in names]
 #decldicts = [None,None]
 declrecs = [None,None]
 for icase in xrange(0,2):
  #decldicts[icase]={}
  #decldict = decldicts[icase]
  filein = fileins[icase]
  with open(filein,"r") as f:  
   decls = [Decl(x) for x in f]
  declrecs[icase]=decls
  print len(decls),"records from",filein
  #for decl in decls:
  # decldict[decl.key] = decl
 (declrecs1,declrecs2) = declrecs
 assert (len(declrecs1) == len(declrecs2))
 # prepare outrecs
 pysan_emptytab = ['nil' for i in xrange(0,24)]
 pysan_emptystr = '[' + (' '.join(pysan_emptytab)) + ']'
 outrecs = []
 nrecs = len(declrecs[0])
 nok = 0
 ndiff = 0
 nf = 0
 for irec in xrange(0,nrecs):
  (rec1,rec2) = (declrecs[0][irec],declrecs[1][irec])
  if (rec1.key != rec2.key):
   print "ERROR in matching # %d: %s != %s" %(irec,rec1.key,rec2.key)
   exit(1)
  key=rec1.key
  tab1str = rec1.tabstr
  tab2str = rec2.tabstr
  outarr = [''] # fill in first line late
  idents=[] # case-number for disagreeing items
  #outarr.append("%s declension differences" % key)
  tab1 = declstr_listify(tab1str)
  tab2 = declstr_listify(tab2str)
  #print len(tab1),len(tab2)
  for jcase in xrange(0,8):
   if jcase == 7:
    # huet never has vocative for nouns. Don't check this
    continue
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
    idents.append(ident)
    outarr.append('     %s (%s) %s != %s (%s)' %(ident,name1,d1,d2,name2))
  #out1 = "     %s declension in %5s = %s" %(key,name1,tab1str)
  #out2 = "     %s declension in %5s = %s" %(key,name2,tab2str)
  if len(idents) == 0:
   outrec = "%s: identical, exc. for vocative" % key
   nok = nok + 1
  elif tab2str == pysan_emptystr:
   nf = nf + 1
   outrec = "%s: %s NOT FOUND" % (key,name2)
  else:
   ndiff = ndiff + 1
   identstr = ','.join(idents)
   outarr[0] = "%s: declension differences @ %s " % (key,identstr)
   out = "\n" .join(outarr)
   outrec = out #(istatus,istem,out)
  outrecs.append(outrec)

 #outrecs.sort(key = lambda outrec: ((1000*outrec[0]) + outrec[1]))

 # generate output to file
 fileout = nameout + filesfx
 fout=open(fileout,"w")
 for outrec in outrecs:
  fout.write('%s\n' % outrec)
 fout.close()
 print (nok + ndiff + nf),"records written to",fileout
 print nf,"of these have no %s declension"%name2
 print nok,"of these differ only in vocative"
 print ndiff,"of these have additional differences"
 return

if __name__ == "__main__":
 name1 = sys.argv[1]
 name2 = sys.argv[2]
 nameout = sys.argv[3]

 main(name1,name2,nameout)
