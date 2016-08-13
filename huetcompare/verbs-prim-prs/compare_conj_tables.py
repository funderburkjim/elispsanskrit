""" compare_conj_tables.py
"""
import sys,re
#from sansort import slp_cmp
#from pysan_nouns import conjstr_listify

def conjstr_listify(tabstr):
 """ convert the string representation of a conjugation to 
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


class Conj(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  (a,self.tabstr) = line.split(':')
  (self.stem,self.tense,self.cp) = a.split(' ')
  self.key = a
 

def main(name1,name2,nameout):
 """ The files huet_conj_tables.txt and pysan_conj_tables.txt have been
  constructed in such a way that they have the same number of lines and
  the lines are in the same order.  Thus, we compare corresponding
  lines from the two files, one by one, to generate our results.
 """
 # Compare conjugation tables which appear in both
 # read in conjugation tables
 filesfx = "_conj_tables.txt"
 names=[name1,name2]
 fileins = [x+filesfx for x in names]
 #conjdicts = [None,None]
 conjrecs = [None,None]
 for icase in xrange(0,2):
  #conjdicts[icase]={}
  #conjdict = conjdicts[icase]
  filein = fileins[icase]
  with open(filein,"r") as f:  
   conjs = [Conj(x) for x in f]
  conjrecs[icase]=conjs
  print len(conjs),"records from",filein
  #for conj in conjs:
  # conjdict[conj.key] = conj
 (conjrecs1,conjrecs2) = conjrecs
 assert (len(conjrecs1) == len(conjrecs2))
 # prepare outrecs
 pysan_emptytab = ['nil' for i in xrange(0,9)]
 pysan_emptystr = '[' + (' '.join(pysan_emptytab)) + ']'
 outrecs = []
 nrecs = len(conjrecs[0])
 nok = 0
 ndiff = 0
 nf = 0
 nden = 0 # number of denominatives (class = 11)
 for irec in xrange(0,nrecs):
  (rec1,rec2) = (conjrecs[0][irec],conjrecs[1][irec])
  if (rec1.key != rec2.key):
   print "ERROR in matching # %d: %s != %s" %(irec,rec1.key,rec2.key)
   exit(1)
  key=rec1.key
  tab1str = rec1.tabstr
  tab2str = rec2.tabstr
  outarr = [''] # fill in first line late
  idents=[] # case-number for disagreeing items
  #outarr.append("%s conjugation differences" % key)
  tab1 = conjstr_listify(tab1str)
  tab2 = conjstr_listify(tab2str)
  #print len(tab1),len(tab2)
  for person in xrange(0,3):
   for jnumber in xrange(0,3):
    j = (person*3) + jnumber
    #print person,jnumber,j
    t1 = tab1[j]
    t2 = tab2[j]
    if t1 == t2:
     continue
    if (len(t1) == 1) and (len(t2) == 1):
     # Huet often ends optative 3p in 'ur' while pysan spells 'uH'.
     # This is not viewed as a serious difference
     s1 = t1[0]
     s2 = t2[0]
     if s1.endswith('r') and s2.endswith('H') and (s1[0:-1] == s2[0:-1]):
      continue
    d1 = ','.join(t1)
    d2 = ','.join(t2)
    if d1 == '':
     d1 = '_'
    if d2 == '':
     d2 = '_'
    ident = "%s%s" %([3,2,1][person],['s','d','p'][jnumber])
    idents.append(ident)
    outarr.append('     %s (%s) %s != %s (%s)' %(ident,name1,d1,d2,name2))
  if len(idents) == 0:
   outrec = "%s: identical" % key
   nok = nok + 1
  elif (tab2str == pysan_emptystr) and (rec1.cp.startswith('11')):
   nden = nden + 1
   outrec = "%s: %s Denominative NOT FOUND" % (key,name2)
  elif tab2str == pysan_emptystr:
   nf = nf + 1
   outrec = "%s: %s NOT FOUND" % (key,name2)
  else:
   ndiff = ndiff + 1
   identstr = ','.join(idents)
   outarr[0] = "%s: conjugation differences @ %s " % (key,identstr)
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
 print (nok + ndiff + nf + nden),"records written to",fileout
 print nden,"denominatives have no %s conjugation"%name2
 print nf,"non-denominatives have no %s conjugation"%name2
 print nok,"of these are identical"
 print ndiff,"of these have additional differences"
 return

if __name__ == "__main__":
 name1 = sys.argv[1]
 name2 = sys.argv[2]
 nameout = sys.argv[3]

 main(name1,name2,nameout)
