""" compare_conj_tables.py
  Sep 1, 2016 (?) started
  Sep 29, 2016 - add known_differences logic to simplify comparison analysis
"""
import sys,re
#from sansort import slp_cmp
#from pysan_nouns import conjstr_listify
import json

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
 

def init_known_diffs(filein):
 with open(filein,"r") as f:
  diffrecs = json.load(f)
 print len(diffrecs),"known differences read from", filein
 d = {}
 for diffrec in diffrecs:
  # diffrec is a a dictionary, with key names of:
  # key, pysan, sanverb.  All values are strings
  key = diffrec['key']
  if key in d:
   print "init_known_diffs: duplicate key=",key
  else:
   d[key] = diffrec
  # Add a 'used' attribute, so
  diffrec['used']=False
 return d

def check_known_diff(known_diffs_d,key,name1,tab1str,name2,tab2str):
 if key in known_diffs_d:
  diffrec = known_diffs_d[key]
  if (diffrec[name1] == tab1str) and (diffrec[name2] == tab2str):
   # mark as used
   diffrec['used']=True
   return True
  else: 
   # print message - the known_diff file may to be updated
   if (diffrec[name1] != tab1str):
    print "Known Difference obsolete?",name1,key,tab1str
   if (diffrec[name2] != tab2str):
    print "Known Difference obsolete?",name2,key,tab2str
 return False

def diffs_class10_long_a(jsondiffs,name1,name2):
 """ filter the known differences which are
   (a) class 10
   (b) the 3s in pysan has longA in base, while that in pysanskrit has
       short a.
 """
 filterdiffs=[]
 for diffobj in jsondiffs:
  key = diffobj['key']
  if not key.endswith(('10A','10P')):
   continue
  tab1str = diffobj[name1] # sanverb
  tab2str = diffobj[name2] # pysan
  tab1 = conjstr_listify(tab1str)
  tab2 = conjstr_listify(tab2str)
  # pull out 3s
  first1 = tab1[0]
  first2 = tab2[0]
  #print "dbg:",key,first1,first2
  # discard if either is a list
  if isinstance(first1,(list,tuple)):
   if len(first1) > 1:
    continue
   else:
    first1 = first1[0]
  if isinstance(first2,(list,tuple)):
   if len(first2) > 1:
    continue
   else:
    first2 = first2[0]
  if len(first1)!= len(first2):
   # some other difference
   continue
  idx = first2.find('A') # long A in pysan
  if idx == -1:
   continue
  if first1[idx] != 'a':
   continue
  # found one!
  filterdiffs.append(diffobj)
 tempout = "temp_class10_Aa.txt"
 with open(tempout,"w") as f:
  # write a json array
  stemp = json.dumps(filterdiffs,sort_keys=True,indent=2)
  f.write(stemp + '\n')
 print "wrote",len(filterdiffs),"json records to",tempout

def main(name1,name2,nameout,filein1,filein2,filein3,fileout):
 """ The files filein1 and filein2 have been
  constructed in such a way that they have the same number of lines and
  the lines are in the same order.  Thus, we compare corresponding
  lines from the two files, one by one, to generate our results.
 """
 # Compare conjugation tables which appear in both
 # read in conjugation tables
 #filesfx = "_conj_tables.txt"
 names=[name1,name2]
 #fileins = [x+filesfx for x in names]
 fileins = [filein1,filein2]
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
 # read known differences
 known_diffs_d = init_known_diffs(filein3)
 # prepare outrecs
 pysan_emptytab = ['nil' for i in xrange(0,9)]
 pysan_emptystr = '[' + (' '.join(pysan_emptytab)) + ']'
 outrecs = []
 nrecs = len(conjrecs[0])
 nok = 0
 ndiff = 0
 nf = 0
 nden = 0 # number of denominatives (class = 11)
 jsondiffs = []
 nknowndiffs=0
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
  elif check_known_diff(known_diffs_d,key,name1,tab1str,name2,tab2str):
   outrec = "%s: known_difference" % key
   nknowndiffs = nknowndiffs + 1
  else:
   ndiff = ndiff + 1
   identstr = ','.join(idents)
   outarr[0] = "%s: conjugation differences @ %s " % (key,identstr)
   # Sep 29, 2016 - insert two lines, for building known differences
   diffobj = {
    "key":key,
    name1:tab1str,
    name2:tab2str
   }   
   jsondiffs.append(diffobj)
   #outarr.insert(1,stemp)
   #outarr.insert(1,"%s:%s:%s" %(name1,key,tab1str))
   #outarr.insert(2,"%s:%s:%s" %(name2,key,tab2str))
   out = "\n" .join(outarr)
   outrec = out #(istatus,istem,out)
  outrecs.append(outrec)

 # differences written to temporary file
 # fileout is a path x/y.txt
 tempout = fileout.replace('.txt','_jsondiff.txt')
 if tempout == fileout:
  tempout = "tempxyz.txt" # in case fileout not as expected
 with open(tempout,"w") as f:
  # write a json array
  stemp = json.dumps(jsondiffs,sort_keys=True,indent=2)
  f.write(stemp + '\n')
 print "wrote",len(jsondiffs),"json records to",tempout

 #outrecs.sort(key = lambda outrec: ((1000*outrec[0]) + outrec[1]))

 # generate output to file
 #fileout = nameout + filesfx
 fout=open(fileout,"w")
 for outrec in outrecs:
  fout.write('%s\n' % outrec)
 fout.close()
 print (nok + ndiff + nf + nden),"records written to",fileout
 print nden,"denominatives have no %s conjugation"%name2
 print nf,"non-denominatives have no %s conjugation"%name2
 print nok,"of these are identical"
 print nknowndiffs,"of these are known differences"
 print ndiff,"of these have additional differences"
 # print message for unused known differences
 n=0
 for key in known_diffs_d:
  diffrec = known_diffs_d[key]
  if not diffrec['used']:
   print "UNUSED KNOWN DIFF WARNING:",key
   n = n + 1
 if n > 0:
  print n,"known differences UNUSED"
 else:
  print "ALL known differences USED"
 if True:
  # temporary test logic
  diffs_class10_long_a(jsondiffs,name1,name2)

if __name__ == "__main__":
 filein1 = sys.argv[1]
 filein2 = sys.argv[2]
 filein3 = sys.argv[3] # known differences
 fileout = sys.argv[4]
 name1 = 'sanverb' # dhaval's
 name2 = 'pysan'  # our pysanskrit
 nameout = 'compare'

 main(name1,name2,nameout,filein1,filein2,filein3,fileout)
