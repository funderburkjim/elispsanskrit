"""conjmap.py
   Sep 10, 2016
   Sep 20, 2016 - Revised to merge conjugation tables when there
      are multiple instances of root-without-anubandha + gana + pada
"""
import sys,re

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

def fjoin(tup):
 """ how to join a tuple of alternates """
 #tup = flatten(tup) # July 1, 2016
 if not isinstance(tup,(tuple,list)):
  if tup == '': # special handling of missing value. Example is vac, 3p 
   tup = 'nil' 
   return tup
  # ASSUME tup is a string.
  # IT MAY BE (example vid, ipv, 2, a) that this string is a comma-delimited
  #  set of alternates. 
  parts = tup.split(',')
  if len(parts) > 1:
   return fjoin(parts)
  # tup is now a string
  # Do some adjustment for comparison 
  #if preserve_elisp_errors:
  # tup = re.sub('^uu','U',tup)  # probably bogus upAr,3.
  return tup # a string
 # a list or tup.
 # for perfect, we sometimes (why?) have extra level of lists
 # e.g.  [[u'Ada'], ['jaGAsa']]   (ad 2 a prf) (July 1, 2016)
 #tup = map(solution,tup)
 if len(tup) == 1:
  # case of a tuple or list of length 1
  return fjoin(tup[0])
 else:
  ans = ('('+' '.join(tup) + ')')
  return ans

def conjtab_stringify(tab):
 ans = tab
 if len(ans) == 9:
  ans1 = [ans]  # case (a)
 else:
  ans1 = ans  # case (b)
 ans2 = zip(*ans1)  
 # this is some Python magic.
 # ans2 is a list of 9 tuples, where x1[i] is a tuple of corresponding
 # i-th elements of the lists in ans1
 ctab = map(fjoin,ans2)
 # now, change this list into a string
 ctab1 = ' '.join(ctab)
 ctab2 = "[%s]" % ctab1
 return ctab2

class Conjtab(object):
 """ Line in dhaval/conjtab/conj_xxx.txt
 """
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.header,self.conjtabstr) = line.split(':')
  # dproot here is WITH anubandha
  (self.dproot,self.tense,self.cpstr) = self.header.split(' ')
  m = re.search(r'^(..)[.](....)(.)$',self.cpstr)
  if not m:
   print "Conjtab error parsing:",line
   exit(1)
  (self.gana,self.number,self.pada) = (m.group(1),m.group(2),m.group(3))
  self.root = None  # filled in later, see 

def init_conjtab(filein):
 with open(filein,"r") as f:
  recs = [Conjtab(line) for line in f]
 print len(recs),"records read from",filein
 return recs

class Verbmap(object):
 d = {}
 def __init__(self,line):
  line = line.rstrip('\r\n')
  # dproot is WITH anubandha, root is WITHOUT anubandha
  (self.dproot,self.gana,self.number,self.root) = line.split(':')
  self.key = '.'.join([self.dproot,self.gana,self.number])
  if self.key in Verbmap.d:
   print "Verbmap. Unexpected duplicate key",line
   exit(1)
  Verbmap.d[self.key]=self

def init_verbmap(filein):
 with open(filein,"r") as f:
  recs = [Verbmap(line) for line in f]
 print len(recs),"records read from",filein
 return recs

class ConjtabMap(object):
 """ One or more lines in dhaval/conjtab/conj_xxx.txt
 """
 def __init__(self,conjtabrec):
  self.conjtabrecs = [conjtabrec]
 def merge(self):
  recs = self.conjtabrecs
  dpids = [] 
  for rec in recs:
   dpid = "%s %s.%s" %(rec.dproot,rec.gana,rec.number)
   dpids.append(dpid)
  dpidstr = ','.join(dpids)
  # check all records have the same root,gana,pada
  rec0 = recs[0]
  for rec in recs[1:]:
   assert (rec.root,rec.gana,rec.pada) == (rec0.root,rec0.gana,rec0.pada),"ConjtabMap problem"
  # Merge conjtabs
  conjtabstr = rec0.conjtabstr
  if len(recs) > 1:
   # tab is a list, and each element of tab is a list
   tab = conjstr_listify(conjtabstr)
   for rec in recs[1:]:
    tab1 = conjstr_listify(rec.conjtabstr)
    for i in xrange(0,len(tab)):
     # ignore duplicate elements
     for t in tab1[i]:
      if t not in tab[i]:
       tab[i].append(t)
     #tab[i] = tab[i] + tab1[i]  # merge two lists
   # now, convert tab back to a string
   self.mergetab = tab
   conjtabstr = conjtab_stringify(tab)
  return (dpidstr,conjtabstr)

def  convert(recs,fileout):
 nok = 0
 nprob = 0
 recs1 = []
 # phase 1, set rec.root for rec in recs, using Verbmap
 for rec in recs:
  # rec is Conjtab object
  key = '.'.join([rec.dproot,rec.gana,rec.number])
  if key not in Verbmap.d:
   print "convert WARNING: Cannot map:",rec.line
   nprob=nprob+1
   continue
  root = Verbmap.d[key].root  # without anubandha
  rec.root = root
  # strip leading '0' from rec.gana, for comparison with Pysanskrit
  if rec.gana.startswith('0'):
   theclass = rec.gana[1:]
  else:
   theclass = rec.gana
  #rec.root = root
  out = "%s %s %s%s:%s" %(root,rec.tense,theclass,rec.pada,rec.conjtabstr)
  nok = nok + 1
 # phase 2.
 # merge items of recs which have the same root, gana, pada.
 # this involves merging the conjtabstr elements and the headers
 # We do this via ConjtabMap objects
 recs1 = []  # list of ConjtabMap objects
 # d is a dictionary of recs1 objects, whose key is 
 #  (root,gana,pada)
 d1 = {}  
 print len(recs),"records in recs"
 for rec in recs:
  key = (rec.root,rec.gana,rec.pada)
  if key not in d1:
   rec1 = ConjtabMap(rec)
   recs1.append(rec1)
   d1[key] = rec1
  else:
   rec1 = d1[key]
   rec1.conjtabrecs.append(rec)
 # Generate output from recs1
 f = open(fileout,"w")
 ndup = 0
 nout = 0
 print len(recs1),"records in recs1"
 for rec1 in recs1:
  (dpid,conjtabstr1) = rec1.merge()
  recs = rec1.conjtabrecs
  rec = recs[0]
  # strip leading '0' from rec.gana, for comparison with Pysanskrit
  if rec.gana.startswith('0'):
   theclass = rec.gana[1:]
  else:
   theclass = rec.gana
  out = "%s %s %s%s:%s" %(rec.root,rec.tense,theclass,rec.pada,conjtabstr1)
  nout = nout+1
  f.write(out + "\n")
  if (len(recs)>1) and (conjtabstr1 != rec.conjtabstr):
   ndup = ndup + 1
   print "duplicate(%s): %s %s%s" %(len(recs),rec.root,theclass,rec.pada)
   print "   mergetab=",rec1.mergetab
   for rec in recs:
    print "   ",rec.line
 f.close()
 print nout,"records written to",fileout
 print nprob,"records skipped"
 print ndup,"True duplicates"

if __name__ == "__main__":
 filein = sys.argv[1] # conj_pre
 filein1 = sys.argv[2] # verbdata_map1
 fileout = sys.argv[3] # conj_pre_map1
 recs = init_conjtab(filein)
 verbmap = init_verbmap(filein1)
 # Verbmap.d is used, not verbmap
 convert(recs,fileout)

