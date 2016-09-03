"""check_class10_pada.py
   Sep 1, 2016
   
"""
import sys,codecs
import verbdata

def get_class_padas(recs):
 """ recs is a list of verbdata objects
 """
 cps = []
 for rec in recs:
  c = rec.gana
  p = rec.pada
  if p == 'pa':
   p = 'P'
  cp = c + p
  cps.append(cp)
 cps.sort()
 return cps

def check(roots,d):
 # check that pada is 'A' for the class 10 records of the roots 
 nok = 0
 for root in roots:
  if root in d:
   root1 = root
  else:
   if root.endswith('!'):
    root1 = root[0:-1] # remove the '!' anubandha
   if root1 not in d:
    print root,"WARNING not in verbdata"
    continue
   else:
    print root,"replaced with",root1
  rootrecs=d[root1]
  cps = get_class_padas(rootrecs)
  cpstr = ','.join(cps)
  cps10 = [cp for cp in cps if cp.startswith('10')]
  if cps10 == ['10A']:
   status = 'OK'
   nok = nok + 1
  elif cps10 == []:
   status = 'PROB:no class 10'
  else:
   status = '?'
  if status != 'OK':
   print "%s:%s:%s"%(root1,cpstr,status)
 print nok,"records are OK  (have exactly 1 class 10, and it is 'A')"
  

if __name__ == "__main__":
 filein = sys.argv[1]
 filein1 = sys.argv[2]
 filein2 = sys.argv[3]
 recs = verbdata.init_Dhaval_verbdata(filein)
 print len(recs),"read from",filein
 # make a dictionary from verbdata, using verbwithanubandha
 d = {}
 for rec in recs:
  root = rec.verbwithanubandha
  if root not in d:
   d[root]=[]
  d[root].append(rec)
 # check first file
 with codecs.open(filein1,'r','utf-8') as f:
  roots1 = [line.rstrip('\r\n') for line in f]
 print len(roots1),"roots read from",filein1
 check(roots1,d)
 # check second file
 with codecs.open(filein2,'r','utf-8') as f:
  roots2 = [line.rstrip('\r\n') for line in f]
 print len(roots2),"roots read from",filein2
 check(roots2,d)

   

