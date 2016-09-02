"""check_class10_pada.py
   Sep 1, 2016
   
"""
import sys,codecs
import verbdata

def check(roots):
 # check that pada is 'A' for the class 10 records of the roots 
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
  for rec in rootrecs:
   print root1,rec.gana,rec.pada

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
 check(roots1)
 # check second file
 with codecs.open(filein2,'r','utf-8') as f:
  roots2 = [line.rstrip('\r\n') for line in f]
 print len(roots2),"roots read from",filein2
 check(roots2)

   

