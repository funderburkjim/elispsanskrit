"""conjmap.py
   Sep 10, 2016
"""
import sys,re

class Conjtab(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.header,self.conjtabstr) = line.split(':')
  (self.root,self.tense,self.cpstr) = self.header.split(' ')
  m = re.search(r'^(..)[.](....)(.)$',self.cpstr)
  if not m:
   print "Conjtab error parsing:",line
   exit(1)
  (self.gana,self.number,self.pada) = (m.group(1),m.group(2),m.group(3))

def init_conjtab(filein):
 with open(filein,"r") as f:
  recs = [Conjtab(line) for line in f]
 print len(recs),"records read from",filein
 return recs

class Verbmap(object):
 d = {}
 def __init__(self,line):
  line = line.rstrip('\r\n')
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

def  convert(recs,verbmap,fileout):
 """ doesn't used verbmap, but uses Verbmap.d global object
 """
 f = open(fileout,"w")
 nok = 0
 nprob = 0
 for rec in recs:
  # rec is Conjtab object
  key = '.'.join([rec.root,rec.gana,rec.number])
  if key not in Verbmap.d:
   print "convert WARNING: Cannot map:",rec.line
   nprob=nprob+1
   continue
  mwroot = Verbmap.d[key].root
  # strip leading '0' from rec.gana, for comparison with Pysanskrit
  if rec.gana.startswith('0'):
   theclass = rec.gana[1:]
  else:
   theclass = rec.gana
  out = "%s %s %s%s:%s" %(mwroot,rec.tense,theclass,rec.pada,rec.conjtabstr)
  f.write(out + "\n")
  nok = nok + 1
 f.close()
 print nok,"records written to",fileout
 print nprob,"records skipped"

if __name__ == "__main__":
 filein = sys.argv[1] # conj_pre
 filein1 = sys.argv[2] # verbdata_map1
 fileout = sys.argv[3] # conj_pre_map1
 recs = init_conjtab(filein)
 verbmap = init_verbmap(filein1)
 convert(recs,verbmap,fileout)

