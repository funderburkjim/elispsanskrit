"""sanverb1_cp.py
   Sep 2, 2016
"""
import codecs,sys
from sansort import slp_cmp

class Verbcp(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.root,self.cpstr)=line.split(':')
  self.cps = self.cpstr.split(',')

def init_verbcp(filein):
 with codecs.open(filein,"r","utf-8") as f:
  verbcprecs = [Verbcp(line) for line in f]
 print len(verbcprecs),"records from",filein
 return verbcprecs

class Rootsa(object):
 def __init__(self,sanverba,sanverb,mwverb):
  self.sanverba = sanverba
  self.sanverb = sanverb
  self.mwverb = mwverb
 
def init_roots_a(filein):
 recs=[]
 with codecs.open(filein,"r","utf-8") as f:
  for line in f:
   (sanverba_str,sanverb_str,mwverb_str) = line.split('#')
   sanverba=Verbcp(sanverba_str)
   mwverb = Verbcp(mwverb_str)
   if sanverb_str == '':
    sanverb = None
   else:
    sanverb=Verbcp(sanverb_str)
   rec = Rootsa(sanverba,sanverb,mwverb)
   recs.append(rec)
 print len(recs),"records from",filein1
 return recs

def editrec(recs,arecs):
 """ recs is the list of sanverb records (Verbcp objects) from sanverb_cp,
      whose root attribute is derived from the verbwithoutanubandha field
      of verbdata.
     arecs record is a list of three Verbcp objects,
      first is sanverb root ending in 'a'
      second is either None OR a sanverb root with 'a' dropped
      third is mwvlex root with 'a' dropped.
     We want to modify the recs list so that the first and second 
      arecs elements are merged and have the root spelled without 'a'
 
 """
 # Let d be dictionary on arecs, with a key for all first and second roots
 d = {}
 for arec in arecs:
  # sanverba root
  root = arec.sanverba.root
  if root in d:
   print "editrec Unexpected duplicate",root
  d[root]=arec
  # sanverb root, if present
  if not arec.sanverb:
   continue
  root = arec.sanverb.root
  if root in d:
   print "editrec Unexpected duplicate 1",root
  d[root]=arec
 print len(d.keys()), "combined roots in arecs"
 # step2. Generate modified recs
 recs1 = []
 for rec in recs:
  root = rec.root
  if root not in d:
   #usual case. just copy rec into recs1
   recs1.append(rec)
   continue
  # root mentioned in arecs. 
  arec = d[root]
  sanverba = arec.sanverba
  sanverb = arec.sanverb
  mwverb = arec.mwverb
  # if root does NOT match sanverba, we'll ignore rec
  if not (root == sanverba.root):
   continue
  # root matches sanverba.  We'll merge sanverba and sanverb cps,
  # and use the 'without a' root
  newroot = mwverb.root
  newcps = sanverba.cps
  if sanverb:
   for cp in sanverb.cps:
    if cp not in newcps:
     newcps.append(cp)
  newcps.sort()
  # construct string form, so we can make a Verbcp object
  newcpstr = ','.join(newcps)
  line = "%s:%s" %(newroot,newcpstr)
  #print "NEW RECORD:",line
  rec1 = Verbcp(line)
  recs1.append(rec1)
 print len(recs1),"modified records"
 return recs1


def write_cprecs(fileout,recs,arecs):
 newrecs = editrec(recs,arecs)
 # make dictionary, so can sort on keys
 d={}
 for rec in newrecs:
  root = rec.root
  root = root.encode('ascii','ignore')  
  if root in d:
   print "unexpected dup in newrecs:",root
  d[root]=rec
 roots = d.keys()
 roots.sort(cmp=slp_cmp)
 # generate output
 fout = codecs.open(fileout,"w","utf-8")
 for root in roots:
  rec = d[root]
  fout.write(rec.line + "\n")
 fout.close()
 print len(roots),"records written to",fileout
if __name__ == "__main__":
 filein1 = sys.argv[1]
 filein2 = sys.argv[2]
 fileout = sys.argv[3]
 sanverbrecs = init_verbcp(filein1)
 arecs = init_roots_a(filein2)
 write_cprecs(fileout,sanverbrecs,arecs)
