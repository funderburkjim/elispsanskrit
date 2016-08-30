"""sanverb_cp.py
   Aug 29, 2016
   Generate list of root + class + pada from SanskritVerb data
"""
import sys,codecs
from sansort import slp_cmp

def init_verbdata(verbdata_dir):
 # set relative path to  directory containing verbdata.txt
 sys.path.append(verbdata_dir) 
 import verbdata
 filein = '%s/verbdata.txt'%verbdata_dir
 verbrecs = verbdata.init_Dhaval_verbdata(filein)
 # Now verbrecs is a list of Dhaval_verbdata objects.
 # A typical element 'rec' of this list has
 # attributes rec.verbwithanubandha, etc.  
 print len(verbrecs),"records from",filein
 return verbrecs

class Sanverb_Conjtab(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.header,self.tabstr) = line.split(':')
  (self.dproot,self.tense,self.gnp) = self.header.split(' ')
  (self.gana,np) = self.gnp.split('.')
  self.number = np[0:-1]
  self.pada = np[-1:]
  assert (self.pada in 'AP'),("Sanverb_Conjtab: bad pada (%s): %s" %(self.pada,self.line))

def init_conjtab(filein):
 with codecs.open(filein,"r","utf-8") as f:
  recs = [Sanverb_Conjtab(line) for line in f]
 print len(recs),"records from",filein
 return recs

def make_verbrecd(recs):
 d = {}
 ndup=0
 for rec in recs:
  # include pada field (pa,A,u) also
  p = rec.pada
  assert (p in ['pa','A','u']),("Unexpected verbrec pada=%s"%p)
  if p == 'pa':
   padas = ['P']
  elif p == 'A':
   padas = ['A']
  else: #both 'u'
   padas = ['U']
  pada = padas[0]
  keyparts = (rec.verbwithanubandha,rec.gana,rec.number,pada)
  key =  '.'.join(keyparts)
  if key in d:
   ndup=ndup+1
  else:
   d[key]=[]
  d[key].append(rec)
 print "make_verbrecd:",ndup,"duplicates found"
 icase=0
 for key,val in d.items():
  if len(val) > 1:
   icase=icase+1
   print "case",icase,"of duplicate verbdata key:",key
   for rec in val:
    print "  ",rec.line.encode('utf-8')
 return d

def  write_cprecs(recs,fileout):
 print "generate_cp:",len(recs),"class pada records"
 #aggregate by root, remove duplicates
 d={}
 for (root,cp) in recs:
  try: 
   # convert root from unicode to ascii. 
   # without this, the slp_cmp sorting failed.
   #root = root.decode("ascii")
   root = root.encode('ascii','ignore')
  except UnicodeDecodeError as err:
   print "root is not ascii:",root.encode('utf-8')
   continue
  if root not in d:
   d[root]=[]
  if cp not in d[root]:
   d[root].append(cp)
 # sort by root
 roots = d.keys()
 roots.sort(cmp=slp_cmp)
 # generate output
 fout = codecs.open(fileout,"w","utf-8")
 for root in roots:
  cps = d[root]
  # sort alphabetically
  cps.sort()
  # turn into comma-delimited string
  cpstr = ','.join(cps)
  out = "%s:%s" %(root,cpstr)
  fout.write(out+"\n")
 fout.close()
 print len(roots),"records written to",fileout

def generate_cp(verbrecs,conjrecs,fileout):
 verbrecd = make_verbrecd(verbrecs)
 recs = []
 for conjrec in conjrecs:
  # conjrec.pada is P or A
  keyparts = (conjrec.dproot,conjrec.gana,conjrec.number,conjrec.pada)
  key = '.'.join(keyparts)
  if key not in verbrecd:
   key0 = key
   # maybe its ubhayapada in verbrecs
   keyparts = (conjrec.dproot,conjrec.gana,conjrec.number,'U')
   key = '.'.join(keyparts)
   if key not in verbrecd:
    print "generate_cp: key not found",key0
    continue
  verbreclist = verbrecd[key]
  for verbrec in verbreclist:
   """
   p = verbrec.pada
   assert (p in ['pa','A','u']),("Unexpected verbrec pada=%s"%p)
   if p == 'pa':
    padas = ['P']
   elif p == 'A':
    padas = ['A']
   else: #both
    padas = ['A','P']
   """
   root=verbrec.verbwithoutanubandha
   gana=verbrec.gana # 
   assert gana == conjrec.gana,"gana problem"
   # use the pada in conjrec
   padas = conjrec.pada
   for pada in padas:
    cp = gana+pada
    recs.append((root,cp))
 write_cprecs(recs,fileout)

def generate_cp1(verbrecs,fileout):
 #verbrecd = make_verbrecd(verbrecs)
 recs = []
 for verbrec in verbrecs:
  p = verbrec.pada
  assert (p in ['pa','A','u']),("Unexpected verbrec pada=%s"%p)
  if p == 'pa':
   padas = ['P']
  elif p == 'A':
   padas = ['A']
  else: #both
   padas = ['A','P']
  root=verbrec.verbwithoutanubandha
  gana=verbrec.gana
  for pada in padas:
   cp = gana+pada
   recs.append((root,cp))
 write_cprecs(recs,fileout)
 #print "generate_cp1:",len(recs),"class pada records"

from sansort import *
def test():
 a = u'stuc'
 a1 =  string.translate(a,trantable)
 print "'%s' -> '%s'"%(a,a1)
 exit(1)
if __name__ == "__main__":
 #test()
 verbdata_dir = sys.argv[1]
 conjtabfile = sys.argv[2]
 fileout = sys.argv[3]
 fileout1 = sys.argv[4]
 verbrecs = init_verbdata(verbdata_dir)
 conjrecs = init_conjtab(conjtabfile)
 generate_cp(verbrecs,conjrecs,fileout)
 generate_cp1(verbrecs,fileout1)
