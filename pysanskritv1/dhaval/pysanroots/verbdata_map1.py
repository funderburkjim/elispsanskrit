"""verbdata_map1.py
   Sep 10, 2016
   Sep 18, 2016. Also use roots_a1, as well as roots_a
"""
import sys
#from sansort import slp_cmp
from sansort import trantable as slp_trantable
import string
def sansortkey(key):
 # assume key is as returned by calckey
 # i.e., a triple rootwithanubandha, gana and number, with
 # the two latter as string with 0-padded digits (of length 2 and 4, resp.)
 return string.translate(key[0],slp_trantable) + key[1] + key[2]

def calckey(rec):
 dproot = rec.verbwithanubandha
 dproot1 =  dproot.encode('ascii','ignore') # for sorting
 gana = rec.gana
 number = rec.number
 #return dproot + gana + number
 # return a tuple, for ease of sorting
 return (dproot1 , gana , number) 

def analyze_dproot(verbrecs):
 dprootd={} # value is a list of normalized spellings
 for rec in verbrecs:
  dproot = rec.verbwithanubandha
  gana = rec.gana
  number = rec.number
  dpnorm = rec.verbwithoutanubandha
  key = calckey(rec)
  if key not in dprootd:
   dprootd[key]=[]
  if dpnorm not in dprootd[key]:
   dprootd[key].append(dpnorm)
 ndups=0
 for dproot,dpnorms in dprootd.items():
  if len(dpnorms) > 1:
   print "%s:%s" % (dproot,','.join(dpnorms))
   ndups=ndups+1
 print ndups,"duplicate normalized root spellings"
 keys = dprootd.keys()
 keys.sort(key=sansortkey)
 fout = open(fileout,"w")
 for key in keys:
  (root,gana,number)=key
  vals = dprootd[key]
  # Sep 24, 2016. Changed to allow multiples
  #assert(len(vals)==1),"vals wrong length: %s => %s" %(key,vals)
  if len(vals) != 1:
   print "WARNING: vals wrong length: %s => %s" %(key,vals)
  dpnorm = vals[0]
  out = "%s:%s:%s:%s" %(root,gana,number,dpnorm)
  fout.write(out + "\n")
 fout.close()
 print len(keys),"records written to",fileout
 # just for fun, find the places this mapping is not one-one
 d = {}
 for key in keys:
  vals = dprootd[key]
  assert(len(vals)==1),"vals wrong length: %s => %s" %(key,vals)
  dpnorm = vals[0]
  if dpnorm not in d:
   d[dpnorm]=[]
  if key not in d[dpnorm]:
   d[dpnorm].append(key)
 dpnorms = d.keys()
 def normkey(x):
  y = x.encode('ascii','ignore')
  return string.translate(y,slp_trantable)
 dpnorms.sort(key = normkey)
 ndup=0
 print "DUPLICATES:"
 for dpnorm in dpnorms:
  keys = d[dpnorm]
  if len(keys) == 1:
   continue
  ndup = ndup + 1
  for (root,gana,number) in keys:
   out = "%s:%s:%s:%s" %(root,gana,number,dpnorm)
   print out
 print ndup,"keys map to the same normalized root, as shown above"

class Verbmap(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  (self.dproot,self.gana,self.number,self.dpnorm) = line.split(':')

def init_verbdata_map(filein):
 with open(filein,"r") as f:
  recs = [Verbmap(line) for line in f]
 print len(recs),"records read from",filein
 return recs

class Rootsa(object):
 # for staticmethod, see stackoverflow reference:
 # http://stackoverflow.com/questions/12179271/python-classmethod-and-staticmethod-for-beginner
 @staticmethod
 def parse_root_cp(x):
  (root,cpstr) = x.split(':')
  cps = cpstr.split(',')
  return (root,cps)
 def __init__(self,line):
  """ from pysanskrit1/roots_a.txt.
      line can have two forms:
      Xa#X#Y Example: rasa:10A,10P#ras:01P#ras:01A,01P,10A,10P
      or
      Xa##Y  Example: raca:10A,10P##rac:10P
      
  """
  line = line.rstrip('\r\n')
  (xa,x,y) = line.split('#')
  (self.dpnorma,self.cpsa) = Rootsa.parse_root_cp(xa)
  if x == '':
   self.dpnorm=None
   self.cps = None
  else:
   (self.dpnorm,self.cps) = Rootsa.parse_root_cp(x)
  (self.mwroot,self.mwcps) = Rootsa.parse_root_cp(y)

def init_roots_a(filein):
 with open(filein,"r") as f:
  recs = [Rootsa(line) for line in f]
 print len(recs),"records read from",filein
 return recs

def update_rec(rec,rootsa):
 # for convenience, add an 'mwroot' field to 'rec'. Initially assume
 # it is the same as dpnorm
 rec.mwroot = rec.dpnorm
 for r in rootsa:
  if rec.dproot == r.dpnorma:
   # change mwroot in rec to its value in r
   rec.mwroot = r.mwroot

def update_recs(recs,rootsa):
 for rec in recs:
  update_rec(rec,rootsa)
if __name__ == "__main__":
 filein = sys.argv[1] # verbdata_map
 filein1 = sys.argv[2] # roots_a
 filein2 = sys.argv[3] # roots_a1
 fileout = sys.argv[4]
 recs = init_verbdata_map(filein)
 rootsa0 = init_roots_a(filein1)
 rootsa1 = init_roots_a(filein2)
 rootsa = rootsa0 + rootsa1
 update_recs(recs,rootsa)
 nchg = 0
 n = 0
 with open(fileout,"w") as f:
  for rec in recs:
   out = "%s:%s:%s:%s" %(rec.dproot,rec.gana,rec.number,rec.mwroot)
   n = n + 1
   if rec.mwroot != rec.dpnorm:
    # for debug
    #out = out + ' CHG'
    nchg = nchg + 1
    print out + "  CHG" # to stdout, for log of changes
   f.write(out + "\n")
 print len(recs),"records written to",fileout
 print nchg,"records changed the corresponding 'normalized' root"

