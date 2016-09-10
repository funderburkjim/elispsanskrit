"""verbdata_cp.py
   Aug 28, 2016
   Aggregate class_pada data from verbdata.txt
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
  assert(len(vals)==1),"vals wrong length: %s => %s" %(key,vals)
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
if __name__ == "__main__":
 fileout = sys.argv[1]
 # set relative path to  directory containing verbdata.txt
 path='../function'
 sys.path.append(path) 
 import verbdata
 verbrecs = verbdata.init_Dhaval_verbdata('%s/verbdata.txt'%path)
 # Now verbrecs is a list of Dhaval_verbdata objects.
 # A typical element 'rec' of this list has
 # attributes rec.verbwithanubandha, etc.  
 print len(verbrecs)

 analyze_dproot(verbrecs)
