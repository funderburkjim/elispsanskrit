"""verbdata_cp.py
   Aug 28, 2016
   Aggregate class_pada data from verbdata.txt
"""
import sys
def analyze_dproot(verbrecs):
 dprootd={} # value is a list of normalized spellings
 for rec in verbrecs:
  dproot = rec.verbwithanubandha
  dpnorm = rec.verbwithoutanubandha
  if dproot not in dprootd:
   dprootd[dproot]=[]
  if dpnorm not in dprootd[dproot]:
   dprootd[dproot].append(dpnorm)
 ndups=0
 for dproot,dpnorms in dprootd.items():
  if len(dpnorms) > 1:
   print "%s:%s" % (dproot,','.join(dpnorms))
   ndups=ndups+1
 print ndups,"duplicate normalized root spellings"
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
