"""verbdata_cp.py
   Aug 28, 2016
   Identify records of verbdata.txt that have
   verbwithanubandha associated with more than 1 verbwithoutanubandha
   
"""
import sys,codecs
def dproot_dup(verbrecs,fileout):
 fout = codecs.open(fileout,"w","utf-8")
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
   out = "%s:%s" % (dproot,','.join(dpnorms))
   fout.write(out + "\n")
   ndups=ndups+1
 print ndups,"duplicate normalized root spellings written to",fileout

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
 dproot_dup(verbrecs,fileout)

