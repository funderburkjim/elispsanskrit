"""verbdata.py
   Aug 28, 2016
   class Dhaval_verbdata describes the records of verbdata.txt
   and function init_Dhaval_verbdata loads verbdata.txt into
   a list of Dhaval_verbdata objects.
"""
import sys,codecs

class Dhaval_verbdata(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  (self.verbwithanubandha,self.meaning,self.verbwithoutanubandha,self.gana,self.number,self.pada,self.iDAgama,self.pureverb,self.mAdhavIyadhAtuvRtti,self.kzIrataraGgiNI,self.dhAtupradIpa,self.uohydlink,self.jnulink) = line.split(':')

def init_Dhaval_verbdata(filein):
 with codecs.open(filein,"r","utf-8") as f:
  recs = [Dhaval_verbdata(x) for x in f]
 return recs

if __name__ == "__main__":
 filein = sys.argv[1]
 recs = init_Dhaval_verbdata(filein)
 print len(recs),"read from",filein
