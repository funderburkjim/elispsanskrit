"""prechange.py
   Generate possible changes in JSON file.
   python prechange.py comparea_class_pada.txt  prechange.txt
"""
import sys,codecs,re
import json

class Comparea(object):
 def __init__(self,line,n):
  line = line.rstrip('\r\n')
  self.line = line # the text
  self.n = n # integer line number 
  (huet,mw) = line.split(' # ')
  (self.huetkey,self.huetcpstr) = huet.split(':')
  (self.mwkey,self.mwcpstr) = mw.split(':')

def init_comparea(filein):
 f = codecs.open(filein,mode='r') 
 recs = [] 
 n = 0 # count of lines in file
 for line in f:
  n = n + 1
  recs.append(Comparea(line,n))
 f.close()
 return recs

class Prechg(object):
 def __init__(self,casenum,crec):
  self.type = 'n' # n = huet != mw,  y = huet == mw
  self.status = 'TODO'
  self.casenum=casenum
  self.crec = crec
 def obj(self):
  """ python dictionary object for this record"""
  crec = self.crec
  obj = {
   'case':self.casenum,
   'type':self.type,
   'status':self.status,
   'line':crec.line,
   'huetkey':crec.huetkey,
   'mwkey':crec.mwkey
  }
  return obj

def writechg(crecs,fileout):
 """ serialize crecs in JSON format
 """ 
 fout = codecs.open(fileout,"w","utf-8")
 ncase = 0
 fout.write('[\n');
 outarr = []
 for crec in crecs:
  ncase = ncase + 1
  outrec = Prechg(ncase,crec)
  obj = outrec.obj()
  out = json.dumps(obj)
  outarr.append(out)
 narr=len(outarr)
 for i in xrange(0,narr):
  out = outarr[i]
  fout.write(out + "\n")
  if (i+1)!=narr:
   fout.write(',\n')
 fout.write(']\n')
 fout.close()


if __name__== "__main__":
 filein = sys.argv[1] #comparea_class_pada.txt
 fileout = sys.argv[2]
 # slurp xxxhw2.txt  into Comparea objects
 crecs = init_comparea(filein)
 print len(crecs),"records parsed from",filein
 writechg(crecs,fileout)

