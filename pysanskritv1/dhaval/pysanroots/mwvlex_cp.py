"""mwvlex_cp.py
   Aug 30, 2016
   Generate list of root + class + pada from 
   MWvlex/step1/verb_cp.txt
"""
import sys,codecs,re
from sansort import slp_cmp

class Mwvlex(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.root,self.L,self.cpstr)=line.split(':')
  self.cps = self.cpstr.split(',')

def init_mwvlex(filein):
 with codecs.open(filein,"r","utf-8") as f:
  mwvlexrecs = [Mwvlex(line) for line in f]
 print len(mwvlexrecs),"records from",filein
 return mwvlexrecs

def normalize_cp(cp):
 m = re.search(r'^([0-9]+)([AP]?)$',cp)
 c = m.group(1)
 p = m.group(2) # could be empty
 ic = int(c)
 c1 = '%02d'%ic  # 0-pad the class to 2 digits
 cp1 = c1 + p
 return cp1

def adjust_cps(cps):
 # we assume cps is already sorted
 # each cp starts with 2 digits.
 # if 00 is an element in cps, then it is the first one, by sorting.
 # remove '00' unless this is the only one
 if cps == ['00']:
  return cps
 # remove any class-without-pada, if there is the same class-with-pada
 # we know by construction that there are no duplicates in cps
 ans=[]
 n = len(cps)
 for i in xrange(0,n-1):
  cp = cps[i]
  if cp == '00':
   # drop it. We know (since cps!=['00'] from above there are otherss
   continue
  if len(cp) == 2:  # cp is  class-without-pada
   if cps[i+1].startswith(cp):
    continue  # drop cp
  # keep cp
  ans.append(cp)
 # get the last item of cps
 ans.append(cps[-1])
 return ans

def  write_cprecs(recs,fileout):
 # recs in mwvlexrecs list
 print "generate_cp:",len(recs),"class pada records"
 #aggregate by root, remove duplicates
 d={}
 for rec in recs:
  root = rec.root
  cps = rec.cps
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
  for cp in cps:
   try:
    cp1 = normalize_cp(cp)
   except:
    print "normalize error: '%s'. Line=%s"%(cp,rec.line)
    continue
   if cp1 not in d[root]:
    d[root].append(cp1)
 # sort by root
 roots = d.keys()
 roots.sort(cmp=slp_cmp)
 # generate output
 fout = codecs.open(fileout,"w","utf-8")
 for root in roots:
  cps0 = d[root]
  # sort alphabetically
  cps0.sort()
  # further adjustment of the class-pada list
  cpstr0 = ','.join(cps0)
  cps = adjust_cps(cps0)
  # turn into comma-delimited string
  cpstr = ','.join(cps)
  out = "%s:%s" %(root,cpstr)
  if False and (cpstr != cpstr0): #dbg
   out = '%s#%s' %(out,cpstr0)
  fout.write(out+"\n")
 fout.close()
 print len(roots),"records written to",fileout

if __name__ == "__main__":
 filein = sys.argv[1]
 fileout = sys.argv[2]
 mwvlexrecs = init_mwvlex(filein)
 write_cprecs(mwvlexrecs,fileout)
