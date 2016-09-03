"""mwvlex1_cp.py
   Aug 31, 2016
   Sep 2, 2016  Use sanverb1_cp.txt as source of sanverb info
    and used mwvlex_cp as source of mw info
   Adjust cp for class 10 to conform with sanverb.
"""
import sys,codecs,re
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

def default_adjust_cps_class10(cpsin):
 """ Assumes cps are sorted.
     If class 10 is present, make sure both padas are present
 """
 # make a copy of cpsin
 cps=[]
 for cp in cpsin:
  cps.append(cp)
 #
 if not cps[-1].startswith('10'):
  # no class 10 information
  return cps
 if len(cps) == 1:
  # could be ['10'] or ['10P'] or ['10A'].
  # in any case change cps to ['10A','10P']
  cps[0] = '10A'
  cps.append('10P')
  return cps
 if (cps[-2],cps[-1]) == ('10A','10P'):
  # no change required. Both padas present in MW
  return cps
 # since we are assuming sorted, 
 # it must be that cps[-2] doesn't start with '10'
 # And we know the last elt does start with '10' (from 1st step above)
 # So, we treat similarly to the len = 1 case above
 cps[-1] = '10A'
 cps.append('10P')
 return cps

def sanverb_override_class10(mwcps,sancps):
 """ replace  class10 info in mwcps with that in sancps.
     Make no change to non-class10 par of mwcps
 """
 cps10 = [cp for cp in mwcps if cp.startswith('10')]
 cpsothr = [cp for cp in mwcps if (not cp.startswith('10'))]
 sancps10 = [cp for cp in sancps if cp.startswith('10')]
 newcps = cpsothr + sancps10 # join the others
 # sort
 newcps.sort()
 return newcps

def write_cprecs(recs,fileout):
 # recs is mwvlexrecs list
 # generate dictionary so can sort on keys
 d={}
 for rec in recs:
  root = rec.root
  try: 
   # convert root from unicode to ascii. 
   # without this, the slp_cmp sorting failed.
   #root = root.decode("ascii")
   root = root.encode('ascii','ignore')
  except UnicodeDecodeError as err:
   print "root is not ascii:",root.encode('utf-8')
   continue
  if root in d:
   print "write_cprecs. Unexpected duplicate",root
  d[root]=rec
 # sort by root
 roots = d.keys()
 roots.sort(cmp=slp_cmp)
 # generate output
 fout = codecs.open(fileout,"w","utf-8")
 for root in roots:
  rec = d[root]
  out = rec.line
  fout.write(out+"\n")
 fout.close()
 print len(roots),"records written to",fileout


def adjust_class10(mwvlexrecs,sanverbrecs):
 # construct dictionary on root spelling in sanverbrecs
 sanverbrecsd = {}
 for rec in sanverbrecs:
  sanverbrecsd[rec.root]=rec
 # selectively modify class-pada information for class 10 roots of mwvlex
 num_mwvlexrecs = len(mwvlexrecs)
 nchg = 0
 for imwvlexrec in xrange(0,num_mwvlexrecs):
  mwvlexrec = mwvlexrecs[imwvlexrec]
  root = mwvlexrec.root
  mwcps10 = [cp for cp in mwvlexrec.cps if cp.startswith('10')]
  if len(mwcps10) == 0:
   # no adjustment
   continue 
  oldcpstr = mwvlexrec.cpstr
  # two cases, root in/not-in sanverbs
  method='None'
  if root in sanverbrecsd:
   sanverbrec = sanverbrecsd[root]
   sancps10 = [cp for cp in sanverbrec.cps if cp.startswith('10')]
   if len(sancps10) != 0:
    newcps = sanverb_override_class10(mwvlexrec.cps,sanverbrec.cps)
    method='sanverb'
   else:
    newcps = default_adjust_cps_class10(mwvlexrec.cps)
    method='default'
  else:
   # root not in sanverbs
   newcps = default_adjust_cps_class10(mwvlexrec.cps)
   method='default'
  newcpstr = ','.join(newcps)
  if newcpstr == oldcpstr:
   # no adjustment required
   continue
  # modify mwvlexrec to use newcps
  newline = "%s:%s" %(root,newcpstr)
  newrec = Verbcp(newline)
  mwvlexrecs[imwvlexrec] = newrec
  print "%s -> %s.  method=%s" %(mwvlexrec.line,newline,method)
  nchg = nchg + 1
 print nchg,"changes in  adjust_class10"
 return mwvlexrecs

def sanverb_override_missing(mwcps,sancps):
 """ A given cp in mwcps has missing data if
     (a) it is '00'  - no class or pada
     (b) it is '00A' or '00P' or
     (c) it is 'xx'  - class but no pada
 """
 cps00 = [cp for cp in mwcps if cp.startswith('00')]
 cpsothr = [cp for cp in mwcps if (not cp.startswith('00'))]
 if (len(cps00) != 0) and (len(cps00) != len(mwcps)):
  # 4 cases
  return cpsothr
 if (len(cps00) != 0):
  # mwcps is all 00 or 00A or 00P
  # replace with sancps
  return sancps
 # Now, we might have case (c) - a naked class
 cpsnaked = [cp for cp in mwcps if (len(cp) == 2)]
 if len(cpsnaked) == 0:
  return mwcps
 cpsothr = [cp for cp in mwcps if(len(cp) == 3) ]  # 01A, etc.
 csothr = [cp[0:2] for cp in cpsothr]  # just the class
 csothr1 = [c for c in csothr if c in cpsnaked]
 assert (len(csothr1) == 0),("Both naked and non-naked class: %s"%mwcps)
 newcps = []
 for cp in cpsnaked:
  # class-pada in sancps that match this class (cp is xx, a class)
  sancps1 = [cp1 for cp1 in sancps if cp1.startswith(cp)]
  newcps = newcps + sancps1
 # now add back in cpsothr
 newcps = newcps + cpsothr
 # sort
 newcps.sort()
 return newcps
 # default - return what we started with
 #return mwcps

def adjust_missingcp(mwvlexrecs,sanverbrecs):
 # construct dictionary on root spelling in sanverbrecs
 sanverbrecsd = {}
 for rec in sanverbrecs:
  sanverbrecsd[rec.root]=rec
 # selectively modify class-pada information for  mwvlex
 num_mwvlexrecs = len(mwvlexrecs)
 nchg = 0
 for imwvlexrec in xrange(0,num_mwvlexrecs):
  mwvlexrec = mwvlexrecs[imwvlexrec]
  root = mwvlexrec.root
  # These modifications only if sanverb has the root
  if not (root in sanverbrecsd):
   continue
  oldcpstr = mwvlexrec.cpstr
  sanverbrec = sanverbrecsd[root]
  newcps = sanverb_override_missing(mwvlexrec.cps,sanverbrec.cps)
  newcpstr = ','.join(newcps)
  if newcpstr == oldcpstr:
   continue
  newline = "%s:%s" %(root,newcpstr)
  newrec = Verbcp(newline)
  mwvlexrecs[imwvlexrec] = newrec
  print "%s -> %s." %(mwvlexrec.line,newline)
  nchg = nchg + 1
 print nchg,"changes in  adjust_missingcp"
 return mwvlexrecs

if __name__ == "__main__":
 filein1 = sys.argv[1] # mwvlex
 filein2 = sys.argv[2] # sanverb1
 fileout = sys.argv[3] # mwvlex1
 mwvlexrecs = init_verbcp(filein1)
 sanverbrecs = init_verbcp(filein2)
 print "-"*60
 print "ADJUSTING class 10 in mwvlex"
 mwvlexrecs1 = adjust_class10(mwvlexrecs,sanverbrecs)
 # next, override any remaining 'missing-data' cases of mwvlexrecs
 print "-"*60
 print "ADJUSTING missing class/pada in mwvlex"
 mwvlexrecs2 = adjust_missingcp(mwvlexrecs,sanverbrecs)
 # write new file
 write_cprecs(mwvlexrecs1,fileout)

