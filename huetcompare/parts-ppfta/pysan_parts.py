"""pysan-parts.py
   08-16-2016
   for fap
"""
import sys
sys.path.append('../../pysanskrit')
#from test1 import *
#from test2 import *
#from test1 import MyException
#from test2 import v_file_init_alt1_pre_helper as pysan_conj
import re,codecs
from compare_decl_tables import declstr_listify
from huet_parts import prettify

def pada_to_voice(pada):
 if pada == 'P':
  return 'a' # active
 elif pada == 'A':
  return 'm' # middle
 else:
  print "pada_to_voice Error: pada=",pada
  exit(1)

def htense_to_slptense(tense):
 """ from sl_morph.dtd
<!ELEMENT md (pr | ip | op | im)> <!-- mode is present, imperative, optative or imperfect -->
 """
 d = {'pr':'pre', #present
      'ip':'ipv', #imperative
      'op':'opt', #optative
      'im':'ipf', # imperfect
      'fut':'fut',# future
      'prf':'prf',# perfect
     'aor':'aor', # aorist (there is a 'kind' number here)
     'inj':'NONE', # injunctive (aorist without augment) had kind 
     'cnd':'con', # conditional
     'ben':'ben', # benedictive
     'pef':'pft', # periphrastic future
 }
 if tense in d:
  return d[tense]
 print "htense_to_slptense ERROR: tense=",tense
 exit(1)

class Input(object):
 def __init__(self,line):
  """ Example input from huet_decl_tables_....txt
aMSayizyat pfut Pf:[aMSayizyantI aMSayizyantyO aMSayizyantyaH aMSayizyantIm aMSayizyantyO aMSayizyantIH aMSayizyantyA aMSayizyantIByAm aMSayizyantIBiH aMSayizyantyE aMSayizyantIByAm aMSayizyantIByaH aMSayizyantyAH aMSayizyantIByAm aMSayizyantIByaH aMSayizyantyAH aMSayizyantyoH aMSayizyantInAm aMSayizyantyAm aMSayizyantyoH aMSayizyantIzu nil nil nil]
  """
  self.line = line.rstrip('\r\n')
  (self.head,self.tab) = self.line.split(':')
  (self.stem,self.partcode,self.cpm) = self.head.split(' ')


def main(filein,fileout,pysandict):
 empty_decl = ['nil' for i in xrange(0,24)]
 empty_tab = '[' + (' '.join(empty_decl)) + ']' # string form
 f = codecs.open(filein,"r","utf-8") # huet declensions
 fout = codecs.open(fileout,"w","utf-8") #pysan declensions
 nin = 0
 nout=0
 for line in f:
  line = line.rstrip('\r\n')
  nin = nin + 1
  if line == '':
   continue
  if nin > 10:
   #print "DEBUG. stop after",nin
   #break
   pass
  rec = Input(line)
  stem = rec.stem
  cpm = rec.cpm # class,pada,gender (class is empty string)
  if stem not in pysandict:
   tab = empty_tab
  else:
   pysanrec = pysandict[stem]
   if cpm not in pysanrec.tables:
    tab = empty_tab
   else:
    tab = pysanrec.tables[cpm]
  out = '%s:%s' %(rec.head,tab)
  fout.write('%s\n' % out)
  nout = nout + 1
 f.close()
 fout.close()
 print nout,'lines written to',fileout

class Pysan_part(object):
 def __init__(self,stem):
  self.stem=stem
  self.cpms = [] # list of (class,pada,gender) strings (10Pf, for instance)
  self.tables={}  # key is a (class,pada,gender)
  # each table is a list of 24 values (1s...8p), and each value is a list of 
  # forms

 def update(self,cpm,tabstr):
  if cpm not in self.cpms:
   self.cpms.append(cpm)
   self.tables[cpm] = tabstr
  else:
   # handle duplicates
   toldstr = self.tables[cpm]
   told = declstr_listify(toldstr)
   tnew = declstr_listify(tabstr)
   for i in xrange(0,len(told)):
    old = told[i] # a list of strings
    new = tnew[i] # ditto
    for x in new:
     if x not in old:
      old.append(x)
    told[i] = old
   self.tables[cpm] = prettify(told)

def pysan_parse_feminine(key1,tabstr):
 """ Example:
:fap aMS-10 f:(aMSApayizyatI aMSApayizyantI):(aMSApayizyatI aMSApayizyantI):[(aMSApayizyatI aMSApayizyantI) (aMSApayizyatyO aMSApayizyantyO) (aMSApayizyatyaH aMSApayizyantyaH) (aMSApayizyatIm aMSApayizyantIm) (aMSApayizyatyO aMSApayizyantyO) (aMSApayizyatIH aMSApayizyantIH) (aMSApayizyatyA aMSApayizyantyA) (aMSApayizyatIByAm aMSApayizyantIByAm) (aMSApayizyatIBiH aMSApayizyantIBiH) (aMSApayizyatyE aMSApayizyantyE) (aMSApayizyatIByAm aMSApayizyantIByAm) (aMSApayizyatIByaH aMSApayizyantIByaH) (aMSApayizyatyAH aMSApayizyantyAH) (aMSApayizyatIByAm aMSApayizyantIByAm) (aMSApayizyatIByaH aMSApayizyantIByaH) (aMSApayizyatyAH aMSApayizyantyAH) (aMSApayizyatyoH aMSApayizyantyoH) (aMSApayizyatInAm aMSApayizyantInAm) (aMSApayizyatyAm aMSApayizyantyAm) (aMSApayizyatyoH aMSApayizyantyoH) (aMSApayizyatIzu aMSApayizyantIzu) (aMSApayizyati aMSApayizyanti) (aMSApayizyatyO aMSApayizyantyO) (aMSApayizyatyaH aMSApayizyantyaH)]
 """
 stems = key1[1:-1].split(' ')  # drop initial '(' and final ')'
 stems.sort() # since declstr_listify sorts (see below)
 if len(stems) != 2:
  print "pysan_parse_feminine error 0:%s"%key1
  exit(1)
 (stem1,stem2) = stems
 assert (stem1.endswith('ntI') and stem2.endswith('tI')),"pysan_parse_feminine error 1:%s"%key1
 s1 = stem1[0:-1] # drop final 'I'
 s2 = stem2[0:-1]
 tabold = declstr_listify(tabstr)
 tab1=[]
 tab2=[]
 for t in tabold:
  t = list(t)
  t.sort()
  (f1,f2)=t
  assert (f1.startswith(s1) and f2.startswith(s2)),"pysan_parse_feminine error 2:(%s,%s)" %(f1,f2)
  tab1.append([f1])
  tab2.append([f2])
 # convert back to strings
 tab1str = prettify(tab1)
 tab2str = prettify(tab2)
 """
 print "stem1=%s,\ntab1=%s\ntab1str=%s"%(stem1,tab1,tab1str)
 print "stem2=%s,\ntab2=%s\ntab2str=%s"%(stem2,tab2,tab2str)
 exit(1)
 """
 ans=[(s2,tab1str),(s2,tab2str)]
 #print ans
 #exit(1)
 return ans

def pysan_parse(line):
 """ in cases of class 6, there are two feminine stems.
     That's what we return a list of (stem,tab) tuples. 
     See pysan_parse_feminine above for more information
 """
 line = line.rstrip('\r\n')
 (empty,head,key1,key2,tabstr) = line.split(':')
 (partcode,rootclass,gender) = head.split(' ')
 (root,theclass) = rootclass.split('-')
 assert (gender in ['m','f','n']), "pysan_parse gender error: %s"%line
 if gender in ['m','n']:
  stemtabs = [(key1,tabstr)]
 elif key1.endswith('ntI'):
  stem = key1[0:-3] + 't' # ntI -> t
  stemtabs = [(stem,tabstr)]
 elif key1.endswith('tI'):
  stem = key1[0:-1] # drop final I
  stemtabs = [(stem,tabstr)]
 else:
  stemtabs = pysan_parse_feminine(key1,tabstr)
 return (theclass,gender,stemtabs)

def init_pysan(pada,pysanfile):
 stemdict={}
 stemlist=[]
 with codecs.open(pysanfile,"r","utf-8") as f:
  for line in f:
   (theclass,gender,stemtabs) = pysan_parse(line)
   theclass = '' # Huet doesn't show class, so we won't either.
   for (stem,tab) in stemtabs:
    if stem not in stemdict:
     stemdict[stem]=Pysan_part(stem)
     stemlist.append(stem)
    rec = stemdict[stem]
    cpm = "%s%s%s" %(theclass,pada,gender)
    rec.update(cpm,tab)
 if False: # dbg
  for i in xrange(0,2):
   stem = stemlist[i]
   rec = stemdict[stem] # a  Pysan_part object
   print i,"stem=%s,cpms=%s"%(stem,rec.cpms)
   for cpm in rec.cpms:
    print "decl %s = %s"%(cpm,rec.tables[cpm])
  exit(1)
 return stemdict

if __name__ == "__main__":
 sfx = sys.argv[1]
 pysanfile = sys.argv[2]
 padad = {'fap':'P','fmp':'A'}
 pada = padad[sfx]
 pysandict = init_pysan(pada,pysanfile)
 print len(pysandict)," records read from",pysanfile
 #exit(1)
 filein = "huet_decl_tables_%s.txt" % sfx
 fileout = "pysan_decl_tables_%s.txt" % sfx
 main(filein,fileout,pysandict)

