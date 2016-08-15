"""pysan-prim-pas.py
"""
import sys
sys.path.append('../../pysanskrit')
#from test1 import *
#from test2 import *
#from test1 import MyException
#from test2 import v_file_init_alt1_pre_helper as pysan_conj
import re,codecs
from compare_conj_tables import conjstr_listify
from verbs_tp import prettify

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
  """ Example inputs
aMSa fut A:[aMSayizyate aMSayizyete aMSayizyante aMSayizyase aMSayizyeTe aMSayizyaDve aMSayizye aMSayizyAvahe aMSayizyAmahe]
  """
  self.line = line.rstrip('\r\n')
  (self.head,self.tab) = line.split(':')
  (self.stem,self.tense,self.pada) = self.head.split(' ')


def main(filein,fileout,pysandict):
 empty_conj = ['nil' for i in xrange(0,9)]
 empty_tab = '[' + (' '.join(empty_conj)) + ']' # string form
 f = codecs.open(filein,"r","utf-8") # huet conjugations
 fout = codecs.open(fileout,"w","utf-8") #pysan conjugations
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
  root = rec.stem
  pada = rec.pada
  # the pysan code requires a class and voice. 
  # We by fiat make theclass = 1
  if root not in pysandict:
   tab = empty_tab
  else:
   pysanrec = pysandict[root]
   if pada not in pysanrec.tables:
    tab = empty_tab
   else:
    tab = pysanrec.tables[pada]
  out = '%s:%s' %(rec.head,tab)
  fout.write('%s\n' % out)
  nout = nout + 1
 f.close()
 fout.close()
 print nout,'lines written to',fileout

class Pysan_verb(object):
 def __init__(self,stem):
  self.stem=stem
  self.padas = [] # 
  self.tables={}  # key is a pada
  # each table is a list of 9 values (3s...1p), and each value is a list of 
  # forms

 def update_pada(self,pada,tabstr):
  if pada not in self.padas:
   self.padas.append(pada)
   self.tables[pada] = tabstr
  else:
   # handle duplicates
   toldstr = self.tables[pada]
   told = conjstr_listify(toldstr)
   tnew = conjstr_listify(tabstr)
   for i in xrange(0,len(told)):
    old = told[i] # a list of strings
    new = tnew[i] # ditto
    for x in new:
     if x not in old:
      old.append(x)
    told[i] = old
   self.tables[pada] = prettify(told)
   #print "update_pada: Duplicate pada:",self.stem,pada,tab

def pysan_parse(line):
 line = line.rstrip('\r\n')
 (empty,head,tabstr) = line.split(':')
 (stem,tense,cp) = head.split(' ')
 theclass = cp[0:-1]
 voice = cp[-1]
 voice_to_pada = {'a':'P','m':'A'}
 pada = voice_to_pada[voice]
 return (stem,tense,theclass,pada,tabstr)

def init_pysan(htense,pysanfile):
 sltense = htense_to_slptense(htense)
 stemdict={}
 stemlist=[]
 with codecs.open(pysanfile,"r","utf-8") as f:
  for line in f:
   (stem,tense,theclass,pada,tab) = pysan_parse(line)
   if tense != sltense:
    # MW-verb-fut.txt has conjugations for several tenses.
    continue
   if stem not in stemdict:
    stemdict[stem]=Pysan_verb(stem)
    stemlist.append(stem)
   rec = stemdict[stem]
   rec.update_pada(pada,tab)
 return stemdict

if __name__ == "__main__":
 htense = sys.argv[1]
 pysanfile = sys.argv[2]
 pysandict = init_pysan(htense,pysanfile)
 print len(pysandict)," records read from",pysanfile
 filein = "huet_conj_tables_%s.txt" % htense
 fileout = "pysan_conj_tables_%s.txt" % htense
 main(filein,fileout,pysandict)

