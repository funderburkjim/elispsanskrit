"""pysan_pre.py
   Sep 11, 2016
"""
import sys,re
sys.path.append('../..')  # pysanskritv1 folder
from test2 import v_file_init_alt1_pre_helper as pysan_conj
from test2 import sanskritverb_flag
from test2 import Elisp_serialize
from compare_conj_tables import conjstr_listify

def pada_to_voice(pada):
 if pada == 'P':
  return 'a' # active
 elif pada == 'A':
  return 'm' # middle
 else:
  print "pada_to_voice Error: pada=",pada
  exit(1)

def Unused_htense_to_slptense(tense):
 """ from sl_morph.dtd
<!ELEMENT md (pr | ip | op | im)> <!-- mode is present, imperative, optative or imperfect -->
 """
 d = {'pr':'pre','ip':'ipv','op':'opt','im':'ipf'}
 if tense in d:
  return d[tense]
 print "htense_to_slptense ERROR: tense=",tense
 exit(1)

class Unused_Input(object):
 def __init__(self,line):
  """ Example inputs
akz im 1P:[Akzat AkzatAm Akzan AkzaH Akzatam Akzata Akzam AkzAva AkzAma]

  """
  self.line = line.rstrip('\r\n')
  (self.head,self.tab) = line.split(':')
  (self.stem,self.tense,self.cp) = self.head.split(' ')
  m = re.search(r'^([0-9]+)(.)$',self.cp)
  self.theclass = m.group(1)
  self.pada=m.group(2)

def conjstr_join(tab1str,tab2str):
 """ tab1str, tab2str are in Elisp format
     tablen is length of the tables
     convert the inputs to lists,
     join the lists (element-wise)
     reconstruct lists in Elisp format
 """
 tab1 =  conjstr_listify(tab1str)
 tab2 =  conjstr_listify(tab2str)
 #print "tab1str=",tab1str,"\ntab2str=",tab2str
 #print "tab1=",tab1,"\ntab2=",tab2
 tablen = len(tab1)
 # join the tab1, tab2 lists
 tab=[]
 for i in xrange(0,tablen):
  # assumes each element is a list
  t = tab1[i]+tab2[i]
  tab.append(t)
 #print "tab=",tab
 tabstr = Elisp_serialize(tab,tablen)
 #print "tabstr=",tabstr
 return tabstr

def main(headers,fileout):
 empty_conj = ['nil' for i in xrange(0,9)]
 empty_tab = '[' + (' '.join(empty_conj)) + ']' # string form
 fout = open(fileout,"w")
 nin = 0
 nout=0
 for rec in headers:
  nin = nin + 1
  if nin > 10:
   #print "DEBUG. stop after",nin
   #break
   pass
  root = rec.stem
  theclass = rec.theclass
  voice = pada_to_voice(rec.pada)
  #tense = htense_to_slptense(rec.tense)
  tense = rec.tense
  dtype = None # or 'c' for causal
  temp=None
  if sanskritverb_flag:
   if (root in ["kfRv","Dinv","Sru"]) and (theclass=='1'):
    # These roots are called class 1 in sanskritverb, but their
    # present tense conjugations (at least) are like class 5 roots
    # For kfRv, Dinv see: https://github.com/funderburkjim/elispsanskrit/issues/47#issuecomment-249459765
    # for Sru, see: ?
    dtemp = {'kfRv':'kf','Dinv':'Di','Sru':'Sf'}
    root1 = dtemp[root]
    theclass1 = '5'
    temp = pysan_conj(root1,theclass1,voice,tense,dtype,dbg=False)
   elif (root == 'akz') and (theclass=='1'):
    # SanskritVerb provides a normal class 1 and a class 5 conjugation
    temp1a = pysan_conj(root,theclass,voice,tense,dtype,dbg=False)
    
    temp1 = temp1a.split(':')[-1]
    theclass1 = '5'
    root1 = root
    temp2a = pysan_conj(root1,theclass1,voice,tense,dtype,dbg=False)
    temp2 = temp2a.split(':')[-1]
    # merge the two
    temp3 = conjstr_join(temp1,temp2)
    temp = ":" + temp3 # see split statement below for why the ':'
  if temp == None: # the usual case, no adjustment
   temp = pysan_conj(root,theclass,voice,tense,dtype,dbg=False)
  if temp == None:
   tab = empty_tab
  else:
   parts = temp.split(':')
   tab = parts[-1]
  out = '%s:%s' %(rec.head,tab)
  fout.write('%s\n' % out)
  nout = nout + 1
 fout.close()
 print nout,'lines written to',fileout

class Header(object):
 def __init__(self,line):
  line = line.rstrip('\r\n')
  m = re.search(r'^([^ ]+?) +([^ ]+?) +([0-9]+)([PA])',line)
  if not m:
   print "Header. Invalid format:",line
   exit(1)
  (self.root,self.tense,self.theclass,self.pada) = (m.group(1),m.group(2),
    m.group(3),m.group(4))
  # these for convenience in 'main' routine
  self.head = "%s %s %s%s" %(self.root,self.tense,self.theclass,self.pada)
  self.stem = self.root
def init_headers(filein):
 with open(filein,"r") as f:
  recs = [Header(line) for line in f]
 print len(recs),"records read from",filein
 return recs
if __name__ == "__main__":
 filein = sys.argv[1] # dhaval's conj-pre.txt
 fileout = sys.argv[2] # pysan_conj_tables.txt
 headers=init_headers(filein)
 main(headers,fileout)

