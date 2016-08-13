"""pysan-prim-pas.py
"""
import sys
sys.path.append('../../pysanskrit')
#from test1 import *
#from test2 import *
#from test1 import MyException
from test2 import v_file_init_alt1_pre_helper as pysan_conj
import re,codecs

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
 d = {'pr':'pre','ip':'ipv','op':'opt','im':'ipf'}
 if tense in d:
  return d[tense]
 print "htense_to_slptense ERROR: tense=",tense
 exit(1)

class Input(object):
 def __init__(self,line):
  """ Example inputs
akz im PASSIVE:[Akzyata AkzyetAm Akzyanta AkzyaTAH AkzyeTAm AkzyaDvam Akzye AkzyAvahi AkzyAmahi]
  """
  self.line = line.rstrip('\r\n')
  (self.head,self.tab) = line.split(':')
  (self.stem,self.tense,self.passive) = self.head.split(' ')


def main(filein,fileout):
 empty_conj = ['nil' for i in xrange(0,9)]
 empty_tab = '[' + (' '.join(empty_conj)) + ']' # string form
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
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
  # the pysan code requires a class and voice. 
  # the voice must be 'p' for PASSIVE
  # We by fiat make theclass = 1
  theclass = '1'
  voice = 'p'
  tense = htense_to_slptense(rec.tense)
  dtype = None # or 'c' for causal
  if theclass == '11':
   tab = empty_tab
  else:
   temp = pysan_conj(root,theclass,voice,tense,dtype,dbg=False)
   if temp == None:
    tab = empty_tab
   else:
    parts = temp.split(':')
    tab = parts[-1]
  out = '%s:%s' %(rec.head,tab)
  fout.write('%s\n' % out)
  nout = nout + 1
  continue
  #print "dictstr=",dictstr,"\ndictwords=",dictwords
  for g in genderstr:
   try:
    (key1,key2) = deduce_gender_stem(subanta,words1,g,genderstr,rec.mwtype)
    outputs = s_file_init_alt_helper(key1,g,key2,dbg=dbg)
   except (NameError,MyException) as err:
    print "\ncase=",nin,"line=",line
    print err
    outputs = None
   if isinstance(outputs,list):
    #print "outputs is a list of length",len(outputs)
    pass
   elif outputs == None:
    outputs = []
   elif  ';' in outputs: ##outputs.startswith(':adj'):
    outputs = outputs.split(';')
   else:
    #print "outputs is not a list"
    outputs = [outputs]
   for output in outputs:
    # change form so comparable with huet_noun_tables.txt
    (p0,p1,p2,p3,decl) = output.split(':')
    output1 = "%s %s:%s" %(subanta,g,decl)
    fout.write("%s\n" % output1)
    nout = nout + 1
   if len(outputs) == 0:
    decl0 = ['nil' for i in xrange(0,24)]
    decl = '[' + (' '.join(decl0)) + ']'
    output1 = "%s %s:%s" %(subanta,g,decl)
    fout.write("%s\n" % output1)
    nout = nout + 1
 f.close()
 fout.close()
 print nout,'lines written to',fileout

if __name__ == "__main__":
 filein = sys.argv[1] # huet_conj_tables.txt
 fileout = sys.argv[2] # pysan_conj_tables.txt
 main(filein,fileout)

