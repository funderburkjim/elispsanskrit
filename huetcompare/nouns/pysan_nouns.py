"""pydecl.py
"""
import sys
sys.path.append('../../pysanskrit')
#from test1 import *
#from test2 import *
from test1 import MyException
from test1 import s_file_init_alt_helper
import re,codecs

class Input(object):
 def __init__(self,line):
  """ Example inputs
aMhIyas:fmn;;NA;;NA
agni:m;;m;;noun;;agni
agnicit:fmn;;fmn;;adj;;agni-cit
agnimaya:fmn;;fmn;;adjI;;agni-maya
  """
  self.line = line.rstrip('\r\n')
  parts = line.split(';;')
  if len(parts) == 4:
   (hparm,self.mwgenderstr,self.mwtype,self.mwkey2) = parts
  elif len(parts) == 3:
   hparm = parts[0]
   (self.mwgenderstr,self.mwtype,self.mwkey2) = (None,None,None)
  (self.hstem,self.hgenderstr) = hparm.split(':')
  if self.mwkey2 == None:
   self.mwkey2 = self.hstem

def deduce_gender_stem(key1,key2,g,hgenders,mwtype):
 """ Alter key1,key2
   hgenders and mwgenders are strings
   mwgenders may have special value 'adjI'
 """
 if g != 'f':
  return (key1,key2) #  no change
 if (mwtype == 'adjI') and (key1.endswith('a')):
  key1 = key1[0:-1] + 'I'
  key2 = key2[0:-1] + 'I'
  return (key1,key2)
 if key1.endswith('a'):
  key1 = key1[0:-1] + 'A'
  key2 = key2[0:-1] + 'A'
  return (key1,key2)
 return (key1,key2) #  no change
 
def main(filein,fileout):
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
  subanta = rec.hstem
  genderstr = rec.hgenderstr
  words1 = rec.mwkey2 # gaRi-mat
  dbg=False
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

    #key1=subanta
    #key2 = words1
    #print "Warning",key1,key2,":",g
 f.close()
 fout.close()
 print nout,'lines written to',fileout

if __name__ == "__main__":
 filein = sys.argv[1] # huet_mwtest.txt
 fileout = sys.argv[2] # compare_noun.txt
 main(filein,fileout)

