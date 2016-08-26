""" test_s_file_init_alt1.py
  Python version of Elisp s-file-init-alt1 in forms.el
  June 26, 2016
  python test_s_file_init_alt1.py MW-noun.txt ../../elispsanskrit/grammar/prod/inputs MW-noun-01.txt prodchk/outputs 1 9999

  python test_s_file_init_alt1.py MW-noun.txt ../../elispsanskrit/grammar/prod/inputs MW-noun-01.txt prodchk/outputs 1 9999 > temp1

  python test_s_file_init_alt1.py MW-noun.txt ../../elispsanskrit/grammar/prod/inputs temp.txt prodchk/outputs 605 605


diff -w prodchk/outputs/MW-noun-01.txt ../../elispsanskrit/grammar/prod/outputs/MW-noun-01.txt > temp
"""
#from test1 import s_file_init_alt_helper,MyException
from test1 import MyException
from test1 import s_file_init_alt_helper
import sys,re,codecs


def s_file_init_alt1(intab,indir,outtab,outdir,n1,n2):
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")

 for line in f:
  line = line.rstrip('\r\n')
  nin = nin + 1
  if line == '':
   continue
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  # Example. line = gaRimat : S m : <MW=gaRi-mat,83017,1>
  #print line
  words = line.split(':')
  subanta = words[0].strip() # gaRimat
  try:
   fg = re.findall(r"[^ ]+",words[1]) # ["S","m"]
  except:
   print "Case",nin,"Problem with fg.Line=",line
   exit(1)
  g = fg[1] 
  #  gender is what current version of s_file_init_alt_helper expects
  #print "fg=",fg
  dictstr = words[2].strip() # <MW=gaRi-mat,83017,1>
  dictwords = re.findall(r"[^ <=,>]+",dictstr)
  words1 = dictwords[1] # gaRi-mat
  dbg=False
  #print "dictstr=",dictstr,"\ndictwords=",dictwords
  try:
   outputs = s_file_init_alt_helper(subanta,g,words1,dbg=dbg)
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
   fout.write("%s\n" % output)
   nout = nout + 1
  if len(outputs) == 0:
   key1=subanta
   key2 = words1
   print "Warning",key1,fg,key2,":",g
 f.close()
 fout.close()

if __name__ == "__main__":
 intab = sys.argv[1]
 indir = sys.argv[2]
 outtab = sys.argv[3]
 outdir = sys.argv[4]
 n1 = sys.argv[5]
 n2 = sys.argv[6]
 s_file_init_alt1(intab,indir,outtab,outdir,int(n1),int(n2))
