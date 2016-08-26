""" test_v_file_init_alt1.py
  Python version of Elisp v-file-init-alt1-pre in forms.el
  June 28, 2016
  python test_v_file_init_alt1.py dcpforms-MW-verb.txt ../../elispsanskrit/grammar/prod/inputs MW-verb-pre.txt prodchk/outputs 1 9999


diff -w prodchk/outputs/MW-verb-pre.txt ../../elispsanskrit/grammar/prod/outputs/MW-verb-pre.txt > temp
"""
from test2 import MyException
from test2 import v_file_init_alt1_pre_helper,reduplicative_liw_P
from test2 import sl_ppfactn,sl_inf,sl_participle,sl_participle_decl
from test2 import aorist_varieties
import sys,re,codecs
preserve_elisp_errors=True

def v_file_init_alt1_participle_decl(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
 Determine various participle stems
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

  NOTE: Uses sl_inf function. 
 """
 partcode = voicecode
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 dbg=False
 dbg1=True
 dbg1=False
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # voice is used only in output, for comparision with Elisp output
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  upasargas = None
  pada1 = pada.lower()
  dtype=None
  vals = sl_participle_decl(root,theclass,voice,partcode,dtype,upasargas,dbg=dbg)
  if vals == None:
   vals = []
  if len(vals) == 0:
   continue # don't write anything
  # Note: all the formatting is done in sl_participle_decl; whereas
  # for sl_participle, part of the formatting is done here
  for val in vals:
   fout.write("%s\n" % val)
  nout = nout+1
  """
  output1 = "(" + (' '.join(vals)) + ")"
  output = ':%s %s %s%s:%s' %(root,partcode,theclass,voice,output1)
  fout.write("%s\n" % output)
  nout = nout + 1
  """
  if dbg1:
   pass
 fout.close()

def v_file_init_alt1_participle(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
 Determine various participle stems
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

  NOTE: Uses sl_inf function. 
 """
 partcode = voicecode
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 dbg=False
 dbg1=True
 dbg1=False
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # voice is used only in output, for comparision with Elisp output
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  upasargas = None
  pada1 = pada.lower()
  dtype=None
  vals = sl_participle(root,theclass,voice,partcode,dtype,upasargas,dbg=dbg)
  if vals == None:
   vals = []
  if len(vals) == 0:
   continue # don't write anything
  output1 = "(" + (' '.join(vals)) + ")"
  output = ':%s %s %s%s:%s' %(root,partcode,theclass,voice,output1)
  fout.write("%s\n" % output)
  nout = nout + 1
  if dbg1:
   pass
 fout.close()

def v_file_init_alt1_inf(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
 Determine infinitives
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

  NOTE: Uses sl_inf function. 
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 dbg=False
 dbg1=True
 dbg1=False
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # voice is used only in output, for comparision with Elisp output
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  upasargas = None
  pada1 = pada.lower()
  dtype=None
  infs = sl_inf(root,theclass,voice,dtype,dbg=dbg)
  if infs == None:
   infs = []
  output1 = "(" + (' '.join(infs)) + ")"
  output = ':%s %s %s%s:%s' %(root,"inf",theclass,voice,output1)
  fout.write("%s\n" % output)
  nout = nout + 1
  if dbg1:
   pass
 fout.close()

def v_file_init_alt1_aor(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
 Determine aorist varieties, and for each variety, compute the
  conjugation of that variety with the given root, class, voice as read from
  input
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

  NOTE: Uses aorist_varieties function.  In this function, the 'pada' is
  assumed to be a lower case 'p' (parasmaipada) or 'a' (atmanepada). I mention
  this since we are, in particular, NOT using the Scharf convention of
  'a' (for active voice, same as parasmaipada) or 'm' (for middle voice, same
  as atmanepada).
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 dbg=False
 dbg1=True
 dbg1=False
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # voice is used only in output, for comparision with Elisp output
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  upasargas = None
  pada1 = pada.lower()
  aorvars=aorist_varieties(root,theclass,pada1,upasargas,dbg)
  if aorvars == None:
   aorvars = []
  for aorvar in aorvars:
   tense = "aor%s" % aorvar
   dtype = None
   #print "v_file_init_alt1_pre_helper(%s,%s,%s,%s,%s)" %(root,theclass,voice,tense,dtype)
   try:
    outputs = v_file_init_alt1_pre_helper(root,theclass,voice,tense,dtype=dtype,dbg=dbg)
   except (NameError,MyException) as err:
    print "\ncase=",nin,"line=",line,"tense=",tense
    print err
    continue
   if isinstance(outputs,list):
    #print "outputs is a list of length",len(outputs)
    pass
   elif outputs == None:
    continue
   else:
    #print "outputs is not a list"
    outputs = [outputs]
   for output in outputs:
    fout.write("%s\n" % output)
    nout = nout + 1
   if len(outputs) == 0:
    print "Warning(no result)",root,theclass,voice,tense
 f.close()
 fout.close()

def v_file_init_alt1_aorvar(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
 Determine aorist varieties
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

  NOTE: Uses aorist_varieties function.  In this function, the 'pada' is
  assumed to be a lower case 'p' (parasmaipada) or 'a' (atmanepada). I mention
  this since we are, in particular, NOT using the Scharf convention of
  'a' (for active voice, same as parasmaipada) or 'm' (for middle voice, same
  as atmanepada).
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 dbg=False
 dbg1=True
 dbg1=False
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # voice is used only in output, for comparision with Elisp output
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  upasargas = None
  pada1 = pada.lower()
  aorvars=aorist_varieties(root,theclass,pada1,upasargas,dbg)
  if aorvars == None:
   aorvars = []
  output1 = "(" + (' '.join(aorvars)) + ")"
  output = ':%s %s %s%s:%s' %(root,"aorvars",theclass,voice,output1)
  fout.write("%s\n" % output)
  nout = nout + 1
  if dbg1:
   pass
 fout.close()

def v_file_init_alt1_ppfactn(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ 
 Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
  Use the class provided in  dcpforms-MW.txt
  to determine if a periphrastic perfect is computable.
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  if pada == 'P':
   voice = 'a'
  else:
   voice = 'm'
  if root not in roots:
   roots[root]=[] # value is a list of class-voice
   rootlist.append(root)
  classvoice = (theclass,voice)
  if classvoice not in roots[root]:
   roots[root].append(classvoice)
 f.close()
 # phase 2
 dbg=False
 dbg1=True
 dbg1=False
 for root in rootlist:
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  thecvs = roots[root]
  actns=[]
  for (theclass,voice) in thecvs:
   actn=sl_ppfactn(root,theclass,voice,None)
   if not actn:
    continue
   if not isinstance(actn,list):
    actn = [actn]
   for a in actn:
    if a not in actns:
     #actns.append(a)
     actns.insert(0,a) # cons
  if len(actns) == 0:
   #print "%s has no ppfactn. Classes=",thecvs
   continue
  output1 = "[" + (' '.join(actns)) + "]"
  output = ':%s %s:%s' %(root,"ppfactn",output1)
  fout.write("%s\n" % output)
  nout = nout + 1
  if dbg1:
   pass
 fout.close()

def v_file_init_alt1_prf(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ A separate routine is needed to  facilitate comparison to elisp
     v-file-init-alt1-fut, since there is arrangement of 'non-duplicate'
     cases prior to the actual conjugation generation.

 
  Output is constructed by routine v-file-init-alt1-pre-helper, and the
  this output is slightly altered before insertion into outtab (dummy class
  removed).  Also, since the passive depends only on the root (and not on
  the class or pada), logic of this function discards duplicate records
  (i.e., only one set of tables for a give root, even if that root
  appears in multiple class-pada combinations in 'intab'.

  The class-voice combinations to use for a given root are derived in a
  complex way by function v-file-init3-prf.  We mimic that function's
  behavior here; the grammatical reason for this is unclear at
  present (10-15-2015).
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # pada is not used
  if root not in roots:
   roots[root]=[] # value is a list of classes
   rootlist.append(root)
  if theclass not in roots[root]:
   roots[root].append(theclass)
 f.close()
 # phase 2
 dbg=False
 dbg1=True
 dbg1=False
 for root in rootlist:
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  multiclass = None
  theclasses = roots[root]
  if dbg1:
   if root != 'akz':
    continue
   print root,theclasses
  cvs=[]
  for theclass in theclasses:
   if (theclass not in cvs) and (reduplicative_liw_P(root,theclass)):
    if multiclass:
     multiclass = multiclass + "/" + theclass
    else:
     multiclass = theclass
    cvs.append(theclass)
  if dbg1 and (root == 'akz'):
   print "cvs=",cvs,"multiclass=",multiclass
  if (len(cvs) == 0):
   #print "%s has no prf. Classes=%s" %(root,theclasses)
   continue
  cvs = [cvs[0]] # just use first case
  for theclass in cvs:  # just one iteration, by previous statement
   tenses = ['prf']
   # 'p' (passive) is same as 'm' (middle) for 'fut pft con ben'
   voices = ['a','m']
   for voice in voices:
    for tense in tenses:
     dtype=None
     try:
      # note classid is not used, except for format of output in helper
      outputs = v_file_init_alt1_pre_helper(root,theclass,voice,tense,dtype=dtype,dbg=dbg)
     except (NameError,MyException) as err:
      print "\ncase=",nin,"line=",line
      print err
      continue
     if isinstance(outputs,list):
      #print "outputs is a list of length",len(outputs)
      pass
     elif outputs == None:
      continue
     else:
      #print "outputs is not a list"
      outputs = [outputs]
     for output in outputs:
      # change labeling from 'theclass' to 'multiclass'
      old = " %s%s:" %(theclass,voice)
      new = " %s%s:" %(multiclass,voice)
      output1 = output.replace(old,new,1)
      if dbg1:
       if root == 'akz':
        print "CHK: output=",output,"\nold=",old,"new=",new,"\noutput1=",output1
        exit(1)
      fout.write("%s\n" % output1)
      nout = nout + 1
      if len(outputs) == 0:
       print "Warning(no result)",root,theclass,voice,tense
  if dbg1:
   print "exiting after aMh"
   exit(1)

 fout.close()

def v_file_init_alt1_fut(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 """ A separate routine is needed to  facilitate comparison to elisp
     v-file-init-alt1-fut, since there is arrangement of 'non-duplicate'
     cases prior to the actual conjugation generation.
 """
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 # phase 1
 roots = {}
 rootlist=[]
 for line in f:
  line = line.rstrip('\r\n')
  if line == '':
   continue
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  # pada is not used
  if root not in roots:
   roots[root]=[] # value is a list of classes
   rootlist.append(root)
  if theclass not in roots[root]:
   roots[root].append(theclass)
 f.close()
 # phase 2
 dbg=False
 dbg1=True
 dbg1=False
 for root in rootlist:
  nin = nin + 1
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  multiclass = None
  class10 = None
  class0 = None
  theclasses = roots[root]
  if dbg1:
   if root != 'aMh':
    continue
   print root,theclasses
  cvs=[]
  for theclass in theclasses:
   if theclass == '10':
    class10 = theclass
   elif class0 == None:
    class0 = theclass
   if (theclass not in cvs) and (theclass != "10"):
    if multiclass:
     multiclass = multiclass + "/" + theclass
    else:
     multiclass = theclass
    cvs.append(theclass)
   # reset cvs artificially
  cvs = []
  if class0 != None:
   cvs.insert(0,[class0,multiclass])
  if class10 != None:
   cvs.insert(0,[class10,class10])
  if dbg1:
   print cvs
  for (theclass,classid) in cvs:
   tenses = ['fut','pft','con','ben']
   # 'p' (passive) is same as 'm' (middle) for 'fut pft con ben'
   voices = ['a','m']
   for voice in voices:
    for tense in tenses:
     dtype=None
     try:
      # note classid is not used, except for format of output in helper
      outputs = v_file_init_alt1_pre_helper(root,classid,voice,tense,dtype=dtype,dbg=dbg)
     except (NameError,MyException) as err:
      print "\ncase=",nin,"line=",line
      print err
      continue
     if isinstance(outputs,list):
      #print "outputs is a list of length",len(outputs)
      pass
     elif outputs == None:
      continue
     else:
      #print "outputs is not a list"
      outputs = [outputs]
     for output in outputs:
      # adjustment for fact that Elisp uses 'pop' instead of 'opt'
      if tense == 'opt':
       output = output.replace(' opt ',' pop ',1) # replace only 1st occurrence
      if voice == 'p':
       output = re.sub(r'[0-9]+p:','p:',output)
      fout.write("%s\n" % output)
      nout = nout + 1
      if len(outputs) == 0:
       print "Warning(no result)",root,theclass,voice,tense
  if dbg1:
   print "exiting after aMh"
   exit(1)

 fout.close()

def v_file_init_alt1_pre(intab,indir,outtab,outdir,n1,n2,tensecode,voicecode):
 nin = 0
 nout = 0
 filein = "%s/%s" %(indir,intab)
 fileout = "%s/%s" %(outdir,outtab)
 f = codecs.open(filein,"r","utf-8")
 fout = codecs.open(fileout,"w","utf-8")
 roots = {}
 for line in f:
  line = line.rstrip('\r\n')
  nin = nin + 1
  if line == '':
   continue
  if not ((n1 <= nin) and (nin <= n2)):
   continue
  # Example. line = (aMS 10 P <MW=aMS,17,1>)
  m = re.search(r'\((.*?)<',line)
  if not m:
   print "PROBLEM WITH INPUT LINE:",line
   exit(1)
  temp =  m.group(1).strip()
  #print "temp=",temp
  words =temp.split(' ')
  #print "words=",words
  (root,theclass,pada) = words
  dbg=False
  dtype=None
  if voicecode == 'p':
   voice = 'p'
   if root in roots:
    continue  # avoid duplicates
   roots[root]=True
   if preserve_elisp_errors:
    #no good reason for this choice of Elisp code.
    #In fact, 'theclass' IS used in constructing the passive base,
    # for example 'aMS 10 p pre' 3s of '(aMSApyate aMSyate)'
    # while aMS 0 p pre -> aMSyate
    theclass = "0"  
  elif (pada == 'P'):
   voice = "a"
  else:
   # pada == 'A'
   voice = "m"
  if tensecode == 'pre':
   tenses = ['pre','ipf','ipv','pop'] # 4 special tenses
  else:
   print "UNKNOWN tensecode=",tensecode
   exit(1)
  for tense0 in tenses:
   if tense0 == 'pop':
    tense = 'opt'
   else:
    tense = tense0
   try:
    outputs = v_file_init_alt1_pre_helper(root,theclass,voice,tense,dtype=dtype,dbg=dbg)
   except (NameError,MyException) as err:
    print "\ncase=",nin,"line=",line
    print err
    continue
   if isinstance(outputs,list):
    #print "outputs is a list of length",len(outputs)
    pass
   elif outputs == None:
    continue
   else:
    #print "outputs is not a list"
    outputs = [outputs]
   for output in outputs:
    # adjustment for fact that Elisp uses 'pop' instead of 'opt'
    if tense == 'opt':
     output = output.replace(' opt ',' pop ',1) # replace only 1st occurrence
    if voice == 'p':
     output = re.sub(r'[0-9]+p:','p:',output)
    fout.write("%s\n" % output)
    nout = nout + 1
   if len(outputs) == 0:
    print "Warning(no result)",root,theclass,voice,tense
 f.close()
 fout.close()

if __name__ == "__main__":
 #print sys.argv
 intab = sys.argv[1]
 indir = sys.argv[2]
 outtab = sys.argv[3]
 outdir = sys.argv[4]
 n1 = sys.argv[5]
 n2 = sys.argv[6]
 tensecode = sys.argv[7]
 if len(sys.argv) > 8:
  voicecode = sys.argv[8]
 else:
  voicecode = None
 if tensecode == 'fut':
  v_file_init_alt1_fut(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'prf':
  v_file_init_alt1_prf(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'ppfactn':
  v_file_init_alt1_ppfactn(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'aorvar':
  v_file_init_alt1_aorvar(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'aor':
  v_file_init_alt1_aor(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'pre':
  v_file_init_alt1_pre(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'inf':
  v_file_init_alt1_inf(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'participle':
  v_file_init_alt1_participle(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 elif tensecode == 'participle-decl':
  v_file_init_alt1_participle_decl(intab,indir,outtab,outdir,int(n1),int(n2),tensecode,voicecode)
 else:
  print "UNKNOWN tensecode:",tensecode
