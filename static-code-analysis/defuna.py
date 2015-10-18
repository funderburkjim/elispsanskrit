""" defuna.py
  Oct 4, 2015
  Generate list of all function calls in all .el files in a directory
  Usage: python defuna.py ../grammar/lisp defuna.txt 
  Also, show defvar 
  Uses sexp.py
"""
import os, sys, re, codecs
import sexp

class CallInfo(object):
 def __init__(self,name,module):
  self.name = name # name of function
  self.module = module # name of file containing function definition
  self.calls = [] # names of functions that this function calls
  self.callers = [] # names of functions that call this function


def find_calls(x):
 """ x is the 'body list' of a function
   find names of all functions that are called.
   Return a list
   recursive
   x = [y ...]
   y = [a b...]
 """
 ans=[]
 #print "find_calls:",x
 if len(x)==0:
  return ans
 y = x[0]
 if isinstance(y,list):
  if len(y)>0:
   a = y[0]
   if not isinstance(a,list):
    if a == 'quote': # ignore a and rest of y
     y=[]
    elif a == 'comment': # ignore a and rest of y
     y=[]
    elif a == 'array': # ignore a and rest of y - ?
     y=[]
    elif a == 't': # assume occurs in 'cond' special form
     pass
    elif a == 'let': 
     # only a partial solution
     # I only use form (let (a b c d) ...)
     # The general form allows (let ((a 5) b) ...
     ans.append(a)
     y = y[1:]  # exclude (a b c d)
    elif a == 'lambda': 
     # treat like let
     ans.append(a)
     y = y[1:]  # exclude argument list
    else:
     ans.append(a)
     #print "found call:",a
   else: 
    for b in find_calls(y):
     ans.append(b)
    y=[] # so next won't repeat
   if len(y) > 1:
    for z in find_calls(y[1:]):
     ans.append(z)
 if len(x)>1:
  for w in find_calls(x[1:]):
   ans.append(w)
 return ans

def find_our_calls(x):
 calls = find_calls(x)
 calls_set = set(calls) # remove duplicates. Also changes order
 exclusions = set(['let','cond','symbolp','setq','car',
   'and','while','<','<=','1+','if','funcall','intern-soft',
   'append','list','+','-','or','not','1-','=',
   'format','listp','symbol-name','cadr',
   'equal','length','cdr','member','elt','substring',
   'when','lambda','progn','aset','make-vector','mapcar','vector',
   'vconcat','sym-with-space','sym-without-space','copy-sequence',
   'eq','get','mapconcat','apply','error','condition-case',
   'save-buffer','nth','*', 'concat', 'erase-buffer', 
   'find-file-noselect','message','with-current-buffer','insert',
   'insert-file-contents', 'kill-buffer','re-search-forward',
   'match-beginning', 'match-end', 'point-min',
   'goto-char','search-forward-regexp', 'with-temp-buffer',
   'point', 'point-max', 'read', 'replace-match', 'search-forward',
   'file-exists-p','buffer-substring','replace-string',
   'min','intern','string-to-number','mapatoms','stringp',
   'current-buffer', 'forward-line','sort','cons','plist-get',
   'arrayp','string-to-number','string-match','aref','upcase',
   'current-time-string','nreverse','downcase',
   'eval','sequencep','aref','vectorp','numberp','make-string',
   'plist-put','end-of-line','>','beginning-of-line', 'sort-lines',
   'directory-files', 'file-name-as-directory','kill-line', 
   'read-from-string','flatten','put','string=','char-to-string',
   'string-to-int','delete-region','mod','save-excursion','fboundp',
   'read-string','interactive',
   'hide-sublevels','find-file','max','byte-compile-file',
   'load-library','expand-file-name','set-frame-position',
   'make-frame-command','/','/=','insert-file-literally',
   'regexp-replace','forward-char','char-after','reverse',
   'skip-chars-forward','delete-char','backward-char',
   'regexp-lines','regexp-kill-lines','caar','string<','eobp',
   'delete-file','find-file-literally','insert-file','goto-line',
   'replace-regexp-in-string',
   # atoms misinterpreted  as function calls
   'a-form','p-form',
   'ivowel','Apte', # aorist.el
   'class','useX',  # construct.el
   'ans2a','ans2b','upasargas','upasargas','baseflag', #explain.el
   'error1', # forms.el
   'sv-vowel', #gram2-futuer.el
   'dtype', #gram2-liT.el
   'sups', #gram3.el
   '2', #kta.el
   'i', # sandhi.el
   # pseudo-calls
   'comment','array',  
   ])
 our_calls_set = calls_set.difference(exclusions)
 our_calls = sorted(list(our_calls_set))
 #print 'calls_set=',calls_set
 #print 'our_calls_set=',our_calls_set
 return our_calls

class Lispfile(object):
 def __init__(self,filename,lispdir):
  self.filename = filename
  self.lispdir=lispdir
  self.lisplist=[]
  self.defuns = []
  self.defvars=[]


def lisp_filenames(lispdir):
 dir1 = os.path.abspath(os.path.join(os.path.dirname(__file__), lispdir))
 ans=[]
 for f in os.listdir(dir1):
  if f.endswith(".el"):
   ans.append(Lispfile(f,lispdir))
 return ans

def prettyprint(y,outarr,indent=0):
 """ recursive """
 indentstr=" "*indent
 if isinstance(y,list):
  outarr.append("%s%s:" %(indentstr,'LIST'))
  if len(y)==0:
   #outarr.append("%s%s : %s" %(indentstr,'LIST',y))
   pass
  else:
   #outarr.append("%sDBG LIST ELEMENTS:%s" %(indentstr,y))
   for z in y:
    prettyprint(z,outarr,indent+1)
 elif isinstance(y,str):
  #outarr.append("%sDBG STR:%s" %(indentstr,y))
  z = r'\n'.join(y.splitlines())
  outarr.append("%s%s : %s" %(indentstr,'STR',z))
 elif isinstance(y,tuple) and (len(y)==1):
  z = y[0]
  outarr.append("%s%s%s" %(indentstr,'',z))
 else:
  outarr.append("%s%s : %s" %(indentstr,type(y),y))

def prettyprint_v1(x,outarr,indent=0):
 """ recursive """
 indentstr=" "*indent
 for y in x:
  if isinstance(y,list):
   if len(y)==0:
    outarr.append("%s%s : %s" %(indentstr,'LIST',y))
   elif isinstance(y[0],tuple) and (len(y[0])==1) and isinstance(y[0][0],str):
    z = y[0][0]
    outarr.append("%s%s : %s" %(indentstr,'LIST',z))
    prettyprint(y[1:],outarr,indent+1)
   elif isinstance(y[0],list):
    outarr.append("%s%s : %s" %(indentstr,'LIST',''))
    prettyprint(y[0],outarr,indent+1)
    prettyprint(y[1:],outarr,indent+1)
   else:
    outarr.append("%s%s : %s" %(indentstr,'LIST',y[0]))
    prettyprint(y[1:],outarr,indent+1)
  elif isinstance(y,str):
   z = r'\n'.join(y.splitlines())
   outarr.append("%s%s : %s" %(indentstr,'STR',z))
  elif isinstance(y,tuple) and (len(y)==1):
   z = y[0]
   outarr.append("%s%s%s" %(indentstr,'',z))
  else:
   outarr.append("%s%s : %s" %(indentstr,type(y),y))

def alterparse(x,outarr,unique=None):
 """ recursive """
 for y in x:
  if isinstance(y,list):
   out1=[]
   if len(y)==0:
    #out1.append([])
    pass  # leave out1 as empty list
   elif isinstance(y[0],tuple) and (len(y[0])==1) and isinstance(y[0][0],str):
    z = y[0][0]
    out1.append(z)
    alterparse(y[1:],out1)
   elif isinstance(y[0],list):
    out2 = []
    alterparse(y[0],out2)
    out1.append(out2)
    alterparse(y[1:],out1)
   else:
    out1.append(y[0])
    alterparse(y[1:],out1)
   # finally, add out1 to outarr
   outarr.append(out1)
  elif isinstance(y,str):
   z = r'\n'.join(y.splitlines())
   if unique: # change DOUBLEQUOTE back to r'\"'
    z = re.sub(unique[1],unique[0],z)
   outarr.append(z)
   #outarr.append("%s%s : %s" %(indentstr,'STR',z))
  elif isinstance(y,tuple) and (len(y)==1):
   z = y[0]
   #outarr.append("%s%s%s" %(indentstr,'',z))
   outarr.append(z)
  else:
   #outarr.append("%s%s : %s" %(indentstr,type(y),y))
   print "alterparse ERROR:",type(y),y

def prepare_test(glob):
 # change semicolon comments to (comment "comment")
 glob = re.sub(r'(;.*?)([\r\n])',r'(comment "\1")\2',glob)
 x = sexp.parse(glob)
 if True:
  print "raw parse:"
  print x
 from pprint import pprint
 with codecs.open("temp2","w","utf-8") as f:
  pprint(x,f)
 outarr=[]
 alterparse(x,outarr)
 with codecs.open("temp2","a","utf-8") as f:
  f.write("\nALTERPARSE\n\n")
  pprint(outarr,f)
 return outarr

def prepare_comment(x):
 """ this is very tricky, and only a partial solution is provided
 """
 lines = x.splitlines()
 for i in xrange(0,len(lines)):
  line = lines[i]
  if ';' not in line:
   continue
  if re.search(r'^[ \t]*;',line):
   #lines[i] = re.sub(r'(;.*?)$',r'(comment "\1")\2',line)
   # whole line is a comment
   lines[i] = '(comment "%s")' % line
   continue
  if re.search(r'"[^")]*;.*"',line):
   print "NOT-COMMENT:",line
   continue
  if re.search(r';[^"]*$',line):
   lines[i] = re.sub(r'(;[^"]*)$',r'(comment "\1")',line)
   continue
  if re.search(r'; ',line):  # maybe
   lines[i] = re.sub(r'(; .*)$',r'(comment "\1")',line)
   continue
  print "COMMENTCHK:",i,line
  
 # join the lines
 return '\n'.join(lines) 

def prepare(glob,testflag=False):
 # try to convert array literals to lists
 glob = re.sub(r'\[','(array ',glob)
 glob = re.sub(r'\]',')',glob)
 # change semicolon comments to (comment "comment")
 glob = prepare_comment(glob)
 # change \" to some unique string (say DOUBLEQUOTE)
 
 unique = (r'\\"','DOUBLEQUOTE')
 glob = re.sub(unique[0],unique[1],glob)
 # use sexp to parse
 x = sexp.parse(glob)
 # 'improve' what sexp did
 outarr=[]
 alterparse(x,outarr,unique)
 return outarr

def prepare_v1(glob):
 # remove comments
 glob = re.sub(r';.*?[\r\n]','',glob)
 x = sexp.parse(glob)
 if True:
  print "raw parse:"
  print x
 outarr=[]
 prettyprint(x,outarr)
 for out in outarr:
  print out

def myparse(lispfile):
 "lispfile is a Lispfile object"
 path = os.path.join(lispfile.lispdir,lispfile.filename)
 with codecs.open(path,"r") as f:
  glob = f.read()
  x=prepare(glob)
 return x

def find_defuns(lisplist,listdefuns):
 """lisplist is a top level Lispfile.lisplist object
  Find the children which have form
    ['defun' 'X' [Y] ...]
  and add ('defun','X',[Y]) to listdefuns
  This is a non-recursive function
 """
 match='defun'
 names = [(x[0],x[1],x[2]) for x in lisplist if 
              (isinstance(x,list) and (len(x)>2) and (x[0]==match))]
 for name in names:
  if name not in listdefuns: # exclude duplicates
   listdefuns.append(name)

def find_defvars(lisplist,listdefvars):
 """lisploist is a top level Lispfile.lisplist object
  Find the children which have form
    ['defvar' 'X' Y ...]
  and add ('defvar','X',[Y]) to listdefvars
  This is a non-recursive function
 """
 match='defvar'
 names = [(x[0],x[1],x[2]) for x in lisplist if 
              (isinstance(x,list) and (len(x)>2) and (x[0]==match))]
 for name in names:
  if name not in listdefvars: # exclude duplicates
   listdefvars.append(name)

def display_docfiles(lispfiles,d,fout):
 nfound=0
 fout.write("-"*72 + "\n")
 fout.write("doc functions\n")
 for f in lispfiles:
  fout.write("%s\n" % f.filename)
  for defun in f.defuns:
   fname = defun[1]
   if re.search(r'doc',fname):
    fout.write(' %s\n' % fname)
    nfound = nfound+1
 return nfound

def find_internal_external(calls,module,d):
 icalls=[]
 ecalls=[]
 for call in calls:
  if call not in d:
   continue
  module1=d[call].module
  if module1 == module:
   icalls.append(call)
  else:
   module1a = re.sub(r'.el$','',module1)
   call1a = "%s.%s" %(module1a,call)
   ecalls.append(call1a)
 return (icalls,ecalls)


def display_functions(lispfiles,fout):
 ntot=0
 ntot1=0
 for f in lispfiles:
  n=len(f.defuns)
  ntot = ntot+n
  n1 = len(f.defvars)
  ntot1 = ntot1+n1
  fout.write("%s %s %s\n" %(f.filename,n,n1))
  for x in f.defvars:
   #fout.write("  %s\n" %x)
   if isinstance(x[2],list):
    try:
     z = ' '.join(x[2])
    except:
     #print "WARNING:",x[2]
     z = '"WARNING: %s"' % x[2]
   else:
    z = x[2]
   #fout.write("  (%s %s (%s))\n" %(x[0],x[1],z))
   fout.write("  (%s %s ETC)\n" %(x[0],x[1]))
  for x in f.defuns:
   #fout.write("  %s\n" %x)
   if isinstance(x[2],list):
    z = ' '.join(x[2])
   else:
    z = x[2]
   #fout.write("  (%s %s (%s))\n" %(x[0],x[1],z))
   fout.write("  (%s %s (%s)\n" %(x[0],x[1],z))
 return (ntot,ntot1)

def prepare1(lispfiles):
 for f in lispfiles:
  print f.filename
  xlist = myparse(f)
  f.lisplist=xlist
  find_defuns(f.lisplist,f.defuns)
  find_defvars(f.lisplist,f.defvars)

def  prepare2(d,lispfiles):
 for f in lispfiles:
  for (defuncon,name,rest) in f.defuns:
   if name in d:
    f0 = d[name]
    print "DUPLICATE",name,f0.filename,f.filename
   d[name]=CallInfo(name,f.filename)
 print len(lispfiles),"Lisp modules"
 print len(d.keys()),"functions"
 for f in lispfiles:
  match='defun'
  defuns = [x for x in f.lisplist if 
              (isinstance(x,list) and (len(x)>2) and (x[0]==match))]
  #print f.filename
  #print len(defuns)
  for x in defuns:
   caller = x[1]
   args = x[2]
   if len(x) ==3:
    calls=[]
   else:
    body= x[3:]
    calls = find_our_calls(body)
   d[caller].calls = calls
   #print f.filename,caller,"calls",calls
  #break # dbg
 # generate callers
 for f in lispfiles:
  for defun in f.defuns:
   fname = defun[1]
   info = d[fname]
   for x in info.calls: # fname calls x
    if x in d:
     d[x].callers.append(fname)
    else:
     print "UNDEFINED:",x,"(",info.module,fname,")"

def display_callers(lispfiles,d,ncallers,fout):
 nfound=0
 outall=[]
 outall.append("-"*72 + "\n")
 outall.append("functions called by %s callers\n" % ncallers)

 for f in lispfiles:
  out=[]
  out.append("%s\n" % f.filename)
  n=0
  for defun in f.defuns:
   fname = defun[1]
   if re.search(r'doc',fname): # skip doc functions
    continue
   info = d[fname]
   callers = info.callers
   calls = info.calls
   module = info.module
   assert module == f.filename
   if len(callers) == ncallers:
    n=n+1
    (icalls,ecalls) = find_internal_external(calls,f.filename,d)
    if len(icalls)>0:     
     icalls1 = ', '.join(icalls)
     out.append(" %s calls INTERNAL: %s\n" %(fname,icalls1))
    if len(ecalls)>0:     
     ecalls = sorted(ecalls)
     ecalls1 = ', '.join(ecalls)
     out.append(" %s calls EXTERNAL: %s\n" %(fname,ecalls1))
    if (len(ecalls) == 0) and (len(icalls)==0):
     out.append(" %s calls NOTHING\n" % fname)
    if ncallers!=0:
     callers1 = ', '.join(callers)
     out.append(" %s called by %s\n" %(fname,callers1))
    out.append("\n")
    nfound = nfound+1
  if n>0:
   for x in out:
    outall.append(x)
 if nfound>0:
  for x in outall:
   fout.write(x)
 return nfound

def display_xref1(lispfiles,d,fout):
 ntot=display_docfiles(lispfiles,d,fout)
 for ncallers in xrange(0,100):
  nfound = display_callers(lispfiles,d,ncallers,fout)
  ntot = ntot + nfound
  if nfound == 0:
   ncallers=ncallers-1
   break
 return (ntot,ncallers)


def display_xref_module(lispfiles,d,fout):
 nfound=0
 outall=[]
 #outall.append("-"*72 + "\n")
 #outall.append("functions called by %s callers\n" % ncallers)

 for f in lispfiles:
  #out=[]
  #out.append("%s\n" % f.filename)
  #n=0
  emodules=[]
  for defun in f.defuns:
   fname = defun[1]
   if re.search(r'doc',fname): # skip doc functions
    continue
   info = d[fname]
   callers = info.callers
   calls = info.calls
   module = info.module
   #assert module == f.filename
   (icalls,ecalls) = find_internal_external(calls,f.filename,d)
   for x in ecalls:
    if not (x in emodules):
     emodules.append(x)
  # print external calls, sorted by module
  emodules = sorted(emodules)
  prev_module=''
  pairs=[]
  calledmodules=[]
  for emodule in emodules:
   (module,fname)=re.split(r'[.]',emodule)
   if module != prev_module:
    curfnames=[]
    curpairs=(module,curfnames)
    pairs.append(curpairs)
    prev_module=module
    calledmodules.append(module)
   curfnames.append(fname)
  curmodule = re.sub(r'.el$','',f.filename)
  for (module,curfnames) in pairs:
   names = ', '.join(curfnames)
   fout.write('MODULE %s calls MODULE %s: %s\n' %(curmodule,module,names))
  # summary 
  calledmodules1 = ', '.join(calledmodules)
  fout.write('MODULE %s calls MODULES  %s\n' %(curmodule,calledmodules1))
  fout.write('\n')

if __name__ == "__main__":
 dispoption = sys.argv[1]
 lispdir = sys.argv[2]
 fileout = sys.argv[3]
 lispfiles = lisp_filenames(lispdir)
 print len(lispfiles)
 # prepare defuns, defvars
 prepare1(lispfiles)
 # prepare 'd', a hash of Callinfo records, based on function names
 d={}
 prepare2(d,lispfiles)
 #Generate display, based on dispoption
 fout = codecs.open(fileout,"w","utf-8")
 if dispoption == '1':
  (ntot,ntot1)=display_functions(lispfiles,fout)
  print ntot,ntot1
  fout.close()
  exit(0)
 if dispoption == '2':
  # second phase, Cross-reference: which functions call which
  (ntot,ncallers)=display_xref1(lispfiles,d,fout)
  fout.close()
  print ntot," functions displayed with callers"
  print ncallers," is maximum number of callers"
  exit(0)
 if dispoption == '3':
  # second phase, module Cross-reference:
  display_xref_module(lispfiles,d,fout)
  fout.close()
  #print ntot," functions displayed with callers"
  #print ncallers," is maximum number of callers"
  exit(0)
