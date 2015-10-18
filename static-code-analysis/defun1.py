""" defun1.py
  Oct 4, 2015
  Generate list of all function calls in all .el files in a directory
  Usage: python defun1.py ../grammar/lisp defun1.txt
  Also, show defvar 
  defun1 also shows, for each defun, the functions it calls
"""
import os, sys, re, codecs

class Lispfile(object):
 def __init__(self,filename,lispdir):
  self.filename = filename
  self.lispdir=lispdir
  self.defuns = []
  self.defvars=[]
  self.calldict={}

def lisp_filenames(lispdir):
 dir1 = os.path.abspath(os.path.join(os.path.dirname(__file__), lispdir))
 ans=[]
 for f in os.listdir(dir1):
  if f.endswith(".el"):
   ans.append(Lispfile(f,lispdir))
 return ans

exclude_calls=[
 'defvar', # There can be defvars. We don't want these, they aren't calls
 'car','cdr','let','or','and','not','equal','setq',
 'listp','list','member','cond',
]
def find_calls(x):
 """
  search for pattern '(xxx ' or '(xxx)' and return all such xxx
 """
 x1 = x.strip()
 doc=""
 m = re.search(r'^(".*?")(.*)$',x1,re.DOTALL)
 if m:
  doc = m.group(1)
  x1 = m.group(2)
 
 lines = re.split('[\r\n]',x1)
 ans=[]
 for y in lines:
  p1 = re.findall(r'\(.*?[ \)]',y)
  for z in p1:
   z = re.sub(r'^\((.*?)[ \)]',r'\1',z)
   
   if z in exclude_calls:
    continue
   if z not in ans:
    ans.append(z)
 return (doc,ans)

def find_defun_name(x):
 m = re.search(r'\(defun *([^ ]*?) *\(.*?\)',x)
 if not m:
  print "ERROR 1",lispfile.filename
  print x
 defun = m.group(1)
 return defun

def find_defuns(lispfile):
 "lispfile is a Lispfile object"
 path = os.path.join(lispfile.lispdir,lispfile.filename)
 with codecs.open(path,"r") as f:
  glob = f.read()
  parts = re.split(r'(\(defun *.*? *\(.*?\))',glob)
  defuns = []
  calldict={}
  prevdefun=''
  for part in parts:
   if part.startswith('(defun'):
    defuns.append(part)
    prevdefun=part
   elif prevdefun != '': # i.e., we just got a defun
    defun = find_defun_name(prevdefun)
    calls = find_calls(part)
    if defun in calldict:
     print "WARNING",lispfile.filename," multiple defun for",defun
    calldict[defun]=calls # actually a pair, string and list
    prevdefun='' # this part is not a defun Prepare fornext part
 lispfile.defuns = defuns
 lispfile.calldict=calldict

def find_defvars(lispfile):
 "lispfile is a Lispfile object"
 path = os.path.join(lispfile.lispdir,lispfile.filename)
 with codecs.open(path,"r") as f:
  glob = f.read()
  parts = re.split(r'(\(defvar *.*?\))',glob)
  ans = []
  for part in parts:
   if part.startswith('(defvar'):
    ans.append(part)
 lispfile.defvars = ans

if __name__ == "__main__":
 lispdir = sys.argv[1]
 fileout = sys.argv[2]
 lispfiles = lisp_filenames(lispdir)
 print len(lispfiles)
 fout = codecs.open(fileout,"w","utf-8")
 #for x in lispfiles:
  #print x.filename

 ntot=0
 ntot1=0
 for f in lispfiles:
  find_defuns(f)
  find_defvars(f)
  n = len(f.defuns)
  ntot = ntot+n
  n1 = len(f.defvars)
  ntot1 = ntot1+n1
  fout.write("%s %s %s\n" %(f.filename,n,n1))
  for x in f.defvars:
   fout.write("  %s\n" %x)
  for x in f.defuns:
   fout.write("  %s\n" %x)
   defun = find_defun_name(x)
   (doc,calls) = f.calldict[defun]
   fout.write("  CALLS: %s\n" % calls)
 fout.close()
 print ntot,ntot1
