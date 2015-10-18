""" defun.py
  Oct 4, 2015
  Generate list of all function calls in all .el files in a directory
  Usage: python defun.py ../grammar/lisp defun.txt
  Also, show defvar 
"""
import os, sys, re, codecs

class Lispfile(object):
 def __init__(self,filename,lispdir):
  self.filename = filename
  self.lispdir=lispdir
  self.defuns = []
  self.defvars=[]

def lisp_filenames(lispdir):
 dir1 = os.path.abspath(os.path.join(os.path.dirname(__file__), lispdir))
 ans=[]
 for f in os.listdir(dir1):
  if f.endswith(".el"):
   ans.append(Lispfile(f,lispdir))
 return ans

def find_defuns(lispfile):
 "lispfile is a Lispfile object"
 path = os.path.join(lispfile.lispdir,lispfile.filename)
 with codecs.open(path,"r") as f:
  glob = f.read()
  parts = re.split(r'(\(defun *.*? *\(.*?\))',glob)
  ans = []
  for part in parts:
   if part.startswith('(defun'):
    ans.append(part)
 lispfile.defuns = ans

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
 fout.close()
 print ntot,ntot1
