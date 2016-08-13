""" huet_mwstems.py
    Aug 10, 2016
"""
import sys,re,codecs

class Huet_noun(object):
 d = {}
 def __init__(self,line):
  line = line.rstrip('\r\n')
  self.line = line
  (self.stem0,self.genderstr) = line.split(':')
  self.stem = re.sub(r'#.*$','',self.stem0)  # Example aMSaka#2. homonym
  self.genders = self.genderstr.split(',')

def init_huetnouns(filein):
 recs=[]
 ndup=0
 with codecs.open(filein,"r","utf-8") as f:
  for x in f:
   rec0 = Huet_noun(x)
   if rec0.stem in Huet_noun.d:
    ndup=ndup+1
    rec = Huet_noun.d[rec0.stem]
    rec.count = rec.count+1
    newgenders=[]
    for g0 in rec0.genders:
     if g0 not in rec.genders:
      rec.genders.append(g0)
      newgenders.append(g0)
    print "Huet_noun Duplicate stem=",rec0.stem,"new genders=",newgenders
   else:
    rec = rec0
    Huet_noun.d[rec.stem]=rec
    rec.count=1
    recs.append(rec) # only keep a record for the first duplicate

 print len(recs),"generated from",filein," with",ndup," duplicate stems"
 return recs

class Pysan_noun(object):
 d = {}
 def __init__(self,line):
  line = line.rstrip('\r\n ')
  self.line = line
  try:
   (self.key1,s,genderstr,self.key2part) = re.split(r'[ :]+',line)
  except:
   parts=re.split(r'[ :]+',line)
   print "PRoblem parsing",line
   print "parts=",parts
   exit(1)
  self.genders = list(genderstr)
  m = re.search(r'=(.*?),',self.key2part)
  try:
   self.key2 = m.group(1)
  except:
   print "ERROR key2",line,"----",self.key2part
   exit(1)

def init_mwnouns(filein):
 recs=[]
 ndup=0
 with codecs.open(filein,"r","utf-8") as f:
  for x in f:
   if x.strip() == '': 
    continue # there is a blank line. skip any such
   rec0 = Pysan_noun(x)
   if rec0.key1 in Pysan_noun.d:
    ndup=ndup+1
    rec = Pysan_noun.d[rec0.key1]
    rec.count = rec.count+1
    newgenders=[]
    for g0 in rec0.genders:
     if g0 not in rec.genders:
      rec.genders.append(g0)
      newgenders.append(g0)
    #print "Pysan_noun Duplicate stem=",rec0.key1,"new genders=",newgenders
   else:
    rec = rec0
    Pysan_noun.d[rec.key1]=rec
    rec.count=1
    recs.append(rec) # only keep a record for the first duplicate
 print len(recs),"generated from",filein," with",ndup," duplicate stems"
 return recs

class Pysan_adj(object):
 d = {}
 def __init__(self,line):
  line = line.rstrip('\r\n ')
  self.line = line
  try:
   (self.key1,s,genderstr,self.key2part) = re.split(r'[ :]+',line)
  except:
   parts=re.split(r'[ :]+',line)
   print "PRoblem parsing",line
   print "parts=",parts
   exit(1)
  if genderstr not in ['adj','adjI']:
   print "Unexpected genderst in",line
  self.genders = ['m','f','n']
  self.genderstr=genderstr
  m = re.search(r'<MW=(.*?),',self.key2part)
  self.key2 = m.group(1)

def init_mwadjs(filein):
 recs=[]
 ndup=0
 with codecs.open(filein,"r","utf-8") as f:
  for x in f:
   if x.strip() == '': 
    continue # there is a blank line. skip any such
   rec0 = Pysan_adj(x)
   if rec0.key1 in Pysan_adj.d:
    ndup=ndup+1
    rec = Pysan_adj.d[rec0.key1]
    rec.count = rec.count+1
    newgenders=[]
    for g0 in rec0.genders:
     if g0 not in rec.genders:
      rec.genders.append(g0)
      newgenders.append(g0)
    #print "Pysan_adj Duplicate stem=",rec0.key1,"new genders=",newgenders
   else:
    rec = rec0
    Pysan_adj.d[rec.key1]=rec
    rec.count=1
    recs.append(rec) # only keep a record for the first duplicate
 print len(recs),"generated from",filein," with",ndup," duplicate stems"
 return recs

def compare(huetnouns,fileout):
 fout = codecs.open(fileout,"w","utf-8")
 nout=0
 nf = 0
 for huetrec in huetnouns:
  stem=huetrec.stem
  hgenders = huetrec.genders
  hgenders.sort()
  hgenderstr = ''.join(hgenders)
  if stem in Pysan_adj.d:
   nout = nout + 1
   mwadj=Pysan_adj.d[stem]
   mwgenders=mwadj.genders;
   mwgenders.sort()
   mwgenderstr=''.join(mwgenders)
   out = "%s:%s;;%s;;%s;;%s\n" %(stem,hgenderstr,mwgenderstr,mwadj.genderstr,mwadj.key2)
  elif stem in Pysan_noun.d:
   nout = nout + 1
   mwnoun=Pysan_noun.d[stem]
   mwgenders=mwnoun.genders;
   mwgenders.sort()
   mwgenderstr=''.join(mwgenders)
   out = "%s:%s;;%s;;%s;;%s\n" %(stem,hgenderstr,mwgenderstr,"noun",mwnoun.key2)
  else:
   out = "%s:%s;;NA;;NA\n" %(stem,hgenderstr)
   nf = nf + 1
  fout.write(out)
 fout.close()
 print nout,"matches and",nf,"non-matches"

if __name__ == "__main__":
 filein = sys.argv[1] # huet_noun_stems_genders.txt
 filein1 = sys.argv[2] # MW-noun.txt
 filein2 = sys.argv[3] # MW-adj.txt
 fileout = sys.argv[4]
 huetnouns = init_huetnouns(filein)
 mwnouns = init_mwnouns(filein1)
 mwadjs = init_mwadjs(filein2)
 # duplicates in MW nouns/adjs
 ndup=0
 for mwadj in mwadjs:
  if mwadj.key1 in Pysan_noun.d:
   mwnoun = Pysan_noun.d[mwadj.key1]
   print "dup: noun=%s, adj=%s" %(mwnoun.line,mwadj.line)
   ndup=ndup+1
 print ndup,"noun/adj dups"
 compare(huetnouns,fileout)

