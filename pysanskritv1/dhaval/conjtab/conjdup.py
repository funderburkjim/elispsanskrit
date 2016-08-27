"""conjdup.py
   Look for duplicate conjugation tables in conj.txt
"""
import sys,re,codecs

class DhavalConj(object):
 def __init__(self,line):
  line=line.rstrip('\r\n')
  (self.header,self.table)=line.split(':')
  (self.root,self.tense,self.sutrapada)=self.header.split(' ')
  (self.theclass,sequencepada)=self.sutrapada.split('.')
  self.sequence = sequencepada[0:-1]
  self.pada = sequencepada[-1:]  # P,A
def main(filein,fileout):
 #fout = codecs.open(fileout,"w","utf-8") 
 recs=[]
 with codecs.open(filein,"r","utf-8") as f:
  for line in f:
   rec = DhavalConj(line)
   recs.append(rec)
 # duplicate conjugation tables
 d={}
 for rec in recs:
  if rec.table not in d:
   d[rec.table]=[]
  d[rec.table].append(rec)
 ndups=0
 for tab,recs in d.iteritems():
  if len(recs)>1:
   ndups=ndups+1
   headers = [rec.header for rec in recs]
   out = ' :: '.join(headers)
   print out
 print ndups,'duplicate conjugation tables'
 # duplicate sutra-pada
 ndups=0
 d={}
 for rec in recs:
  if rec.sutrapada not in d:
   d[rec.sutrapada]=[]
  d[rec.sutrapada].append(rec)
 for sutrapada,recs in d.iteritems():
  if len(recs)>1:
   ndups=ndups+1
   sutrapadas = [rec.sutrapada for rec in recs]
   out = ' :: '.join(sutrapadas)
   print out
 print ndups,"duplicate sutra-padas"

if __name__ == "__main__":
 slptense = sys.argv[1]
 filein = "conj_%s.txt" % slptense
 fileout="conj_%s_nodup.txt" % slptense  # unused
 main(filein,fileout)
