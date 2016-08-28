"""verbdata_dupsutra.py
   Aug 28, 2016
   Identify sutra numbers that appear in multiple records of verbdata.txt
   
"""
import sys,codecs

def sutra_dup(verbrecs,fileout):
 """ examine sutra number = gana + '.' + number for duplicates
     No duplicates are expected
 """
 fout = codecs.open(fileout,"w","utf-8")
 sutrad={} # value is a list of records with given sutra-number
 for verbrec in verbrecs:
  sutra = verbrec.gana + '.' + verbrec.number
  #rec.sutra = sutra
  if sutra not in sutrad:
   sutrad[sutra]=[]
  sutrad[sutra].append(verbrec)
 ncase=0
 for sutra,recs in sutrad.items():
  if len(recs) > 1:
   #out = "%s:%s" % (sutra,','.join(dpnorms))
   #fout.write(out + "\n")
   ncase=ncase+1
   ndup = len(recs)
   fout.write('case %02d: sutra %s appears in %s records\n' %(ncase,sutra,ndup))
   for verbrec in recs:
    fout.write('%s\n' % verbrec.line)
   fout.write('\n')
 fout.close()
 print ncase,"duplicate sutra numbers"
 print "duplicates written to",fileout

if __name__ == "__main__":
 fileout = sys.argv[1]
 # set relative path to  directory containing verbdata.txt
 path='../function'
 sys.path.append(path) 
 import verbdata
 verbrecs = verbdata.init_Dhaval_verbdata('%s/verbdata.txt'%path)
 # Now verbrecs is a list of Dhaval_verbdata objects.
 # A typical element 'rec' of this list has
 # attributes rec.verbwithanubandha, etc.  
 print len(verbrecs)
 sutra_dup(verbrecs,fileout)

