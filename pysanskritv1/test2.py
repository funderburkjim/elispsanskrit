"""test2.py Begun May 14, 2016
   Tests of conjugation
Tracing info:
cd ~/Documents/GitHub/pysanskrit/
cp -r  pysanskrit tracetest
cd tracetest
Jim@JIM-DELL ~/Documents/GitHub/pysanskrit/tracetest
$ python -m trace --trace --ignore-module=init,re,sys,codecs,sandhi test2.py temp_conjugation_test.txt > temp

June 18, 2016. ForC_join1 is almost identical to perfect_join1.  How to avoid
 code duplication?
June 19, 2016. In class10_base, should 'kfp' be 'kxp' ?
"""
# for purpose of comparing Python and Elisp code, we will preserve
# likely errors in Elisp code.
preserve_elisp_errors=True
import init
import re
from sandhi import *
from util import flatten
from causal import *
from test1 import declension_join  # used in kta
from test1 import declension_get_sups # used in declension of pres participles
#from aorist import *

#from declension_general_1cons import *
# from declension_general_1cons import declension_general_1cons

class GlobalVars(object):
 """ contains a few variables which are global in the Elisp.
     This is an awkward practice.
 """
 liw_r_bitab = []
class MyException(Exception):
 """ suggested by 
   http://stackoverflow.com/questions/1319615/proper-way-to-declare-custom-exceptions-in-modern-python
 """
 pass


def construct_conjtab1a_gentense(root,theclass,pada,upasargas,tense,voice,dbg=False):
 """
 """
 err0="construct_conjtab1a_gentense(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 if dbg:
  print err0
 lc = tense[-1:] 
 if lc in '1234567':
  lcnum = int(lc)
 else:
  lcnum = 0
 if lc == 'p': # periphrastic perfect
  if periphrastic_liw_P(root,theclass):
   return conjugation_tab_liw_p(upasargas,theclass,pada,root,voice,dbg)
  err = err0+" ERROR 1" 
  raise NameError(err)
 #
 if lc == 'r': # reduplicative perfect
  if reduplicative_liw_P(root,theclass):
   return conjugation_tab_liw_r(upasargas,theclass,pada,root,voice,dbg)
  err = err0+" ERROR 2" 
  raise NameError(err)
 #
 if lcnum!=0: # aorist variety
  return conjugation_tab_aorist(upasargas,theclass,pada,root,lcnum,voice,dbg)
 if (tense == 'lfw'): # fut = simiple future
  return conjugation_tab_lfw(upasargas,theclass,pada,root,voice,dbg)
 if (tense == 'lfN'): # con = conditional
  return conjugation_tab_lfN(upasargas,theclass,pada,root,voice,dbg)
 if (tense == 'luw'): # pft = periphrastic future
  return conjugation_tab_luw(upasargas,theclass,pada,root,voice,dbg)
 if (tense == 'ASIrliN'): # ben = benedictive
  return conjugation_tab_ASIrliN(upasargas,theclass,pada,root,voice,dbg)
 err = err0+ "ERROR 3"
 raise NameError(err)

def class_a_base_irreg_dict():
 """ return a dictionary.
  Key has form headword_class
  value has form [base,...] (1 or more)
  Programming note: For efficiency, rearrange to do this initialization
  just once.
 """
 import codecs,os
 thisdir=os.path.dirname(os.path.realpath(__file__))
 #print "thisdir=",thisdir
 filename="class_a_base_irreg.txt"
 pathname = os.path.join(thisdir,filename)
 #print "pathname=",pathname
 with codecs.open(pathname,"r","utf-8") as f:
  lines = f.readlines()
 d = {} # returned
 for line in lines:
  line = line.strip()
  if line == '':
   continue
  if line.startswith((';','#')):
   continue
  try:
   (key,val) = line.split('=')
  except:
   tmp = "ERROR class_a_base_irreg_dict, file=%s\n"%filename
   tmp = tmp+"cannot parse line=%s" % line
   print tmp.encode('utf-8')
   exit(1)
  #(hw,theclass) = key.split(',')
  bases = val.split(',')
  d[key]=bases
 return d

def class_a_base_irreg(root,theclass,pada,dbg=False):
 """  Returns a list, even if only 1 base is returned.
  Returns empty list when NO irregular base is found
 """
 d = class_a_base_irreg_dict()
 key = "%s,%s" %(root,theclass)
 if key in d:
  return d[key] #.split(',')
 # a few cases, pada is part of key
 key = "%s,%s,%s" %(root,theclass,pada)
 if key in d:
  return d[key] #.split(',')
 # indicate not found by empty list 
 return []

def dhAtu_parts(root):
 """  functional equivalent of "dhaatu-parts"
 """
 x = word_parts(root)
 (parts,types)=x
 if 3 < len(parts):
  # 03-19-02 dealing with [s a M m aa n] where
  # parts = [[s] [a] [M m] [aa] [n]]
  # types = "cvcvc"
  # we want to return ([s a M m] [aa] [n] "cvc")
  # so want p1 = [[s a M m] [aa] [n]] t1 = "cvc"
  t1 = types[-3:]
  w = parts[-2:]
  u = parts[0:-2]
  u1 = u
  v = ''.join(u)  # string
  v1 = [v]
  p1 = v1 + w # concatenate string lists
 elif 3 == len(parts):
  p1 = parts
  t1 = types
 elif 2 == len(parts):
  t1 = types
  if types == 'cv':
   # this is tricky edge case.  
   # parts is a list of 2 strings, a constant and a vowel  (['B' 'U'])
   # We return a list of 3 strings, 
   # the last being the empty string (['B' 'U' ''])
   p1 = parts + ['']
  else: # types='vc'
   p1 = [''] + parts
 elif 1 == len(parts): 
  # must be a 1-vowel root, like 'f', 'i'
  t1 = types
  if types == 'v':
   p1 = ['']+parts+['']
  else:
   # types == 'c', should not happen
   p1 = parts + [''] + ['']
 # return a list of strings, by appending t1 to p1
 return p1 + [t1]

def kale_394(c1,v,c2,thetype,dbg=False):
 """
   ;  returns a possibly modified vowel
   ;   the following rule is due to Kale (sec. 394) and applies
   ;  to roots of conjugations 1,4,6,10. It applies when guna
   ;  (or vrddhi) has not applied
   ;  assume already tested that v = [RI] (long)
  ; Note: the condition Kale phrases as 
  ;   'a labial or ##va## precedes'
  ; is interpreted by the predicate labial-P, since I 
  ; include ##va## among the labials
 """
 if thetype == 'cvc':
  if c1 in init.labial_set: # python version of labial-P 
   return 'Ur'
  else:
   return 'Ir'
 elif thetype == 'cv':
  if c1 in init.labial_set:
   return 'ur'
  else:
   return 'ir'
 elif thetype == 'vc':
  return 'Ir'
 elif thetype == 'v':
  return 'ir'
 else:
  # this condition should never be reached
  raise NameError("kale_394(%s,%s,%s,%s) ERROR bad type=%s" %(c1,v,c2,thetype,thetype))

def kale_395(c1,v,c2,thetype,dbg=False):
 """
 ; returns a possibly modified vowel
   ;   the following rule is due to Kale (sec. 395) and applies
   ;  to roots of conjugations 1,4,6,10. It applies when guna
   ;  (or vrddhi) has not applied
   ;  assume already tested that 
   ;   a. v = [v0] and v0 is i u R^i or L^i
; we first check that c2
;  a. begins with either "r" or "v", and
;  b. is a compound consonant
 """
 if len(c2) == 0:
  # when c2 is not present, v is unchanged
  return v
 # in subsequent cases, c2 is not the empty string
 if not c2.startswith(('r','v')):
  # when c2 does not start with "r" or "v", v is unchanged
  return v
 # in subsequent cases, c2 begins with 'r' or 'v'
 if 1 < len(c2):
  # when c2 is a compound consonant, v is lengthened
  return lengthen_vowel(v)
 # otherwise, v unchanged
 return v


def class_a_base_1(root,dbg=False):
 """ functional equivalent of "dhaatu-a~Nga-1".
     following Antoine I.7 and Kale 388
 """
 err0 = "class_a_base_1(%s)" % root
 if dbg:
  print err0
 x = dhAtu_parts(root)
 (c1,v,c2,thetype) = x
 if thetype in ['cv','v']:
  # 1.  A final vowel takes guna
  v = guna(v)
 elif (v in 'iufx') and (1<len(c2)):
  # 2. penultimate i,u,f,x  may be modified 
  #    preceding a compound consonant beginning with r or v
  v = kale_395(c1,v,c2,thetype)
 elif (len(c2) == 1) and (v in init.shortsimplevowel_set):
  # 3. A short medial vowel takes guna
  #  a medial vowel is a vowel which stands between consonants
  #  Note: Kale refers to 'the penultimate' short vowel - i.e.,
  #  a vowel followed by a consonant, but not necessarily also
  #  preceded by a consonant - i.e. type = "VC".  The 
  #  following uses the Kale interpretation
  #  a short medial vowel is a medial vowel provided
  #    1) it is a short simple vowel
  #    2) the final consonant (c2) is not compound.
  # Note that when the length of c2 is non-zero, then the
  # type is either cvc or vc (it cannot be cv or v)
  v = guna(v)
 elif v == 'F':
  # 4. Vowel (long) 'F' is modified :
  #  For conjugation 1 verbs, this could not happen in types cv or v,
  #  as guna is already applied by case 1. However, it could happen
  #  with types cvc or vc as 'F' is not short
  v = kale_394(c1,v,c2,thetype)
 # in get_conj_elt_1, the next letter will be "a" or "A"
 # when the original root ends in a vowel whose
 # guna is "e" or "o", this may require a sandhi change
 # to v
 if thetype in ["cv","v"]:
  v = sandhi_internal_diphthong_a(v)
  return [c1 + v]
 else:
  return [c1 + v + c2]
 
def class_a_base_4(root,dbg=False):
 """ Elisp dhaatu-a~Nga-4
  Returns a list of strings, the list has length 1
 """
 err0 = "class_a_base_4(%s)" % root
 if dbg:
  print err0
 # following Antoine I.16 and Kale 389
 # 'y' is added to root
 root1 = root + "y"
 x =  dhAtu_parts(root1)
 c1 = x[0]
 v =  x[1]
 c2 = x[2]
 thetype = x[3]
 if (v in 'iufx') and (1 < len(c2)):
  # penultimate i u f x may be modified 
  # preceding a compound consonant beginning with "r" or "v"
  v = kale_395(c1,v,c2,thetype)
 elif (v == 'F'):
  # Vowel F may be modified
  v = kale_394(c1,v,c2,thetype)
 ans = c1 + v + c2
 # subsequent logic requires that a list be returned, not a string
 return [ans] 

def class_a_base_6(root,dbg=False):
 """ Elisp dhaatu-a~Nga-4
  Returns a list of strings, the list has length 1
 ; following Antoine I.23, Kale 390
 ; 05-13-04: brU has base bru (Whitney) when in class 6.
 """
 err0 = "class_a_base_6(%s)" % root
 if dbg:
  print err0
 root1 = root
 x =  dhAtu_parts(root1)
 c1 = x[0]
 v =  x[1]
 c2 = x[2]
 thetype = x[3]
 if (v in 'iufx') and (1 < len(c2)):
  # 1. penultimate i u R^i L^i may be modified 
  #    preceding a compound consonant beginning with "r" or "v"
  v = kale_395(c1,v,c2,thetype)
 elif len(c2)==0:
  # 2. final vowel may be changed
  v0 = v
  if v0 in 'iI':
   v = v + 'y'
  elif root=='brU':
   v = 'uv'
  elif v0 in 'uU':
   v = v + 'v'
  elif v0 == 'f':
   v = 'riy'
  elif v0 == 'F':
   v = 'ir'
 ans = c1 + v + c2
 # subsequent logic requires that return value be a list
 return [ans]

def class_a_base_10(root,dbg=False):
 """ = dhaatu-a~Nga-10 of Elisp.
; following Antoine I.32, Kale 391 p. 243
 ; tokar is assumed to be an array of alphabetical tokens
 ; the function returns a token array
 ; Kale's description of the 10th class member roots (p. 243, footnote)
 ;  This class contains a few primitive verbs, almost all the roots
 ;  belong to it being derivative; besides, all Causals and some Nominal
 ;  verbs may be regarded as belong to this class.
 ; Kale's description of base formation:
 ;  Roots of the 10th or 'churaadi' class add 'aya' before the personal
 ;  terminations. Before 'aya',
 ;   - the penultimate short vowel (except 'a') takes the guna substitute
 ;   - the final vowel and the penultimate 'a', not prosodially long, take
 ;     the vriddhi substitute
 ;  Note: By Kale 11 (p. 14), a short vowel is prosodially long when
 ;   followed by a conjunct consonant.
 """
 err0 = "class_a_base_10(%s)" % root
 if dbg:
  print err0
 root1 = root
 x =  dhAtu_parts(root1)
 c1 = x[0]
 v =  x[1]
 c2 = x[2]
 thetype = x[3]
 #print "dhAtu_parts=",x
 if thetype in ["cv","v"]:
  # 1. A final vowel takes vrddhi and may be subject to sandhi
  #    change before the affixation of "ay"
  v = vfdDi(v)
  v = sandhi_internal_diphthong_A(v)  # E -> Ay
 elif (len(c2) == 1) and (v == 'a'):
  # short 'a' not prosodially long
  # 2. A penultimate 'a' takes vrddhi
  # Note: Kale also has additional requirement 'not prosodially long'.
  # Later Note: (Kale 14 p. 11). Short vowels when followed by a
  # conjunct consonant are said to be 'prosodially long' ; e.g. the
  # 'a' in 'daRq'
  v = vfdDi(v)
 elif (v in 'iufx') and (1 < len(c2)):
  # 3a. penultimate i u R^i L^i may be modified 
  #   preceding a compound consonant beginning with "r" or "v"
  v = kale_395(c1,v,c2,thetype)
 elif (len(c2)==1) and (v in init.shortsimplevowel_set):
  # 4. penultimate short vowel (other than A) takes guna
  #  a short medial vowel is a medial vowel provided
  #    1) it is a short simple vowel
  #    2) the final consonant (c2) is not compound.
  # Note that when the length of c2 is non-zero, then the
  # type is either CVC or VC (it cannot be CV or V)
  v = guna(v)
 elif v == 'F':
  # 4. Vowel (long) F is modified (when it does not take guna or vrddhi)
  v = kale_394(c1,v,c2,thetype)
 # 'ay' is added to the adjusted word
 # This adjusted for example sAmaya.  
 # It could be that (a) sAmaya is not a real class 10 root or
 # (b) the analysis of this function is faulty.
 #  Currently, c1 = 'sAma', v='y', and c2='a' for this word.
 #print "chk: c1,v,c2=%s,%s,%s" %(c1,v,c2)
 ans = c1 + v + c2 + 'ay'
 if preserve_elisp_errors and (c2 == 'a'):
  if root == 'sAmaya':
   ans = 'sAmayAy' #'sAmayaay'
  else:
   ans = c1 + v + 'Ay'
 # usage requires that a list of strings be returned
 return [ans]

class Class10(object):
 """ some constants used in class10_base
 """
 unchanged = [
  # Kale 400 : these preserve the root unchanged
  "aG", # to sin
  "kaT", # to tell
  "kzap", # to send , to pass
  "gaR", # to count
  # NOTE : 'gal' ; U to filter , A to throw appears in
  # Kale 'dhaautkosha' as 10A, and lengthens 'a': gaalayate
  # Thus, I exclude it here
  # gal 
  "var", # choose , seek
  "Dvan", # sound
  "mah", # to honor
  "rac", # to compose
  "ras", # taste
  "rah", # forsake
  # 'raT' (shout) appears only as class 1 in dhaatukosha of Kale
  "pat", # go
  "stan", # thunder
  "svar", # blame
  "pad", # go
  "vaw", # separate
  "karR", # bore , pierce : This is prosodially long. so 'a' normally short
  # Chad ; conceal (This shows as lengthening 'a' - Why here?
  "cap", # grind , cheat
  "SraT", # be-weak
  # shlath ; be-weak (not in dhaatukosha or Apte dictionary)
  "vyay", # spend , give
  "spfh", # desire
  "mfg", # hunt
  # mRiSh ; bear ; 'marShayati - e' in dhaatukosha and Apte dictionary
  "guR", # invade , advise , multiply
  # kuuN  ; speak , converse ('kuuNay' is REGULAR)
  # grah ; take (not in dhaatukosha or Apte dictionary)
  "kuh", # astonish
  "sPuw", # break-open (Kale dhaatukosha shows 'sphoTayati')?
  "suK", # make-happy
  ]
 twoforms = [
  # in class 10these have two forms, one with the root vowel unchanged,
  # and one with the root vowel lengthened (per Kale Dhatukosha)
  "SaW", # (samyagavaBAzaRe, to speak ill or well, to deceive) SaWayati
         # (asaMskAragatyoH, to leave unfinished, to go) SAWayati
         # (SlAGAyAM, to flatter) SAWayate
  "paw", # (granTe, to clothe,to envelop) pawayati
         # (BAzAYaM vezwaRe ca, to speak, to cover) pAwayati
  "kal", # (gatO saMKyAne ca, to go, to count) kalayati
         # (kzepe, to throw) kAlayati
  "laj", # (prakASane, to appear) lajayati
         # (apavAraRe, to conceal) lAjayati
  "vas", # (snehacCedApaharaRezu, to love, to cut, to take away) (vAsayati
         # (nivAse, to dwell) vasayati
  "puw", # (saMsarge, to bind together) (puwayati)
         # (BAzAyAM dIptO ca, to speak, to shine, to reduce to powder) powayati
  ]


def class10_base(root,eng_def=None,dbg=False):
 """
 ; Given the token representation 'tok' of a root,
 ; construct a list of token arrays, representing 
 ; the base(s) (ending in '[a y]') of the root as
 ; formed for the 10th conjugational class.
 ; This incorporates the information of Kale  400 p. 253,
 ; where the root vowel is unchanged.
 ; Eng-def is assumed to be a list of definitions,
 ; each of which is itself a list of words
 """
 err0 = "class10_base(%s,%s)" % (root,eng_def)
 if dbg:
  print err0
 def1 = modify_eng_def(eng_def)
 if root in Class10.unchanged:
  ans = root + 'ay'
 elif root in Class10.twoforms:
  b1 = root + 'ay'
  b2 = class_a_base_10(root)
  if (root == 'SaW') and ("speak" in def1):
   ans = b1
  elif (root == 'SaW') and ("leave" in def1):
   ans = b2
  elif (root == "paw") and ("weave" in def1):
   ans = b1
  elif (root == "paw") and ("tear" in def1):
   ans = b2
  elif (root == "kal") and ("count" in def1):
   ans = b1
  elif (root == "kal") and ("throw" in def1):
   ans = b2
  elif (root == "laj") and ("shine" in def1):
   ans = b1
  elif (root == "laj") and ("conceal" in def1):
   ans = b2
  elif (root == "vas") and ("dwell" in def1):
   ans = b1
  elif (root == "vas") and ("love" in def1):
   ans = b2
  elif (root == "puw") and ("bind" in def1):
   ans = b1
  elif (root == "puw") and ("speak" in def1):
   ans = b2
  else:
   # Note this will be the case if 'def' is None
   ans = [b1,b2]
 elif (root == 'aMs'):
  b1 =  class_a_base_10(root)
  b2 = 'aMsApay'
  ans = [b1,b2]
 elif root == 'kfp':  #'kxp' ?
  b1 = class_a_base_10(root)
  b2 = 'kfpAy'
  ans = [b1,b2]
 else:
  ans = class_a_base_10(root,dbg=dbg)
 if not isinstance(ans,list):
  ans = [ans] # always return a list
 return ans

def modify_eng_def(eng_def):
 """ defined as modify-Eng-def in causal.el
  This Python version differs from the original.
  It assumes eng_def is a string or None.
  It returns a list of strings as the words of eng_def (or [] for None)
 """
 if eng_def == None:
  return []
 def1 = eng_def.split()
 #def1 = flatten(def1)
 return def1

def class_a_base(root,theclass,pada,dbg=False):
 """ 
  Returns base as a list of strings
 """
 err0 = "class_a_base(%s,%s,%s)" %(root,theclass,pada)
 if dbg:
  print err0
 base = class_a_base_irreg(root,theclass,pada,dbg=dbg)
 if len(base) != 0:
  return base
 if theclass == "1":
  return class_a_base_1(root,dbg=dbg)
 if theclass == "4":
  return class_a_base_4(root,dbg=dbg)
 if theclass == "6":
  return class_a_base_6(root,dbg=dbg)
 if theclass == "10":
  #return class_a_base_10(root)
  # this is confusing.  Although there is a class_a_base_10 function, it is NOT
  # called here. Rather class10_base calls class_a_base_10
  return class10_base(root,eng_def=None,dbg=dbg)
 return []

def benedictive_base(root,theclass,pada,upasargas,sewgen=None,dbg=False):
 """ Elisp benedictive_base in gram2-future.el
  pada should be 'a' or 'p' (Atmanepada or Parasmaipada)
  upasargas is a list.  In Python, we may test whether upasargas is NOT the
  empty list by 'if upasargas'
 """
 err0 = "benedictive_base(%s,%s,%s,%s,%s)"%(root,theclass,pada,upasargas,sewgen)
 if dbg:
  print err0
 tense = "ASIrliN"
 (parts,types) = word_parts(root)
 b = root
 nb = len(b)
 lc = b[-1:] # last char
 # Set pc to be penultimate character
 if nb == 1:
  pc = None
 else:
  if types in ["cvc","vc"]: 
   #e.g. pracC = "pr"+"a"+"cC"
   pc = parts[-2:-1]  # a list of length 1
   pc = pc[0] # string
  else:
   pc = b[-2:-1]
 # Modify 'b' as appropriate for this general tense
 # Major branch on pada
 if dbg:
  print "benedictive-base chk: parts=%s, types=%s,pada=%s,lc=%s,pc=%s,b=%s" %(parts,types,pada,lc,pc,b)
 if pada == 'p': 
  # Parasmaipada
  if (root == 'I') and upasargas:
   # Kale 588. 'I' shortens its 'I' when joined with prepositions
   b = b[0:-1]+'i'
  elif (root == 'Uh') and upasargas:
   # Kale 588. 'uuh' shortens its 'uu' when joined with prepositions
   # before weak terminations beginning with 'y', as is the
   # case in benedictive parasmaipada.
   b = 'u' + root[1:]
  elif lc == 'i':
   # Kale 581. final 'i' lengthened
   b = b[0:-1]+'I'
  elif lc == 'u':
   # Kale 581. final 'u' lengthened
   b = b[0:-1]+'U'
  elif root == 'f':
   # Kale 582. The root 'Ri' is gunated
   b = gunate_final_vowel(b)
  elif (lc == 'f') and (2 < nb):
   # Kale 582. Final 'f' preceded by conjunct consonant is gunated
   # NOTE: probably better to use (1<len(pc)) as 'conjunct consonant'
   # b is unchanged
   b = gunate_final_vowel(b)
  elif lc == 'f':
   # (and not preceded by conjunct consonant)
   #  Kale 581. Final 'f' changed to 'ri'
   b = b[0:-1]+'ri'
  elif lc == 'F':
   # Kale 581. Final 'F' becomes 'Ir' after non-labial
   # and becomes 'Ur' after labial or 'v'
   if (pc in init.labial_set) or (pc == 'v'):
    b = b[0:-1]+'Ur'
   else:
    b = b[0:-1]+'Ir'
  elif kale_584_P(root):
   # Kale 584. A penultimate nasal is dropped for these
   b = b[0:-2] + b[-1:]  # drop b[-2:-1]
  elif samprasarana_P(root,theclass) and (root != 'jyA'):
   # Kale 583. Roots capable of taking samprasaaraNa take it
   b = samprasarana(b)
   if dbg:
    print err0,"base via samprasarana=",b
  elif (root=='SAs'):
   # Kale 583 SAs substitutes 'i' for its vowel
   b = 'Siz'
  elif (root in ["dA","DA","mA","sTA","gE"]) or \
       ((root == 'pA') and (theclass == '1')) or \
       ((root == 'hA') and (theclass == '3')):
   # Kale 585. in these cases, the final 'A' changes to 'e'
   b = b[0:-1]+ 'e'
  elif (types == 'cv') and (lc in 'AeEo') and (1 < len(parts[0])):
   # last condition is that there is initial conjunct consonant
   # Kale 585. final 'A' (original or substituted), if it be
   # preceded by a conjunct consonant, is changed to 'e' optionally:
   b1 = b[0:-1]+'e'
   b2 = b[0:-1]+'A'
   b = [b1,b2]
   #print "FOUND THE BASE HERE",b
  elif (root == 'han'):
   # Kale p. 359 example
   b = 'vaG'
  else:
   # otherwise, no guna or vrddhi for benedictive Parasmaipada
   pass
 elif pada == 'a':
  # Atmanepada
  if not sewgen:
   sewgen = ForC_sewCode(root,theclass,pada,upasargas,tense,dbg=dbg)
  if (root == 'I') and upasargas:
   # Kale 588. 'I' shortens to 'i' when joined with prepositions
   b = b[0:-1]+'i'
  elif root == 'dfS':
   # By Kale p. 364 example
   # as 'dRish is 1 'P in dhaatukosha, this case only
   # occurs during the 'regular' passive formation
   # There is no gunation, so nothing to do
   pass
  elif (lc == 'f') and (sewgen == 'aniw'):
   # Kale 587 Final 'f' unchanged when 'i' not prefixed
   pass
  elif (lc == 'F') and (sewgen == 'aniw'):
   # Kale 587 When 'i' is not prefixed,
   # final 'F' preceded by a labial or 'v' is changed to 'Ur';
   # otherwise, it is changed to 'Ir'
   if (pc in init.labial_set) or (pc == 'v'):
    b = b[0:-1]+'Ur'
   else:
    b = b[0:-1]+'Ir'
  elif lc == 'A':   
   # don't gunate final 'A' - leave it unchanged
   pass
  elif (types=='cv') and (lc in 'eEo'):
   # Kale 585. final 'e', 'E', 'o' become 'A'
   b = b[0:-1]+'A'
  elif (root == 'Brasj'):
   # Kale 464. The root 'Brasj' (6 P 'to fry') assumes the
   # forms 'Brasj' and 'Barj' in the non-conjugational tenses
   # PYTHON NOTE: The previous line says 'Brasj', but next line says 'Brajj'?
   b = ['Brajj','Barj']
  else:
   # otherwise, the radical vowel takes its guNa substitute
   b = gunate_final_vowel(b)
 else:
  # ? Is this an error condition that needs to be 
  err = "benedictive_base(%s,%s,%s,%s,%s)\n" % (root,theclass,pada,upasargas,sewgen)
  err = "  Pada error. pada = %s" % pada
  NameError(err)
 if dbg:
  print err0,"RETURNS",b
 return b

class ForC(object):
 # ForC_sym is a global variable (defined in gra2-future.el)
 # It represents the conjugation tense in question, and, if set,
 # is one of 'lfw','lfY','luw','ASIrliN'
 ForC_sym = None

def ForC_sewCode(root,theclass,pada,upasargas,tense=None,dbg=False):
 """
  pada should be 'a' or 'p' (Atmanepada or Parasmaipada)
 """
 err0 = "ForC_sewCode(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense)
 if not tense:
  tense = ForC.ForC_sym
 if (tense == 'ASIrliN') and (pada == 'p'): 
  temp = 'aniw'
 elif (root == 'buD') and (pada == 'a'):  
  # example Kale p.366
  temp = 'sew'
 else:
  if (pada == 'passive'): # passive
   temp = construct_sew_code1a(root,theclass,'a',upasargas,dbg=dbg)
  else:
   temp = construct_sew_code1a(root,theclass,pada,upasargas,dbg=dbg)
 ans = solution(temp)
 if dbg:
  err0 = "ForC_sewCode(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense)
  print err0," => ",ans
 return ans

def construct_sew_code1a(root,theclass,pada,upasargas,dtype=None,dbg=False):
 """
; This is from Antoine2#134. Compare Kale#458
; (1) First general rule.
;   The following classes of roots are 'seT' (insert 'i'):
;     All roots of the 10th conjugation and
;     all roots of the derivative conjugations (#144),
;     all roots ending with consonants,
;     all roots ending with the long vowel 'RI'
;     all roots ending with the long vowel 'uu'
;  Kale #458. (a): Roots of more than one syllable.
; (2) Second general rule.
;     The following classes of roots are 'aniT' (do not insert 'i'):
;     All roots of the first 9 conjugations ending in vowels other
;      than the long vowels 'RI' or 'uu'.
; The following list of 102 roots comprise exceptions to (1),
; in that they are 'aniT' (do not insert 'i')
; (3) Third general rule.
;     The following roots are 'veT' (optionally insert 'i'):
;  NOTE: Kale has a much longer list (see below)
; Based on Kale (p. 296-7), the following roots are 'veT'
; Here is the verse from Kale:
; svaratiH suuyate suute pa~nchame navame cha dhu~n .
; tanaktirvRishchatishchaantaavanaktishcha tanaktinaa .. 1 ..
; maarShTi maarjati anteShu daantau klidyatisyandate .
; radhyatiH sedhatirdhaantau paantaaH pa~nchaiva kalpate .. 2 ..
; gopaayatistRipyatishcha trapate dRipyatistathaa .
; maantau kShaamyatiH kShamate.ashnute klishnaati nashyati .. 3 ..
; shaantaasrayothaakShatishcha niShkuShNaatishcha takShatiH .
; tvakShatishcha Shakaaraantaa hyatha haantaashcha gaahate .. 4 ..
; padadvaye guuhatishcha Rikaaroopaantyagarhate .
; tRihatitRiMhatidruhyatayo vRihatimuhyatii .. 5 ..
; vRihistRihii snihyasnuhyaavete veTkaa hi dhaatavaH .
; ajantaanaaM tu thalyeve ved syaadanyatra sarvadaa .. 6 ..
; NOTE: These couplets ass well as those bearing on the 2nd variety
; of the Aorist are composed by Mr. ChintaamaNa Aatmaarama Kelkar, the
; present learned Shaastri at the Poona Training College.

; ADDENDA: Some changes:
;  vah:  from aniT to veT (ref Whitney, example Scharf)
; shii: removed from 'seT-exceptions', put in 'veT-exceptions' (Whitney)
 """
 err0 = "construct_sew_code1a(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,dtype)
 if dbg:
  print err0
 sew_exceptions_string = \
  "daridrA Sri Svi qI yu ru nu snu kzu kzRu UrRu jAgf vf"
 sew_exceptions = sew_exceptions_string.split(' ')
 aniw_exceptions_string = \
  "Sak pac muc ric vac vic sic pracC tyaj nij Baj BaYj Buj Brasj masj muj yaj yuj raYj ruj vij sfj saYj svaYj ad kzud Kid Cid tud nud pad Bid vid Sad sad skand svid kruD kzuD buD banD yuD ruD vyaD SuD sAD siD man han Ap kzip tap dfp lip lup vap Sap sfp svap yaB laB raB gam nam yam ram kruS daMS diS dfS mfS riS ruS liS viS spfS kfz tuz tviz duz dviz piz puz mfz viz Siz Suz Sliz Gas vas dah dih duh nah ruh lih mih"
 aniw_exceptions = aniw_exceptions_string.split(' ')
 vew_exceptions_string=\
 "raD muh druh snih naS luB sah kft nft prI Df kxp syand tfp klid kliS kzam gAh gup guh takz trap tfh mfj vraSc svf kuz gfh stfh bfh vfh vah SI"
 vew_exceptions = vew_exceptions_string.split(' ')
 if (theclass == '10'):
  ans = 'sew'
 elif (dtype == 'c'): #causal
  ans = 'sew'
 elif (root in sew_exceptions):
  ans = 'sew'
 elif (root in vew_exceptions):
  ans = 'vew'
 elif (root in aniw_exceptions):
  ans = 'aniw'
 else:
  lc = root[-1:] # last char
  (parts,types) = word_parts(root)
  if (lc in init.consonant_set):
   ans = 'sew'
  elif (lc in 'FU'):
   ans = 'sew'
  elif (3 < len(types)): 
   # word of more than 1 syllable
   ans = 'sew'
  else:
   ans = 'aniw'
 if dbg:
  err = err0 + (" => %s" % ans)
  print err # dbg
 return ans

def kale_584_P(root):
 rootlist = [
  "aYc","aYj","BaYj","raYj","saYj","svaYj",
  "granT","manT", "und","skand","syand",
  "inD","banD","damB","stamB",
  "daMS","BraMS","sraMs","tfMh"
 ]
 return root in rootlist

def samprasarana_P(root,theclass):
 # Kale 502, p. 308
 rootlist = [
  "vac","yaj","vap","vah",
  "vad","svap","vaS","vyac",
  "pracC","vraSc",
  "grah","vyaD",
  "ve","hve","Svi",
  "vye","jyA",
  "Brasj"
 ]
 if root in rootlist:
  return True
 if (root == "vas") and (theclass == "1"):
  return True
 return False

def samprasarana(word):
 """ Kale 502, p. 308"""
 (parts,types) = word_parts(word)
 nparts = len(parts)
 c1 = None
 v = None
 if 1<nparts:
  c1 = parts[0]
  v = parts[1]
 c2 = None
 if 2<nparts:
  c2 = parts[2]
 if c1:
  c1a = c1[0:-1] # all but last
  c1b = c1[-1:]  # last
  sv = c1b[0]  # first
 dtemp = {'y':'i', 'r':'f', 'v':'u'}
 if sv in dtemp:
  sv_vowel = dtemp[sv]
 else:
  sv_vowel = None
 # Preliminaries are done. Get the answer
 if word == 'Brasj':
  ans = 'Bfjj'
 elif word in ['ve','hve','Svi']:
  ans = c1a+'U'
 elif word in ['vye','jyA']:
  ans = c1a + 'I'
 elif sv_vowel:
  c2a = c2 + 'a'
  ans1 = conjugation_join(sv_vowel,c2a)
  ans1 = ans1[0:-1] # remove final 'a'
  ans = c1a + ans1
 else:
  # samprasarana not applicable. return original word
  ans = word
 return ans

def construct_conjpassbase1a(root,theclass,pada,upasargas,dbg=False):
 """ 
  Assume upasaragas is a list.
  Note theclass is a string (representing an integer 1...10)
; Kale 591.
; (a) 'ya' is added to the root, which is weak, i.e., no
;    guna or vrddhi substitute takes place before it
; (b) Before 'ya', roots undergo the same changes as they
;    do before the P terminations of the Benedictive
;   NOTE: There are some exceptions to this
;   - han : in benedictive, base is 'vagh', but passive base is 'hany'
; (c) The final 'aa' (original or substituted) changes to 'ii' in cases:
;     'daa' (to give) (class = 1,3)
;     'de do dhaa dhe maa gai so'
;     'paa' (to drink) (class = 1)
;     'haa' (to abandon) (class = 3, pada=P)
; Kale 593 
; (a) The roots 'khan jan tan san' optionally drop their 
;   'n' and at the same time lengthen their 'a'
; (b) 'shii' (to lie down) -> shayy
;     'shvi' -> shuuy
; (c) 'uuh' shortens its 'uu' when a preposition is prefixed to it
; (d) 'bruu' uses the passive of 'vach'
;     'as' (class=2)uses the passive of 'bhuu'
;     'ghas' uses the passive of 'ad'
;     'aj' uses the passive of 'vii'
; ADDENDA:
;  'sthaa' -> 'sthiiy'
;  'jyaa' -> 'jiiy'
;  'pai' -> paay
;  'pan' -> pany, panaayy
;  'paN' -> paNy, paNaayy
;  'gup' -> gupy gopy gopaayy
;  'kam' -> kam, kaamy
;  'Rit' -> Rity, Ritiiy
;  'vichCh' -> vichChy, vichChaayy
;  'div' -> 'diivy'
;  'han' -> 'hany'
; based on Antoine2 (appendix class 10),
; class 10 usually forms its passive base
; by (a) forming the class10 conjugational base,
;    (b) changing the ending 'ay' to 'y'
 """
 err0 = "construct_conjpassbase1a(%s,%s,%s,%s)"%(root,theclass,pada,upasargas)
 if dbg:
  print err0
 special_passive={
  'SI':'Say', 'Svi':'SU',
  # 'Uh':'uh', # if upasargas present
  'brU':'uc',
  # 'as':'BU', # theclass=2
  'Gas':'ad',
  'aj':'vI',
  'jyA':'jI',
  'sTA':'sTI',
  'pE':'pA',
  'pan':['pan','panAy'], 
  'paR':['paR','paRAy'], 
  'gup':['gup','gop','gopAy'],
  'kam':['kam','kAm'],
  'ft':['ft','ftI'],
  'vicC':['vicC','vicCAy'],
  'div':'dIv',
  'han':'han',
 }
 if root in ['Kan','jan','tan','san']:
  base = [root,root[0:-2]+'A']
 elif root in special_passive:
  base = special_passive[root]
 elif (root == 'Uh') and upasargas:
  base = 'uh'
 elif (root == 'as') and (theclass == '2'):
  base = 'BU'
 elif (theclass == '10'):
  cb = construct_conjbase1a(root,theclass,pada,upasargas,dbg)
  # cb is a list of strings
  def fdropay(b):
   return b[0:-2]  # drop the ay
  base = map(fdropay,cb)
 elif (root in ('de','do','DA','De','mA','gE','so')) or\
      ((root == 'dA') and (theclass in ['1','3'])) or\
      ((root == 'pA') and (theclass == '1')) or\
      ((root == 'hA') and (theclass == '3') and (pada == 'a')):
  # replace ending vowel with 'I'
  base = root[0:-1]+'I'
 else:  # the default construction of passive base
  pada1 = 'p' # parasmaipada
  base = benedictive_base(root,theclass,pada1,upasargas,dbg=dbg)
  #This will be a list of length 2 only in case of
  #Kale 585. final 'A' (original or substituted), if it be
  #preceded by a conjunct consonant, is changed to 'e' optionally:
  #In this case, we drop the 1st element (with the 'e')
  if isinstance(base,list) and (len(base)==2):
   base = base[1]
  # some 'A'  words go to a single 'e'. We change these back
  if root.endswith('A') and isinstance(base,basestring) and base.endswith('e'):
   base = base[0:-1]+'A'
 if not isinstance(base,list):
  # make it a list
  base = [base]
 ans = map(lambda b: b+'y',base)
 return ans

def construct_conjbase1a(root,theclass,pada,upasargas,voice=None,dbg=False):
 if dbg:
  print "construct_conjbase1a(%s,%s,%s,%s,%s)"%(root,theclass,pada,upasargas,voice)
 if voice == 'passive':
  base = construct_conjpassbase1a(root,theclass,pada,upasargas,dbg)
 elif theclass in ["1","4","6","10"]:
  base = class_a_base(root,theclass,pada,dbg=dbg)
 elif theclass == "2":
  # 06-10-04 What weirdness is this?
  # Normally, upasargas enters as the empty list []
  # so we are returning a 1-element list [[]]
  # This just so happens to mesh with the details of 
  # construct_conjtab1a_spcltense
  base = [upasargas]
 else:
  base = class_b_base(root,theclass,pada,upasargas,dbg)
 return base

def class_b_base(root,theclass,pada,upasargas=None,dbg=False):
 """ Return a list of strings
 """
 if dbg:
  print "class_b_base(%s,%s,%s,%s)"%(root,theclass,pada,upasargas)
 tok = root
 lc = root[-1:] 
 if  1 < len(tok):
  pc = tok[-2:-1] # penultimate
 else:
  pc = ''
 if theclass == '5':
  if root == 'Sru':
   tok = 'Sf' #Antoine2#16 p.11
 elif theclass == '9':
  if (pc in init.nasal_set) and (not (lc in init.vowel_set)):
   # Antoine2#27. Roots having a penultimate nasal drop it
   # Note: this includes 'j~naa' of #29 
   tok = tok[0:-2]+lc
  elif root == 'jyA':
   tok = 'ji' # Antoine2#29
  elif root == 'grah':
   tok = 'gfh' # Antoine2#29
  elif root == 'jYA':
   tok = 'jA' # Antoine2#29
  elif kale_414_P(root):
   lc = shorten_vowel(lc)
   tok = tok[0:-1]+lc
  elif kale_414_opt_P(root):
   lc = shorten_vowel(lc)
   tok1 = tok[0:-1]+lc
   if preserve_elisp_errors:
    pass
   else:
    tok = [tok,tok1] # a list
  elif (lc in init.longsimplevowel_set) and (not (root in ['krI','prI'])):
   lc = shorten_vowel(lc)
   tok = tok[0:-1]+lc
   if not (root in ['lI','DU','pU','lU','dF','SF','stF']):
    if dbg:
     print "class_b_base (class %s) Root %s should be checked" %(theclass,root)
 #
 if not isinstance(tok,list):
  tok = [tok]
 return tok

def kale_414_P(root,dbg=False):
 """
; Kale 414. 
; The following roots of the 9th class have their finals
; shortened necessarily in the Special Tenses (laT, etc):
;  rii lii vlii plii dhuu puu luu RI kRI gRI jRI nRI
;  pRI bhRI mRI vRI shRI stRI
; Antoine2#29: 'Several' roots ending with long vowels shorten
; their final vowel. Examples are 'lii', 'dhuu', 'puu',
; 'luu', 'dRI', 'shRI', 'stRI'.
; NOTE: Antoine has 'dRI' but Kale does not. 
;  However, in example under #690, 'dRI' appears to be included
 """
 err0 = "kale_414_P(%s)" % root
 return root in [
  "rI","lI","vlI","plI","DU","pU","lU",
  "F","kF","gF","jF","nF","dF",
  "pF","BF","mF","vF","SF","stF"
 ]

def kale_414_opt_P(root,dbg=False):
 """
; Kale 414
; The following roots have their finals shortened optionally:
;  kShii bhrii vrii
 """
 err0 = "kale_414_opt_P(%s)" % root
 return root in [
  "kzI","BrI","vrI"
 ] 
 NameError(err0 + " NOT IMPLEMENTED")


def conj_endings(tense,theclass,pada,dbg=False,strengths=False):
 """ This is retrieved from a dictionary init.Vsup_data.d
     Since sometimes the array of endings is mutated by a user routine,
     we construct a copy.
 """
 if theclass in ["1","4","6","10"]:
  conj_class = 1
 else:
  conj_class = 2
 # May 22, 2016. A Kluge. If 'pada' is 'm', then we change it to 'a' in the
 # following. 
 if pada == 'm':
  apada = 'a'
 else:
  apada = pada
 name = "%s-%s-%s" %(tense,conj_class,apada)
 if strengths:
  name = name + "-strengths"
 if name in init.Vsup_data.d:
  rec = init.Vsup_data.d[name]
  endings = rec.endings
  # this is the technique used to copy:
  import copy
  endings = copy.copy(endings)
  return endings
 err="NOT FOUND: conj_endings(%s,%s,%s,%s)"%(tense,theclass,pada,strengths)
 raise NameError(err)

def conjugation_join1(base,sup,option=None,dbg=False):
 """ from gram2.el
  Assume base, sup are strings. Return a string 
  Very similar to declension_join
 """
 sandhiget = SandhiGet(['Antoine72-4'])
 ans = None
 x = sandhiget.sandhi_pair(base,sup,'internal','join')
 """
 if len(x)==1:
  ans = x[0][0]
 """
 # x might be an empty list
 if x:
  ans = solution(x)  #[['Iwwe']] => 'Iwwe'
 if ans == None:
  x=sandhiget.sandhi_pair(base,sup,None,'join')
  """
  if len(x) == 1:
   ans = x[0][0]
  """
  if x:
   ans = solution(x)
 if ans == None:
  ans = base + sup 
 if not (option == 'not-n-R'):
  ans1 = sandhi_single(ans,False)
  if ans1:
   ans = ans1
 return ans

def conjugation_join(base,sup,option=None,dbg=False):
 """ from gram2.el
  base is either a string, or a list of strings
  similar with sup.
  Return a comma-delimited string
  Option can also be 'not-n-R'
 """
 if isinstance(sup,list):
  def f(s):
   return conjugation_join(base,s,option,dbg)
  anslist = map(f,sup)
  return ','.join(anslist)
 if isinstance(base,list):
  def f(b):
   return conjugation_join(b,sup,option,dbg)
  anslist = map(f,base)
  return ','.join(anslist)
 # assume sup and base are strings
 return conjugation_join1(base,sup,option,dbg)


def conjugation_tab_1(base,tense,theclass,pada,dbg=False):
 """ from gram2.el
 """
 endings = conj_endings(tense,theclass,pada,dbg)
 #if dbg:
 #print "conj_endings(%s,%s,%s) = %s"%(tense,theclass,pada,endings)
 n = len(endings)
 i1 = 0
 i2 = n
 # perform certain adjustments (these could be stored)
 for i in xrange(0,n):
  ending = endings[i]
  if ending == '':
   u = ''
  else:
   u = ending[0]
  if u in ['m','v']:
   y = 'A'
  elif u in ['a','e']:
   # "a" dropped before "a"
   # "a" dropped before "e" (Antoine I.40)
   y = ''
  elif u == '':
   y = 'a' # e.g. imperative P 2 S
  else:
   y = 'a' # the usual, default case for the A-conjugations
  endings[i] = conjugation_join(y,ending)
 #--- for imperfect, the augment 'a' must be prefixed to base
 if tense == 'laN':
  # prefix augment, which forms vrddhi with an initial vowel
  u = base[0:1]
  if u in init.vowel_set:
   base0 = vfdDi(u) + base[1:]
  else:
   base0 = 'a' + base
 else:
  base0 = base
 # join the parts for each ending
 ans = []
 for i in xrange(0,n):
  ans1 = conjugation_join(base0,endings[i])
  ans.append(ans1)
 #tmp = "conjugation_tab_1(%s,%s,%s,%s) => %s"%(base,tense,theclass,pada,ans)
 #print tmp
 return ans


def conjugation_tab_2(base,tense,theclass,pada,root,dbg=False):
 """ from gram2.el
  'base' here <-> 'a~Nga-sym' in elisp conjugation-tab-2
 """
 err0="conjugation_tab_2(%s,%s,%s,%s,%s)"%(base,tense,theclass,pada,root)
 if dbg:
  print err0
 upasargas = base # a list, possibly empty
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. 
 atok = root
 dhatu_ending = root[-1:] # last char
 ibeg = 0 # position of 1st char
 # 4. for imperfect, the augment 'a' must be prefixed to atok
 if tense == 'laN':
  u = atok[0] # first char
  if u in init.vowel_set:
   atok = vfdDi(u) + atok[1:]
  else:
   atok = 'a' + atok
   ibeg = 1 # change position of 1st char
 # 5. combine base and endings to get ans
 #   45.2 Final and short medial vowels take guna in the strong forms
 #   48: Final 'aa' does not take guna (it remains 'aa' in all forms)
 (parts,types) = word_parts(atok)
 # Get atok
 if atok.endswith('A'):
  stok = atok # strong
 elif types.endswith('v'): 
  # root ends in vowel
  p1 = parts[0:-1] # all but last part
  p2 = parts[-1:]  # last part
  v0 = p2[0] # the ending vowel
  v = guna(v0)
  stok = ''.join(p1) + v
 elif types.endswith('vc'):
  p1 = parts[0:-2] # all but last two
  v0 = parts[-2:-1][0] # the vowel
  p3 = parts[-1:][0] # the ending consonant
  # only gunate when v0 is a short simple vowel AND
  # the final consonant is not compound
  if root == 'mfj':
   # 59. mRij takes vrddhi (not guna) in strong form
   v = vfdDi(v0)
  elif (len(p3) == 1) and (v0 in init.shortsimplevowel_set):
   v = guna(v0)
  else:
   v = v0
  stok = (''.join(p1)) + v + p3
 else:
  stop = atok
 # Get wtok
 wtok = atok 
 # join endings with stok/wtok to get ans.
 # Very complicated with many special situations
 for i in xrange(0,n):  # Recall n is number of endings, i.e., 9
  strength = strengths[i].lower()  
  if strength == 's':
   atok = stok
  else:
   atok = wtok
  atok0 = atok[0:-1] # all but last char
  asave = atok[-1:] # last char
  dasave=de_aspirate(asave) #deaspirated version of last char
  abeg = atok[ibeg] # first char 
  aspabeg = aspirate(abeg) # aspirated version of 1st char
  a = asave
  ending = endings[i]
  e = ending[0] # first char of ending
  base = atok  # a copy
  ending0 = ending
  base0 = base
  if dbg:
   print "i=%s,strength=%s, ending=%s" %(i,strength,ending)
   print "atok=%s,atok0=%s,asave=%s,\ndasave=%s,ibeg=%s,abeg=%s,aspabeg=%s,a=%s,e=%s" %(atok,atok0,asave,dasave,ibeg,abeg,aspabeg,a,e) 
  if (strength=='w') and (e in init.vowel_set):
   #36. Before weak terminations beginning with a vowel, the
   # final 'i' and 'u', short or long,
   # are changed respectively to 'iy' and 'uv'
   if (root == 'i'):
    # Kale 426, p. 270.
    # The 'i' of the root 'i' P is changed to 'y' before a
    # weak vowel termination. But, 'i' with 'adhi' ('A) is
    # regularly conjugated
    if (not upasargas) and (pada == 'p'):
     base = atok[0:-1] + 'Ay'
    else:
     base = atok + 'y'
   elif a in 'iI':
    base = atok0 + 'iy'
   elif a in 'uU':
    base = atok0 + 'uv'
  elif (e in init.vowel_set):
   # no other considerations if termination is a vowel
   pass
  else:
   ### This is very tricky - a peculiarity of Elisp. How to do in Python??
   #  Elisp allows arbitrary code at this point. And then additional
   #  conditions in the main 'if elif' controls continue.
   #  To accomplish the same in Python, I'll make a major 'else' clause,
   #  and then subsidiary if..elif. clauses
   ### 
   # #51. For verbs ending in 'u' (like 'nu'), the 'u' takes
   # vrddhi before strong terminations beginning with a consonant
   # (if it were regular, then the 'u' would be gunated.)
   if (dhatu_ending == 'u') and (strength == 's'):
    a = 'O'
    base = atok0 + a
   if (e in init.nasal_set) or \
      ((e in init.semivowel_set) and (ending != 'hi')):
    # #37. Before terminations beginning with a nasal or a semi-vowel,
    # (other than 'hi')
    # the consonant sandhi offers no special difficulty
    pass
   # Note: from here on, 'e' is a consonant but not a nasal or semivowel
   else:
    #Similar to above Elisp/Python. Execute some code, and indent the 
    #rest of the elif clauses
    if a=='h':
     ##40. Before terminations beginning with a consonant except a
     #  nasal or a semi-vowel,
     #  the final 'h' of a verbal base is changed to 'Dh'.
     #  When, however, the verbal root begins with 'd', the final
     #  'h' is changed to 'gh'
     if abeg == 'd':
      a = 'G'
     else:
      a = 'Q' ### NEW
     asave = a # last char
     dasave = de_aspirate(asave)
     base = atok0 + a
     atok = base
     if dbg:
      print "RESETTING: a=%s,asave=%s,dasave=%s,base=%s,atok=%s" %(a,asave,dasave,base,atok)
    if (ending == 'hi') and (a in init.consonant_set):
     # #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
     # the 2nd pers. singular imperative parasmaipada is changed
     # to 'dhi' when the verbal base ends with a consonant
     # except a nasal or a semi-vowel
     # Note: This change is sometimes modified by #42. Thus,
     # I implement in such a way as to change the ending, but
     # have the condition fail, so further change possibilities
     # will be examined
     ending = 'Di'
     e = 'D'
    if (tense == 'laN') and (pada == 'p') and (a in init.consonant_set) and\
       (ending in ['t','s']):  # 3s or 2s
     # #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
     # imperfect parasmaipada are dropped after a verbal base ending
     # with a consonant. The final consonant of the base is then treated
     # according to the rule given in Part I-72(3).
     if (dasave != asave):
      #1. If 'a' is aspirate, deaspirate it
      #  and if 'a' is a soft aspirate, after losing the aspiration,
      #  throw it back if possible on previous syllable. This is like
      #  39.1 and 2 below.
      a = dasave
      base = changestringitem(base,ibeg,aspabeg)
      if dbg:
       print "RESETTING: a=%s,base=%s, ibeg=%s, aspabeg=%s" %(a,base,ibeg,aspabeg)
     if ending == 't': 
      # 2. case of 3s
      ending = ''
      if a == 's':
       base = atok0 + 't'
      else:
       base = sandhi_legalise_final_cons(base) # a list
     elif ending == 's':
      # 3. case of 2s
      ending = ''
      basetmp=base
      base = sandhi_legalise_final_cons(base) #  a list
      if dbg and (base!=basetmp):
       print "base changed from %s to %s" %(basetmp,base)
      if a == 'd':
       base = append_if_new(base,atok+'H')
      elif a == 's':
       base = append_if_new(base,atok0+'t')
     # resolve base to a string, if possible
     base = solution(base)
     # end if (tense == 'laN') and (pada == 'p') ...
    elif (a == 'Q') and (e in 'tTD') and (len(ending)!=1):
     ##42. Before terminations beginning with 't' 'th' and 'dh',
     # the final 'Dh' of a verbal root is dropped, while the following
     # 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
     # vowel is lengthened.
     # NOTE: 'beginning' with does not mean 'equal'; i.e. this not applicable
     # to 3S of imperfect
     e = 'Q'
     ending = e + ending[1:]
     v = atok0[-1:] # preceding vowel
     v = lengthen_vowel(v)
     if 1<len(atok0):
      atok1 = atok0[0:-1] # all but last
     else:
      atok1 = ''
     base = atok1 + v
    elif (a in 'Qz') and (e == 's'):
     ##41. Before terminations beginning with 's' (i.e., before
     # 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
     a = 'k'
     base = atok0 + a
    elif (dasave != asave): # so 'a' is an aspirate
     ##39.1 The final aspirate of a verbal base loses its aspiration
     ##39.2 A soft aspirate, after losing its aspiration, throws it back,
     #   if possible, on the previous syllable
     ##39.3 The previous rule does not apply before the termination
     #   'dhi' of the 2nd pers. sing. imperative parasmaipada
     #   (Note: implemented indirectly by #34 above)?????
     ##39.4 It does not apply either before terminations beginning with
     #   't' or 'th', in which case the lost aspiration is thrown forward
     #   on the following 't' or 'th' which are softened (to 'dh')
     # NOTE: this IS applicable to 3S of imperfect whose ending = 't'
     a = dasave # 'a' loses aspiration
     if (i == 5) and (e == 'D'):
      # 2p
      base = changestringitem(base,ibeg,aspabeg)
      #base[ibeg]=aspabeg
      base = base[0:-1] +a
     elif e in 'tTD':
      e = 'D'
      ending = e + ending[1:]
      base = atok0 + a
     else:
      base = changestringitem(base,ibeg,aspabeg)
      #base[ibeg]=aspabeg
      base = base[0:-1]+a
    elif (a == 's') and (e in init.soft_set) and (e in init.dental_set):
     ##43. The final 's' of a verbal base is dropped before
     # soft dentals
     base = base[0:-1]
    else:
     ##44. When a verbal root ends with a conjunct consonant having
     # 'k' or 's' for its first member, it drops that 'k' and 's'
     # before a termination beginning with a consonant except a
     # nasal or a semi-vowel.
     p2 = parts[-1:][0] # last part of atok
     if (1 < len(p2)) and p2.startswith(('k','s')):
      p2b = p2[1:] # all but initial letter
      base = ''.join(parts[0:-1]) + p2b
  # end of 'if (strength=='w') and (e in init.vowel_set):'
  # thisans could be a list, since base could be a list
  thisans = conjugation_join(base,ending)  
  if dbg:
   print "before irregs, i=%s: (%s,%s) => thisans=%s" %(i,base,ending,thisans)
  #
  # ----- irregularity overrides
  #
  if (i == 2) and (tense == 'laN') and (pada == 'p') and\
     (a == 'A') and (root != 'daridrA'):
   #48. verbs ending in 'A': 3P imperfect Parasmaipada ending
   thisans1 = atok0 + 'uH'
   thisans = [thisans,thisans1]
  elif (root == 'i') and (i == 2) and (pada == 'p') and (tense in ['law','low']):
   ##49 'i' (to go) is regular except for the 3P present and imperfect
   # parasmaipada which are 'yanti' and 'yantu' (rather than
   # 'iyanti' and 'iyantu'
   thisans = thisans[1:] # drop initial 'i'
  elif (root in ['stu','tu','ru']) and (e in init.consonant_set):
   ##51. The three verbs 'stu' (to praise), 'tu' (to grow), and
   # 'ru' (to sound) are conjugated like 'nu', but they optionally
   # insert 'ii' before all terminations beginning with a consonant
   ending1 = conjugation_join('I',ending)
   if strength == 's':
    base = atok0 + 'o' # undo #51
   else:
    base = atok0 + 'uv' # do #36
   thisans1 = conjugation_join(base,ending1)
   thisans = [thisans,thisans1]
  elif (root == 'SI'):
   ##52. 'SI' (2A) (to lie down) takes guna all through. The 3P of
   # present, imperfect, and imperative insert 'r' before ending.
   base = stok # gunated form
   if (i == 2) and (tense in ['law','laN','low']):
    thisans = conjugation_join(base,'r'+ending)
   else:
    thisans = conjugation_join(base,ending)
  elif (root == 'brU') and (strength == 's') and (e in init.consonant_set):
   ##53. 'bruu' (2P/2A) (to speak) inserts 'ii' before strong terminations
   # beginning with a consonant
   thisans = conjugation_join(base,conjugation_join('I',ending))
  elif (root == 'ad') and (tense == 'laN') and (i in [0,3]): # 3s,2s
   ##55. 'ad' (2P) (to eat) is regular except in the 2nd and 3rd pers. sing.
   # imperfect where it inserts 'a' before the terminations 's' and 't'.
   # 3S = aadat, 2s = aadaH
   ending = endings[i]
   base = atok
   thisans = conjugation_join(base,conjugation_join('a',ending))
  elif (root == 'dviz') and (tense == 'laN') and (pada == 'p') and (i == 2):
   ##58. 'dviSh' (2P/A) (to hate) optionally takes the termination 'us' in
   # 3P imperfect parasmaipada.
   thisans1 = base + 'uH'
   thisans = [thisans,thisans1]
  elif (root == 'dviz') and (a == 'z') and (e == 'D'):
   ##58a. In both Antoine and Kale (#431), the combination
   # 'Sh' + 'dh' -> 'D' + 'Dh'.  Neither author mentions this as
   # an irregularity, but I could find no sandhi rule to justify
   # this transition.
   # I code it to apply just to  'dviz'
   thisans = atok0 + 'qQ' + ending[1:]
  elif (root == 'mfj'):
   ##59. 'mRij' (2P) takes vrddhi in its strong forms.
   # It optionally takes vrddhi before weak terminations beginning with
   # a vowel.
   # Note 1: part of this logic appears at the top of
   # the routine, where s-atok is determined.
   # Note 2: Special rules of sandhi seem to apply, although
   # neither Kale nor Antoine seem to consider them special
   base = base0
   ending = ending0
   if (ending in ['t','s']):
    #  change last letter to 'w', drop ending
    thisans = base[0:-1]+'w'
   elif e in 'tT': # but ending != 't'
    # change the last letter ('j') to 'Sh'
    base = base[0:-1] + 'z'
    thisans = conjugation_join(base,ending)
   elif e == 's': # but ending != 's'
    base = base[0:-1]+'k'
    thisans = conjugation_join(base,ending)
   elif ending == 'hi':
    # change last letter ('j') to 'q')
    base = base[0:-1]+'q'
    thisans = conjugation_join(base,'Di')
   elif (strength == 'w') and (e in init.vowel_set):
    thisans1 = conjugation_join(stok,ending)
    thisans = [solution(thisans),solution(thisans1)]
  elif (root == 'han'):
   ##60. 'han' (to kill) (2P/A)
   # (1) drops is 'n' before weak terminations beginning with
   # any consonant except a nasal or semivowel
   # (2) drops its 'a' and changes the 'h' to 'gh' before vowel endings.
   # (3) has 'jahi' for imperative 2S
   if (ending0 == 'hi'):
    thisans = 'jahi'
   elif ending0 in ['ti','tu']:
    # otherwise, get 'haMti, rather than 'hanti, etc.
    thisans = base0 + ending0
   elif (strength == 'w') and (e in init.consonant_set) and \
        (not (e in init.semivowel_set))and (not (e in init.nasal_set)):
    base = base[0:-1] # drop the 'n' in 'han'
    thisans = conjugation_join(base,ending)
   elif (e in init.vowel_set) and (strength == 'w'):
    # don't do the usual vowel change when
    # (a) ending starts with 'aa'
    # (b) ending = [a m]
    # drop 'han' and put back 'Gn'
    base = base[0:-3]
    base = base + 'Gn'
    thisans = conjugation_join(base,ending)
  elif root in ['an','Svas','svap','rud','jakz']:
   ##61. 'an' (to breathe), 'shvas' (to sigh), 'svap' (to sleep), and
   # 'rud' (to weep)
   # (a) insert 'i' before all terminations beginning with a consonant
   #     except 'y'
   # (b) They insert 'ii' or 'a' before the 's' and 't' of the 2nd
   #     and 3rd pers. sing. imperfect
   # Note: Kale includes 'jakSh' (to eat) in this category.
   #'jakSh' (to eat) also has this change; in addition, 
   #  (a) 3rd pl. laT and loT drop the 'n' (ending = 'ati' or 'atu')
   #  (b) 3rd pl. la~N has ending 'uH'
   base = base0
   ending = ending0
   e = ending[0]
   if (e == 'y'):
    pass
   elif ending in ['s','t']:
    thisans1 = conjugation_join(base,'I' + ending)
    thisans2 = conjugation_join(base,'a' + ending)
    thisans = [thisans1,thisans2]
   elif e in init.consonant_set:
    thisans = conjugation_join(base,conjugation_join('i',ending))
   elif (root == 'jakz') and (i == 2):
    if (tense == 'law'):
     ending = 'ati'
     thisans = conjugation_join(base,ending)
    elif (tense == 'low'):
     ending = 'atu'
     thisans = conjugation_join(base,ending)
    elif (tense == 'laN'):
     ending = 'uH'
     thisans = conjugation_join(base,ending)
  elif root in ['cakAs','jAgf','daridrA','SAs']:
   ##62. Five verbs belong to the second conjugation have some
   # characteristics of the 3rd conjugation: All drop the
   # 'n' of the 3rd pers pl. laT and loT, and use 'uH'
   # for the 3rd per pl. of la~N.
   # Each has some other peculiarities
   # (a) chakaas (to shine)
   if (i == 2): # 3p
    if tense == 'law':
     ending ='ati'
     thisans = conjugation_join(base,ending)
    elif tense == 'low':
     ending ='atu'
     thisans = conjugation_join(base,ending)
    elif tense == 'laN':
     ending ='uH'
     thisans = conjugation_join(base,ending)
   if root == 'cakAs':
    if (ending == 'Di'):
     # Kale has an optional form where ending 's' becomes 'd'
     base = atok0 + 'd'
     thisans1 = conjugation_join(base,ending)
     thisans=[thisans,thisans1]
   elif root == 'jAgf':
    if (i == 2) and (tense == 'laN'):
     # in la~N 3P, gunate before adding ending [u H]
     ending ='uH'
     base = atok0 + guna(a)
     thisans = conjugation_join(base,ending)
    elif (ending in ['ti','tu']):
     # when conjugation-join is used, the results are (AntoineI-54)
     # jaagar + ti = jaagasti (Kale has jaagarti) and
     # jaagar + tu = jaagastu (Kale has jaagartu)
     thisans = base + ending
   elif root == 'daridrA':
    # drops final 'aa' before weak terminations beginning with a vowel
    # changes final 'aa' to 'i' before weak terminates beginning with cons.
    if strength == 's':
     pass # no change
    elif e in init.consonant_set:
     base = atok0 + 'i'
     thisans = conjugation_join(base,ending)
    elif ending0 == 'an':
     base = atok0
     ending = 'uH'
     thisans = conjugation_join(base,ending)
    elif e in init.vowel_set:
     base = atok0
     thisans = conjugation_join(base,ending)
   elif root == 'SAs':
    # (a) changes 'aa' to 'i' before weak terminations beginning with a
    #  consonant. Thus sh i s, which becomes sh i Sh
    # (b) 2nd pers. sing. imperative is 'shaadhi' (already taken care
    #   of by #43)
    if (ending0 == 'hi'):
     # no change for 2nd pers. sing. imperative
     pass
    elif (strength == 'w') and (e in init.consonant_set):
     base = atok0[0:-1] + 'iz'
     thisans = conjugation_join(base,ending)
     if dbg:
      print "base=%s, ending=%s => thisans=%s" %(base,ending,thisans)
  elif a == 'c':
   # Kale #434. 'vac' (to speak) is deficient in 3rd pers pl.
   # Note:  Although not mentioned by Kale or Antoine, some special
   # sandhi rules are required for 'vach'. I presume these
   # are also applicable to other roots ending in 'ch'.
   # Namely, before consonants (other than semivowels) the 'ch' of
   # 'vach' is changed to 'k', then other (usual) sandhi changes are
   # applied.
   # Note: I apply these to 'pfc' (the only other conj-2 verb in
   # Antoine that ends in 'c'
   if (root == 'vac') and (tense == 'law') and (i == 2):
    thisans = ''  # not sure what the 'missing value' should be
   elif e == 'm':
    # Note (conjugation-join('vac','mi') => 'vagmi', but, acc. to Kale,
    # the correct answer in 'vacmi'
    thisans = base + ending
   elif (e in init.consonant_set) and (not (e in init.semivowel_set)):
    base = atok0 + 'k'
    thisans = conjugation_join(base,ending)
  if isinstance(thisans,list):
   thisans = flatten(thisans)
  if dbg:
   print "thisans[%s]=%s" %(i,thisans)
  ans.append(thisans)
 return ans
 #raise NameError(err0)

def changestringitem(x,i,c):
 """ Assume x is a string and 'c' a character
     and i an index from 0 to len(x)
     Return the string whose ith character is replaced by c.
     Here is another solution:
word = word[:index] + char + word[index + 1:]
i.e., return x[:i]+c+x[i+1:]
 """
 L = list(x)
 L[i] = c
 return ''.join(L)


def conjugation_tab_3(base,tense,theclass,pada,root,dbg=False):
 """ from gram2.el
  'base' here <-> 'a~Nga-sym' in elisp conjugation-tab-2
 """
 err0="conjugation_tab_3(%s,%s,%s,%s,%s)"%(base,tense,theclass,pada,root)
 if dbg:
  print err0
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. 
 dtok = root
 rtok = reduplicative_pfx(dtok)
 if root in ['nij','vij']:
  # 76. 'nij' (to cleanse) and 'vij' (to separate)
  #     take guna in reduplication
  rtok = gunate_final_vowel(rtok)
 #dhaatu_ending = dtok[-1:]
 ibeg = 0 # position of 1st character
 #4. for imperfect, the augment 'a' must be prefixed to rtok
 if tense == 'laN':
  u = rtok[0]
  if u in init.vowel_set:
   rtok = vfdDi(u) + rtok[1:]
  else:
   rtok = 'a' + rtok
   ibeg = 1 # change position of 'first' character
 # 5. combine base and endings to get ans
 if pada == 'p':
  #69.3 In parasmaipada, in 3P of laT and loT, the 'n' is dropped.
  #     3P is subscript 2 of endings
  if tense == 'law':
   endings[2] = 'ati'
  elif tense == 'low':
   endings[2] = 'atu'
 # 69.2 Final and short medial vowels take guna in the strong forms   
 s_atok = gunate_final_vowel(dtok)
 s_atok = rtok + s_atok
 w_atok = rtok + dtok
 if root in ['dA','DA']:
  # 72. 'daa' (to give) and 'dhaa' (to put) form their weak bases
  # in 'dad' and 'dadh' (i.e., the 'aa' is dropped)
  # the strong bases keep 'aa' (rather than gunate to 'a')
  w_atok = w_atok[0:-1]
  s_atok = s_atok[0:-1] + 'A'
 if (root == 'hA') and (pada == 'a'):
  ##74. 'haa' (3A) (to depart) has (strong and weak) base
  # 'jihii', but the 'ii' is dropped before terminations beginning
  # with a vowel.
  if tense == 'laN':
   w_atok = 'ajihI'
  else:
   w_atok = 'jihI'
  s_atok = w_atok
 if (root == 'mA') and (pada == 'a'):
  ##74. 'maa' (3A) (to measure) has (strong and weak) base
  # 'mimii', but the 'ii' is dropped before terminations beginning
  # with a vowel.
  if tense == 'laN':
   w_atok = 'amimI'
  else:
   w_atok = 'mimI'
  s_atok = w_atok
 if (root == 'hA') and (pada == 'p'):
  ##75. 'haa' (3P) (abandon) has strong base 'jahaa' and weak base
  # either 'jahii' or 'jahi'.  Before weak terminations beginning
  # with a vowel or with 'y', the base is 'jah'
  # The weak stem situation is straightened out in the irregulities section
  if tense == 'laN':
   s_atok = 'ajahA'
  else:
   s_atok = 'jahA'
  w_atok = s_atok[0:-1] + 'I'
 #  not set atok. BUT this atok is not used. See for loop below
 atok = w_atok
 (parts,types) = word_parts(root)
 # Loop over all the endings
 if dbg:
  print "chk: s_atok=%s, w_atok=%s" %(s_atok, w_atok)
 for i in xrange(0,n):
  strength = strengths[i].lower() # s or w
  if strength == 's':
   atok = s_atok
  else:
   atok = w_atok
  ending = endings[i]
  if dbg:
   #print "chk: i=%s, strength=%s, ending=%s, atok=%s" %(i,strength,ending,atok)
   pass
  e = ending[0] # first char of ending
  if (strength == 'w') and (1 < len(dtok)) and (dtok[1] == 'F') and\
     (not ((i==2) and (pada=='p') and (tense == 'laN'))):
   ##77. Final 'RI after labial (or 'v) becomes 'uur (in weak stem)
   #Kale#394. This change to 'uur occurs when the ending starts
   # with a consonant.  When the ending starts with a vowel,
   # 'RI after labial (or 'v) becomes 'ur
   # However, the imperfect 3P follows the special rule of 69.4
   a0 = dtok[0] # 1st letter of root
   if (a0 in init.labial_set) or (a0 == 'v'):
    if e in init.consonant_set:
     atok = w_atok[0:-1] + 'Ur'
    else:
     atok = w_atok[0:-1] + 'ur'
  atok0 = atok[0:-1] # all but last char
  a_save = atok[-1:] # last char
  da_save = de_aspirate(a_save) # deaspirated last char
  abeg = atok[ibeg] # 1st char
  asp_abeg = aspirate(abeg) # aspirated version of 1st char
  a = a_save
  base = atok
  ending0 = ending
  base0 = atok
  # Many adujstments to base and ending in special cases
  if (i == 2) and (pada == 'p') and (tense == 'laN'):
   # #69.4.  The 3rd pers plur. imperf. parasmaipada takes the
   # termination 'us' before which
   # (a) a final 'aa' is dropped
   # (b) a final 'i' 'u' or 'Ri' (short or long) takes guna
   ending = 'us'
   if (a == 'A'):
    base = atok0
   elif a in 'iufIUF':
    base = atok0 + guna(a)
  elif e in init.vowel_set:
   # no other considerations if termination is a vowel
   pass
  elif (e in init.nasal_set) or\
       ((e in init.semivowel_set) and (ending != 'hi')):
   # #37. Before terminations beginning with a nasal or a semi-vowel,
   # (other than [h i])
   # the consonant sandhi offers no special difficulty
   pass
  # NOTE: from here on, 'e' is a consonant but not a nasal or semivowel
  else:
   ##40. Before terminations beginning with a consonant except a
   #  nasal or a semi-vowel,
   #  the final 'h' of a verbal base is changed to 'Dh'.
   #  When, however, the verbal root begins with 'd', the final
   #  'h' is changed to 'gh'
   # we implement this in a cond step which fails
   # so subsequent steps see this change
   if a == 'h':
    #  Rule 39 will now consider root to end with an aspirate
    if abeg == 'd':
     a = 'G'
    else:
     a = 'Q'
    a_save = a # last char.
    da_save = de_aspirate(a_save)
    base = atok0 + a
    atok = base
   # #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
   # the 2nd pers. singular imperative parasmaipada is changed
   # to 'dhi' when the verbal base ends with a consonant
   # except a nasal or a semi-vowel
   # Note: This change is sometimes modified by #42. Thus,
   # I implement in such a way as to change the ending, but
   # have the condition fail, so further change possibilities
   # will be examined
   if (ending == 'hi') and (a in init.consonant_set) and\
      (not (a in init.nasal_set)) and (not (a in init.semivowel_set)):
    ending = 'Di'
    e = 'D'
   # Now several more alternative situations ( a big if elif switch)
   if (tense == 'laN') and (pada == 'p') and (a in init.consonant_set) and\
      (ending in 'ts'): # 3s or 2s
    # #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
    # imperfect parasmaipada are dropped after a verbal base ending
    # with a consonant. The final consonant of the base is then treated
    # according to the rule given in Part I-72(3).
    #1. If 'a' is aspirate, deaspirate it
    #  and if 'a' is a soft aspirate, after losing the aspiration,
    #  throw it back if possible on previous syllable. This is like
    #  39.1 and 2 below.
    if da_save != a_save: # 
     a=da_save
     base = changestringitem(base,ibeg,asp_abeg)
    #2. case of 3s
    if ending == 't':
     ending = ''
     if a == 's':
      base = atok0 + 't'
     else:
      base = sandhi_legalise_final_cons(base)
    #. case of 2s
    if ending == 's':
     ending = ''
     #print "i=%s, base=%s before legalise" % (i,base)
     base = sandhi_legalise_final_cons(base) # a list
     #print "i=%s, base=%s after legalise" % (i,base)
     if a == 'd':
      base = append_if_new(base,atok+'H')
     elif a == 's':
      base = append_if_new(base,atok0+'t')
     #print "i=%s, base=%s after legalise (final)" % (i,base)
    base = solution(base) # don't want a list of length 1
   elif (a == 'Q') and (e in 'tTD') and (len(ending)!= 1):
    ##42. Before terminations beginning with 't' 'th' and 'dh',
    # the final 'Dh' of a verbal root is dropped, while the following
    # 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
    # vowel is lengthened.
    # NOTE: 'beginning' with does not mean 'equal'; 
    # i.e. this not applicable to 3S of imperfect
    e = 'Q'
    ending = e + ending[1:]
    v = atok0[-1:] # preceding vowel
    v = lengthen_vowel(v)
    if (1 < len(atok0)):
     atok1 = atok0[0:-1]
    else:
     atok1 = ''
    base = atok1 + v
   elif (a in 'Qz') and (e == 's'):
    ##41. Before terminations beginning with 's' (i.e., before
    # 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
    a = 'k'
    base = atok0 + a
   elif da_save != a_save: # so variable 'a' is an aspirate
    ##39.1 The final aspirate of a verbal base loses its aspiration
    ##39.2 A soft aspirate, after losing its aspiration, throws it back,
    #   if possible, on the previous syllable
    ##39.3 The previous rule does not apply before the termination
    #   'dhi' of the 2nd pers. sing. imperative parasmaipada
    #   (Note: implemented indirectly by #34 above)
    ##39.4 It does not apply either before terminations beginning with
    #   't' or 'th', in which case the lost aspiration is thrown forward
    #   on the following 't' or 'th' which are softened (to 'dh')
    # NOTE: this IS applicable to 3S of imperfect whose ending = 't'
    # NOTE: for 'dhaa', #39.2 is used rather than #39.4
    a = da_save # a loses aspiration
    if (e in 'tT') and (root != 'DA'):
     e = 'D'
     ending = e + ending[1:]
     base = atok0 + a
    else:
     base = changestringitem(base,ibeg,asp_abeg)
     base = base[0:-1] + a
   elif (a == 's') and (e in init.soft_set) and (e in init.dental_set):
    ##43. The final 's' of a verbal base is dropped before
    # soft dentals
    base = base[0:-1]
   else:
    ##44. When a verbal root ends with a conjunct consonant having
    # 'k' or 's' for its first member, it drops that 'k' and 's'
    # before a termination beginning with a consonant except a
    # nasal or a semi-vowel.
    part1 = parts[0] # a string
    p2 = part1[-1:]  # last char
    if (1 < len(p2)) and (p2[0] in 'ks'):
     p2b = p2[1:] # all but initial letter
     p2a = p[0:-1] # all but last letter
     p2 = p2a + p2b
     base = p2
  # End of Many adujstments to base and ending in special cases
  # Now join the adjusted base and ending
  thisans = conjugation_join(base,ending)
  if dbg:
   print "Before irregularity overrides, %s + %s => %s" %(base,ending,thisans)
  
  #
  # ---- irregularity overrides
  #
  if (root == 'hu'):
   ##71 'hu' (sacrifice) has 2S loT of 'juhudhi (regular would be 'juhuhi)
   if (i == 3) and (tense == 'low') and (pada == 'p'):
    thisans = 'juhuDi'
  elif (root == 'dA'):
   ##72. 'daa' (to give) has 2S loT of 'dehi
   if (i == 3) and (tense == 'low') and (pada == 'p'):
    thisans = 'dehi'
  elif (root == 'DA'):
   ##72. 'dhaa' (to put) has 2S loT of 'dhehi
   if (i == 3) and (tense == 'low') and (pada == 'p'):
    thisans = 'Dehi'
  elif (root == 'BI'):
   # #73. 'bhii' (to fear) optionally changes its final 'ii' to 'i'
   # before weak terminations beginning with a consonant
   if (strength == 'w') and (e in init.consonant_set):
    base = atok0 + 'i'
    thisans1 = conjugation_join(base,ending)
    thisans=[thisans,thisans1]
  elif (root == 'hA') and (pada == 'a'):
   #74. 'haa' (3A) (to depart) has (strong and weak) base
   # 'jihii', but the 'ii' is dropped before terminations beginning
   # with a vowel. The Base was handled above.
   if e in init.vowel_set:
    base = atok0
    thisans = conjugation_join(base,ending)
  elif (root == 'mA') and (pada == 'a'):
   ##74. 'maa' (3A) (to depart) has (strong and weak) base
   # 'mimii', but the 'ii' is dropped before terminations beginning
   # with a vowel. The Base was handled above.
   if e in init.vowel_set:
    base = atok0
    thisans = conjugation_join(base,ending)
  elif (root == 'hA') and (pada == 'p'):
   ##75. 'haa' (3P) (abandon) has strong base 'jahaa' and weak base
   # either 'jahii' or 'jahi'.  Before weak terminations beginning
   # with a vowel or with 'y', the base is 'jah'
   # imperative 2S has 3 forms: jahaahi jahiihi jahihi
   if (tense == 'low') and (i == 3):
    thisans = ['jahAhi','jahIhi','jahihi']
   elif (strength == 'w'):
    if (e in init.vowel_set) or (e == 'y'):
     base = atok0
     thisans = conjugation_join(base,ending)
    else:
     # other weak endings have an option
     base = atok0 + 'i' # use 'i' in place of 'I'
     thisans1 = conjugation_join(base,ending)
     thisans = [thisans,thisans1]
  elif (root in ['nij','vij']):
   ##76. 'nij' (to cleanse) and 'vij' (to separate) 
   # (a) take guna in reduplication (done earlier in routine)
   # (b) the radical vowel does not take guna before strong
   #     terminations beginning with a vowel 
   #Note 2: Special rules of sandhi seem to apply, although
   # neither Kale nor Antoine seem to consider them special.
   # These are similar to (but not identical to) those for the
   # 2nd conjugation verb 'mRij'
   base = base0
   ending = ending0
   if ending == 'hi':
    ending = 'Di'
   e = ending[0]
   if (strength == 's') and (e in init.vowel_set):
    base = w_atok
    thisans = conjugation_join(base,ending)
   elif (ending in 'ts'):
    # change last letter to k and drop ending
    base = base[0:-1]+'k'
    thisans = base
   elif (e in 'tT'): # but ending != 't'
    # change last letter 'j' to k 
    base = base[0:-1]+'k'
    thisans = conjugation_join(base,ending)
   elif (e == 's'):  # but ending != 's'
    # change last letter 'j' to k 
    base = base[0:-1]+'k'
    thisans = conjugation_join(base,ending)
   elif (e == 'D'):
    # change last letter 'j' to g 
    base = base[0:-1]+'g'
    thisans = conjugation_join(base,ending)
  elif dtok.endswith(('f','F')):
   #77. root ends in 'f' or 'F'
   if (a == 'r') and (e in 'tT') and (1 < len(ending)):
    thisans = base + ending # join by concatenation, not by conjugation_join
  # END of irregularity overrides
  thisans1 = thisans
  thisans = flatten(thisans1)
  if dbg:
    print "(FINAL) i=%s, base=%s,ending=%s => %s (%s)" %(i,base,ending,thisans, thisans1)
  ans.append(thisans)
 
 return ans

def conjugation_tab_5(basein,tense,theclass,pada,root,dbg=False):
 """ from gram2.el
  'basein' here <-> 'a~Nga-sym' in elisp conjugation-tab-5
 """
 err0="conjugation_tab_5(%s,%s,%s,%s,%s)"%(basein,tense,theclass,pada,root)
 if dbg:
  print err0
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. atok
 atok = basein
 #4. for imperfect, the augment 'a' must be prefixed to rtok
 if tense == 'laN':
  u = atok[0]
  if u in init.vowel_set:
   atok = vfdDi(u) + atok[1:]
  else:
   atok = 'a' + atok
 # 5. combine base and endings to get ans
 a = atok[-1:] # last char
 for i in xrange(0,n):
  strength = strengths[i].lower() # s or w
  ending = endings[i]
  e = ending[0] # first char of ending
  #base = None
  if a in init.vowel_set:
   # (a) root ends in vowel
   if strength == 's':
    base = atok + 'no'
   else:
    if e in init.vowel_set:
     base = atok + 'nv'
    elif e in 'vm':
     base = [atok + 'nu',atok + 'n'] # two bases
    else:
     base = atok + 'nu'
    if ending == 'hi': # low p 2s
     ending=''
  else:
   # (b) root ends in consonant
   if strength == 's':
    base = atok + 'no'
   else:
    if e in init.vowel_set:
     base = atok + 'nuv'
    elif e in 'vm':
     base = atok + 'nu'
    else:
     base = atok + 'nu'
  thisans = conjugation_join(base,ending)
  ans.append(thisans)
 return ans

def conjugation_tab_7(basein,tense,theclass,pada,root,dbg=False):
 """ from gram2.el
   basein == a~Nga-sym of Elisp code. unused(?)
 ; Antoine#84.
 ; (1) All the verbs of the 7th conjugation end with a consonant
 ; (2) Before the formation of the verbal base, a penultimate nasal
 ;     is dropped
 ; (3) In the strong forms, 'na' is inserted between the radical vowel
 ;     and the final consonant.
 ; (4) In the weak forms, 'n' is inserted between the radical vowel 
 ;      and the final consonant.
 """
 err0="conjugation_tab_5(%s,%s,%s,%s,%s)"%(basein,tense,theclass,pada,root)
 if dbg:
  print err0
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. atok
 dtok = root
 # #84.2 the verbal base (rtok) removes a penultimate nasal
 # In the case of 'hiMs', this nasal is spelled with 'M'
 rtok = dtok
 c = rtok[-2:-1] # penultimate character
 if (c in init.nasal_set) or (c == 'M'):
  rtok = rtok[0:-2]+rtok[-1:] # drop penultimate nasal
 root_ending = dtok[-1:] # last char of root
 ibeg = 0 # position of 1st char
 # 4. for imperfect, the augment 'a' must be prefixed to rtok
 if tense == 'laN':
  u = rtok[0]
  if u in init.vowel_set:
   rtok = vfdDi(u) + rtok[1:]
  else:
   rtok = 'a' + rtok
   ibeg = 1 # change position of 1st char
 # 5.  combine base and endings to get ans
 ##84.3 In strong forms, insert 'na' between radical vowel and final cons
 # Note, after a preceding 'r', 'Na' is inserted.
 ##84.4 In weak forms, insert 'n' between radical vowel and final cons
 ## NOTE: The weak nasal is consistent with the varga of the final
 #  consonant
 s_atok = rtok[0:-1]+ 'na' + rtok[-1:]
 tmp = sandhi_nR(s_atok)
 if tmp:
  s_atok = tmp
 # Sometimes, the anusvara, M, is used; but the following
 # logic does not show this
 if root_ending in init.guttural_set:
  weak_nasal = 'N'
 elif root_ending in init.palatal_set:
  weak_nasal = 'Y'
 elif root_ending in init.cerebral_set:
  weak_nasal = 'R'
 else:
  weak_nasal = 'n'
 w_atok = rtok[0:-1] + weak_nasal + rtok[-1:]
 atok = w_atok
 (parts,types) = word_parts(root)
 # Loop to construct conjugation table
 for i in xrange(0,n):
  strength = strengths[i].lower()  # s or w
  ending = endings[i]
  e = ending[0] # first char
  if strength == 's':
   atok = s_atok
  else:
   atok = w_atok
  # Antoine2-#87. 'tRih' (7P to kill) inserts 'ne instead of 'na'
  # before strong terminations beginning with a consonant
  if root == 'tfh':
   if (strength == 's') and (e in init.consonant_set):
    atok = rtok[0:-1] + 'ne' + rtok[-1:]
  atok0 = atok[0:-1] # all but last char
  a_save = atok[-1:] # last char
  da_save = de_aspirate(a_save) # deaspirated version of last char
  abeg = atok[ibeg] # first char
  asp_abeg = aspirate(abeg) # aspirated version of 1st char
  a = a_save
  base = atok
  ending0 = ending
  base0 = atok
  # adjustments prior to joining base and ending
  if e in init.vowel_set:
   #  no other considerations if termination begins with a vowel
   pass
  elif (e in init.nasal_set) or ((e in init.semivowel_set) and (ending!='hi')):
   # #37. Before terminations beginning with a nasal or a semi-vowel,
   # (other than [h i])
   # the consonant sandhi offers no special difficulty
   pass
  else:
   #--- Note: from here on, 'e' is a consonant but not a nasal/semivowel
   #
   # In the case of weak endings (beginning with consonant), ???
   ##40. Before terminations beginning with a consonant except a
   #  nasal or a semi-vowel,
   #  the final 'h' of a verbal base is changed to 'Dh'.
   #  When, however, the verbal root begins with 'd', the final
   #  'h' is changed to 'gh'
   # we implement this in a cond step which fails
   # so subsequent steps see this change
   if a == 'h':
    if abeg == 'd':
     # Rule 39 will now consider root to end with an aspirate
     a = 'G'
    else:
     a = 'Q'
     if strength == 'w':
      # for weak endings, also change the nasal to 'N
      weak_nasal = 'R'
      atok0 = rtok[0:-1]+weak_nasal
      atok = atok0 + a
    a_save = a # last char
    da_save = de_aspirate(a_save)
    base = atok0 + a
    atok = base
   if (ending == 'hi') and (a in init.consonant_set) and\
      (not (a in init.nasal_set)) and (not (a in init.semivowel_set)):
    # #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
    # the 2nd pers. singular imperative parasmaipada is changed
    # to 'dhi' when the verbal base ends with a consonant
    # except a nasal or a semi-vowel
    # Note: This change is sometimes modified by #42. Thus,
    # I implement in such a way as to change the ending, but
    # have the condition fail, so further change possibilities
    # will be examined
    ending = 'Di'
    e = 'D'
   if (tense == 'laN') and (pada == 'p') and (a in init.consonant_set) and\
      (ending in 'ts'):
    # #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
    # imperfect parasmaipada are dropped after a verbal base ending
    # with a consonant. The final consonant of the base is then treated
    # according to the rule given in Part I-72(3).
    #1. If 'a' is aspirate, deaspirate it
    #  and if 'a' is a soft aspirate, after losing the aspiration,
    #  throw it back if possible on previous syllable. This is like
    #  39.1 and 2 below.
    #print "chk1a: i=%s,base=%s, ending=%s" %(i,base,ending)
    if (da_save != a_save):
     a = da_save
     base = changestringitem(base,ibeg,asp_abeg)
    #2. case of imperf. 3S
    if ending == 't':
     ending = ''
     if a == 's':
      base = atok0 + 't'
     else:
      base = sandhi_legalise_final_cons(base)
    #3. case of imperf. 2S
    if ending == 's':
     ending = ''
     base = sandhi_legalise_final_cons(base) # base now a list
     if a == 'd':
      base = append_if_new(base,atok0+'H')
     elif a == 's':
      base = append_if_new(base,atok0+'t')
    base = solution(base) # don't want a list unless necessary
    #print "chk1b: i=%s,base=%s, ending=%s" %(i,base,ending)
   elif (a == 'Q') and (e in 'tTD') and (len(ending)!=1):
   ##42. Before terminations beginning with 't' 'th' and 'dh',
   # the final 'Dh' of a verbal root is dropped, while the following
   # 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
    # vowel is lengthened.
    # NOTE: 'beginning' with does not mean 'equal'; 
    # i.e. this not applicable to 3S of imperfect
    e = 'Q'
    ending = e + ending[1:]
    v = atok0[-1:] # preceding vowel
    v = lengthen_vowel(v)
    if 1 < len(atok0):
     atok1 = atok0[0:-1]
    else:
     atok1 = ''
    base = atok1 + v
   elif (a in 'Qz') and (e == 's'):
    ##41. Before terminations beginning with 's' (i.e., before
    # 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
    a = 'k'
    base = atok0 + a
   elif (da_save != a_save): # so 'a' is an aspirate
    ##39.1 The final aspirate of a verbal base loses its aspiration
    ##39.2 A soft aspirate, after losing its aspiration, throws it back,
    #   if possible, on the previous syllable
    ##39.3 The previous rule does not apply before the termination
    #   'dhi' of the 2nd pers. sing. imperative parasmaipada
    #   (Note: implemented indirectly by #34 above)
    ##39.4 It does not apply either before terminations beginning with
    #   't' or 'th', in which case the lost aspiration is thrown forward
    #   on the following 't' or 'th' which are softened (to 'dh')
    # NOTE: this IS applicable to 3S of imperfect whose ending = 't'
    # NOTE: for 'dhaa', #39.2 is used rather than #39.4
    a = da_save # 'a' loses aspiration
    if (e in 'tT') and (root != 'DA'):
     e = 'D'
     ending = e + ending[1:]
     base = atok0 + a
    else:
     base = changestringitem(base,ibeg,asp_abeg)
     base = base[0:-1] + a
   elif (a == 's') and (e in init.soft_set) and (e in init.dental_set):
    ##43. The final 's' of a verbal base is dropped before
    # soft dentals
    base = base[0:-1]
   else:
    ##44. When a verbal root ends with a conjunct consonant having
    # 'k' or 's' for its first member, it drops that 'k' and 's'
    # before a termination beginning with a consonant except a
    # nasal or a semi-vowel.
    p2 = parts[-1:] # last consonant cluster of root
    if (1 < len(p2)) and (p2[0] in 'ks'):
     p2b = p2[1:] # all but first letter
     p2a = p2[0:-1] # all but last letter
     p2 = p2a + p2b
     base = p2
  # join base and ending
  thisans = conjugation_join(base,ending)
  #
  # ----------  irregularity overrides
  #
  if root == 'piz':
   # Without the next override, the answer from above is
   # [p i N Sh] + [dh i] -> [p i N Sh Dh i] = piNShDhi
   # This disagrees with both Antoine/Kale
   if (tense == 'low') and (i == 3):
    base = 'piRq'
    thisans = conjugation_join(base,ending)
  elif root_ending in 'jc':
   #Note 2: Special rules of sandhi seem to apply, although
   # neither Kale nor Antoine seem to consider them special.
   # These are similar to  those for the
   # 2nd conjugation verb 'mRij' and 3rd conj verb 'nij'.
   # In addition, the type of the inserted nasal for weak
   # terminations requires change, depending on other changes.
   # The changes are coded to work similarly for roots ending
   # in 'ch'. 
   base = base0
   ending = ending0
   old_thisans = thisans
   if strength == 'w':
    # default nasal is '~n' , since it occurs before palatal
    base = base[0:-2] + 'Y' + base[-1:]
    thisans = conjugation_join(base,ending)
   if (ending == 'hi'):
    ending = 'Di'
   e = ending[0]
   if (root_ending == 'c') and ((e in init.semivowel_set) or (e == 'm')):
    # conjugation-join changes 'ari~nch' + 'mahi' to 'ari~njmahi'
    # Kale/Antoine show answer as 'ari~nchmahi'
    thisans = base + ending
   elif (ending in 'ts'):
    #  change last letter to 'k', drop ending
    base = base[0:-1]+'k'
    thisans = base
   elif e in 'tT': # but ending != 't'
    # change the last letter ('j') to 'k'
    # change nasal for weak terminations
    if strength == 's':
     base = base[0:-1] + 'k'
    else:
     base = base[0:-2] + 'Nk'
    thisans = conjugation_join(base,ending)
   elif e == 's': # but ending != 's'
    # change the last letter ('j') to 'k'
    # change nasal for weak terminations
    if strength == 's':
     base = base[0:-1] + 'k'
    else:
     base = base[0:-2] + 'Nk'
    thisans = conjugation_join(base,ending)
   elif e == 'D':
    # change the last letter ('j' or 'c') to 'g'
    # change nasal for weak terminations
    if strength == 's':
     base = base[0:-1] + 'g'
    else:
     base = base[0:-2] + 'Ng'
    thisans = conjugation_join(base,ending)
  # End of irregularities
  thisans = flatten(thisans)
  ans.append(thisans)
 return ans

def conjugation_tab_8(basein,tense,theclass,pada,dbg=False):
 """ from gram2.el. 
 """
 err0="conjugation_tab_8(%s,%s,%s,%s)"%(basein,tense,theclass,pada)
 if dbg:
  print err0
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. atok
 atok = basein
 # 4. for imperfect, the augment 'a' must be prefixed to rtok
 if tense == 'laN':
  u = atok[0]
  if u in init.vowel_set:
   atok = vfdDi(u) + atok[1:]
  else:
   atok = 'a' + atok
 # 5.  combine base and endings to get ans
 a = atok[-1:] # last char of base
 for i in xrange(0,n):
  strength = strengths[i].lower()  # s or w
  ending = endings[i]
  e = ending[0] # first char
  if strength == 's':
   base = atok + 'o'
  else:
   if (e in init.vowel_set):
    base = atok + 'v'
   elif e in 'vm':
    base = [atok+'u',atok]
   else:
    base = atok + 'u'
  if ending == 'hi': # low p 2s
   ending = ''
  thisans = conjugation_join(base,ending)
  ans.append(thisans)
 return ans

def conjugation_tab_9(basein,tense,theclass,pada,dbg=False):
 """ from gram2.el
 """
 err0="conjugation_tab_9(%s,%s,%s,%s)"%(basein,tense,theclass,pada)
 if dbg:
  print err0
 # 1. construct endings and strengths
 endings = conj_endings(tense,theclass,pada,dbg=dbg)
 strengths = conj_endings(tense,theclass,pada,dbg=dbg,strengths=True)
 # 2. init ans
 n = len(endings)
 ans = []
 # 3. atok
 atok = basein
 # 4. for imperfect, the augment 'a' must be prefixed to rtok
 if tense == 'laN':
  u = atok[0]
  if u in init.vowel_set:
   atok = vfdDi(u) + atok[1:]
  else:
   atok = 'a' + atok
 # 5.  combine base and endings to get ans
 a = atok[-1:] # last char of base
 for i in xrange(0,n):
  strength = strengths[i].lower()  # s or w
  ending = endings[i]
  e = ending[0] # first char
  if strength == 's':
   base = atok + 'nA'
  else:
   if (e in init.vowel_set):
    base = atok + 'n'
   else:
    base = atok + 'nI'
   if (ending == 'hi') and (a in init.consonant_set):
    base = atok
    ending = 'Ana'
  if basein == 'kzuB':
   thisans = conjugation_join(base,ending,option='not-n-R')
  else:
   thisans = conjugation_join(base,ending)
  ans.append(thisans)
 return ans

def append_if_new(L,x):
 """ Assumes L is a list.
     if x is not an element of L,
     append x to L, and return L (This returning is superfluous for
     Python, but mimics the Elisp code)
 """
 if x not in L:
   L.append(x)
 return L


def conjugation_citation_irreg(root,tense,theclass,pada,dbg=False):
 err = "conjugation_citation_irreg(%s,%s,%s,%s)" %(root,tense,theclass,pada)
 iform = "%s-%s-%s" %(tense,theclass,pada)
 irregkey = "%s.%s" %(root,iform)
 if irregkey in init.Dhatu_irreg_data.d:
  irregs = init.Dhatu_irreg_data.d[irregkey].vals
 else:
  irregs=None
 if dbg:
  err = err + (" %s => %s" %(irregkey,irregs))
  print err
 return irregs
 #raise NameError("conjugation_citation_irreg(%s,%s,%s,%s)"%(root,tense,theclass,pada))

def conjugation_tab_ForC_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ Elisp version in gram2-future.el
   pada is 'a' (Atmanepada),'p' (Parasmaipada) or 'passive'
 """
 err0 = "conjugation_tab_ForC_main(%s,%s,%s,%s,%s,%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 if dbg:
  print "\n" + err0
 # step1. construct endings and strengths; init ans
 tense = ForC.ForC_sym # global.
 if not i_insert:
  i_insert = 'i'
 conj_class = '1'
 if pada == 'passive': # passive
  # passive is same as atmanepada
  apada = 'a' # Atmanepada
 else:
  apada = pada
 endings = conj_endings(tense,conj_class,apada)
 strengths = conj_endings(tense,conj_class,apada,strengths=True)
 n = len(endings)
 (parts,types)=word_parts(root)
 # get sew_gen
 if sew_code:
  temp=sew_code
 else:
  temp=ForC_sewCode(root,theclass,pada,upasargas,dbg=dbg)
 sew_gen = solution(temp)
 #--- 5a. get itab, default table of i-inserts:
 # All  endings in the luw, lfw, and lfN begin with a consonant
 # ('t' or 's'), so it is applicable to insert 'i' if this
 # is required by sew_code.
 itab = [sew_gen]*n
 for i in xrange(0,n):
  if itab[i]=='vew':
   itab[i]=['aniw','sew']
 #--- 5b. get table of base-seT codes (bitab)
 # Usually, each element is the list of elements of btab and itab.
 # However, for some exceptions, e.g. 'aa' roots, elements are
 # made idiosyncratically
 if (tense == 'ASIrliN') and (pada == 'p'): 
  strong=False
 else:
  strong=True
 b = root
 nb = len(b)
 lc = b[-1:]
 if nb == 1:
  pc=None
 elif types in ['cvc','vc']:
  # e.g. pracC = [['pr','a','cC'],'cvc'], pc = 'a'
  pc = parts[-2:-1][0]
 else:
  pc = b[-2:-1] # penultimate char of root
 # modify 'b' as appropriate for this general tense
 if pada == 'passive': # passive
  # do no adjustments to 'b'. They have been done already
  pass
 elif (tense == 'ASIrliN'):
  b = benedictive_base(root,theclass,pada,upasargas,sew_gen,dbg=dbg)
 else:
  b = future_base(root,theclass,pada,upasargas,tense,sew_gen,dbg=dbg)
  #print "future_base returns",b
 # in case of 'lfN', join prefix 'a' to b
 if tense == 'lfN':
  # handle case that variable 'b' may be a string or a list at this point
  if isinstance(b,list):
   allb=b
  else:
   allb=[b]
  #print "chk: allb=",allb
  b = [augment_a(b0) for b0 in allb]
  b = solution(b)
 # construct btab and bitab
 #print "chk:b=",b
 btab = [b]*n
 bitab = ForC_bitab(btab,itab)
 # combine base and endings to get ans
 if dbg:
  print "conjugation-tab-ForC-main: bitab=",bitab
  print "conjugation-tab-ForC-main: endings=",endings
 ans = ForC_bitab_join(bitab,endings,root,strengths,i_insert,dbg)
 if dbg:
  print "conjugation-tab-ForC-main: ans=",ans
 # Deal with irregularities not yet coverted
 if (tense == 'ASIrliN') and (pada == 'a'):
  if root == 'kf':
   ans[5] = 'kfzIQvam' # instead of kfzIDvam
  elif root == 'ci':
   # ref. Kale p. 358
   ans[5] = 'cezIQvam' # instead of cezIDvam
 #
 if dbg:
  err0 = "conjugation_tab_ForC_main(%s,%s,%s,%s,%s,%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
  print "\n" + err0 + ("\n  ans = %s\n\n" %ans)

 return ans

def future_base(root,theclass,pada,upasargas,tense=None,sew_gen=None,dbg=False):
 """
  pada should be 'a' or 'p' (Atmanepada or Parasmaipada)
 """
 err0="future_base(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,sew_gen)
 if dbg:
  print err0
 if not sew_gen:
  sew_gen = ForC_sewCode(root,theclass,pada,upasargas,tense,dbg=dbg)
 (parts,types) = word_parts(root)
 if (tense == 'ASIrliN') and (pada == 'p'):
  strong = False
 else:
  strong = True
 b = root
 nb = len(root)
 lc = b[-1:] # last char
 # pc = penultimate character
 if nb == 1:
  pc=None
 elif types in ['cvc','vc']:
  # e.g. pracC = [['pr','a','cC'],'cvc'], pc = 'a'
  pc = parts[-2:-1][0]
 else:
  pc = b[-2:-1] # penultimate char of root
 # modify 'b' as appropriate for this general case
 if theclass == '11': # causal
  pass # assume causal base comes in as dhaatu
 elif root == 'daridrA':
  # Kale 467. 'daridraa' drops its 'aa' before a non-conjugation
  # termination except in the Desiderative and the Aorist where
  # it retains it optionally
  b = b[0:-1]
 elif (root == 'gA') and (upasargas==['aDi']):
  # Kale 486. See note in 'conjugation-tab-ForC'
  # No guna but final vowel changed to 'ii'
  b = b[0:-1]+'i'
 elif lc in 'AeEo':
  # Kale 459. Roots ending in 'e', 'ai', and 'o' are treated as
  # roots ending in 'aa'
  b = b[0:-1]+'A'
 elif (root in ['mI','mi','dI']) and strong:
  # Kale 459. The roots 'mi' (5 U 'to throw'),
  # 'mii' (9 U 'to kill'), and 'dii' (4 A 'to perish')
  # are treated as roots ending in 'aa' before a termination
  # causing guna or vrddhi.
  # In particular, this is the case for luT, lRiT, lRi~N;
  # but not the case for benedictive-A
  b = b[0:-1]+'A'
 elif (root == 'lI') and strong:
  # Kale 459. The root 'lii' (9 P, 4 A 'to adhere or cling to') changes
  # its vowel optionally to 'aa' before a termination causing
  # guna or vrddhi.  
  # In particular, this is the case for luT, lRiT, lRi~N
  b0 = gunate_final_vowel(b)
  b1 = 'lA'
  b = [b0,b1] # so now b is a list
 elif theclass == '10':
  # Kale 460 In the general tenses,
  # roots of the tenth class preserve their 'ay' (i.e.,
  # 'aya' with the final 'a' dropped), with all the changes that the
  # root undergoes before it.
  # The function 'dhaatu-a~Nga' performs this task.
  # (dhaatu-a~Nga 'gaN 10 'P) -> (([g a N a y] ((count)) nil))
  b = solution(class_a_base(root,'10',pada))
 elif (root == 'as') and (theclass == '2'):
  # Kale 462. 'as' substitutes 'bhuu' for itself
  # NOTE: Although Kale does not say that the class 2 form of 'as'
  # is intended, I have assumed this is meant. i.e., the
  # class 4 form of 'as' (meaning 'to throw') is not altered
  b = 'BU'
  if strong:
   b = gunate_final_vowel(b)
 elif root == 'brU':
  # Kale 462. 'bruu' substitutes 'vach' for itself
  b = 'vac'
  if strong:
   b = gunate_final_vowel(b)
 elif kuwAdi_P(root):
  # Kale 463.
  # Neither guNa nor vRiddhi is substituted for the vowel of a few
  # roots of the 6th class even before a strong termination except
  #  (a) the 'a' of 1S and 3S of the Perfect
  #  (b) the 'ay' of the causal
  #  (c) the 'i' of the 3S of the Passive Aorist
  # no change to b. It is already 'atok'
  pass
 elif root == 'Brasj':
  # Kale 464. The root 'bhrasj' (6 P 'to fry') assumes the
  # forms 'bhrajj' and 'bharj' in the non-conjugational tenses
  b = ['Brajj','Barj']
 elif (root == 'vij') and (theclass in ['6','7']):
  # Kale 466. The intermediate 'i' is weak in the case of the
  # root 'vij' (6 A 7 P 'to tremble')
  # In the case of luT, lRiT, lRi~N, I think this means
  # no gunation. Thus, no change to 'b' is required here
  pass
 elif (root == 'DU') and (theclass == '6'):
  # Kale example p. 305.
  # end result is 'dhuvitaasmi', etc.
  # thus, no gunation - no change to 'b' is required here
  pass
 elif (root == 'UrRu') and strong:
  # Kale 466. The intermediate 'i' is optionally weak in the case of the
  # root 'uuRNu' (to cover)
  # In the case of luT, lRiT, lRi~N, I think this means
  # no gunation as an option.
  b1 = gunate_final_vowel(b)
  b = [b,b1]
 elif root in ['dIDI','vevI']:
  # Kale 467. The root 'diidhii' (2 A 'to shine') does not take
  # guNa or vRiddhi before any termination. It also drops its
  # final vowel before the intermediate 'i' and before 'y'.
  # The root 'vevii' (2 A 'to go') takes the same changes.
  # Note: As these are roots of more than one syllable, they are 'seT'.
  # For the luT, lRiT, lRi~N, this means that the final 'ii'
  # is always dropped.
  # Similarly, for ashiirli~N, whether P or A
  b =b[0:-1]
 else:
  # the default situation. gunate the vowel
  b = gunate_final_vowel(b)
 return b

def kuwAdi_P(root,dbg=False):
 # Kale 463.
 # (In regard to the non-conjugation tenses)
 # Neither guNa nor vRiddhi is sustituted for the vowel of a few
 # roots of the 6th class even before a strong termination.
 # However, the substitution is made in the following cases:
 #  1. before the 'a' of the 1S and 3S of perfect tense (parasmaipada)
 #  2. the 'aya' of the causal
 #  3. the 'i' of the 3S of the passive aorist
 # This function check whether 'dhaatu' is one of these few roots.
 # There are a few more not often to be met with.
 return root in [
  'kuw','puw','kuc','kuj','Dur',
  'sPuw','truw','luw','sPur','gur',
  'nu','du','ku']
 #raise NameError("kuwAdi_P not finished")

def ForC_bitab(btab,itab,dbg=False):
 """ btab and itab are tables of equal length (9)
     This is very close to the Python 'zip' function applied to btab, ctab.
     As a first approximation (May 28, 2016), I'll just use zip
 """
 #return zip(btab,itab)
 # Here is the logic of ForC-bitab in gram2-future.el
 ans = []
 n = len(btab)
 for i in xrange(0,n):
  b = btab[i]
  c = itab[i]
  if not isinstance(b,list):
   b = [b]
  if not isinstance(c,list):
   c = [c]
  ans1=[]
  c0 = c
  for b1 in b:
   for c1 in c0:
    ans1.append([b1,c1])
  ans.append(ans1)
 return ans

def ForC_bitab_join(bitab,endings,root,strengths,i_insert,dbg=False):
 """ Assume bitab and endings are list of same length (9 = conj.table length)
     Also strengths
 """
 err0="ForC_bitab_join(%s,%s,%s,%s,%s)" %(bitab,endings,root,strengths,i_insert)
 if dbg:
  print err0
 ans=[]
 for i in xrange(0,len(bitab)):
  ending=endings[i]
  strength = strengths[i]
  bi = bitab[i] # a list of base-sewcode pairs
  thisans = []
  for (base,sew_code) in bi:
   thisans1=ForC_join(base,sew_code,ending,root,strength,i_insert,dbg=dbg)
   if not isinstance(thisans1,list):
    thisans1 = [thisans1] # make it a list
   for thisans2 in thisans1:
    if thisans2 not in thisans:
     thisans.append(thisans2)
  thisans = solution(thisans)
  ans.append(thisans)
 return ans

def ForC_join(base,sew_code,sup,root,strength,i_insert,dbg=False):
 """
 """
 sup = solution(sup)
 base = solution(base)
 if isinstance(sup,list):
  return map(lambda(x): ForC_join(base,sew_code,x,root,strength,i_insert,dbg=dbg),sup)
 if isinstance(base,list):
  return map(lambda(x): ForC_join(x,sew_code,sup,root,strength,i_insert,dbg=dbg),base)
 if sew_code == 'vew':
  a = ForC_join(base,'sew',sup,root,strength,i_insert,dbg=dbg)
  b = ForC_join(base,'aniw',sup,root,strength,i_insert,dbg=dbg)
  c = zip(a,b) 
  # convert tuples to list
  return map(lambda(x): list(x),c)
 # default case
 return ForC_join1(base,sew_code,sup,root,strength,i_insert,dbg=dbg)

def ForC_join1(y,sew_code,ending0,root,strength,i_insert,dbg=False):
 """ based on conjugation_join. 
     sew_code is either None, 'sew' or 'aniw'
     Variable 'y' is a string
     'strength' parameter is not used.
     
 """
 # insert 'i' if needed
 # 'ending' may then be either a token, or a list of 2 tokens
 if sew_code == 'sew':
  ending = conjugation_join(i_insert,ending0)
 else:
  ending = ending0
 # prepare for doing sandhi
 sandhiget = SandhiGet(['Antoine72-4'])
 efirst = ending[0] # is this a character o
 # NOTE 2: The logic is put here, because other changes, e.g.,
 #  before 'tha', are required. This logic applies to other
 #  forms than the future
 # make next Python set object for convenience
 cons_nonsemivowel = set(init.consonant_set).difference(set(init.semivowel_set))
 if (root == 'naS') and (efirst in  cons_nonsemivowel):
  # Kale 476. nash
  # 'n' is inserted before the ending consonant of 'nash' when
  # it is followed by any consonant except a nasal or a semi-vowel.
  # NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
  y = y[0:-1]+'M'+y[-1:]
 elif (root == 'masj') and (efirst in  cons_nonsemivowel):
  # Kale 476. masj
  # 'n' is inserted before the ending consonant and
  # the  's' is dropped when they are followed by any consonant
  # except a nasal or a semi-vowel. In particular this is applicable
  # to all the periphrastic future forms:  ma~Nktaa.
  # When the 'n' is not dropped, the 's' is changed to 'j': mamajja
  y = y[0:-2]+'Y'+y[-1:]
 elif (root == 'jaB') and (efirst in init.vowel_set):
  # Kale p.320 footnote
  # 'jabh' inserts a nasal when its final is followed by a vowel
  y = y[0:-1]+'m'+y[-1:]
 elif (root == 'raB') and (efirst in init.vowel_set):
  # Kale p.320 footnote
  # 'rabh' inserts a nasal when its final is followed by a vowel;
  # however, 'rabh' does not do it
  #  (a) in the Aorist
  #  (b) when it takes 'i', except in the Perfect
  # In this case, (luT lRiT lRi~N), the only way 'efirst' is a vowel
  # is if it is an 'i', presetn because seT-code is 'seT'. Since 
  # the tense is not Perfect, no nasal is inserted
  #(setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
  pass
 #
 ny = len(y)
 ylast = y[-1:] # last char
 yfirst = first_cons(y)
 if (efirst in init.vowel_set) and (ylast in 'iIf') and (ForC.ForC_sym=='luw'):
  # 1st special sandhi rule for perfect (Antoine2#110)
  (parts,types) = word_parts(y)
  y0 = y[0:-1] # all but last char
  if y0 == '':  #   07-24-03 for dhaatu='i', y='ii'
   y0 = y
  nw = len(types)
  if (1 < nw) and (1 < len(parts[nw-2])):
   # compound consonant precedes the final 'i ii Ri'
   if ylast == 'i':
    ans = y0 + 'iy' + ending
   elif ylast == 'I':
    ans = y0 + 'iy' + ending
   else: # ylast == 'f':
    ans = y0 + 'ar' + ending
  else:
   # word is a single vowel ('i ii Ri') or
   # a simple constant  precedes final 'i ii Ri'
   if ylast == 'i':
    ans = y0 + 'y' + ending
   elif ylast == 'I':
    ans = y0 + 'y' + ending
   else: # ylast == 'f':
    ans = y0 + 'r' + ending
 elif (efirst in init.vowel_set) and (ylast in 'uUF'):
  # 2nd special sandhi rule for future (Antoine2#110)
  y0 = y[0:-1]
  if ylast == 'u':
   ans = y0 + 'uv' + ending
  elif ylast == 'U':
   ans = y0 + 'uv' + ending
  elif ylast == 'F':
   ans = y0 + 'ar' + ending
 elif (efirst =='D') and (ylast in init.vowel_set) and (ylast not in 'aAi'):
  # 3rd special sandhi rule for future (Antoine2#110)
  ans = y + 'Q' + ending[1:]
 elif (efirst in 'tT') and (2 < ny) and (y.endswith('ar')):
  # this rule so [ch a k a r] + [th a]
  # becomes [ch a k a r th a] rather than [ch a k a s th a], which
  # is what 'sandhi-pair' does
  ans = y + ending
 elif (efirst in 'tT') and (2 < ny) and y.endswith(('cC','Sc','rj','kz','sj','jj')):
  # pracC, vraSc, mfj, akz
  y0 = y[0:-2]
  if y.endswith('rj'):
   y0 = y[0:-1]
  if y0.endswith('Y'):
   y0 = y0[0:-1] + 'N'
  if efirst == 't':
   efirst = 'w'
  else:
   efirst = 'W'
  ans = y0 + 'z' + efirst + ending[1:]
 elif (efirst in 'tT') and (ylast == 'S'):
  # Kale p. 321. Example of klish
  y0 = y[0:-1]
  if efirst == 't':
   efirst = 'w'
  else:
   efirst = 'W'
  ans = y0 + 'z' + efirst + ending[1:]
 elif (efirst in 'tT') and (root in ('vah','sah')):
  # Kale #506. p. 317
  # When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
  # is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
  #  vavah + tha =
  #   uvah + tha =
  #   uvaDh + Dha (by #416-3,4) =
  #   uvaDh + Dha =
  #   uvoDha
  ans = y[0:-2] + 'oQ' + ending[1:]
 elif (efirst == 't') and (ForC.ForC_sym == 'luw') and\
      (root in ('muh','druh','snih','snuh')):
  # Note: (ForC.ForC_sym == 'luw') may be redundant
  # A few verbs take two forms
  # muh : mogdhaa moDhaa (Kale p. 305)
  # druh : drogdhaa droDhaa (Kale, dhaatukosha)
  y0 = y[0:-1]
  ans1 = y0 + 'Q' + ending[1:]
  ylast1 = 'g'
  ans2 = y0 + ylast1 + 'D' + ending[1:]
  ans = [ans1,ans2]
 elif (efirst in 'tT') and (2 < ny) and (y.endswith('ah')):
  # 'dah' : dagdhaa (luT)
  # 'nah' : naddhaasmi (luT)
  if yfirst == 'd':
   ylast1 = 'g'
  elif yfirst == 'n':
   ylast1 = 'd'
  else:
   ylast1 = ylast
  ans = y[0:-1] + ylast1 + 'D' + ending[1:]
 elif (efirst in 'tT') and (ylast == 'h'):
  # Kale p. 322. Example of 'muh', 'druh', 'snih', 'snuh'
  y0 = y[0:-1]
  ans = y0 + 'Q' +  ending[1:]
 elif (efirst in 'tT') and (ylast in 'jc' ):
  # this rule [bh a j] + [th a] -> [bh a k th a]
  # rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
  # but [bh a ~n j] + [th a] -> [bh a ~N k th a]
  if (2 < ny) and y.endswith('aj'):
   # yaj, sRij (has be changed to 'sraj')
   y0 = y[0:-1]
   if efirst == 't':
    efirst = 'w'
   else:
    efirst = 'W'
   ans = y0 + 'z' + efirst + ending[1:]
  elif (2 < ny) and y.endswith('Yj'):
   # BaYj, masj
   ans = y[0:-2] + 'Nk' + ending
  else:
   y0 = y[0:-1]
   ans = y0 + 'k' + ending
 elif (efirst in 'tT') and (ylast in 'DB'):
  # so [v i v y a dh] + [th a] -> [v i v y a d dh a]
  # sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
  y0 = y[0:-1]
  ans = y0 + de_aspirate(ylast) + 'D' + ending[1:]
 elif (efirst in 'tT') and (ylast in 'mn'):
  # For 'gam', sandhi-pair gives 'jagaMtha', but
  # Kale and Antoine both show 'jagantha'
  # Similaraly, for 'han' we want 'jaghantha' rather than 'jagaMtha'
  y0 = y[0:-1]
  ans = y0 + 'n' + ending  
 elif (efirst == 'm') and (ylast == 'c'):
  ans = y + ending # otherwise, 'c' is changed to 'j'
 elif (efirst in 'mv') and (ylast == 'm'):
  #Kale p. 321 (footnote)
  #Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
  #note 'n' may be changed to 'N' by sandhi-single (see below)
  y0 = y[0:-1]
  ans = y0 + 'n' + ending  
 # The code for efirst == 's' is tricky, in that possibly none of 
 # the subconditions (like ylast == 'S') will hold; and in this circumstance
 # the next case ('elif (root == 'guh')...) is to be tried.
 # This is handled in Elisp by having all the subconditions be clauses in an
 # 'and' condition, but this doesn't work in Python.
 # Thus, I separate these out via
 elif (efirst == 's') and (ylast == 'S'):
  # Kale p. 321. based on example of 'ash'
  y0 = y[0:-1]
  ans = y0 + 'kz' + ending[1:]
 elif (efirst == 's') and (ylast in 'Bb'):
  # case of 'labh': Kale p. 301
  y0 = y[0:-1]
  ans = y0 + 'p' + ending
 elif (efirst == 's') and (ylast in 'Dd'):
  # case of 'vRidh': Kale p. 301
  # case of 'bandh': Kale p. 303 : previous 'b' gets the aspiration
  y0 = y[0:-1]
  if ylast == 'D':
   y0 = aspirate_first_cons(y0)
  ans = y0 + 't' + ending
 elif (efirst == 's') and (ylast == 'h'):
  # Examples:
  # 'nah' : natsyaami
  # 'vah' : vakShyaami
  # 'muh' : mokShyaami
  # 'tRih' : tarkShyati
  # The following also aspirate the first consonant: 
  # 'dah' : dhakShyaami
  # 'duh' : dhokShyaami
  # 'guh' : ghokShyaami
  # 'gaah' : ghaakShyate
  # 'gRih' : gharkShyate
  y0 = y[0:-1]
  if yfirst == 'n':
   ans = y0 + 't' + ending
  elif yfirst in 'vmt':
   ans = y0 + 'kz' + ending[1:]
  elif yfirst in 'dg':
   ans = aspirate_first_cons(y0 + 'kz' + ending[1:])
  else:
   ans = y0 + 'kz' + ending[1:]
 elif (efirst == 's') and (ylast == 's'):
  # Kale 480. 't' is substituted for the ending 's' of a root
  # when followed by any non-conjugational termination
  # beginning with 's'
  y0 = y[0:-1]
  ans = y0 + 't' + ending
 elif (efirst == 's') and (2 < ny) and y.endswith(('cC','Sc')):
  # eample pracC (Antoine2 p. 89)
  y0 = y[0:-2]
  ans = y0 + 'kz' + ending[1:]
 elif (efirst == 's') and (ylast in 'jcz'):
  # from example of 'sRij' (Antoine2 p. 89),
  # and of 'kRiSh'
  # takSh (p. 304)
  y0 = y[0:-1]
  if y0.endswith('Y'):
   y0 = y0[0:-1] + 'N'
  elif y0.endswith(('k','j','s')):
   y0 = y0[0:-1] # drop the penultimate letter of y
  ans = y0 + 'kz' + ending[1:]
  #print "CHK2 %s + %s => %s" %(y,ending,ans)
 # end of efirst=='s'
 elif (root == 'guh') and (sew_code == 'sew'):
  # the 'u' is lengthened, rather than gunated (Kale example p. 304)
  ans = 'gUh' + ending
 else:
  #  joining for other cases like 'conjugation-join'
  sandhiget = SandhiGet(['Antoine72-4','Antoine72-5'])
  ans = None
  x = sandhiget.sandhi_pair(y,ending,'internal','join')
  if len(x)==1:
   ans = x[0][0]
  if ans == None:
   x=sandhiget.sandhi_pair(y,ending,None,'join')
   if len(x) == 1:
    ans = x[0][0]
  if ans == None:
   ans = y + ending
 try:
  ans
 except:
  # ERRPR ForC_join1(kf,aniw,sIzwa,kf,S,i)
  print "ERROR ForC_join1(%s,%s,%s,%s,%s,%s)" % (y,sew_code,ending0,root,strength,i_insert)
  print "efirst=%s, ylast=%s" %(efirst,ylast)
  raise NameError("ans is not defined")
 ans1 = sandhi_single(ans,False)

 if ans1:
  ans = ans1
 ans = solution(ans)
 #if root == 'smf':
 # print "DBG ForC_join1(%s,%s,%s,%s,%s,%s)" % (y,sew_code,ending0,root,strength,i_insert)
 err0 = "ForC_join1(%s,%s,%s,%s,%s,%s)" % (y,sew_code,ending0,root,strength,i_insert)
 if dbg:
  print err0," => ",ans
 return ans

def aspirate_first_cons(word):
 """ aspirate the first consonant found in word, and return word after
     this modification
 """
 ans = list(word) # break into mutable list
 for i in xrange(0,len(ans)):
  x = ans[i]
  if x in init.consonant_set:
   y = aspirate(x)
   ans[i] = y
   break
 return ''.join(ans) # convert back to string
 raise NameError('aspirate_first_cons Not finished')

def aspirate_last_cons(word):
 """ aspirate the first consonant found in word, and return word after
     this modification
 """
 ans = list(word) # break into mutable list
 n = len(ans)
 for i in xrange(n-1,-1,-1): # traverse letters of word in reverse order
  x = ans[i]
  if x in init.consonant_set:
   y = aspirate(x)
   ans[i] = y
   break
 return ''.join(ans) # convert back to string
 raise NameError('aspirate_first_cons Not finished')

def first_cons(word):
 """ return the first consonant found in string word
  or None if word contains no consonants
 """
 for x in word:
  if x in init.consonant_set:
   return x
 return None

def augment_a(b):
 """
 ; b is string
 ; add the augment 'a' 
 ; applicable to conditional (lfN)
 ; imperfect (laN)
 """
 bfirst = b[0] # first char
 if bfirst in init.vowel_set:
  # 08-20-03. Use vfdDi1 so 'e'->'E'
  b1 = vfdDi1(bfirst) + b[1:]
 elif bfirst == 'C':
  # based upon example of 'Cfd' (lfN)
  b1 = 'a' + 'c' + b
 else:
  b1 = 'a' + b
 return b1

def conjugation_tab_ForC(upasargas,theclass,pada,root,voice=None,dbg=False):
 """ first future, second future, and conditional, benedictive
     some irregular forms are included here
     Note: in this routine, pada is supposed to have values of
     'a' = Atmanepada, 'p' = Parasmaipada, or 'passive'
 """
 err0 = "conjugation_tab_ForC(%s,%s,%s,%s,%s) %s" %(upasargas,theclass,pada,root,voice,ForC.ForC_sym)
 if dbg:
  print err0 # dbg
 ForC_sym = ForC.ForC_sym
 tok = root
 ylast = tok[-1:] # last char
 (parts,types) = word_parts(tok)
 # Use python ternary operator
 pc = (parts[1][0] if (types == 'cvc') else '')
 if pada == 'passive': # is this ever possible?
  sew_code = construct_sew_code1a(root,theclass,'a',upasargas,dbg=dbg)
 else:
  sew_code = construct_sew_code1a(root,theclass,pada,upasargas,dbg=dbg)
 sew_code = solution(sew_code) # really needed?
 if (pada == 'passive') or (voice == 'passive'):
  # recursive!
  #if dbg:
  # print "Calling conjugation_tab_ForC recursively"
  ans1 = conjugation_tab_ForC(upasargas,theclass,'a',root,None,dbg)
  #print "DBG: ans1=",ans1
  root2 = None
  if ylast == 'A':
   root2 = tok + 'y'
  elif ylast in init.vowel_set:
   if (ylast == 'e'):
    ylast1 = 'E'
   else:
    ylast1 = vfdDi(ylast)
   root2 = tok[0:-1] + ylast1
  elif root == 'han':
   root2 = 'GAn'
  elif root == 'grah':
   root2 = 'grAh'
  elif root == 'dfS':
   root2 = 'darS'
  elif theclass == '10':
   # Kale 598. The 'ay' is optionally dropped in the general tenses,
   # except the Perfect.
   # In the benedictive, the 'ay' has already been dropped;
   # so to get the alternate form, it must be added
   cb = causal_base(root,theclass,pada,upasargas,None)
   cb = solution(cb)
   if not isinstance(cb,basestring):
    # can't handle multiple values for cb now
    root2=None
   elif ForC_sym == 'ASIrliN':
    root2 = cb
   else:  
    # drop ay
    root2 = cb[0:-2]
  elif theclass == '11':
   # What is class 11? it is Causal
   if ForC_sym == 'ASIrliN':
    root2 = None
   else:
    root2 = root
  #
  if root2:
   ans2 = conjugation_tab_ForC_main(upasargas,theclass,'passive',root2,['sew'],dbg=dbg)
  else:
   ans2 = None
  #print "DBG: ans2=",ans2

  if ans2:
   ans = join_arrays(ans1,ans2)
   #print "DBG: ans1+ans2, ans=",ans
  else:
   ans = ans1
 elif (root == 'han') and (pada == 'a') and (ForC_sym == 'ASIrliN'):
  # 'han' is 2P in dhaatukosha.
  # however, its passive uses the 'A' logic.
  # In Kale examples on p. 364, the normal passive benedictive is
  # given as 'vaghiShiiya'.  So this
  # (a) uses the base 'vagh'
  # (b) inserts 'i'.
  # Since 'han' is an 'aniT', this also must be adjusted
  # By Kale 483, 'han' admits 'i' in lRiT, lRi~N (handled elsewhere)
  ans = conjugation_tab_ForC_main(upasargas,theclass,'m','vaG',['sew'],dbg=dbg)
 elif (root == 'aj') and (pada == 'p'):
  # Kale #477 p. 300
  # 'vii' (2 P) is substituted for 'aj' (1 P 'to go') necessarily before any
  # non-conjugational termination, and optionally before such as
  # begin with any consonant except 'y':
  #   'vetaa  ajitaa'
  #   'veShyati ajiShyati'
  # all forms of 'aj' have optional answers with 'vii'
  ans1 = conjugation_tab_ForC_main(upasargas,'2',pada,'vI',dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif (root == 'dfS') and (pada == 'a') and (ForC_sym == 'ASIrliN'):
  # Note2: for the 1 P root dRish, when it appears as 'A', in
  # formation of passive, the change of Kale 465 (below)
  # does not occur (Kale p. 364, example)
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
 elif root in ['sfj','dfS']:
  # Kale 465. The penultimate 'f' of 'sfj' and of 'dfS'
  # is changed to 'ra' before
  # a consonantal strong termination in the general tenses.
  # Note: both are 'aniw'
  c1 = parts[0] # initial consonant
  v = 'ra'  # replace 'f' with 'ra'
  c2 = parts[2] # final consonant
  root2 = c1 + v + c2
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root2,sew_code,dbg=dbg)
 elif root == 'tfp':
  # Kale 471. In the example of 'tfp' on p. 303, there are three
  # forms : an 'i' form with 'f' (gunated), and two forms without
  # 'i' (one with 'f' gunated, and one with 'ra' instead of 'f').
  # This logic (along with that in '..-main') achieve these three
  # forms.
  # Note: 'tfp' has seT-code 'vew'
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  c1 = parts[0] # initial consonant
  v = 'ra' # replace 'f' with 'ra'
  c2 = parts[2] # final consonant
  root2 = c1 + v + c2
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root2,'aniw',dbg=dbg)
  ans = join_arrays(ans1,ans2)
  #print "check1. ans=",ans
 elif (sew_code == 'aniw') and (pc == 'f') and (types=='cvc') and (ForC_sym=='luw'):
  # Kale 471.
  # 'aniT' roots with a penultimate 'f' change it to
  # 'ra' optionally before a strong termination beginning
  # with any consonant except a nasal or a semi-vowel.
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,sew_code,dbg=dbg)
  c1 = parts[0] # initial consonant
  v = 'ra' # replace 'f' with 'ra'
  c2 = parts[2] # final consonant
  root2 = c1 + v + c2
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root2,sew_code,dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif (sew_code == 'aniw') and (root in ['kfz','spfS','tfp']) and (ForC_sym in ['lfw','lfN']):
  # Antoine2#137, p.89. gives the optional 'ra' forms for kfz,spfS
  # Kale p. 303 gives the optional 'ra' form for 'tfp'
  # in simple future
  # For these roots
  # 'ra' optionally before a strong termination beginning
  # with any consonant except a nasal or a semi-vowel.
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  c1 = parts[0] # initial consonant
  v = 'ra' # replace 'f' with 'ra'
  c2 = parts[2] # final consonant
  root2 = c1 + v + c2
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root2,sew_code,dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif (ForC_sym == 'luw') and \
  ((root in ['iz','sah','luB','riz','ruz']) or\
   ((root == 'sah') and (theclass=='1')and(pada=='a'))):
  # Kale 472. These roots admit 'i' optionally in the First Future
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'vew',dbg=dbg)
 elif (root == 'kxp') and (pada=='p'):
  # Kale 473. 'kxp' is optionally 'p' (parasmaipada) 
  # in the futures and conditional,
  # and when so it rejects 'i'
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'aniw',dbg=dbg)
 elif (root == 'grah'):
  # Kale 474. The augment 'i' as added to 'grah' is long
  # in all non-conjugational tenses, except in the Perfect
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,None,'I',dbg=dbg)
 elif ((root == 'vf') or (ylast == 'F') or ((ylast == 'f') and (1 < len(parts[0])))) and\
  ((ForC_sym == 'ASIrliN') and (pada == 'a')):
  # Kale 586 . These roots admit 'i' optionally in benedictive A
  # For all changes to work properly, it is needed to
  # call '..-main' with aniT and with seT
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'aniw',dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'sew',dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif (ForC_sym == 'ASIrliN') and (pada == 'a') and (sew_code == 'vew'):
  # For logic to work properly, 'veT' must be separated here
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'aniw',dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'sew',dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif ((root == 'vf') or (ylast == 'F')) and\
      (not ( (ForC_sym == 'ASIrliN') and (pada == 'a'))):
  # Kale 475. The intermediate 'i' is optionally lengthened in the
  # case of 'vRi' and roots ending in 'RI', except in the
  # Perfect, the Benedictive atmanepada, and the Aorist parasmaipada
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,None,'i',dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,None,'I',dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif ( ((root,pada)==('gam','p')) or (root=='han') or (ylast=='f')) and\
      (ForC_sym in ['lfw','lfN']):
  # Kale 483 p. 301
  # 'gam P', 'han', and 'aniT' roots ending in 'Ri'
  # admit 'i' in the Second Future and the conditional.
  # 'gam P' - also that substituted for 'i' (to go) and
  #  with 'adhi' (to remember) - also admits it in the Desiderative
  # svRi is 'seT' in the 2nd Fut and Conditional (Kale p.305)
  # NOTE: 'han' also admits 'i' in passive (atmanepada)
  #  of 'aashiirli~N' (p .364 example)
  #       This is handled elsewhere
  # however, normal benedictive of 'han' does not admit 'i' (dhaatukosha)
  if (ylast == 'f') and (sew_code != 'aniw') and (root != 'svf'):
   err = err0 + "ForC warning. Continuing"
   #print err
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'sew',dbg=dbg)
 elif (root in ['kxp','vft','vfD','SfD','syand']) and (pada == 'a') and\
      (ForC_sym in ['lfw','lfN']):
  # Kale 484 p. 301
  # The roots (above) optionally take parasmaipada terminations in
  # the Second Future, Conditional, and Desiderative.
  # They reject the augment 'i' when parasmaipada terminations are taken.
  # Note: the present logic (implicitly) assumes this applies
  # in the passive voice (whose form is 'A'), as well as active voice
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,'p',root,'aniw',dbg=dbg)
  ans = join_arrays(ans1,ans2)
  #print "check Kale484\nans1=",ans1,"\nans2=",ans2,"\nans=",ans
 elif (root in ['kft','cft','Cfd','tfd','nft']) and (ForC_sym != 'luw'):
  # Kale 485 p.302
  # The roots above take 'i' optionally when followed by
  # an 'aarchadhaatuka' (non-conjugational) termination beginning
  # with an 's' except in the Aorist
  # By the examples on p. 305 and on p. 306, I inferred that
  # this rule does not apply for the 'luT'.
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'vew',dbg=dbg)
 elif (root == 'nft') and (ForC_sym == 'luw'):
  # 'nRit' is classed as 'veT'
  # however, from Kale dhaatukosha, its 'luT' (periphrastic future)
  # is 'seT' (inserts 'i'). Also, on p. 306, Kale says 'nRit P' is
  # to be conjugated like 'Chrid'
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'sew',dbg=dbg)
 elif (root == 'i') and (upasargas == ['aDi']) and (pada=='a') and \
      (ForC_sym == 'lfN'):
  # Kale 486. p. 302
  # In the case of 'i' with 'adhi', the root 'gaa' is
  # optionally substituted for 'i' in the conditional and the aorist.
  # In this case, 'i' is substituted for the final vowel of 'gaa'
  # before a consonantal weak termination; all terminations added
  # to 'gaa' for 'i' are weak. This can be due to the establishment
  # of 'gaa' as a 'seT' verb
  # NOTE: By the example, actually 'ii' is substituted for 'aa'
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,'gA','sew',dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif ((root == 'dA') and (theclass in ['3','1'])) or\
      (root in ['DA','do','de','mA','sTA','pA','hA','so']):
  # Kale 486. 'i' is substituted for the final vowel of these verbs
  # before a consonantal weak termination.
  # Note: for lRiT and lRi~N, all terminations are consonantal but
  #  strong, thus these changes do not apply
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
 elif (root in ['aYj','aS']):
  # Kale p. 304. Roots which are 'veT' in luT, lRit, lRi~N
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'vew',dbg=dbg)
 elif (root in ['gup','DUp','vicC','paR','pan','kam','ft']):
  # Kale 461.
  # These roots preserved their conjugational bases optionally.
  # These have present-system conjugational forms that look
  # like conjugation 10 forms.
  # When the conjugation-10 form is used, the roots are 'seT',
  #  which is the conjugation-10 standard.
  # When the non-conjugation-10-form is used, the roots take
  # the general seT-code associated with the root.
  root2map = {'gup':'gopAy', 'DUp':'DUpAy','vicC':'vicCAy',
   'paR':'paRAy','pan':'panAy','kam':'kAmay','ft':'ftIy'}
  root2 = root2map[root]
  ans1 = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
  ans2 = conjugation_tab_ForC_main(upasargas,theclass,pada,root2,'sew',dbg=dbg)
  ans = join_arrays(ans1,ans2)
 elif (root == 'DU') and (theclass=='6'):
  #  Kale example p. 305
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'sew',dbg=dbg)
 elif (root == 'DU') and (theclass=='1'):
  #  Kale example p. 305
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,'vew',dbg=dbg)
 else:
  ans = conjugation_tab_ForC_main(upasargas,theclass,pada,root,dbg=dbg)
 return ans

def join_arrays_f(x):
 if isinstance(x,tuple):
  y = list(x) # assume x is a tuple. convert it to list
 else:
  y = x
 if not isinstance(y,list):
  return x
 y = flatten(y)
 z = []  
 for w in y:
  if w not in z:
   z.append(w)
 return solution(z)

def join_arrays(a,b):
 """ Not sure this is what is wanted.  
   Elisp join-arrays is in gram2-liT.el
   Revised June 25, 2016
 """
 c = zip(a,b)
 ans = map(join_arrays_f,c)
 return ans

def join_arrays_many(a):
 c = zip(*a)
 #ans = map(lambda x: solution(list(x)),c
 ans = map(join_arrays_f,c)
 #for i in xrange(0,len(c)):
 # d = join_arrays_f(c[i])
 # print "join_arrays_many: (%d) %s -> %s" %(i,c[i],d)
 return ans

def reduplicative_liw_P(root,theclass,dbg=False):
 """
  ; Antoine#106 The reduplicative perfect is common to all
  ; monosyllabic roots beginning with a consonant or with
  ; 'a', 'aa', or short 'i', 'u', or 'Ri'
  ; The periphrastic perfect is used with roots beginning
  ; with a long vowel (other than 'aa') and with roots of
  ; the 10th conjugation and other derivative roots.
  ; The roots 'day' (to pity), 'kaas' (to cough),
  ; 'aas' (to sit) take the periphrastic perfect.
  ; The roots 'uurNu' (to cover) and 'RichCh' (to go) take
  ; the reduplicative perfect
  ; The roots 'uSh' (to burn), 'vid' (to know), 'bhii' (to fear),
  ; 'bhRi' (to support), 'hRi' (to take away), 'hrii' (to blush),
  ; 'jaagRi' (to awake), and 'daridraa' (to be poor) take both
  ; forms of the perfect
 """
 tok = root
 (parts,types) = word_parts(tok)
 if root in ['UrRu','fcC']:
  return True
 if root in ['uz','vid','BI','Bf','hf','hrI','jAgf','daridrA']:
  return True
 if theclass not in ['1','2','3','4','5','6','7','8','9']:
  return False
 if tok[0] in ['I','U','F','X']:
  return False
 if types not in ['cv','cvc','v','vc']:
  return False
 if root in ['day','kAs','As']:
  return False
 return True # default

def conjugation_tab_liw_p(upasargas,theclass,pada,root,voice=None,dbg=False):
 """
 ; periphrastic perfect
 ; NOTE: It is assumed that the arguments provide an instance to
 ; which the periphrastic perfect is applicable.
 ; Antoine #122.
 ; To form the periphrastic perfect, a verbal noun in the accusative
 ; is derived from the verbal base by the addition of 'aam'.
 ; To that verbal noun, the reduplicative perfect of 'kRi', 'bhuu',
 ; or 'as' is added. In classical Sanskrit, the perfect of 'as' is used
 ; much more frequently than that of 'kRi' or 'bhuu'.
 ; Before the addition of 'aam', a final vowel and a short medial vowel
 ; take guna, except the short 'i' of 'vid.
 ; When the roots 'bhii', 'hrii', 'bhRi' and 'hu' are conjugated in the
 ; periphrastic perfect, they are reduplicated as in the 3rd conjugation
 ; before the addition of 'aam'. 
 ; Kale #525 p. 329.
 ;  When the forms of 'kRi' are added, a parasmaipada root takes the
 ;  parasmaipada forms, and an atmanepadi one takes the atmanepadi forms.
 ;  NOTE: When the forms of 'bhuu' or 'as' are added, the parasmaipadi forms
 ;   are used, regardless of the form of the root
 ; NOTE: However, when the voice is PASSIVE, the atmanepadi forms of
 ; 'bhuu' and 'as' are added
 ; Implementation Notes:
 ; 1. For a given pada (P or A), there will be three conjugation tables;
 ;    namely, one each for 'as', 'kRi' and 'bhuu' (in that order)
 ;    These are joined, element-wise, into one table to provide a single
 ;    table as an answer.
 ; 2. Since the reduplicatives of 'as', 'kRi', and 'bhuu' are required
 ;    each time,  they are only computed once, and kept in global variables.
 ;    The global variables 'periphrastic-suffix-P' and 'periphrastic-suffix-A'
 ;    in fact have also joined 'aam' to the various reduplicative perfect
 ;    forms.
 ;    'periphrastic-suffix-P' = ( as-P bhuu-P kRi-P )
 ;    'periphrastic-suffix-A' = ( as-P bhuu-P kRi-A ) (only kRi form is 'A')
 ; voice should be 'ACTIVE or 'PASSIVE
; (if (equal voice 'PASSIVE) (setq pada 'A))
 """
 err0 = "conjugation_tab_liw_p(%s,%s,%s,%s,%s)"%(upasargas,theclass,pada,root,voice)
 if dbg:
  print err0
 
 if voice == 'passive':
  sfxpada = 'passive'
 else:
  sfxpada = pada
 sfxtab = periphrastic_suffix(sfxpada)
 base = periphrastic_base(root,theclass,pada)
 if not (base and sfxtab):
  return None # periphrastic perfect not applicable
 ans = []
 for xar in sfxtab: # xar is a list of strings
  #xar1 = flatten(xar)
  yar = map(lambda x: map(lambda b: conjugation_join(b, x),base), xar)
  yar = flatten(yar)
  #print "xar=",xar,"yar=",yar
  ans.append(yar)
 return ans

def periphrastic_init1(theclass,pada,root):
 """ prepare reduplicative perfect forms for roots 'as', 'kf','BU',
     and prefix with the 'Am' endings of periphrastic perfect action nouns.
 """
 ctab = conjugation_tab_liw_r([],theclass,pada,root);
 ans = []
 """
 # There may be comma-delimited alternates. If so, replace them with
 # lists
 def f(x):
  if isinstance(x,list):
   return x
  parts = x.split(',')
  if len(parts) > 1:
   return parts
  else:
   return x
 """
 for x in ctab:
  if not isinstance(x,list):
   x = [x]
  y = map(lambda x1: conjugation_join('Am',x1),x)
  #z = flatten(y)
  z = solution(y) # not in Elisp version of peripharstic-init1
  #print "x=",x,"y=",y,"z=",z
  ans.append(z)
 return ans

def periphrastic_init(pada):
 if pada == 'p': #parasmaipada
  ans1 = periphrastic_init1("2",pada,"as")
  ans2 = periphrastic_init1("8",pada,"kf")
  ans3 = periphrastic_init1("1",pada,"BU")
 elif pada == 'a': # atmanepada
  ans1 = periphrastic_init1("2","p","as")  # why parasmaipada used?
  ans2 = periphrastic_init1("8",pada,"kf")
  ans3 = periphrastic_init1("1","p","BU") #  why parasmaipada used?
 elif pada == 'passive': # 
  ans1 = periphrastic_init1("2","a","as")
  ans2 = periphrastic_init1("8","a","kf")
  ans3 = periphrastic_init1("1","a","BU")
 else:
  err= "ERROR periphrastic_init(%s) wrong pada %s" %(pada,pada)
  raise NameError(err)
 anst = zip(ans1,ans2,ans3) 
 # anst is a list of tuples. Convert it to a list of lists
 # also, flatten the list
 ans = map(lambda x: flatten(list(x)),anst)
 #print "anst=",anst,"ans=",ans
 return ans

class PeriphrasticSfx(object):
 """ Class variables used to keep results of periphrastic_init. 
     Maintained and used by periphrastic_suffix
 """
 d = {'p':None,'a':None,'passive':None}

def periphrastic_suffix(pada,dbg=False):
 """  
 """
 err0 = "periphrastic_suffix(%s)" % pada
 if dbg:
  print err0
 if pada not in PeriphrasticSfx.d:
  return None  # error condition. Should never occur
 ans = PeriphrasticSfx.d[pada]
 if ans == None:
  # recompute ans
  ans = periphrastic_init(pada)
  # and update so recomputation not required
  PeriphrasticSfx.d[pada]=ans
 return ans

def periphrastic_base(root,theclass,pada,dtype=None,dbg=False):
 """ returns a list of strings
 """
 err0 = "periphrastic_base(%s,%s,%s,%s)" % (root,theclass,pada,dtype)
 if dbg:
  print err0
 if dtype in ["c"]:  # 'c' is Causal
  ans = causal_bases_gentense(root,theclass,pada,[],'liw_p','active')
 elif theclass in ["10","1","4","6"]:
  ans = construct_conjbase1a(root,theclass,pada,[],None,dbg)
  #print (err0 + " construct_conjbase1a -> %s " % ans).encode('utf-8')
 elif root in ['BI','hrI','Bf','hu']:
  ans = reduplicate(root)
 else:
  ans = root
 if not isinstance(ans,list):
  ans = [ans]
 if ans != ['vid']:
  ans = map(lambda a: gunate_final_vowel(a),ans)
  #print "ans, after gunate=",ans
 return ans

def periphrastic_liw_P(root,theclass,dtype=None,dbg=False):
 """
  ; Antoine#106 The reduplicative perfect is common to all
  ; monosyllabic roots beginning with a consonant or with
  ; 'a', 'aa', or short 'i', 'u', or 'Ri'
  ; The periphrastic perfect is used with roots beginning
  ; with a long vowel (other than 'aa') and with roots of
  ; the 10th conjugation and other derivative roots.
  ; 10-05-04: The presence of a derivative root is indicated
  ;   by the optional argument 'dtype' (e.g. 'c' for causal)
  ; The roots 'day' (to pity), 'kaas' (to cough),
  ; 'aas' (to sit) take the periphrastic perfect.
  ; The roots 'uurNu' (to cover) and 'RichCh' (to go) take
  ; the reduplicative perfect
  ; The roots 'uSh' (to burn), 'vid' (to know), 'bhii' (to fear),
  ; 'bhRi' (to support), 'hRi' (to take away), 'hrii' (to blush),
  ; 'jaagRi' (to awake), and 'daridraa' (to be poor) take both
  ; forms of the perfect
 """
 tok = root
 (parts,types) = word_parts(tok)
 if dtype: # derived types take the periphrasitc perfect
  return True
 if root in ['UrRu','fcC']:
  return False
 if root in ['uz','vid','BI','Bf','hf','hrI','jAgf','daridrA']:
  return True
 if theclass == '10':
  return True
 if tok[0] in ['I','U','F','X']:
  return True
 if types not in ['cv','cvc','v','vc']:
  return True
 if root in ['day','kAs','As']:
  return True
 return False # default

def conjugation_tab_liw_r(upasargas,theclass,pada,root,voice=None,dbg=False):
 """
 ; reduplicative perfect tense.
 ; A few roots have two optional forms. In this routine,
 ; the main perfect routine is called for two roots,
 ; and the two results combined.
 ; For the majority of roots, the main routine is called
 ; for the given dhaatu.
 ; Some other irregular forms, algorithmically problematic,
 ; are also included here.
 ; voice should be 'ACTIVE or 'PASSIVE or nil
 """
 err0 = "conjugation_tab_liw_r(%s,%s,%s,%s,%s)"%(upasargas,theclass,pada,root,voice)
 if dbg:
  print err0
 if voice == 'passive':
  pada = 'a'
 liw_r_bitab=None
 if root in ['Svi','ve','SranT','granT','damB','svaYj','ad','ci','aj','cakz','vid']:
  # verbs with optional forms
  ans = [] # a list of answers
  if root == 'Svi':
   # Kale p. 318
   # 'shvi' is to be optionally considered as 'shu' in the Perfect
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,'Su',dbg=dbg))
  elif root == 've':
   #Kale p. 318
   # ve (to weave) has a regular form and an irregular form.
   # In the irregular form,
   #  it is considered 'uvay' before strong forms,
   #  and it is considered either 'uuy' or 'uuv' before weak forms.
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   if pada == 'p':
    ans2 = ['uvAya',['UyatuH','UvatuH'],['UyuH','UvuH'],
            'uvayiTa',['UyaTuH','UvaTuH'],['Uya','Uva'],
            ['uvAya','uvaya'],['Uyiva','Uviva'],['Uyima','Uvima']
           ]
   else:
    ans2 = [['Uye','Uve'],['UyAte','UvAte'],['Uyire','Uvire'],
            ['Uyize','Uvize'],['UyATe','UvATe'],['UyiDve','UviDve','UyiQve','UviQve'],
            ['Uye','Uve'],['Uyivahe','Uvivahe'],['Uyimahe','Uvimahe']
           ]
   ans.append(ans2)
  elif root in ['SranT','granT','damB','svaYj']:
   #Kale p.323
   # The roots 'shranth', 'granth', 'dambh', and 'sva~nj' drop their
   # nasal optionally before the terminations of the Perfect.
   # When the nasal is dropped,
   # 'shranth', 'granth', 'dambh' obey #500, even before the strong
   # terminations
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   tok = root
   tok1 = tok[0:-2]+tok[-1:]
   root1 = tok1
   sewPerfCodes=construct_sewPERF_code1a(root,theclass,pada,upasargas)
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root1,sewPerfCodes,dbg=dbg))
  elif root == 'ad':
   #Kale #511, p. 324
   # 'ghas' (1P) is to be optionally substituted for 'ad' in the Perfect
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   ans.append(conjugation_tab_liw_r_main(upasargas,"1",pada,"Gas",dbg=dbg))
  elif root == 'ci':
   #Kale #514, p. 326
   # 'chi' optionally changed to 'ki' after the reduplicative
   # syllable in the Perfect and the Desiderative.
   # The change to 'ki' appears in the basic algorithm in ans1.
   # We simply assert the value in 'ans2'
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   ans2 = ["cicAya","cicyatuH","cicyuH",["ciceTa","cicayiTa"],"cicyaTuH","cicya",["cicaya","cicAya"],"cicyiva","cicyima"] 
   ans.append(ans2)
   
  elif (root == 'aj') and (pada == 'p'):
   #Kale #515 p. 327, #477
   # 'vii' (2 P) is substituted for 'aj' (1 P 'to go')
   #   necessarily before any
   # non-conjugational termination, and optionally before such as
   # begin with any consonant except 'y':
   #   'vetaa  ajitaa'
   #   'veShyati ajiShyati'
   ans1 = conjugation_tab_liw_r_main(upasargas,'2',pada,'vI',dbg=dbg)
   ans2 = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
   #print "ans1=",ans1
   #print "ans2(a)=",ans2
   for i in xrange(0,len(ans2)):
    if i not in [3,7,8]:
     ans2[i]= ans1[i] # Effectively drops the alternate
   #print "ans2(b)=",ans2
   ans.append(ans1)
   ans.append(ans2)
  elif root == 'cakz':
   C = construct_sewPERF_code1a(root,theclass,pada,upasargas)
   if pada == 'p':
    ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,'KyA',C,dbg=dbg))
    ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,'kzA',C,dbg=dbg))
   else:
    ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,C,dbg=dbg))
    ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,'KyA',C,dbg=dbg))
    ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,'kzA',C,dbg=dbg))
  elif (root == 'vid') and (pada == 'p'):
   # ans1 is the form described by Antoine2#121. (no reduplication)
   # However, Kale(p. 330) shows 'viveda', etc.
   # Whitney shows both forms, identifying the no-redup form as part
   # of vedic Sanskrit, and the Kale form as part of classical Sanskrit.
   # ans1 gives the Vedic form.
   # ans2 gives the classical form
   ans.append(conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg))
   ans2 = ['viveda','vividatuH','vividuH',
           'vivediTa','vividaTuH','vivida',
           'viveda','vividiva','vividima']
   ans.append(ans2)
  else:
   # otherwise, the pada of the suspect root means it is
   # treated without optional form. We mimic this by setting
   # This could arise, for instance, for the passive of a normally
   # 'P' root, since the passive is just the 'A' form conjugation
   # ans2 to ans1
   ans1 = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
   ans.append(ans1)
   ans.append(ans1)
  # Now, reformat ans
  #ans = join_liw_arrays(ans)
  ans = join_arrays_many(ans)
 elif root == 'vye':
  # Kale 506 p. 319 'vye'.
  # 'vye' becomes 'vivyay' before strong terminations and 'vivii'
  # before the weak ones in the Perfect
  if pada == 'p':
   ans = ['vivyAya','vivyatuH','vivyuH',
          'vivyayiTa','vivyaTuH','vivya',
          ['vivyAya','vivyaya'],'vivyiva','vivyima']
  else:
   ans = ['vivye','vivyAte','vivyire',
          'vivyize','vivyATe',['vivyiDve','vivyiQve'],
          'vivye','vivyivahe','vivyimahe']
 elif root == 'hve':
  # Kale 506, p. 319.
  # 'hve' (to call) is to be considered 'hu' (to sacrifice) in the Perfect
  # class is changed to that of 'hu' (namely, 3)
  ans = conjugation_tab_liw_r_main(upasargas,"3",pada,'hu',dbg=dbg)
 elif (pada == 'p') and (root == 'taYc'):
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  ans[4] = [ans[4],'tataNkTuH']
 elif (pada == 'p') and (root == 'mfj'):
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # otherwise 1D and 2D are
  # (mamaarjva mamaarjiva mamRijva mamRijiva) and
  # (mamaarjma mamaarjima mamRijma mamRijima).
  # the 1st form of each is not used
  ans[7] = ans[7][1:]
  ans[8] = ans[8][1:]
 elif (pada == 'p') and (root == 'tfp'):
  # Kale p. 320
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  tmp = ans[3]
  if not isinstance(tmp,list):
   tmp = [tmp]
  tmp.append('tatrapTa')
  ans[3] = tmp
 elif (pada == 'p') and (root == 'dfp'):
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  tmp = ans[3]
  if not isinstance(tmp,list):
   tmp = [tmp]
  tmp.append('dadrapTa')
  ans[3] = tmp
 elif (pada == 'a') and (root == 'trap'):
  # Kale p. 321
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  ans[5] = ['trebDve','trepiDve'] # was ['trepDve','trepiDve']
 #elif (pada == 'p') and (root == 'aS'):
 # # Kale p. 321
 # ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
 # ans[5] = ['AnaRQve','AnaSiDve'] # was ['AnaSDve','AnaSiDve']
 elif (pada == 'a') and (root == 'aS'):
  # Kale p. 321
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  ans[5] = ['AnaNQve','AnaSiDve']
 elif (pada == 'a') and (root == 'gAh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was ['jagAkze','jagAhize']
  ans[3]=['jaGAkze','jagAhize'] 
  # was ['jagAhDve','jagAhiDve']
  ans[5] = ['jaGAQve','jagAhiDve','jagAhiQve']
 elif (pada == 'a') and (root == 'gfh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was ['jagfkze','jagfhize']
  ans[3] = ['jaGfkze','jagfhize']
  # was ['jagfhDve','jagfhiDve']
  ans[5] = ['jaGfQve','jagfhiDve','jagfhiQve']
 elif (pada == 'a') and (root == 'guh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was ['juguhse','juguhize']
  ans[3] = ['juGukze','juguhize']
  # was ['juguhDve','juguhiDve']
  # When 'Q' or 'r' is dropped,
  # the preceeding 'a','i' or 'u' is lengthened
  ans[5] = ['juGUQve','juguhiDve','juguhiQve']
 elif (pada == 'p') and (root == 'guh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was 'jugoha'
  ans[0] = 'jugUha'
  ans[6] = 'jugUha'
  # was ['jugoQa','jugohiTa']
  # Kale actually shows 'jagoDha', but I considered the 'ja' an error
  ans[3] = ['jugoQa','jugUhiTa']
 elif (pada == 'p') and (root == 'druh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was ['dudroQa','dudrohiTa']
  # The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
  # changed to 'gh' or to 'Dh' when followed by
  #  (a) any consonant except a nasal or a semivowel, or
  #  (b) by nothing
  ans[3] = ['dudroQa','dudrogDa','dudrohiTa']
 elif (pada == 'p') and (root == 'muh'):
  # Kale p. 322
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was (mumoDha mumohitha)
  # The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
  # changed to 'gh' or to 'Dh' when followed by
  #  (a) any consonant except a nasal or a semivowel, or
  #  (b) by nothing
  ans[3] = ['mumoQa','mumogDa','mumohiTa']
 elif (pada == 'p') and (root == 'snih'):
  # Kale p. 323
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was (siShNeDha siShNehitha)
  # The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
  # changed to 'gh' or to 'Dh' when followed by
  #  (a) any consonant except a nasal or a semivowel, or
  #  (b) by nothing
  ans[3] = ['sizReQa','sizRegDa','sizRehiTa']
 elif (pada == 'p') and (root == 'snuh'):
  # Kale p. 323
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was (suShNoDha suShNohitha)
  # The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
  # changed to 'gh' or to 'Dh' when followed by
  #  (a) any consonant except a nasal or a semivowel, or
  #  (b) by nothing
  ans[3] = ['suzRoQa','suzRogDa','suzRohiTa']
 elif (root == 'i') and (pada == 'a') and (upasargas == ['aDi']):
  # Kale #517, p. 327
  # The base of 'i' with 'adhi' (to study) is 'ajijagaa'
  sewPerfCodes = construct_sewPERF_code1a(root,theclass,pada,upasargas)
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,'jag',sewPerfCodes,dbg=dbg)
 elif (pada == 'p') and (root == 'UrRu'):
  # Kale p. 327
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was 'UrRunaviTa'
  ans[3] = ['UrRunaviTa','UrRunuviTa']
 elif (pada == 'a') and (root == 'pyE'):
  # Kale p. 329
  # 'pii' is substituted for 'pyai' (1 A 'to grow fat')
  # in the Perfect and in the Frequentative
  sewPerfCodes = construct_sewPERF_code1a(root,theclass,pada,upasargas)
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,'pI',sewPerfCodes,dbg=dbg)
 elif (pada == 'p') and (root == 'vij'):
  # Kale 525, p. 329. For the forms of vij see #466:
  # 'viveja' 1S, 'vivijitha' 2S, 'vivijathuH' 2D, 'vivija' 2P
  # Kale #466: The intermediate 'i' is weak in the case of
  # the root 'vij' (6 A 7 P); and optionally so in the case of 'uurNu'
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  # was 'vivejiTa'
  ans[3] = 'vivijiTa'
 else:
  # the usual case. liT-main provides the answer
  ans = conjugation_tab_liw_r_main(upasargas,theclass,pada,root,dbg=dbg)
  #print "check1: ans=",ans
 return ans

def join_liw_arrays(L):
 """ gram2-liT.el join-arrays.
     This is modified for Python
     Assume L is a list of arrays.
     e.g., when L is of length 2 = [X,Y], where X and Y are arrays
     Say X = [1,2], Y = [3,4]
     Then zip(*L) = [(1,3),(2,4)]
     We then return [[1,3],[2,4]]
     However, suppose Y is [1,5]
     Then zip(*L) = [(1,1),(2,5)]
     and we return [1,[2,5]]
 """
 a = zip(*L)
 def distinctF(x):
  y = []
  for z in x:
   if z not in y:
    y.append(z)
  if len(y) == 1:
   return y[0]
  else:
   return y
 return map(distinctF,a)

def conjugation_tab_liw_r_main(upasargas,theclass,pada,root,sewPerfCodes=None,dbg=False):
 """
 ; perfect conjugation, Main routine
  upasargas is needed for case of sam-kf
 """
 err0 = "conjugation_tab_liw_r_main(%s,%s,%s,%s,%s)"%(upasargas,theclass,pada,root,sewPerfCodes)
 if dbg:
  print err0
 #1. construct endings and strengths
 endings = conj_endings("liw","1",pada)
 liw_r_endings = endings
 #strengths = conj_endings("liw","1",pada,strengths=True)
 #2. init ans
 n = len(endings)
 ans=[]
 #3. Get bitab, which will be joined to endings
 bitab = liw_main_get_bitab(upasargas,theclass,pada,root,sewPerfCodes,dbg=dbg)
 #print "check: bitab=",bitab
 # one modification to 'endings'
 if pada == 'p':
  atok = root
  lc = atok[-1:]
  if lc in 'AeEo':
   # Antoine2##112
   # When the root ends in 'aa', the perfect
   # a. takes ending 'au' in 3S and 1S parasmaipada 
   endings[0] = 'O'
   endings[6] = 'O'
 # 6. combine base and endings to get ans
 ans = perfect_bitab_join(bitab,endings,dbg)
 # 7. Irregularities not yet covered
 if (root == 'ah'):
  #Antoine2#120. The root 'ah' (to say) has not all the forms of the
  #perfect. The first person is completely lacking, and so is 2P.
  #NOTE: 2S is irregular (the regular form would be 'aahitha'
  ans[3] = 'AtTa'
  ans[5] = ''  # use empty string instead of Elisp nil
  ans[6] = ''
  ans[7] = ''
  ans[8] = ''
 elif (pada == 'p') and (root in ['mi','mI']):
  # Kale 505 (p. 312). confirmed by Antoine appendix
  # 'mi 5 P/A'
  # 'mii 9 P/A'
  ans[0] = 'mamO' # was 'mimAya'
  ans[3] = ['mamATa','mamiTa'] # was ['mimeTa','mimayiTa']
  ans[6] = 'mamO' # was ['mimaya','mimAya']
 elif (pada == 'p') and (root == 'lI'):
  # Kale 505 (p. 312) confirmed by Antoine appendix
  # lii 9 P , 4 A : to adhere
  # lii 1 P : to melt
  ans[0] = ['lilAya','lalO'] # was 'lilAya'
  ans[3] = ['lileTa','lilayiTa','lalATa','laliTa'] # was ['lileTa','lilayiTa']
  ans[6] = ['lilaya','lilAya','lalO'] # was ['lilaya','lilAya']
 elif (pada == 'p') and (root == 'pracC'):
  #Kale 505, p.313  Why skipped in Elisp code?
  #   (aset ans 3 '(paprachChitha papraShTa)) ; was paprachChitha
  pass
 elif (pada == 'p') and (root == 'Brasj'):
  ans[3] = ['baBrazWa','baBrajjiTa','baBarzTa','baBarjiTa']
  # was ['baBrajkTa','baBrajjiTa','baBarkTa','baBarjiTa']
 elif (pada == 'p') and (root == 'sfj'):
  ans[3] = ['sasrazWa','sasarjiTa']
  # was ['sasarkTa','sasarjiTa'] # Kale, p. 314
 elif (pada == 'p') and (root == 'dfS'):
  ans[3] = ['dadrazWa','dadarSiTa']
  # was 'dadarziTa' # Kale p. 314
 elif (pada == 'p') and (root == 'dah'):
  ans[3] = ['dadagDa','dehiTa']
  # was ['dadahTa','dehiTa'] # Kale p. 315
 elif (pada == 'p') and (root == 'nah'):
  ans[3] = ['nanadDa','nehiTa']
  # was ['nanahTa','nehiTa'] # Kale p. 315
 if dbg:
  print err0," returns ",ans
 return ans

def liw_main_get_bitab_condition(pc,types,parts,atok,redup,root,pada,upasargas):
 """ A returns True or False.
    A very complex condition used twice in liw_main_get_bitab  
 """
 # medial 'a', with simple first consonant, simple last consonant,
 # and whose initial consonant was unchanged in reduplication;
 # and root is not jan (Antoine),
 # and root is not Sas, dad (Kale #500)
 # and root is not  raD, jaB (Kale #508, p. 320)
 # and, if pada is 'a' and root is not jag (Kale #516. 'jag' used for 'adhi-i')
 if (pc == 'a') and (types == 'cvc') and\
    (len(parts[0]) == 1) and (len(parts[2]) == 1) and\
    (atok[0] == redup[0]) and\
    (root not in ['jan']) and\
    (atok[0]!='v') and\
    (root not in ['Sas','dad']) and\
    (root not in ['raD','jaB']) and\
    ((pada == 'p') or \
     ((pada == 'a') and (root not in ['jag']))\
    ):
    return True
 # roots which follow the rule, though not fitting usual mold
 # Kale#509. Optional forms of 'shranth', 'granth'
 # ; Kale #512. The logic provides optional forms for these roots
 if (root in ['Baj','tF','trap','Pal']) or\
   ((root == 'rAD') and (upasargas == ['apa'])) or\
   (root in ['SraT','graT']) or\
   (root in ['jF','Bram','tras','PaR','rAj','BrAj',
    'BrAS','BlAS','syam','svan']):
  return True
 return False

def liw_main_get_bitab(upasargas,theclass,pada,root,sewPerfCodes=None,dbg=False):
 """ liT-main-get-bitab in   gram2-liT.el

 """
 err0 = "liw_main_get_bitab(%s,%s,%s,%s,%s)"%(upasargas,theclass,pada,root,sewPerfCodes)
 if dbg:
  print err0
 # 3a. atok
 if (root == 'kf') and (upasargas in [['sam'],['saMs']]):
  # Kale 504
  atok = 'skf'
 else:
  atok = root
 # 3b. parts, types
 wparts = word_parts(atok)
 (parts,types)= wparts
 # 3c. redups (a list)
 redups = reduplicate_perfect(atok,wparts)
 #print "check: redups=",redups
 redup = redups[0]
 if len(redups)>1:
  redup2 = redups[1]
 else:
  redup2 = redup
 # 4. sew codes
 if sewPerfCodes:
  temp = sewPerfCodes
 else:
  temp = construct_sewPERF_code1a(root,theclass,pada,upasargas,dbg)
 temp = solution(temp)
 #print "check: temp=",temp
 if temp and (not isinstance(temp,list)):
  temp = [temp]
 if root == 'vid':
  temp = ['aniw','aniw'] # Antoine2#120
 if (root == 'kf') and (upasargas == ['sam']): # Kale 515
  temp = ['sew','sew']
 if (root == 'kuz') and (upasargas == ['nir']):
  temp = ['vew','vew']
 # assume temp is a list with 1, 2, or 3 elements
 # fill it to length 3 with None.
 if len(temp) == 2:
  temp.append(None)
 elif len(temp) == 1:
  temp.append(None)
  temp.append(None)
 # extract the 3 elements of temp into variables
 (sew_gen,sew_th,sew_upa) = temp
 #print "check: sew_gen=%s,sew_th=%s,sew_upa=%s" %(sew_gen,sew_th,sew_upa)
 # 5a. get default table of i-inserts. 
 # Python note: Use None for missing elements. Elisp uses symbol 'NA
 if pada == 'p':
  itab = [None,None,None,
          sew_th,None,None,
          None,sew_gen,sew_gen]
 else: # pada == 'a'
  itab = [None,None,None,
          sew_gen,None,sew_gen,
          None,sew_gen,sew_gen]
 n = len(itab) # 9
 # replace 'vew' with ['aniw','sew'] in itab
 for i in xrange(0,len(itab)):
  if itab[i] == 'vew':
   itab[i] = ['aniw','sew']
 #print "check: itab=",itab
 # 5b. get table of base-seT codes (bitab)
 # Usually, each element is the list of elements of btab and itab.
 # However, for some exceptions, e.g. 'aa' roots, elements are
 # made idiosyncratically
 b = atok
 nb = len(atok)
 lc = b[-1:] # last char
 if nb == 1:
  pc = None  # no penultimate character
 else:
  if types in ['cvc','vc']:
   # e.g. pracC, pc = a. parts = ['pr','a','cC']
   temp = parts[-2:-1]  # ['a']
   pc = temp[0][0]  #temp[0] = string 'a'. temp[0][0] is first char, also = 'a'
  else:
   pc = b[nb-2]
 b = redup # note reuse of variable 
 bw = redup2
 if lc in 'AeEo':
  bw = b[0:-1] # drop lc; Antoine2#112
 #print "check: b=%s, bw=%s, lc=%s,pc=%s"%(b,bw,lc,pc)
 if pada == 'p':
  if lc in 'AeEo':
   # Antoine2##112
   # When the root ends in 'aa', the perfect
   # a. takes ending 'au' in 1S and 3S parasmaipada 
   # b. optionally drops 'aa' in 2S paramaipada
   # c. drop 'aa' before other terminations
   # Similarly, for roots ending in 'e ai o'; Note, for 2S-Parasmaipada,
   # the ending vowel is changed to 'aa'
   b = bw + 'A' # change last char to A
   b3s = bw
   b1s = bw
   bth = [b,bw]
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   bitab = perfect_bitab(btab,itab,dbg)
   bitab[3] = [[b,None],[bw,'sew']] # override 2S
   #print "check bitab=",bitab
  elif liw_main_get_bitab_condition(pc,types,parts,atok,redup,root,pada,upasargas):
   # Antoine2#113. Kale#500
   # Roots which have a medial 'a' preceded and followed by a single
   # consonant, and which keep their initial consonant unchanged in
   # reduplication - such roots form their perfect as follows:
   # 1. before weak terminations, there is no reduplication and the
   # medial 'a' of the root is changed to 'e'.
   # 2. In the parasmaipada 2S, there are two forms:
   #   a. one form using the base as above and inserting 'i'
   #   b. another using 'redup' and not inserting 'i'
   #N.B. The root 'bhaj', although beginning with an asplirate, is
   #conjugated like 'pat', e.g. 'babhaaja', 'bhejatuH', 'bhejuH'.
   # The same applies to
   #  'tRi' ('tataara', 'teratuH', 'teruH'),
   #  'trap' ('tatraapa' ,  'trepatuH' , 'trepuH'
   # 'apa' 'raadh' ('raraadha', 'redhatuH', 'redhuH')
   #print "liw_main_get_bitab: chk1"
   if len(atok) in [1,2,3]:
    bw = atok[0:1] + 'e' + atok[2:]  
   else:
    bw = atok[0:2] + 'e' + atok[3:]
   bs = b
   b3s = gunate_final_vowel(bs,True) #vrddhi
   b1s = [gunate_final_vowel(bs),gunate_final_vowel(bs,True)] # guna or vrddhi
   bth = [gunate_final_vowel(bs), bw]
   if root == 'tF':
    bw = 'ter'
    bth = bw
   elif root == 'jF':
    bw = 'jer'
   elif root == 'rAD':
    bth = bw
   elif root in ['SraT','graT','daB']: 
    # Kale #509
    b1s = bw
    b3s = bw
    bth = bw
   elif root == 'Pal':
    # Kale 512, p. 325
    bth = [bw]
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   if root in ['jF','Bram','tras','PaR','rAj',
               'BrAj','BrAS','BlAS','syam','svan']:
    # the form is optional
    btab = [b3s,[b,bw],[b,bw],
           [b,bw],[b,bw],[b,bw],
           b1s,[b,bw],[b,bw]]
   bitab = perfect_bitab(btab,itab,dbg=dbg)
   if root not in ['tF','rAD','SraT','graT','daB','Pal',
                   'jF','Bram','tras','PaR','rAj',
                   'BrAj','BrAS','BlAS','syam','svan']:
    # override for 2s
    bitab[3] = [[bth[0],'aniw'],[bth[1],'sew']]
   # end of the actions for complex elif
  elif (root == 'Brasj'):
   # Kale 505, p. 314
   b = [redup,redup2]
   btab = [b]*9
   bitab = perfect_bitab(btab,itab,dbg=dbg)
  elif kuwAdiP(root):
   # Kale 463; Kale 505 p. 316
   # By the general rule of how kuTaadi roots behave in the
   # non-conjugational tenses, the 2S is not strengthed,
   # while the 1S and 3S are strengthed
   # Roots of the kuTaadi class retain their vowel unchanged
   # optionally in the 1S of the perfect:
   #  nu -> (nunaava nunuva)  (otherwise, (nunava nunaava  ))
   # kuT -> (chukoTa chukuTa) (otherwise, (chukoTa chukauTa))
   # Notice in the case of 'nu', the unstrengthed vowel and the
   #   vrddhi-strenghthened one are used
   # However, in the case of 'kuT', the unstrengthed vowel and
   # the guna-strengthened one are used.
   # How to decide which subcase is not discussed by Kale.
   bv = gunate_final_vowel(b,True) # vrddhi
   bg = gunate_final_vowel(b) # guna
   bth = b # don't gunate in 2S
   if root == 'sPur':
    # per Kale, p. 316. This violates the general rule
    b3s = bg
    b1s = b3s
   elif types == 'cv':  # treat like 'nu'
    b3s = bv
    b1s = [b,b3s]
   else: # treat like 'kuw'
    b3s = bg
    b1s = [b,b3s]
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   bitab = perfect_bitab(btab,itab,dbg=dbg)
  elif (pc == 'a') or (lc in init.vowel_set):
   # medial 'a' or final vowel
   #print "check: pc=a"
   if root in ['i','f']:
    b = redup2
    bw = redup
   if root == 'i':
    vrddhi = 'iyE'
   else:
    vrddhi = gunate_final_vowel(b,True)
   guna = gunate_final_vowel(b)
   b3s = vrddhi
   b1s = [guna,vrddhi]
   bth = guna
   if root in ['SF','dF','pF']:
    #Kale #499. Guna is optionally used before weak terminations for
    # 'shRI' , 'dRI', and 'pRI'
    #NOTE: Without this code, the 'guna' substitute is the only one
    # used - this occurs in 'perfect-join1'. To allow the other
    #  option, I replace 'RI' with 'r' and have this, along with
    # the original weak form, as the new weak form
    bwalt = bw[0:-1] + 'r'
    bw = [bw,bwalt]
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   bitab = perfect_bitab(btab,itab,dbg=dbg)
   #print "check2. bitab=",bitab
  elif pc in init.simplevowel_set:
   b3s = gunate_final_vowel(b)
   bth = b3s
   b1s = b3s
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   bitab = perfect_bitab(btab,itab,dbg=dbg)
  else:
   # default situation, rarely encountered!  (e.g. 'raadh', 'uSh')
   b3s = b
   bth = b
   b1s = b
   btab = [b3s,bw,bw,
           bth,bw,bw,
           b1s,bw,bw]
   bitab = perfect_bitab(btab,itab,dbg=dbg)
 else:  # pada = 'a'
  if lc in 'AeEo':
   b = redup[0:-1] # drop last char
   btab = [b]*9
   bitab = perfect_bitab(btab,itab,dbg=dbg)
  elif liw_main_get_bitab_condition(pc,types,parts,atok,redup,root,pada,upasargas):
   # Antoine2#113.
   # Roots which have a medial 'a' preceded and followed by a single
   # consonant, and which keep their initial consonant unchanged in
   # reduplication - such roots form their perfect as follows:
   # 1. before weak terminations, there is no reduplication and the
   # medial 'a' of the root is changed to 'e'.
   #print "liw_main_get_bitab: chk2"
   if len(atok) in [1,2,3]:
    bw = atok[0:1] + 'e' + atok[2:]  
   else:
    bw = atok[0:2] + 'e' + atok[3:]
   if root == 'tF':
    bw = 'ter'
    bth = bw
   elif root == 'jF':
    bw = 'jer'
   btab = [bw]*9
   if root in ['jF','Bram','tras','PaR','rAj',
               'BrAj','BrAS','BlAS','syam','svan']:
    # the form is optional
    btab = [[b,bw]]*9
   bitab = perfect_bitab(btab,itab,dbg=dbg)
   # end of the actions for complex elif
  elif (root == 'Brasj'):
   # Kale 505, p. 314
   b = [redup,redup2]
   btab = [b]*9
   bitab = perfect_bitab(btab,itab,dbg=dbg)
  else:
   # default situation
   btab = [bw]*9
   bitab = perfect_bitab(btab,itab,dbg=dbg)
 if bitab not in GlobalVars.liw_r_bitab:
  GlobalVars.liw_r_bitab.append(bitab)
 return bitab

def kale_463_P(root):
 """ NOTE: don't need to have both functions
 """
 return kuwAdiP(root)

def kuwAdiP(root,dbg=False):
 """ 
 ; Kale 463.
 ; (In regard to the non-conjugation tenses)
 ; Neither guNa nor vRiddhi is sustituted for the vowel of a few
 ; roots of the 6th class even before a strong termination.
 ; However, the substitution is made in the following cases:
 ;  1. before the 'a' of the 1S and 3S of perfect tense (parasmaipada)
 ;  2. the 'aya' of the causal
 ;  3. the 'i' of the 3S of the passive aorist
 ; This function check whether 'dhaatu' is one of these few roots.
 ; There are a few more not often to be met with.
 """
 err0 = "kuwAdiP(%s)" %(root,)
 if dbg:
  print err0
 roots = ['kuw','puw','kuc','kuj','Dur',
          'sPuw','truw','luw','sPur',
          'gur','nu','du','ku']
 return (root in roots)

def perfect_join(base,sew_code,sup,dbg=False):
 """ somewhat recursive
 """
 if isinstance(sup,list):
  return map(lambda x: perfect_join(base,sew_code,x), sup)
 elif isinstance(base,list):
  return map(lambda x:  perfect_join(b,sew_code,sup),base)
 elif sew_code == 'vew':
  return map(lambda x: perfect_join(base,x,sup),['sew','aniw'])
 else:
  return perfect_join1(base,sew_code,sup,dbg)

def perfect_join1(y,sew_code,ending0,dbg=False):
 """ based on conjugation_join.
     sew_code is either Falsey (e.g. None, False,[]) or 'sew' or 'aniw'
     Assume all parameters are Strings (or Falsey)
 """
 err0 = "perfect_join1(%s,%s,%s)" %(y,sew_code,ending0)
 if sew_code == 'sew':
  # insert 'i'
  ending = conjugation_join('i',ending0)
 else:
  ending = ending0
 #sandhiget = SandhiGet(['Antoine72-4'])
 ny = len(y) # length of base 'y'
 ylast = y[-1:] # last char
 efirst = ending[0] # first char
 if dbg:
  print "y=%s,ending=%s,ylast=%s,efirst=%s" %(y,ending,ylast,efirst)
 #Kale 476. nash
 # 'n' is inserted before the ending consonant of 'nash' when
 # it is followed by any consonant except a nasal or a semi-vowel.
 # NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
 # NOTE 2: The logic is put here, because other changes, e.g.,
 #  before 'tha', are required. This logic applies to other
 #  forms than the perfect
 if (y == 'nanaS') and (efirst in init.consonant_set) and (not (efirst in init.semivowel_set)):
  y = 'nanaMS'
 if (efirst in init.vowel_set) and (ylast in 'iIf'):
  # 1st special sandhi rule for perfect (Antoine2#110)
  (parts,types) = word_parts(y)
  nparts = len(parts)
  y0 = y[0:-1] # all but last char
  if (y0 == ''):  
   # 07-24-03 for dhaatu='i', y='ii'
   y0 = y  
  if (1 < nparts) and (1 < len(parts[-2])):
   # compound consonant precedes ylast 'iIf'
   if ylast == 'i':
    ans = y0 + 'iy' + ending
   elif ylast == 'I':
    ans = y0 + 'iy' + ending
   else: # ylast == 'f'
    ans = y0 + 'ar' + ending
  else:
   # word is a single vowel, (i, I, f) OR
   # a simple consonant precedes final i,I,f
   if ylast == 'i':
    ans = y0 + 'y' + ending
   elif ylast == 'I':
    ans = y0 + 'y' + ending
   else: # ylast == 'f'
    ans = y0 + 'r' + ending
 elif (efirst in init.vowel_set) and (ylast in 'uUF'):
  # 2nd special sandhi rule for perfect (Antoine2#110)
  y0 = y[0:-1]
  if ylast == 'u':
   ans = y0 + 'uv' + ending
  elif ylast == 'U':
   ans = y0 + 'uv' + ending
  else: # ylast == 'F'
   ans = y0 + 'ar' + ending
 elif (efirst == 'D') and (ylast in init.vowel_set) and (not (ylast in 'aAi')):
  # 3rd special sandhi rule for perfect (Antoine2#110)
  ans = y + 'Q' + ending[1:]
 elif (efirst == 'T') and (2 < ny) and (y[-2:] == 'ar'):
  # this rule so [ch a k a r] + [th a]
  # becomes [ch a k a r th a] rather than [ch a k a s th a], which
  # is what 'sandhi-pair' does
  ans = y + ending
 elif (efirst == 'T') and (2 < ny) and (y[-2:] in ('cC','Sc','rj','kz')):
  # prachCh , vrashch, mRij, akSh
  y0 = y[0:-2]
  if y[-2:] == 'rj':
   y0 = y[0:-1]
  ans = y0 + 'zWa'
 elif (efirst == 'T') and (ylast == 'S'):
  # Kale p. 321. Example of klish
  y0 = y[0:-1]
  ans = y0 + 'zWa'
 elif (efirst == 'T') and (y == 'uvah'):
  # Kale #506. p. 317
  ans = 'uvoQa'
 elif (efirst == 'T') and (ylast == 'h'):
  # Kale p. 322.  Example of 'muh', 'druh', 'snih', 'snuh'
  y0 = y[0:-1]
  ans = y0 + 'Qa'
 elif (efirst == 'T') and (ylast in 'jc'):
  # this rule [bh a j] + [th a] -> [bh a k th a]
  # rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
  # but [bh a ~n j] + [th a] -> [bh a ~N k th a]
  if y == 'iyaj':
   ans = 'iya' + 'z' + 'Wa'
  elif y == 'baBaYj':
   ans = 'baBaNk' + ending
  elif y == 'mamajj':
   ans = 'mamaNk' + ending
  else:
   y0 = y[0:-1]
   ans = y0 + 'k' + ending
 elif (efirst == 'T') and (ylast == 'D'):
  # so [v i v y a dh] + [th a] -> [v i v y a d dh a]
  # sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
  y0 = y[0:-1]
  ans = y0 + 'd' + 'D' + ending[1:]
 elif (efirst == 'T') and (y == 'sasah'):
  # Kale # 506
  ans = 'soQa'
 elif (efirst == 'T') and (ylast in 'mn'):
  # For 'gam', sandhi-pair gives 'jagaMtha', but
  # Kale and Antoine both show 'jagantha'
  # Similaraly, for 'han' we want 'jaghantha' rather than 'jagaMtha'
  ans = y[0:-1] + 'n' + ending
 elif (efirst == 'm') and (ylast == 'c'):
  ans = y + ending # otherwise, 'c' is changed to 'j'
 elif (efirst in 'mv') and (ylast == 'm'):
  #Kale p. 321 (footnote)
  #Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
  #note 'n' may be changed to 'N' by sandhi-single (see below)
  y0 = y[0:-1]
  ans = y0 + 'n' + ending
 elif (efirst == 's') and (ylast in 'Sh'):
  # Kale p. 321. based on example of 'ash'
  # 'gaah', 'gRih' also have this change, but they have an
  # additional change (aspiration of 'g')
  y0 = y[0:-1]
  ans = y0 + 'kz' + ending[1:]
 else:
  #print "using sandhi_pair"
  sandhiget = SandhiGet(['Antoine72-4','Antoine72-5'])
  ans = None
  x = sandhiget.sandhi_pair(y,ending,'internal','join')
  # x might be an empty list
  if x:
   ans = solution(x)  #[['Iwwe']] => 'Iwwe'
  if ans == None:
   x=sandhiget.sandhi_pair(y,ending,None,'join')
   if x:
    ans = solution(x)
   if ans == None:
    ans = y + ending
 #
 ans1 = sandhi_single(ans,False)
 if ans1:
  ans = ans1
 ans = solution(ans)
 if dbg:
  print err0," => ",ans
 return ans

def perfect_bitab_join(bitab,endings,dbg=False):
 """
  bitab is returned by perfect_bitab
 """
 err0 = "perfect_bitab_join(%s,%s)"%(bitab,endings)
 if dbg:
  print err0
 ans = [] # returned value
 for (bt,ending) in zip(bitab,endings):
  thisans=[]
  for (base,sew_code) in bt:
   thisans1 = perfect_join(base,sew_code,ending,dbg)
   if thisans1 not in thisans:
    thisans.append(thisans1)
  ans.append(thisans)
 return ans 

def perfect_bitab(btab,itab,dbg=False):
 """ btab is a list; each btab element is either a string or a list of strings.
     itab is similar in form.
     Each list has the same length (9, for conjugation table).
     In itab (and possibly in btab?), an element may be None
    The returned value is also a list (of the same length);
    For a given index, the returned list is a list of all pairs (bt it) 
    where bt and it are chosen from the corresponding index of btab and itab.
      
 """
 err0 = "perfect_bitab(%s,%s)" %(btab,itab)
 if dbg:
  print err0
 ans = []
 for (b,i) in zip(btab,itab):
  ans0 = []
  if not isinstance(b,list):
   b = [b]
  if not isinstance(i,list):
   i = [i]
  ans0 = [[bt,it] for bt in b for it in i]
  ans.append(ans0)
 if dbg:
  print "perfect_bitab=",ans
 return ans
 raise NameError(err0 + " NOT IMPLEMENTED")

def reduplicate_join(base,sup,dbg=False):
 """ 07-01-03. based on declension-join
 """
 err0 = "reduplicate_join(%s,%s)" %(base,sup)
 if dbg:
  print err0
 sandhiget = SandhiGet([])
 ans = None
 if sup in ['snih','snuh']:
  ans = base + 'zR' + sup[2:]
 if ans == None:
  x = sandhiget.sandhi_pair(base,sup,'internal','join')
  if len(x) == 1:
   ans = x[0][0]
 if ans == None:
  x=sandhiget.sandhi_pair(base,sup,None,'join')
  if len(x) == 1:
   ans = x[0][0]
 if ans == None:
  ans = base + sup
 x =  sandhi_nR(ans)
 if x:
  ans = x
 return ans

def reduplicate_perfect(root,wparts,dbg=False):
 """ 
 """
 err0 = "reduplicate_perfect(%s,%s)" %(root,wparts)
 if dbg:
  print err0
 (parts,types)= wparts
 tok = root
 if tok in ['vac','vad','vap','vas','vah','vaS']:
  #Antoine2#114 The roots 'vach' (to speak), 'vad' (to speak),
  #  'vap' (to sow), 'vas' (to dwell), and 'vah' (to carry) reduplicate in
  #  'u' (e.g. 'uvach') Furthermore, before weak terminations, the radical
  #  'v' is also changed to 'u', which, together with the 'u' of the 
  #  reduplication, contracts into 'uu' (e.g., 'uuch'). This routine 
  #  returns both forms (e.g., '([u v a ch] [uu ch]))
  # Kale #506, p. 317 : 'vash' 2P to desire
  ans1 = 'u' + tok
  ans2 = conjugation_join('U',tok[-1:])
  if tok == 'vas':
   # otherwise we are joining [uu] [s]. Since 's' is a final 's' (at
   # this point)
   # it gets turned into visarga by conjugation-join
   ans2 = 'Uz'
  ans = [ans1,ans2]
 elif tok == 'yaj':
  #Antoine2#115
  #Antoine2#115. [y a j] (to sacrifice) reduplicates in 'i'.
  #Before weak terminations, the radical 'y' is also changed to 'i',
  #which, together with the 'i' of the reduplication, contracts to 'ii'
  ans1 = 'i' + tok
  #ans2 = conjugation_join('I',tok[0:-1])
  ans2 = conjugation_join('I',tok[-1:])
  ans = [ans1,ans2]
 elif tok == 'vyaD':
  #Antoine2#116. The roots 'vyadh' (to pierce), 'svap' (to sleep),
  # and 'grah' (to seize) reduplicate as follows:
  # 'vivyadh', 'suShvap', 'jagrah'.  Before weak terminations, the
  #radical 'y', 'v' and 'r' are changed to 'i', 'u', and 'Ri'
  #respectively.  The changed of 'y' to 'i', 'v' to 'u', and 'r' to 'Ri'
  #is called 'samprasaaraNa'
  ans = ['vivyaD','viviD']
 elif tok == 'vyac':
  # Kale #506, p. 317
  ans = ['vivyac','vivic']
 elif tok == 'svap':
  # Antoine2#116
  ans = ['suzvap','suzup']
 elif tok == 'grah':
  # Antoine2#116
  ans = ['jagrah','jagfh']
 elif tok == 'jyA':
  # Kale 306., p. 317.
  ans = ['jijyA'] # otherwise, 'jajyA'
 elif tok == 'ci':
  #Antoine2#118. The roots 'chi' (to collect), 'ji' (to conquer), and
  # 'hi' (to impel) change their radical consonants to a guttural;
  # namely 'ch'->'k', 'j'->'g', and 'h'->'gh'
  # Note: Kale 514 says this is an optional form for 'chi';
  # the other form is [ch i ch i].  Only the form 'chiki' is implemented
  # here. It would be awkward to get both forms.
  # The other form is implemented in an ad-hoc way in conjugation-tab-liT
  ans = ['ciki']
 elif tok == 'ji':
  ans = ['jigi']
 elif tok == 'hi':
  ans = ['jiGi']
 elif tok == 'BU':
  #Antoine2#119. The root 'bhuu' takes the irregular base 'babhuuv' and
  #keeps its long 'uu' throughout the perfect conjugation.
  # (otherwise, its base would be [b u bh uu])
  ans = ['baBUv']
 elif tok == 'vid':
  #Antoine2#121. The root 'vid' (to know) forms a perfect without
  #reduplication, which has present meaning.
  ans = ['vid']
 elif tok == 'f':
  ans = ['Ar'] # Kale #515
 elif tok == 'Brasj':
  # Kale 505 p. 314. bhrasj ; previously [b a bh r a s j]
  ans = ['baBrajj','baBarj']
 elif tok == 'masj':
  # Kale 476. masj.
  # When followed by any consonant except a nasal or a semi-vowel,
  #  'n' is inserted before the ending consonant and the 's' is dropped.
  # Otherwise, the 's' is changed to 'j'.
  # The parasmaipada 2S without 'i' will have to be changed elsewhere
  ans = ['mamajj']
 elif tok == 'Cid':
  # Kale 505 p. 314. Chid ; previously [ch i Ch i d]
  ans = ['cicCid']
 elif tok == 'fcC':
  # Kale 505 p. 315. RichCh 6 P (to go).
  ans = ['AnarcC'] # previously 'AnfcC'
 elif tok == 'vye':
  # Kale 506 p. 319 'vye'.
  # 'vye' becomes 'vivyay' before strong terminations and 'vivii'
  # before the weak ones in the Perfect
  ans = ['vivyay','vivI']
 elif tok == 'kxp':
  #  Kale 508, p. 320 'kLip'
  ans = ['cakxp']
 elif tok == 'mfj':
  # Kale 508, p. 320 'mRij'
  # otherwise, ([m a m Ri j])
  # note two variations for weak endings
  ans = ['mamArj',['mamArj','mamfj']]
 elif tok == 'raD':
  # Kale 508, p. 321.
  # 'radh' and 'jabh' insert a nasal in non-conjugational tenses
  # when their final is followed by a vowel.
  # 'radh', however, does not insert the nasal in the Aorist, or
  # when it takes 'i'; howver, it does insert the nasal in the Perfect
  ans = ['raranD']
 elif tok == 'jaB':
  ans = ['jajamB']
 elif tok == 'jag':
  # Kale 517, p. 327
  # 'jag' is used as base for Perfect of 'adhi-i'
  ans = ['jag']
 elif tok == 'UrRu':
  # Kale 518, p. 327
  # 'uurNu' forms its base as 'uurNunu'
  # its vowel is optionally not gunated before a strong termination
  ans = [tok + 'nu']
 elif tok == 'dI':
  # Kale 520, p. 329.
  # 'y' is prefixed to vowel weak terminations in the case of
  # 'dii' (4 A 'to obey).
  ans = ['didIy'] # was ['didI']
 elif tok == 'de':
  # Kale 521, p. 329
  # 'de' (1 A 'to protect') assums as its base the form
  # 'digi' in the Perfect
  # NOTE: based on the sample 1S, 1P, and 2P forms, I use 'digyi'
  ans = ['digyi'] # was ['dade']
 elif tok == 'dyut':
  # Kale 522, p. 329
  # 'dyut' (1 A 'to shine') assumes as its base the form 'didyut'
  ans = ['didyut'] # was ['dudyut']
 elif tok == 'vyaT':
  # Kale 524, p. 329
  # 'vyath' (1 A 'to suffer') takes samprasaaraNa in the reduplicative
  # syllable in the Perfect
  ans = ['vivyaT'] # was ['vavyaT']
 else:
  # default case
  pfxes = reduplicative_pfx_perfect(tok,wparts)
  if not isinstance(pfxes,list):
   pfxes = [pfxes]
  ans = []
  for pfx in pfxes:
   thisans = reduplicate_join(pfx,tok)
   if thisans not in ans:
    ans.append(thisans)
  if tok in ['jan','gam','han','Kan','Gas']:
   #Antoine2#117. The roots 'jan' (to be born), 'khan' (to dig),
   #'gam' (to go), 'ghas' (to eat) and 'han' (to kill) drop their
   #medial 'a' before weak terminations.
   #The 'h' of 'han' is changed to 'gh' (in weak and strong forms).
   #The 'j' of 'jan' is changed to 'j~n' (in weak form)
   ans1 = ans[0]
   if tok == 'han':
    ans1 = ans1[0:-3] + 'G' + ans1[-2:] # 'jaGan'
   if tok == 'jan':
    ans2 = ans1[0:-2] + 'Y'
   elif tok == 'Gas':
    ans2 = ans1[0:-3] + 'kz'
   else:
    ans2 = ans1[0:-2] + ans1[-1:]
   ans = [ans1,ans2]
 return ans


def reduplicative_join(pfx,tok,dbg=False):
 """
 """
 err0 = "reduplicative_join(%s,%s)" %(pfx,tok)
 if dbg:
  print err0
 raise NameError(err0 + " NOT IMPLEMENTED")

def reduplicative_pfx_perfect(tok,wparts=None,dbg=False):
 """
 ;Antoine2##107. The reduplicative perfect follows the rules
 ;given under #70, with the following modifications:
 ;1. A non-initial radical 'Ri' becomes 'a' in reduplication: 'kRi' -> 'chakRi'
 ;   Note: This also applies to the 1-letter verb 'Ri'
 ;2. Initial 'a' followed be a single consonant is reduplicated in 'aa':
 ;   'ad' -> 'aad'
 ;3a. Initial 'a' followed by a conjunct consonant becomes 'aan' in
 ;   reduplication: aMsh -> aanaMsh
 ;   Kale # 501: This also applies to 'ash' (to pervade), although it
 ;   does not end in a compound consonant;
 ;    and to 'RichCh' (to go), though it does not start in 'Ri'
 ;3b. Initial 'Ri' becomes 'aan' in reduplication: 'Rich' -> 'aanRich'
 ;4.  nitial 'i' and 'u' reduplicate:
 ;  (a)(weak) in 'ii' and 'uu' for those endings where
 ;       the radical does not take guna or vrddhi
 ;      'iSh' -> 'iiSh'  , 'uSh' -> 'uuSh'
 ;  (b)(strong) in 'iy' and 'uv' for those endings where
 ;      the radical does take guna
 ;      'iSh' -> 'iyeSh' , 'uSh' -> 'uvoSh'
 ;  NOTE: This function returns a list : (strong weak):
 ;   e.g. when tok = [i Sh], there is returned ([i y a] [i] )
 ;  When joined with tok, these give 
 ;  [i] + [i Sh] -> [ii Sh], and [i y a] + [i Sh] -> [i y e Sh]
 ;Antoine2#70. 
 ;Reduplication consists in repeating before a verbal root that
 ;initial portion of it which ends with its first vowel.
 ;Reduplication is subject to special rules:
 ;1. An initial aspirate loses its aspiration in reduplication
 ;2. An initial guttural is changed to the corresponding palatal
 ;   in reduplication. Initial 'h' is changed to 'j'
 ;3. When a root begins with a conjunct consonant, its first
 ;   consonant alone appears in reduplication
 ;4. But when a root begins with a sibilant followed by a hard
 ;   consonant, it is the latter which appears in reduplication
 ;5. A long vowel becomes short in reduplication
 ; 6. Medial 'e' becomes 'i' in reduplication;
 ;    medial 'o' and 'au' become 'u' in reduplcation
 ; 7. Final 'e', 'o', and 'au' become 'a' in reduplication
 ; 8. 'Ri' and 'RI' become 'i' in reduplication
 ;NOTES:
 ; (1). reduplication is applied to to 3rd conjugation dhaatus to
 ;      get the present system stem. 
 ; (2) It plays a part in some other (not yet discussed) grammatical entities
 """
 err0 = "reduplicative_pfx_perfect(%s,%s)" %(tok,wparts)
 if dbg:
  print err0
 if not wparts:
  wparts = word_parts(tok)
 (parts,types) = wparts
 #print err0
 #print wparts
 ans=[]
 if types in ['cv','cvc']:
  ctok = parts[0] # initial consonant
  vtok = parts[1] # final or intermediate vowel
  # 3. Only 1st letter of initial conjunct consonant is used
  c = ctok[0]
  # 4. But if initial consonant is sibilant + hard cons, then
  # use the hard cons
  # 5. A long vowel becomes short in reduplication
  # 6. Medial 'e' becomes 'i' in reduplication;
  #    medial 'o' and 'au' become 'u' in reduplcation
  # 7. Final 'e', 'o', and 'au' become 'a' in reduplication
  #*** 8. 'Ri' and 'RI' become 'i' in reduplication
  #8. A radical 'Ri' becomes 'a' in reduplication
  # NOTE: Not sure whether 'RI' is handled like 'Ri'.
  # The code assumes so
  # NOTE2: in 'kLip' (Kale p. 320), 'Li' becomes 'a' in reduplication
  if (len(ctok) == 2) and (c in init.sibilant_set) and (ctok[1] in init.hard_set):
   c = ctok[1]
  # 1. initial aspirate loses aspiration
  if c not in 'S':
   c = de_aspirate(c)
  # 2. initial guttural changed to palatal. initial 'h' -> 'j'
  if c == 'h':
   c = 'j'
  elif (c in init.guttural_set):
   c = corresponding_letter(c,init.guttural_set,init.palatal_set)
  v = vtok[0]
  # 5. shorten the vowel
  v = shorten_vowel(v)
  if (types == 'cvc') and (v == 'e'):
   v = 'i' # medial 'e' -> 'i'
  elif (types == 'cvc') and (v in 'oO'):
   v = 'u' # medial 'o', 'au' -> 'u'
  elif (types == 'cv') and (v in 'eEoO'):
   v = 'a'  # final e,E,o,O -> a
  elif v == 'f':
   v = 'a'
  ans = c + v
 elif types in ['v','vc']:
  # 8. Initial 'Ri' becomes 'aan' in reduplication
  # (Note: The word 'Ri' is an exception, handled in 'reduplicate-perfect')
  # For the conj-7 verb 'Ri', we return [i y] (???)
  # Initial 'a' followed by a simple consonant becomes 'aa'
  # Initial 'a' followed by a conjunct consonant becomes 'aan'
  # Initial 'i' and 'u' may reduplicate in 'ii' and 'uu' or
  # in 'iy' and 'uv'. Both are returned
  vtok = parts[0]
  v = vtok[0] # initial vowel
  if v in 'fF':
   ans = 'An'
  elif (v == 'a') and (types == 'vc'):
   ctok = parts[1] # final consonant
   if ctok == 'S':
    ans = 'An'
   elif len(ctok)==1:
    ans = 'A'
   else:
    ans = 'An'
  elif tok == 'i':
   ans = ['I','iya']
  elif v == 'i':
   ans = ['iya','I']
  elif v == 'u':
   ans = ['uva','U']
  else:
   ans = v
 else:
  # multisyllabic roots: use same as perfect
  ans = reduplicative_pfx(tok,wparts)
 return ans

def construct_sewPERF_code1a(root,theclass,pada,upasargas,dbg=False):
 """ Unlike 'reduplicate', this returns a list, possibly with just one
     element.
 ; it is assumed that the 8 exceptional roots (kRi ... shru) are
; handled elsewhere. This routine does not check for them
; Kale #495. Compare Antoine2#109
; This pertains to the perfect tense.
; Special rules about the admission of the intermediate 'i' before
; the terminations 'va ma tha se vahe mahe dhve'
; (a) All roots, whether 'seT' or 'aniT', admit 'i' except:
;     kRi sRi bhRi vRi
;     stu dru sru shru
;  The eight roots just mentioned are aniT. 
;  In addition to this general rule, there are some modifications
;  applicable only to the ending 'tha'.
;  Before 'tha':
;      the root 'kRi' (only with prefix 'sam') admits 'i'.
;      the root 'vRi' admits 'i'.
;      aniT roots ending in 'Ri', except the root 'Ri', reject 'i'
;      aniT roots with a final vowel other that 'Ri' admit 'i' optionally
;      aniT roots with a penultimate 'a' admit 'i' optionally
;      the root 'Ri' admits 'i' necessarily (Kale #515)
;kRi : aniT seT sam
; the case  'sam-kRi' is handled in function 'conjugation-tab-liT'
; ADDENDA: 
; 1.  based on example on p. 314,
;  the aniT root'sRij' admits 'i' optionally before 
; 'th' (otherwise, it would be required)
; Also, it is treated as seT before other consonants
; 2. Kale p. 318
; In the perfect, 'shvi' is optionally considered as 'shu'.
; 'shvi' is considered 'seT' before 'tha' and other consonants.
; Based on the table in Kale, this is also the way 'shu' is
; considered.  Otherwise, 'shu' was 'seT veT' for perfect
; Kale #508, p. 319. The roots 'svRi', 'suu' amd 'dhuu' admit
; 'i' necessarily before consonantal terminations except 'tha';
; before 'tha' they admit 'i' optionally.
; Note: Kale has these roots mentioned in the general category
; of 'veT' roots. However, in my general classification of seT-code,
; 'dhuu' and 'suu' show as 'seT', and 'svRi' shows as 'aniT'
; Kale #508, p. 319
; My algorithm gives different results for the following verbs. 
; So I explicitly state the answer (based on Kale)
; All these are classified as general 'veT' by Kale, but this
; often disagrees with my classification
; 'ta~nj' is stated to be 'same as ta~nch' in Kale dhaatukosha.
; Thus, I do not include it
; the case  'nir-kuSh' is handled in function 'conjugation-tab-liT'
;nir kuSh : veT veT  Handle
; Kale #515. 'ad', 'Ri', and 'vye' admit 'i' necessarily before 'tha'
; Since 'ghas' is a substitute for 'ad' in the Perfect, it too 
; admits 'i' necessarily before 'tha'
; Kale #512, p. 325
"""
 err0 = "construct_sewPERF_code1a(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 exceptions = {
  'kf':['aniw','aniw'],
  'sf':['aniw','aniw'],
  'Bf':['aniw','aniw'],
  'vf':['aniw','sew'],
  'stu':['aniw','aniw'],
  'dru':['aniw','aniw'],
  'sru':['aniw','aniw'],
  'Sru':['aniw','aniw'],
  'sfj':['sew','vew'],
  'Su':['sew','sew'],
  'svf':['sew','vew'],
  'DU':['sew','vew'],
  'sU':['sew','vew'],
  'taYc':['vew','sew'],
  'vraSc':['vew','vew'],
  'mfj':['vew','vew'],
  'aYj':['vew','vew'],
  'klid':['vew','vew'],
  'syand':['vew','vew'],
  'raD':['vew','vew'],
  'jaB':['vew','vew'],
  'siD':['vew','vew'],
  'kxp':['vew','vew'],
  'tfp':['vew','vew'],
  'dfp':['vew','vew'],
  'trap':['vew','vew'],
  'kzam':['vew','vew'],
  'aS':['vew','vew'],
  'kliS':['vew','vew'],
  'naS':['vew','vew'],
  'akz':['vew','vew'],
  'tvakz':['vew','vew'],
  'takz':['vew','vew'],
  'gAh':['vew','vew'],
  'gfh':['vew','vew'],
  'guh':['vew','vew'],
  'tfh':['vew','vew'],
  'tfMh':['vew','vew'],
  'druh':['vew','vew'],
  'muh':['vew','vew'],
  'stfh':['vew','vew'],
  'vfh':['vew','vew'],
  'snih':['vew','vew'],
  'snuh':['vew','vew'],
  'ad':['sew','sew'],
  'Gas':['sew','sew'],
  'vye':['sew','sew'],
  'f':['sew','sew'],
  'Pal':['sew','sew']
 }
 ans1 = 'sew'
 ans2 = 'sew' # some overrides to this below
 sew_codes = construct_sew_code1a(root,theclass,pada,upasargas,dbg=dbg)
 sew_codes = solution(sew_codes)
 if not isinstance(sew_codes,list):
  sew_codes = [sew_codes]
 tok = root
 lc = tok[-1:] # last char
 n = len(tok)
 #print "check: sew_codes=%s, tok=%s,lc=%s,n=%s" %(sew_codes,tok,lc,n)
 if ('aniw' in sew_codes) and (lc == 'f') and (root != 'f'):
  ans2 = 'aniw'
 elif ('aniw' in sew_codes) and (lc in init.vowel_set):
  ans2 = 'vew'
 elif ('aniw' in sew_codes) and (1 < n) and (tok[-2:-1] == 'a'):
  ans2 = 'vew'
 elif ('aniw' in sew_codes) and (3 < n) and\
      (tok[-1:] in init.consonant_set) and\
      (tok[-2:-1] in init.consonant_set) and\
      (tok[-3:-2] == 'a') and\
      (tok[-4:-3] in init.consonant_set) :
  ans2 = 'vew'
 ans = [ans1,ans2]
 #print "check: ans=",ans
 if root in exceptions:
  ans = exceptions[root]
 if dbg:
  print err0," returns",ans
 return ans

def conjugation_tab_liw(base,theclass,pada,root,dbg=False):
 raise NameError("conjugation_tab_liw(%s,%s,%s,%s)"%(base,theclass,pada,root))

def conjugation_tab_luw(upasargas,theclass,pada,root,voice=None,dbg=False):
 ForC.ForC_sym = 'luw'
 return conjugation_tab_ForC(upasargas,theclass,pada,root,voice,dbg)

def conjugation_tab_lfw(upasargas,theclass,pada,root,voice=None,dbg=False):
 ForC.ForC_sym = 'lfw'
 return conjugation_tab_ForC(upasargas,theclass,pada,root,voice,dbg)

def conjugation_tab_lfN(upasargas,theclass,pada,root,voice=None,dbg=False):
 ForC.ForC_sym = 'lfN'
 return conjugation_tab_ForC(upasargas,theclass,pada,root,voice,dbg)

def conjugation_tab_ASIrliN(upasargas,theclass,pada,root,voice=None,dbg=False):
 ForC.ForC_sym = 'ASIrliN'
 return conjugation_tab_ForC(upasargas,theclass,pada,root,voice,dbg)

all_special_tenses = ['law','laN','low','viDiliN']

def conjugation_tab(base,tense,theclass,pada,root,voice,dbg=False):
 """ from gram2.el
 """
 if dbg:
  print "construct_tab(%s,%s,%s,%s,%s,%s)"%(base,tense,theclass,pada,root,voice)

 if tense in all_special_tenses:
  # 1. present-system conjugations : depend on class of root
  if (voice == 'passive'):
   # base assumed to have (a) passive base
   return conjugation_tab_1(base,tense,'4','a',dbg)
  if theclass in ['1','4','6','10']:
   return conjugation_tab_1(base,tense,theclass,pada,dbg)
  if theclass == '5':
   return conjugation_tab_5(base,tense,theclass,pada,dbg)
  if theclass == '8':
   if root == 'kf':
    return conjugation_citation_irreg(root,tense,theclass,pada,dbg)
   else:
    return conjugation_tab_8(base,tense,theclass,pada,dbg)
  if theclass == '9':
   return conjugation_tab_9(base,tense,theclass,pada,dbg)
  if theclass == '2':
   if root in ['as','vid']:
    return conjugation_citation_irreg(root,tense,theclass,pada,dbg)
   elif (root == 'brU') and (pada == 'p') and (tense == 'law'):
    return conjugation_citation_irreg(root,tense,theclass,pada,dbg)
   else:
    return conjugation_tab_2(base,tense,theclass,pada,root,dbg)
  if theclass == '3':
   return conjugation_tab_3(base,tense,theclass,pada,root,dbg)
  if theclass == '7':
   return conjugation_tab_7(base,tense,theclass,pada,root,dbg)
  # should not happen
  raise NameError("conjugation_tab(%s,%s,%s,%s,%s,%s)"%(base,tense,theclass,pada,root,voice))
 else:
  # other tenses
  if tense == 'liw': # perfect tense
   return conjugation_tab_liw(base,theclass,pada,root,dbg)
  if tense == 'luw': # periphrastic future
   return conjugation_tab_luw(base,theclass,pada,root,dbg)
  if tense == 'lfw': # simple future
   return conjugation_tab_lfw(base,theclass,pada,root,dbg)
  # should not happen
  raise NameError("conjugation_tab(%s,%s,%s,%s,%s,%s)"%(base,tense,theclass,pada,root,voice))

def construct_conjtab1a_spcltense(root,theclass,pada,upasargas,tense,voice,dbg=False):
 """ for tenses ['law','laN','low','viDiliN']
 """
 # bases should be a list of strings
 bases = construct_conjbase1a(root,theclass,pada,upasargas,voice,dbg)
 if dbg:
  print "construct_conjtab1a_spcltense(%s,%s,%s,%s,%s,%s)"%(root,theclass,pada,upasargas,tense,voice),"bases=",bases

 noans = 'N/a'
 ctabs = []
 for base in bases:
  ctab = conjugation_tab(base,tense,theclass,pada,root,voice,dbg)
  ctabs.append(ctab)
 #print "construct_conjtab1a_spcltense(%s,%s,%s,%s,%s,%s) => %s" %(root,theclass,pada,upasargas,tense,voice,ctabs)
 return ctabs


def construct_conjtab1a(root,theclass,pada,upasargas,tense,voice,derived_type=None,dbg=False):
 err0 = "construct_conjtab1a(%s,%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice,derived_type)
 if dbg:
  print err0
 if derived_type == 'causal':
  return causal_conjtab1a(root,theclass,pada,upasargas,tense,voice,dbg)
 if tense in ['law','laN','low','viDiliN']:
  return construct_conjtab1a_spcltense(root,theclass,pada,upasargas,tense,voice,dbg)
 #if tense in sl_all_tense_dict().keys():
 santensesd = sl_all_tense_dict()
 santensekeys=santensesd.keys()
 santenses = [santensesd[k] for k in santensekeys]
 #print zip(santensekeys,santenses)

 if tense in santenses:
  # other known tenses
  return construct_conjtab1a_gentense(root,theclass,pada,upasargas,tense,voice,dbg)
 raise NameError('construct_conjtab1a. Unknown tense=%s'%tense)


def sl_all_tense_dict(opt=1):
 all_tenses=  {
  "pre":"law",
  "ipf":"laN",
  "ipv":"low",
  "opt":"viDiliN",
  "ppf":"liw-p",
  "prf":"liw-r",
  "fut":"lfw",
  "con":"lfN",
  "pft":"luw",
  "ben":"ASIrliN",
  "aor1":"luN1",
  "aor2":"luN2",
  "aor3":"luN3",
  "aor4":"luN4",
  "aor5":"luN5",
  "aor6":"luN6",
  "aor7":"luN7"
 }
 if opt == 1:
  return all_tenses
 raise NameError('sl_all_tense_dict. Wrong opt=%s' % opt)

def sl_tense_tran(sltense):
 """ sltense is a string.
     Return a list of strings.
     Except when sltense == 'aor', the returned list has just one element.
     for sltense == 'aor', it returns the list
      luN1 luN2 luN3 luN4 luN5 luN6 luN7
     The symbols 'aop' (aorist optative) and 'asb' (aorist subjunctive)
      also return this list.
     Python Conversion question: aop and asb should be treated differently.
 """
 # next dictionary derived from variable all-tenses-SL-plist in construct.el
 # The spellings of Sanskrit grammar words is in SLP1, as with 
 all_tenses = sl_all_tense_dict()

 if sltense in all_tenses:
  return [all_tenses[sltense]]
 if sltense in ['aor','aop','asb']:
  return ["luN1","luN2","luN3","luN4","luN5","luN6","luN7"]
 raise NameError("sl_tense_tran: Unknown tense=%s" %sltense)


def sl_conjtab(root,theclass,evoice,upas,sltense,dtype,dbg=False):
 """ functional equivalent of 
  SL-conjtab (root class evoice upas sltense dtype)
  upas is a list
  theclass is a string which is numeric (1,...,10)
  dtype = derived type - only 'c' recognized currently.
 """
 if dbg:
  tmp = "sl_conjtab(%s,%s,%s,%s,%s,%s)" %(root,theclass,evoice,upas,sltense,dtype)
  print tmp
 noans = 'N/a'
 # derive pada and voice from evoice
 if evoice == 'a': # active
  (pada,voice)= ('p','active')  # p == Parasmaipada
 elif evoice == 'm': # middle
  (pada,voice)= ('a','active')  # a == Atmanepada
 elif evoice == 'p': # passive
  (pada,voice)= ('p','passive') # pada irrelevant
 else:
  err = "SL_conjtab error in evoice: %s (should be m,a, or p)" % evoice
  print err
  return noans
 # derive derived_type from dtype
 if dtype == 'c':
  derived_type = 'causal'
 else:
  derived_type = None
 #
 tenses = sl_tense_tran(sltense)
 #print "tenses = ",tenses
 #
 err = False
 ans = []
 for tense in tenses:
  upasargas = []
  ctab = construct_conjtab1a(root,theclass,pada,upasargas,tense,voice,derived_type,dbg)
  ans.append(ctab)
 # because of the possibility of multiple tenses and bases and sups, ans
 # may be a nested list.
 # remove all the unneeded nesting
 ans = solution(ans)
 return ans

def solution(ans):
 while True:
  if isinstance(ans,(list,tuple)) and (len(ans) == 1):
   ans=ans[0]
  else:
   break
 return ans


######################################################################## 
#aorist
#######################################################################
""" aorist.py
    June 20, 2016 (begun). Conversion of aorist.el in ElispSanskrit
-----
; begun 08-13-03
; Based upon Kale, p.332 ff
 "Kale 529.
 There are seven varieties or forms of the Aorist. The augment
 'a' is prefixed to the root as in the Imperfect.
 "
aorist1-doc 
-----------
"Kale 530.
   The terminations of the first variety are the same as those
   of the imperfect, except that the 3P terminations is 'us'.
  Kale 531.
   The radical 'aa' is dropped before the 3P ending 'us'.
  Kale 532.
   'i', 'sthaa', 'daa', 'dhaa', and roots assuming the form of
   'daa' and 'dhaa' (see Kale 459), 'paa 1P (to drink)', and 
   'bhuu' take this variety.
  Kale 533.
   The roots 'ghraa', 'dhe', 'sho', 'so', and 'Cho' belong to the
   first variety optionally. They optionally take the 6th form.
   'dhe' also takes the 3rd form.
  Kale 534.
   'bhuu' takes 'an' instead of 'us' as the 3P ending; it changes
   its vowel to 'uuv' before the vowel terminations:
   abhuuvam, abhuuva, abhuuma (1st pers)
   abhuut, abhuutaam, abhuuvan (3rd pers)
  Kale 535.
   'gaa' is substituted for 'i' in the Aorist:
    1s pers: agaam, agaava, agaama
    1st pers, with 'adhi': adhyagam, adhyagaava, adhyagaama
  Kale 536.
   The 1st variety is exclusively parasmaipadi.
   The roots 'daa', 'dhaa', and 'sthaa' take the 4th variety in atmanepada.
   'bhuu' takes the 5th variety in the atmanepada.
   'i' with 'adhi' takes the 4th variety.

aorist2-doc
-----------
Kale 537.
   In this variety, 'a' is added on to the root, and then the
   terminations of the Imperfect of the first group of conjugational
   tenses are added. (See tables lu~N2-1-P, lu~N2-1-A)
  Kale 538.
   The preceding 'a' is dropped before endings 'am', 'an', 'anta',
   and lengthened before 'va' and 'ma'. 
   The radical vowel does not undergo guna or vrddhi strengthening,
   except for
     - final 'Ri'
     - final 'RI'
     - the 'Ri' of 'dRish'
  Kale 539. 
   This variety is parasmaipadi with a few exceptions:
    - 'Ri' with 'sam'
    - 'khyaa', 'vach'
    - 'as' (class 4) with a preposition
   The roots 'lip', 'sich', and 'hve' take this form in both P and A;
   they also take the 4th form in the 'A'.
  Kale 540.
   The penultimate nasal of a root is dropped:
    bhraMsh -> abhrashat
    skand -> askadat
  Kale 541.
   Some other alterations are made to particular roots:
    'as' : 'asth'  3S = aasthat
    'khyaa' : 'khya'  3S = akhyat
    'pat' : 'papt' 
    'vach' : 'voch' 3S = avochat
    'shaas' : 'siSh'  3S = ashiShat
    'shvi' : 'shva'
    'hve' : 'hva'
  Kale 542.
   A list of roots necessarily belongs to the 2nd variety
   (see aorist-2-542-P)
  Kale 543.
   A list of roots belong optionally to the 2nd variety.
   They also belong to the 4th variety if they are 'aniT' or
   the 5th variety if they are 'seT' (see aorist-2-543-P)
  Kale 544.
   A list of 25 roots, which are all atmanepada, are conjugated
   in the parasmaipada when they belong to this variety.
   In the atmanepada, they belong to the 4th or 5th variety 
   according as they are 'aniT' or 'seT' (see aorist-2-544-P)
 Notes.
  1. lip, sich, and hve also take the 4th variety in A

aorist3-doc
-----------
Kale 545.
   The terminations are the same is in the 2nd variety.
  Kale 546.
   Roots taking the 3rd variety:
   - roots of the 10th class
   - causals (recognized by class = 10)
   - some derivatives (not implemented)
   - 'kam'
   - the roots 'shri', 'dru', and 'sru' when expressing the agent
   - the roots 'dhe' and 'shvi' take the 3rd variety optionally.
  Kale 547.
   The root is first reduplicated and then the augment 'a' and the
   terminations are added as in the 2nd variety.
   By example, the base of 'dhe' is 'dadh'
  Kale 548. Roots of 10th class and causals
   a. 1. The root is modified as in accordance with present-tense of
      verbs of the 10th class
      2. he 'ay' of the base is dropped
      3. vowels are shortened:
         aa -> a, ii -> i, uu -> u, RI -> Ri, LI -> Li
         e and ai -> i, o and au -> u
  b1. 'i' is substituted for the 'a' of the reduplicative syllable
     if the syllable following it be short and not prosodially long.
  b2. If 'i' is the vowel of the reduplicative syllable (whether by
      virtue of (b1) or otherwise),  then that 'i' is lengthened
       when both of the following conditions hold:
       (1) it is not followed by a long syllable, and
       (2) it is not followed by a conjunct consonant.
  NOTE:  These alterations occur in function 'aorist3_alter10'.
  c. Roots having a penultimate 'Ri' or 'RI' optionally preserve it,
     the long 'RI' being changed to the short one.
  Kale 549. Bases with initial vowels
  a. If a root begins with a vowel and ends in a single consonant,
     - the consonant is reduplicated (NOTE: 'reduplicate-cons')
     - 'i' is added to it in reduplicate syllable
     NOTE: 'as' -> aasisat (rather than 'aasiShat')
  b. If a root begins with a vowel and ends in a conjunct consonant,
    whose first member is a nasal or 'd' or 'r', then
    the second member of the conjunct consonant is reduplicated.
    NOTE 1: In either case, the last letter of the root is the
    one reduplicated.
  a'. If a root begins with a vowel and ends in a conjunct consonant
    other than that of the type in b, it is treated as case (a).
    Example : aTT -> aaTTiTat
  c. The roots uun a~Nk a~Ng andh aMs arTh, and some others,
     substitute 'a' for 'i' in the reduplicative syllable.

 Kale 550.
  Roots ending in 'u' or 'uu' substitute 'u' (changeable to 'uu' like 'i')
  for 'i' in the reduplicative syllable, provided
  it be not (immediately) followed by 
   a labial consonant or a semivowel or j,
   followed by 'a' or 'aa'

 Kale 551.
  Several roots (bhraaj bhaas bhaSh diip jiiv miil piiD ...)
  shorten their penultimate optionally.
 Kale 552.
  In several roots the 'a' of the reduplicative syllable is not
  changed to 'i'; in two others, it is optionally changed to 'i'
 Kale 553.
  The root 'hve' takes samprasaaraNa; 
  The root 'shvi' takes samprasarraNa optionally
 Kale 554.
  The roots given under #400 (re 10th conj) preserve their
  vowel unchanged; i.e., do not substitute 'i' in the reduplicative
  syllable.
  Note : Not all roots listed in Kale-400 follow this. 'aMs' is one such
  Note 2: Kale gives 'suuch' as an example here, but it is not mentioned
  in #400.
 Kale 555. Several roots (shaas ,...) do not shorten their penultimate.
  Note: To match examples, I had to add some irregularities

aorist4-doc ?
-------------
Kale 564. There are both P and A forms
 Kale 565.
  a1. aniT roots not belonging to any of the preceding varieties
    (i.e. 1,2,3,6,7) take this form 
  a2. aniT roots that optionally take any of the preceding varieties
    take this form.
    NOTE: aniT roots in variety '6' generally don't take this variety
  a3. weT roots optionally belong to this variety
  Exceptions:
   1. 'stu' and 'su' when P belong to 5th variety
   2. Roots ending in 'Ri' preceded by a conjunct consonant may take 4th or 5th
   3. 'a~nj' and 'dhuu' Par. take 5th only;
      'dhuu' A takes 4th or 5th
   4. Of 'seT' roots, 'vRi' and those ending in 'RI' when A take 4th or 5th.
      'snu' and 'kram' when 'A' belong to 4th.
 
  Kale 566.
   a. In the parasmaipada, the radical vowel takes its vrddhi substitute
   b. In the atmanepada, 
     - guna is substituted for final 'i ii u uu'
     - final 'Ri' is unchanged
     - penultimate vowel is unchaged
     - final 'RI' is changed to 'iir uur' in accordance with #394,
       NOTE: 394 is written in another context. In this context,
       we use 'uur' when a labial or 'v' precedes 'RI';
       we use 'iir' otherwise.
        'stRI' -> astiirShTa, vRI -> avuurShTa
    c. The penultimate 'Ri' is optionally changed to 'ra':
       kRiSh -> akaarkShiit or akraakShiit
  Kale 567.
    After a short vowel, and after a consonant, except a nasal or a semivowel,
    the 's' of terminations beginning with 'sta' and 'stha' is dropped.
  IRREGULAR AORIST OF THE 4th FORM
  Kale 568.
    'daa', 'dhaa', and roots assuming the form of 'daa' and 'dhaa' (Vide
    #459) and 'sthaa' substitute 'i' for their final vowel in the A.
    This 'i' does not take its guna substitute. In the P, these roots
    take the 1st variety (see #532).
  Kale 569.
   'han' (with 'aa' A) drops its nasal before the terminations.
   It takes the 5th form optionally both in the P and A, in which case
   'vagh' is substituted for it.
  Kale 570.
   a. gam optionally drops its nasal with the A terminations.
   b. yam with 'upa' (to marry) optionally drops nasal with A terminations
   c. yam when it means 'to give out' (as the faults of others) necessarily
      drops its nasal with A terminations. NOTE: This is all but with '(upa)'
  Kale 571.
   a. the 3S of 'pad' (4 A) is 'apaadi'
   b. 'budh' (4 A) takes the termination 'i' optionally in the 3S,
      before which the penultimate 'u' takes guna. e.g. 'abodhi'
  From example p. 352.
  See Kale 486.
    In the case of 'i' with 'adhi', 'gaa' is optionally substituted for 'i'
    in the conditional and the Aorist.
    In the case of the aorist, this 'gaa' is treated like 'gii'
  From example p. 352. 'mii' (A) is conjugated like 'maa'

aorist5-doc
-------------
Kale 572. 
   The terminations are obtained by prefixing the augment 'i' to the
   terminations of the 4th variety. dropping the 's' in the case of
   the 2S and 3S (P). 
   NOTE: These are reflected in 'lu~N5-1-P-endings', 'lu~N5-1-A-endings'
  Kale 573.
   All roots not restricted to any of the preceding varieties 
   (1,2,3,4,6,7) take the 5th variety.  Consequently, it is peculiar
   to 'seT' roots.
   NOTE 1: This is unclear in application. For instance, I could not
   see why 'budh' accepts 5th variety.
   NOTE 2: This is Antoine's 3rd for (p. 155, # 221). His statement of
   the criterion is:
     Roots ending in consonants or in other vowels than 'aa' take this form.
   I partially use this.
 Kale 574.
 a. In the Parasmaipada, the vowel is strengthened by vrddhi in the cases:
    root ends in a vowel
    root has penultimate 'a' and root ends in 'r' or 'l'
    root is 'dah' or 'vraj'
 b. In the Parasmaipada, the vowel is strenghthened by guna in the cases:
    root has a penultimate short vowel (except those mentioned above)
 c. In the Parasmaipada, vrddhi is optionally substituted in the case:
    root has a penultimate 'a' (not prosodially long - root ends in simple
      consonant) and root has an initial consonant and root does not
      end in 'r' or 'l'
 d. In the Parasmaipada, , The vrddhi substitute is NOT used in any
    of the following cases (rather, the guna substitute is used)
    - root ends in 'h m y'
    - root is 'kShaN', 'shvas', 'shvi', 'kaT' (cover, surround)
      'chaT' (break, pierce), 'chat', 'chad' (ask, beg),
      'path' (go, move), 'math' (churn), 'lag'(stick, cling to),
      'has', 'hlas' (sound, be diminished)
 e. In the Atmanepada, the radical vowel takes its guna substitute.
   NOTE: Antoine2 p 155 says: 
    A final vowel and short medial vowel take guna in the atmanepada
    The function 'gunate-final-vowel' takes these circumstances into
    account, so it is used. 'aorist-gunate-final-vowel' does not
    so it is unused here.
 Kale 575.
  The roots 'diip jan puur taay vyaay' optionally substitute 'i' for
   the 3S Atmanepada ending 'iShTa'.
 Kale 576.
  Roots of the 8th class ending in 'N' or 'n' have an optional form
  in the 2S and 3S Atmanepada: they drop the nasal and substitute
  'thaaH' for the ending 'iShTaaH' (2S) and 'ta' for ending 'iShTa'.
  The root 'san', further,  lengthens its vowel in 2S and 3S A.
 Kale 577.
  The vowel of 'uurNu' takes vrddhi optionally in the P (i.e., it
  optionally takes guna substitute). Also, it optionally remains unchanged
  before 'i' (P and A). Thus, it has 3 forms in P and 2 forms in A
 Kale 578.
  the 'aa' of 'daridraa' is optionally dropped in the aorist,
  consequently it takes the 6th and 5th forms ('aa' is dropped in 5th form)

aorist6-doc
-----------
Kale 557.
   The 6th variety is only parasmaipada
  Kale 558.
   Roots ending in 'aa' (including those that change their final to 'aa')
   are conjugated in the 6th variety (See 559)
   Also taking the 6th variety are:
   - 'yam' in the parasmaipada
   - 'ram' in the parasmaipada (i.e., with 'vi' and 'aa')
   - 'pari' (?? this is a prefix??)
   - 'nam'
   - 'yam' in atmanepada (e.g. with 'upa' or 'ud') takes 4th variety
   - 'ram' in the atmanepada takes 4th variety
 Kale 559. 
   Roots ending in 'aa' which are restricted to varieties 1-3 do not
   take the 6th form. 
   From the first aorist, probably permitted are only 
     'ghraa dhe sho so Cho' (See Kale 533)
     'mi mii lii' (See Kale 559 p. 345)

aorist7-doc
-----------
 Kale 560. This is both P and A. terminations given (all start with 's')
 Kale 561. 
  - 'aniT' roots ending in 'sh Sh s h' and
    having 'i u Ri Li' for their penultimate take the 7th variety
  - dRish, however, takes 4th form
 Kale 562.
  'mRish spRish and kRiSh' 1P 6 PA optionally belong to the 7th variety.
 Kale 563.
  The roots 'duh dih lih guh' in 'A' 
    optionally drop the initial 'sa' or 'saa' of the endings of
    1D, 3S, 2S, 2P

aorist-passive-doc
------------------
Kale 597(a)
   The Passive of the Aorist of roots belong to the
   4th, 5th, and 7th varieties is made up by appending
   the A terminations to the base.
  Kale 597(b)
   The Passive of the Aorist of roots belong to the
   1st, 2nd, 3rd, and 6th varieties take the
   4th, 5th, or 7th varieties in the Passive, in acccordance
   with the general rules.
  Kale 597(c)
   The 3S of the Aorist Passive of all roots is formed by
   adding 'i'.
  Kale 597(c-1)
   Before this 'i',
   - the penultimate (prosodially) short vowel takes its guna substitute
   - the penultimate 'a' takes its vrddhi substitute, in general
   - however, the root 'jan' is unchanged
   - roots which are 'seT' and end in 'am' are unchanged, in general.
     However, the following 'seT' roots ending in 'am' do take vrddhi:
     'cham' with prefix 'aa', 'kram', and 'vam'
   - the final vowel takes its vrddhi substitute
  Kale 597(c-2)
   Roots ending in 'aa', original or substituted (i.e. roots ending
   in 'e ai o') insert 'y' before this 'i'.
  Kale 597(c-3)
   The roots 'radh jabh rabh' insert a nasal before their final
   consonant; thus, in this situation, they are prosodially long
   so their penultimate 'a' cannot take guna or vrddhi substitute;
   i.e., their vowel is unchanged.
  Kale 597(c-4)
   'labh' with a preposition inserts a nasal, and vowel is unchanged
   'labh' without a preposition has two forms, one with an inserted
     nasal and no vowel change, one with vowel-vrddhi and no nasal.
  Kale 597(c-5)
   'bha~nj' (to break) forms 'abha~nji' or 'abhaaji'
   'sham' in the sense of 'to observe' (10 A) forms 'ashami' and 'ashaami'
  Kale 597(c-6)
   'sRij' takes vrddhi ('asaarji')
   'guh' lengthens its vowel ('aguuhi')
  Kale 597(c-7)
   'i' (to go) has 'agaayi'
   'i' with preposition 'adhi' has 'adhyaayi' or 'adhyagaayi'
  Kale 597(d)
   The roots at #461 (e.g. gup dhuup vichCh paN pan kam)
   will have two forms, one formed according to above logic,
   the other with the 'conjugational base'
  Kale 597(e)
   The optional forms of Section 596(b) (see below) hold good in the
   Passive aorist except in the 3S. 
   The optional forms must be made up by appending the A terminations
   of the 5th variety as the roots necessarily take 'i'.
  Kale 598: Roots of the 10th class
  Kale 598(a)
   The 'ay' (i.e., the 'aya' with the final 'a' dropped) is optionally
   dropped in the General Tenses, except the Perfect.
   The Aorist forms, except that of the 3S, are made up by adding the
   terminations of the 5th form.
  Kale 598(b)
   Roots which do not lengthen their penultimate 'a' (see also #603)
   lenghten it optionally in the general tenses of the Passive, except
   in the Perfect, when 'ay' is dropped.
  Kale 598(c)
   The 3S of the Passive Aorist is formed by dropping 'ay' necessarily
   and adding 'i'.
  N.B. The Passive forms of roots of the 10th class do not differ from
   the Passive forms of Causals.

----------------------------------------------------------------------
DESHPANDE description of aorist.  The Numbers (1-7) correspond to
aor1, etc. of our code.
----------------------------------------------------------------------
Deshpande #37.
  SUMMARY :
  7 : 'a' + root + 'sa' + final termination : few roots ending in 'sh' and 'h'
  4 : 'a' + root + 's' + final termination : roots ending in cons or non-aa
  5 : 'a' + root + 'iSh' + final termination : roots ending in cons or non-aa
  6 : 'a' + root + 'siSh' + final termination : (P) roots ending in 'aa'
  2 : 'a' + root + 'a' + final termination : 
  1 : 'a' + root  + final termination : (P)
  3 : 'a' + reduplicated-root + 'a' + final termination : conj. 10

  There are seven different varieties, all of which take the past tense
  augment 'a', like the past imperfect. These seven varieties may be
  divided between two general classes, i.e. sigmatic aorist and
  non-sigmatic aorist.  The sigmatic aorist has an infix ''s' and the
  non-sigmatic aorist does not.
 SIGMATIC AORIST VARIETIES
  There are four varieties of the sigmatic aorist, depending on whether
  the form shows 'sa s iSh siSh'
  ** 'sa' variety (7th variety)
   This variety has the infix 'sa' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'sa' + final termination.
   Only a few roots ending in 'sh' and 'h' have this variety.
   Examples : 
    dish 6 P:
      adikShat adikShataam adikShan
      adikShaH adikShatam adikShata 
      adikSham adikShaava adikShaama
    duh 2 P : (NOTE: As shown, Kale has optional forms in 3S 2S 2P 1D)
      (adhukShata adugdha) adhukShaataam adhukShanta 
      (adhukShathaaH adugdhaaH) adhukShaathaam (adhukShadhvam adhugdhvam) 
       adhukShi (adhukShaavahi aduhvahi) adhukShaamahi
  ** 's' variety (4th variety)
   This variety has the infix 's' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 's' + final termination.
   This variety is generally used for several roots ending in consonsants
   or in vowels other than 'aa'.
   Examples :
    kRi 8 P:
       akaarShiit akaarShTaam akaarShuH 
       akaarShiiH akaarShTam akaarShTa 
       akaarSham akaarShva akaarShma
    shap 1 A:
       ashapta ashapsaataam ashapsata 
       ashapthaaH ashapsaathaam ashapdhvam (NOTE: D=ashabdhvam)
       ashapsi ashapsvahi ashapsmahi
  ** 'iSh' variety (5th variety)
   This variety has the infix 'iSh' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'iSh' + final termination.
   This variety is used for several roots ending in consonsants
   or in vowels other than 'aa'.   
   Examples :
    budh 1 P:
       abodhiit abodhiShTaam abodhiShuH 
       abodhiiH abodhiShTam abodhiShTa 
       abodhiSham abodhiShva abodhiShma
    shii 2 A:
       ashayiShTa ashayiShaataam ashayiShata 
       ashayiShThaaH ashayiShaathaam ashayidhvam (D=ashayiDhvam)
       ashayiShi ashayiShvahi ashayiShmahi
  ** 'siSh' variety (6th variety)
   This variety has the infix 'siSh' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'siSh' + final termination.
   Only a few roots ending in 'aa' have this variety, and there
   are no middle forms.   
   Examples :
    yaa 2 P:
       ayaasiit ayaasiShTaam ayaasiShuH 
       ayaasiiH ayaasiShTam ayaasiShTa 
       ayaasiSham ayaasiShva ayaasiShma

 NON-SIGMATIC AORIST VARIETIES
  These varieties do not have any kind of 's' infix.
  ** 'a' infix variety (2nd variety)
   This variety has the infix 'a' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'a' + final termination.
   Examples :
    gam 1 P:
       agamat agamataam agaman 
       agamaH agamatam agamata 
       agamam agamaava agamaama
    vach 2 A: 
    (NOTE: is only 2P in conjugational tenses; but (Apte) is 'A' in
      non-conjugational tenses)
       avochata avochetaam avochanta 
       avochathaaH avochethaam avochadhvam 
       avoche avochaavahi avochaamahi
  ** zero infix or root aorist variety (1st variety)
   This variety has no infix  between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root  + final termination.
   This variety is found only in the active (Parasmaipada)
   Examples :
    daa 3 P:
       adaat adaataam aduH 
       adaaH adaatam adaata 
       adaam adaava adaama
    bhuu 1 P:
       abhuut abhuutaam abhuuvan 
       abhuuH abhuutam abhuuta 
       abhuuvam abhuuva abhuuma
  ** reduplicated aorist variety (3rd variety)
   In this variety, the root undergoes reduplication and 
    there is an infix 'a' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + reduplicated-root + 'a' + final termination.
   This variety is found mostly for 10 conjugation verbs and
   secondary verbs such as causatives.

   Examples :
    chur 10 P:
       achuuchurat achuuchurataam achuuchuran 
       achuuchuraH achuuchuratam achuuchurata 
       achuuchuram achuuchuraava achuuchuraama
    chur 10 A:
       achuuchurata achuuchuretaam achuuchuranta 
       achuuchurathaaH achuuchurethaam achuuchuradhvam 
       achuuchure achuuchuraavahi achuuchuraamahi
   NOTE: THE CAUSATIVE EXAMPLE is given on p. 316. I could
    not duplicate it.  By modifying it, the example of 'chur'
    above was seen to agree with the algorithm.
    much 6 P (causative):
       amuumuchat amuumuchataam amuumuchan 
       amuumuchaH amuumuchatam amuumuchata 
       amuumucham amuumuchaava amuumuchaama
    much 6 A (causative):
       amuumuchata amuumuchetaam amuumuchanta 
       amuumuchathaaH amuumuchethaam amuumuchadhvam 
       amuumuche amuumuchaavahi amuumuchaamahi

"""
## global variables
#import init
#from test2 import *
"""
global construct_sew_code1a,solution,word_parts
"""
global aorist_tok,aorist_id,aorist_voice, aorist_passive_P,\
       aorist_sym, aorist_pada
aorist_id = None
aorist_passive_P = False
aorist_tok = ''

def aorist_varieties(root,theclass,pada,upasargas,dbg=False):
 """
 ; returns a list of number-strings from 1 to 7, indicating
 ; which of the seven varieties of aorist apply.
 """
 global aorist_tok, aorist_pada
 aorist_tok = root
 aorist_pada = pada
 ans = []
 if aorist_1_P(root,theclass,pada,upasargas):
  ans.append("1")
 if aorist_2_P(root,theclass,pada,upasargas):
  ans.append("2")
 if aorist_3_P(root,theclass,pada,upasargas):
  ans.append("3")
 if aorist_4_P(root,theclass,pada,upasargas):
  ans.append("4")
 if aorist_5_P(root,theclass,pada,upasargas):
  ans.append("5")
 if aorist_6_P(root,theclass,pada,upasargas):
  ans.append("6")
 if aorist_7_P(root,theclass,pada,upasargas):
  ans.append("7")
 return ans

def aorist_1_P(root,theclass,pada,upasargas,dbg=False):
 """ Kale 532, 533
 """
 err0 = "aorist_1_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if not (pada == 'p'):
  return False
 #  I think next 3 cases take variety 1 necessarily
 if root in ['i','sTA','dA','DA','BU']:
  return True
 if (root == 'pA') and (theclass == '1'): # to drink
  return True
 if kale_459_P(root,theclass):
  return True
 # these are stated to optionally  take variety 1.
 # they also may take variety 6.
 # 'dhe' also takes variety 3.
 if root in ['GrA','De','So','so','Co']:
  return True
 return False

def kale_459_P(root,theclass,pada=None,tok=None):
 """ roots treated as roots ending in 'A'
 """
 if not tok:
  tok = root
 lc = tok[-1:]
 if lc in 'eEo':
  return True
 if (root,theclass) in [
  # when before a termination causing guna or vrddhi
  ("mi","5"), ("mI","9"),("dI","4"),
  ("lI","9"),("lI","4") # optional (?)
  ]:
  return True
 return False

def aorist_2_P(root,theclass,pada,upasargas,dbg=False):
 """ Kale 539
 """
 err0 = "aorist_2_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if (root == 'f') and (upasargas == ["sam"]):
  return True
 if (root == 'KyA') and upasargas: # i.e., when KyA is used with prefixes
  return True
 if (root == 'vac') and upasargas: # i.e., when vac is used with prefixes
  return True
 if (root == 'as') and (theclass == '4') and upasargas:
  return True
 if root in ['lip','sic','hve']:
  return True
 if aorist_2_542_P(root,theclass,pada,upasargas):
  return True
 if aorist_2_543_P(root,theclass,pada,upasargas):
  return True
 if aorist_2_544_P(root,theclass,pada,upasargas):
  return True
 return False

def aorist_2_542_P(root,theclass,pada,upasargas):
 """
  ; The 'data' table below is an abbreviated list of
  ; elements of form (dhaatu1 class1 pada1 upasargas1)
  ; Incomplete records are interpreted as follows:
  ; 'upasargas1' missing means a match with any 'upasargas'
  ; 'pada1' missing means that pada must be 'P'
  ; 'class1' missing means match with any 'class'. In this case,
  ; the element of 'data' is actually a symbol.
  ; Sometimes definitions are given by Kale on p.336; these
  ; are shown below, but do not enter the algorithm
 """
 data = [
  ["KyA" , "2" , "p"],
  ["KyA" , "2" , "a" , ["sam"]],
  ["f" , "3" , "p"],
  ["f" , "3" , "a" , ["sam"]],
  ["sf" , "1" , "p"],
  ["hve" , "1" , "p"],
  ["vac" , "2" , "p"],  # also that substituted for brU P,A
  ["vac" , "2" , "a"],
  ["sic" , "6" , "p"],
  ["sic" , "6" , "a"],
  ["lip" , "6" , "p"],
  ["lip" , "6" , "a"],
  ["as" , "4" , "p"],
  ["as" , "4" , "a" , ["pari"]],
  ["Sak" , "5" , "p"],
  ["Sak" , "5" , "a"],
  ["Sak" , "4" , "p"],
  ["Sak" , "4" , "a"],
  ["uc" , "4" , "p"],
  ["muc"],
  ["luw" , "4" , "p"],
  ["pat"],
  ["klid" , "4" , "p"],
  ["kzvid" , "4" , "p"],
  ["mad"],
  ["mid" , "1" , "a"],
  ["mid" , "4" , "p"],
  ["vid" , "6" , "p"],
  ["vid" , "6" , "a"],
  ["Sad" , "1"],
  ["sad"],
  ["svid"],
  ["fD" , "4"],
  ["fD" , "5"],
  ["kruD"],
  ["kzuD"],
  ["gfD" , "4"],
  ["raD" , "4"],
  ["SuD"],
  ["siD"],
  ["Ap"],
  ["kup"],
  ["gup" , "4"],
  ["qip" , "4"],
  ["yup"],
  ["rup"],
  ["lup" , "4" , "p"],
  ["lup" , "4" , "a"],
  ["lup" , "6" , "p"],
  ["lup" , "6" , "a"],
  ["sfp"],
  ["kzuB"],
  ["tuB"],
  ["naB" , "4"],
  ["luB"],
  ["klam"],
  ["kzam"],
  ["gam"],
  ["tam"],
  ["dam"],
  ["Bram"],
  ["Sam"],
  ["Sram"],
  ["sam" , "1"],
  ["kfS" , "4"],
  ["naS"],
  ["BfS" , "4"],
  ["BraMS"],
  ["vfS" , "4"],
  ["tuz"],
  ["tfz" , "4"],
  ["duz" , "4"],
  ["piz"],
  ["puz"],
  ["pluz" , "4"],
  ["riz" , "4"],
  ["ruz" , "4"],
  ["viz" , "3" , "p"],
  ["viz" , "3" , "a"],
  ["trup"],
  ["vyuz" , "4"],
  ["Siz"],
  ["Suz" , "4"],
  ["hfz"],
  ["kus" , "4"],
  ["Gas" , "1"],
  ["jas" , "4"],
  ["tas" , "4"],
  ["das" , "4"],
  ["bas" , "4"],
  ["bis" , "4"],
  ["byus" , "4"],
  ["mas" , "4"],
  ["mus" , "4"],
  ["yas" , "4"],
  ["vas" , "4"],
  ["vis" , "4"],
  ["bus"],
  ["vus"],
  ["SAs"],
  ["druh"],
  ["muh"],
  ["snih"],
  ["snuh"],
 ]
 found = False
 for data1 in data:
  if ([root] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass,pada] == data1):
   found = True
  elif ([root,theclass,pada,upasargas] == data1):
   found = True
  if found:
   break
 return found

def aorist_2_543_P(root,theclass,pada,upasargas):
 """
  ; The table is an abbreviated list of
  ; elements of form (dhaatu1 class1 pada1 upasargas1)
  ; Incomplete records are interpreted as follows:
  ; 'upasargas1' missing means a match with any 'upasargas'
  ; 'pada1' missing means that pada must be 'P'
  ; 'class1' missing means match with any 'class'. In this case,
  ; the element of 'data' is actually a symbol.
  ; Sometimes definitions are given by Kale on p.336; these
  ; are shown below, but do not enter the algorithm
  NOTE: This should be same interpretation as above
"""
 data = [
  ["Svi"],
  ["jF"],
  ["gruc"],
  ["gluc"],
  ["gluYc"],
  ["mruc"],
  ["mluc"],
  ["ric"],
  ["vic"],
  ["Suc"],
  ["nij"],
  ["yuj"],
  ["vij"],
  ["sPuw"],
  ["cut"],
  ["cyut"],
  ["jyut"],
  ["Scut"],
  ["Scyut"],
  ["kzud"],
  ["Cid"],
  ["Cfd"],
  ["tfd"],
  ["bund"],
  ["Bid"],
  ["rud"],
  ["skand"],
  ["buD"],
  ["ruD"],
  ["tfp"],
  ["dfp"],
  ["stamB"],
  ["dfS"],
  ["Sliz"],
  ["Guz"],
  ["uh"],
  ["tuh"],
  ["duh"],
  ["bfh"],
 ]
 # This is duplicate code to previous function
 found = False
 for data1 in data:
  if ([root] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass,pada] == data1):
   found = True
  elif ([root,theclass,pada,upasargas] == data1):
   found = True
  if found:
   break
 return found

def aorist_2_544_P(root,theclass,pada,upasargas):
 """
  ; The table is an abbreviated list of
  ; elements of form (dhaatu1 class1 pada1 upasargas1)
  ; Incomplete records are interpreted as follows:
  ; 'upasargas1' missing means a match with any 'upasargas'
  ; 'pada1' missing means that pada must be 'P'
  ; 'class1' missing means match with any 'class'. In this case,
  ; the element of 'data' is actually a symbol.
  ; Sometimes definitions are given by Kale on p.336; these
  ; are shown below, but do not enter the algorithm
  NOTE: This should be same interpretation as above
"""
 data = [
  ["ruc" , "1" , "a"],
  ["Guw" , "1" , "a"],
  ["ruw" , "1" , "a"],
  ["luw" , "1" , "a"],
  ["luW" , "1" , "a"],
  ["dyut" , "1" , "a"],
  ["vft" , "1" , "a"],
  ["Svit" , "1" , "a"],
  ["kzvid" , "1" , "a"],
  ["Bid" , "7" , "a"],
  ["syand" , "1" , "a"],
  ["svid" , "1" , "a"],
  ["vfD" , "1" , "a"],
  ["SfD" , "1" , "a"],
  ["kxp" , "1" , "a"],
  ["kzuB" , "1" , "a"],
  ["tuB" , "1" , "a"],
  ["SuB" , "1" , "a"],
  ["sraMB" , "1" , "a"],
  ["BraS" , "1" , "a"],
  ["BraMS" , "1" , "a"],
  ["DvaMs" , "1" , "a"],
  ["BraMs" , "1" , "a"],
  ["sraMs" , "1" , "a"],
 ]
 # This is duplicate code to previous function
 found = False
 for data1 in data:
  if ([root] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass] == data1) and (pada == 'p'):
   found = True
  elif ([root,theclass,pada] == data1):
   found = True
  elif ([root,theclass,pada,upasargas] == data1):
   found = True
  if found:
   break
 return found

def aorist_3_P(root,theclass,pada,upasargas,dbg=False):
 """ Kale 546, 533
 """
 err0 = "aorist_3_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if (theclass == "10"): # Kale 546
  return True 
 if (root == "kam"): # Kale 546
  return True 
 if root in ["Sri","dru","sru"]: # Kale 546
  return True 
 if root in ["De","Svi"]: # Kale 546, 533
  return True 
 if (theclass == "11"): #Here, class 11 means causal of root
  return True
 return False

def aorist_4_P(root,theclass,pada,upasargas,dbg=False):
 """ 
 """
 err0 = "aorist_4_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 sew_code = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_code = solution(sew_code)
 dbg=False
 tok = root
 lc = tok[-1:] # last char
 if dbg:
  if root == 'kF':
   print err0," sew_code=",sew_code,"lc=",lc
 (parts,types) = word_parts(tok)
 if [root,pada] in [['dA','a'],['DA','a'],['sTA','a']]:
  # Kale 536
  return True
 if root == 'han': 
  # Kale 536
  return (pada == 'a')
 if [root,pada,upasargas] == ["i","a",["aDi"]]:
  # Kale 536
  return True
 if [root,pada] == ["vas","a"]:
  # is sew. Takes variety 5. See Kale, p.350, footnote
  return False
 if [root,theclass,pada] in [
    # Kale footnote p.335
    ["lip","6","a"],["sic","6","a"],["hve","6","a"], 
    # Kale footnote p.335
    ["Sak","4","a"],["vid","6","a"],["lup","4","a"],["lup","6","a"]
  ]:
  return True
 if root in ['ric','vic','nij','yuj','vij','kzud','Cid',
    'Bid','skand','buD','ruD','dfp','dfS','Sliz','duh',
    'tfp' # this is vew
  ]:
  # Kale#542. This aniT roots take 2nd and 4th varieties
  return True
 if [root,pada] in [["stu","p"], ["su","p"],["aYj","p"],["DU","p"]]:
  # Kale 565, these are in variety 5
  return False
 if [root,pada] == ["DU","a"]:
  # Kale 565. 4th or 5th variety
  return True
 if (sew_code == 'aniw') and aorist_6_P(root,theclass,pada,upasargas):
  return False;
 if (sew_code == 'aniw'):
  #Kale 565 (a)
  if aorist_1_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_2_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_3_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_6_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_7_P(root,theclass,pada,upasargas):
   found = True
  else:
   # not variety 1,2,3,6 or 7. 
   found = False
  if not found:
   return True
 if (sew_code == 'vew'):
  #Kale 565 (a)
  return True
 if (pada == 'a') and (lc == 'f') and (types == "cv") and (1 < len(parts[0])):
  # last condition means the initial consonant in conjunct
  # Kale 565. Takes 4th or 5th
  return True
 if [root,pada,sew_code] == ["vf","a","sew"]:
  # Kale 565(4) 4th or 5th
  return True
 if [lc,pada,sew_code] == ["F","a","sew"]:
  # Kale 565(4) 4th or 5th
  return True
 if [root,pada] in [["snu","a"],["kram","a"]]:
  # Kale 565(4)
  return True
 if root in ["mfS","spfS","kfz"]:
  # Kale 562
  # July 10, 2016. Upon re-reading Kale 562, I think this applies to the
  # 7th variety, not this 4th variety.
  return True
 return False

def aorist_5_P(root,theclass,pada,upasargas,dbg=False):
 """ 
 """
 err0 = "aorist_4_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 sew_code = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_code = solution(sew_code)
 tok = root
 lc = tok[-1:] # last char
 (parts,types) = word_parts(tok)
 if root == 'han':
  return True # Kale 569
 if [root,pada] == ["BU","a"]:
  return True # Kale 536
 if [root,theclass,pada] in [["Sak","4","a"],["vid","6","a"]]:
  return True # Kale footnote p. 336
 if root in ["Svi" , "jF" , "gruc" , "gluc" , "gluYc" , "mruc" , "mluc" ,
             "Suc" , "sPuw" , "cut" , "cyut" , "jyut" , "Scut" , "Scyut" ,
             "Cfd" , "tfd" , "bund" , "rud" , "stamB" , "Guz" , "uh" , "tuh" ,
             "bfh" , "tfp" ]:
  # Kale 542. These sew roots take 2nd and 5th varieties
  return True
 if root == 'daridrA': # Kale 578
  return True
 if [root,pada] in [["stu","p"], ["su","p"],["aYj","p"],["DU","p"]]:
  # Kale 565, these are in variety 5
  return True
 if [root,pada] == ["DU","a"]:
  # Kale 565. 4th or 5th variety
  return True
 if (pada == 'a') and (lc == 'f') and (types == "cv") and (1 < len(parts[0])):
  # last condition means the initial consonant in conjunct
  # Kale 565. Takes 4th or 5th
  return True
 if [root,pada,sew_code] == ["vf","a","sew"]:
  # Kale 565(4) 4th or 5th
  return True
 if [lc,pada,sew_code] == ["F","a","sew"]:
  # Kale 565(4) 4th or 5th
  return True
 if aorist_3_P(root,theclass,pada,upasargas):
  return False
 if True:
  #Kale 573(a).
  # If this case is not in any other aorist variety, then put it in
  # this, the 5th, variety
  if aorist_1_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_2_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_3_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_4_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_6_P(root,theclass,pada,upasargas):
   found = True
  elif aorist_7_P(root,theclass,pada,upasargas):
   found = True
  else:
   # not variety 1,2,3,4,6 or 7. 
   found = False
  if not found:
   return True
 if [root,pada] == ["vas","a"]:
  # is sew. Takes variety 5. See Kale, p.350, footnote
  return True
 if [root,pada] == ["gAh","a"]:
  # ftnnote p. 350. Also 4th
  return True
 if (lc in init.consonant_set) or \
   ((lc in init.vowel_set) and (not (lc == 'A'))):
  # Antoine2#221 p. 155
  return True
 return False

def aorist_6_P(root,theclass,pada,upasargas,dbg=False):
 """ 
 """
 err0 = "aorist_6_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 tok = root
 lc = tok[-1:] # last char
 if not (pada == 'p'): # 6th form is only parasmaipada
  return False
 if theclass in ['10','11']: # 10th and 11th (causal) take 3rd variety
  return False
 if root in ['i','sTA','dA','DA','BU']: # takes variety 1 only
  return False 
 if (root == 'pA') and (theclass == '1'): # to drink ;  takes variety 1 only
  return False
 if root in ['GrA','De','So','so','Co']: # variety 1 or 6. Kale 533
  return True
 if root in ['yam','ram','nam']: # Kale 558
  return True
 if root in ['mi','mI','lI']: # Kale 559
  return True
 if root == 'daridrA': # Kale 559
  return True
 if kale_459_P(root,theclass):
  return False
 if aorist_2_542_P(root,theclass,pada,upasargas): # variety 2 necessarily
  return False
 if lc == 'A':
  return True
 return False

def aorist_7_P(root,theclass,pada,upasargas,dbg=False):
 """ 
 """
 err0 = "aorist_7_P(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 sew_code = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_code = solution(sew_code)
 #dbg=True
 if dbg:
  if root == 'kF':
   print err0," sew_code=",sew_code
 tok = root
 lc = tok[-1:] # last char
 pc = tok[-2:-1] # penultimate char. In Python, will be '' if len(tok)==1
 if [root,theclass,pada] == ["viz","3","a"]:
  return True
 if (sew_code in ['aniw','vew']) and (lc in 'Szsh') and (pc in 'iufx'):
  # Kale 561
  return True
 if root in ['mfS','spfS','kfz']:
  # Kale 562. Redundant by 561
  return True
 return False

def conjugation_tab_aorist(upasargas,theclass,pada,root,variety0=None,voice=None,dbg=False):
 """
; When 'variety' is non-nil, the answer is nil or 
;   an array which is the conjugation table for this variety of the aorist.
; When 'variety' is nil and 'voice' is 'ACTIVE', 
;   the answer is a list with elements of the form
;   (n ctab), where 'n' is a variety (1-7) and 'ctab' the associated 
;   conjugation table.
; When 'variety is nil and 'voice' is 'PASSIVE',
;  the answer is 'nil' or a sequence (the conj. tab. for PASSIVE aorist)
; When 'variety' is 'ALL, then all aorists are returned, without
;   regard for the applicability screening of 'aorist-varieties'

  In this Python version, we assume variety0 is an 'int'
 """
 global aorist_passive_P,aorist_pada,aorist_voice
 aorist_pada=pada
 #dbg=True
 err0 = "conjugation_tab_aorist:(%s,%s,%s,%s,%s,%s):" %(upasargas,theclass,pada,root,variety0,voice)
 variety = "%s" %variety0  # now variety is a string
 if dbg:
  print err0
 if voice == 'passive':
  aorist_voice = 'passive'
 else:
  aorist_voice = 'active'
 aorist_passive_P = (voice == 'passive')
 """
 if isinstance(root,list):
  # used for causals ?
  d = root[0]
 else:
  d = root
 varieties = aorist_varieties(d,theclass,pada,upasargas)
 """
 # 04-14-05. Essentially omit 'aorist-varieties' check
 varieties_all = ["1","2","3","4","5","6","7"]
 if variety in varieties_all:
  varieties = [variety]
 elif variety == 'all':
  varieties = varieties_all
 else:
  varieties = []
 def f(variety):
  return conjugation_tab_aorist_helper(upasargas,theclass,pada,root,variety,dbg=dbg)
 if dbg:
  print err0
  #print "varieties=",varieties
  #print "variety0 is int:",isinstance(variety0,int)

 ans = map(f,varieties)
 ans = solution(ans)
 return ans

def conjugation_tab_aorist_helper(upasargas,theclass,pada,root,variety,dbg=False):
 """ In Elisp, this was an anonymous function
  Uses 
 """
 err0 = "conjugation_tab_aorist_helper(%s,%s,%s,%s,%s)" %(upasargas,theclass,pada,root,variety)
 if dbg:
  print err0
 global aorist_passive_P,aorist_id
 ans=[]
 z = variety # given aorist variety
 # x = computed aorist variety (different for passive)
 if not aorist_passive_P:
  x = z
 elif z in ['4','5','7']:
  x = z
 elif z in ['1','2']:
  if root == 'jF':
   x = '5'  # either 4 or 5 works
  elif root == '':  #???
   x = '4'
  else:
   if root[-1:] in init.vowel_set:
    x = '4'
   else:
    x = '5'
 elif z == '6':
  x = '7'
 elif theclass in ['10','11']:  # 11 = causal
  x = '5'
 elif z == '3':
  if root in ['dru','sru']:
   x = '4'
  else:
   x = '5'
 else:
  x = z
 aorist_id = x # used only once, I think
 #print err0," aorist_id=",x
 if x == '1':
  thisans = conjugation_tab_aorist1(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '2':
  thisans = conjugation_tab_aorist2(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '3':
  thisans = conjugation_tab_aorist3(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '4':
  thisans = conjugation_tab_aorist4(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '5':
  thisans = conjugation_tab_aorist5(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '6':
  thisans = conjugation_tab_aorist6(upasargas,theclass,pada,root,dbg=dbg)
 elif x == '7':
  thisans = conjugation_tab_aorist7(upasargas,theclass,pada,root,dbg=dbg)
 else:
  thisans = None
 #print err0," thisans=",thisans, "aorist_passive_P=",aorist_passive_P
 if thisans:
  if aorist_passive_P:
   # get 3s independently
   thisans0 = aorist_passive_3S(upasargas,theclass,pada,root,dbg=dbg)
   #print err0," thisans0=",thisans0
   thisans[0] = thisans0
  # Note. Elisp applies join-arrays here, which is comparable to Python 'zip'
  # function.  We do this at the end
  #ans.append(thisans)
  if ans == []:
   ans = thisans
  else:
   ans = join_arrays(ans,thisans)
  #print err0,"ans=",ans
  if aorist_passive_P:
   # Kale 597(e), refers 596(b)
   endings = aorist_endings('luN5')
   n = len(endings)
   tok = aorist_tok
   # Note, Elisp does not set lc at this point. The Elisp code
   # works (lc is nil or a number from earlier). So this appears to
   # be a correction to the Elisp.
   lc = tok[-1:]  
   if lc in 'AeEo':
    x = tok[0:-1] + 'Ay'
   elif lc in init.vowel_set:
    if lc == 'e':  # Odd. Never happens due to previous if clause ?
     x = tok[0:-1] + 'E'  
    else:
     x = tok[0:-1] + vfdDi(lc)
   elif root == 'han':
    x = 'GAn'
   elif root == 'grah':
    x = 'grAh'
   elif root == 'dfS':
    x = 'darS'
   else:
    x = None
   if x:
    btab = [None]*n
    # This logic looks misplace. Should be done to thisans above?
    # For now, I mimic the Elisp logic
    for i in xrange(0,n):
     ending = endings[i]
     y = augment_a(x)
     z = conjugation_join(y,ending)
     w = z
     wold = ans[i]
     if not isinstance(wold,list):
      wold = [wold]
     w1 = append_if_new(wold,w)
     ans[i] = w1 
 #ans1 = zip(*ans)
 #return ans1
 return ans

def aorist_passive_3S(upasargas,theclass,pada,root,dbg=False):
 """ START HERE
 """
 global aorist_tok
 aorist_tok = root # varies from Elisp, where set in aorist-varieties
 err0 = "aorist_passive_3S(%s,%s,%s,%s), aorist_tok=%s" %(upasargas,theclass,pada,root,aorist_tok)
 if dbg:
  print err0
 ending = 'i'
 tok = aorist_tok
 ntok = len(tok)
 lc = tok[-1:]
 pc = tok[-2:-1]
 if lc in 'AeEo':
  # Kale 597c2
  x = tok[0:-1] + 'Ay'
 elif root == 'sfj':
  x = 'sArj'
 elif root == 'guh':
  x = 'gUh'
 elif root == 'i':
  if upasargas == ['aDi']:
   x = ['Ay','gAy']
  else:
   x = 'gAy'
 elif root == 'kfz':
  x = tok[0:-2] + vfdDi(pc) + tok[-1:]
 elif pc in 'iufx':
  x = tok[0:-2] + guna(pc) + tok[-1:]
 elif root in ['raD','jaB','raB']:
  # insert nasal
  if root == 'raD':
   nasal = 'n'
  else:
   nasal = 'm'
  x = tok[0:-1] + nasal + tok[-1:]
 elif root == 'laB':
  nasal = 'm'
  b = tok[0:-1] + nasal + tok[-1:]
  if upasargas:
   x = b
  else:
   x = [tok,b] # two forms
 elif root == 'BaYj':
  x = ['BaYj','BAj']
 elif (root == 'Sam') and (theclass == '10') and (pada == 'a'):
  x = ['Sam','SAm']
 elif pc == 'a':
  # prosodially short penultimate 'a'
  if root == 'jan':
   x = tok # unchanged
  elif (root == 'cam') and (upasargas == ['A']):
   # taks vrddhi
   x = tok[0:-2] + 'A' + tok[-1:]
  elif root in ['kram','vam']:
   # taks vrddhi
   x = tok[0:-2] + 'A' + tok[-1:]
  else:
   sew_code = construct_sew_code1a(root,theclass,pada,upasargas)
   sew_code = solution(sew_code)
   if (lc == 'm') and (sew_code == 'sew') and (root != 'kam'):
    x = tok
   else:  
    # this is the default case for pc == 'a': Apply vrddhi
    x = tok[0:-2] + 'A' + tok[-1:]
 elif lc in init.vowel_set:
  x = tok[0:-1] + vfdDi(lc)
 else:
  x = tok
 # now for some adjustments to x
 if root in ['gup','DUp','vicC','paR','pan','kam','ft']:
  xalt = kale_461_alt(root)
  x = [x,xalt]
 if not isinstance(x,list):
  x = [x]
 cb = causal_base(root,theclass,pada,upasargas,None)
 cb = solution(cb)
 if (pc == 'a') and (theclass in ['10','11']) and (tok == cb[0:-2]):
  # Kale 598 (b,c)
  xalt = aorist3_shorten_1st_vowel(tok)
  x = append_if_new(x,xalt)
 # x is a list.
 ans=[]
 for b in x:
  y = augment_a(b)
  z = conjugation_join(y,ending)
  ans.append(z)
 ans = solution(ans)
 return ans

def conjugation_tab_aorist1(upasargas,theclass,pada,root,dbg=False):
 """
 """
 global aorist_sym
 err0 = "conjugation_tab_aorist1(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 aorist_sym = 'luN1'
 tok = pada
 ylast = tok[-1:]
 (parts,types) = word_parts(tok)
 if types == 'cvc':
  pc = parts[1][0]  # the vowel
 else:
  pc = None
 sew_code = 'aniw'
 ans = conjugation_tab_aorist1_main(upasargas,theclass,pada,root,sew_code)
 return ans

def conjugation_tab_aorist1_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ 
 """
 global aorist_sym,aorist_voice
 err0 = "conjugation_tab_aorist1_main(%s,%s,%s,%s,%s.%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 # 5b. get table of base-sew codes (bitab)
 b = atok
 nb = len(b)
 lc = b[-1:]
 # pc is penultimate character
 if nb == 1:
  pc = None
 elif types in ['cvc','vc']:
  pc = parts[-2:-1][0]  # the vowel
 else:
  pc = b[-2:-1]
 # step1: modify 'b' as appropriate for this aorist
 if pada == 'passive':
  # do no adjustments to 'b'. They have been done already
  pass
 elif (kale_459_P(root,theclass,pada,b)) or (root in ['So','so','Co']):
  b = b[0:-1] + 'A'
 elif root == 'i':
  b = 'gA'
 bc = b #  before consonant endings
 bv = b[0:-1] # before vowel endings
 # Programming note: could adjust endings[2] and endings[6] in i-loop below
 if (root == 'BU'):
  bv = b + 'v'
  endings[2] = 'an' # 3P is 'an' instead of 'us'
 elif b[-1:] == 'A':
  # 'am' treated like a consonant except for 'bhuu'
  endings[6] = endings[6][1:] # drop 'a' in 'am'
 # step1a: All aorists use augment 'a'
 btab = [] 
 for i in xrange(0,n):
  ending = endings[i]
  efirst = ending[0]
  if efirst in init.vowel_set:
   x = bv
  else:
   x = bc
  y = augment_a(x)  # prefix augment 'a'
  z = conjugation_join(y,ending) # why not aorist_join?
  btab.append(z)
 return btab

def conjugation_tab_aorist2(upasargas,theclass,pada,root,dbg=False):
 """
 """
 global aorist_sym,aorist_pada
 err0 = "conjugation_tab_aorist2(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 aorist_sym = 'luN2'
 tok = pada
 ylast = tok[-1:]
 (parts,types) = word_parts(tok)
 if types == 'cvc':
  pc = parts[1][0]  # the vowel
 else:
  pc = None
 sew_code = 'aniw'
 if (pada == 'a') and (aorist_2_544_P(root,theclass,pada,upasargas)):
  # Kale 544. these atmanepada verbs are conjugated in Parasmaipada
  # when in the 2nd variety
  pada = 'p'
  aorist_pada = pada
 ans = conjugation_tab_aorist2_main(upasargas,theclass,pada,root,sew_code)
 return ans

def conjugation_tab_aorist2_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ 
 """
 global aorist_sym,aorist_voice
 err0 = "conjugation_tab_aorist2_main(%s,%s,%s,%s,%s.%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 b = atok
 nb = len(b)
 lc = b[-1:]
 # pc is penultimate character
 if nb == 1:
  pc = None
 else:
  pc = b[-2:-1]
 # step1: modify 'b' as appropriate for this aorist
 if pada == 'passive':
  # do no adjustments to 'b'. They have been done already
  pass
 elif (lc in 'fF') or (root == 'dfS'):
  # ale 538: gunate final 'Ri' 'RI' or the 'Ri' in 'dRish'
  b = gunate_final_vowel(b)
 elif (pc in init.nasal_set) or (pc == 'M'):
  # Kale 539: drop penultimate nasal
  b = b[0:-2] + b[-1:]
 elif (root == 'as'):
  b = 'asT'  #Kale 541
 elif (root == 'KyA'):
  b = 'Ky'
 elif (root == 'pat'):
  b = 'papt'
 elif (root == 'vac'):
  b = 'voc'
 elif (root == 'SAs'):
  b = 'Siz'
 elif (root == 'Svi'):
  b = 'Sv'
 elif (root == 'hve'):
  b = 'hv'
 # step1a: All aorists use augment 'a'
 btab = [] 
 for i in xrange(0,n):
  ending = endings[i]
  efirst = ending[0]
  x = b
  # In this aorist, 'a' is added to the root;
  # this 'a' is
  #   dropped before 1S [a m]
  #   dropped before 3P [a n]
  #   dropped before 3P [a n t a]
  #   lengthened before 'v' and 'm'
  if (efirst == 'a'):
   # no action
   pass
  elif (efirst in 'vm'):
   x = conjugation_join(x,'A')
  else:
   x = conjugation_join(x,'a')
  y = augment_a(x)  # prefix augment 'a'
  z = conjugation_join(y,ending) # why not aorist_join?
  btab.append(z)
 return btab

def conjugation_tab_aorist3(upasargas,theclass,pada,root,dbg=False):
 """
 """
 global aorist_sym
 err0 = "conjugation_tab_aorist3(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 if dbg:
  print err0
 aorist_sym = 'luN3'
 sew_code = 'aniw'
 ans = conjugation_tab_aorist3_main(upasargas,theclass,pada,root,sew_code,dbg=dbg)
 return ans

def conjugation_tab_aorist3_main(upasargas,theclass,pada,rootx,sew_code=None,i_insert=None,dbg=False):
 """ Note 'rootx' is special to aorist3 See below
 """
 global aorist_sym,aorist_voice
 err0 = "conjugation_tab_aorist3_main(%s,%s,%s,%s,%s,%s)" %(upasargas,theclass,pada,rootx,sew_code,i_insert)
 if dbg:
  print err0
 tense_sym = aorist_sym
 #--- 0. interpret 'dhaatux'
 #--- dhaatux is special in aorist3.
 # If 'dhaatux' is a symbol, then it is just the 'dhaatu'
 # If 'dhaatux' is a list, its first symbol should be the 'dhaatu'
 # and its second symbol should be the base, ending in 'ay'
 # This second form allows for the distinction by definition
 # of certain class 10 forms otherwise indistinguishable.
 # aorist-tok refers to 'dhaatu' in either sense of 'dhaatux'
 if isinstance(rootx,list):
  root = rootx[0]
  base = rootx[1]
 else:
  root = rootx
  base = None
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 if base:
  b = base
 else:
  b = atok
 # step1: modify 'b' as appropriate for this aorist
 # reduplicate the root
 b1 = reduplicate(b) # Kale 547
 if pada == 'passive':
  # do no adjustments to 'b'. They have been done already
  pass
 elif (root == 'De'):
  b1 = 'daD' # Kale 547
 elif theclass in ['10','11']:  # 11 used for causal
  Eng_def = None
  b1 = aorist_causal_base_irreg(root,theclass,pada,upasargas,Eng_def,dbg=dbg)
  if not b1:
   b1 = aorist_causal_base(root,theclass,pada,upasargas,Eng_def,dbg=dbg)
 b1 = solution(b1)
 b1 = flatten(b1)
 if dbg:
  print err0,"b1=",b1
 if not isinstance(b1,list): # Usual case
  btab = aorist3_make_btab(b1,endings,dbg=dbg)
 else:
  # alternate case: 2 options
  b1tab = aorist3_make_btab(b1[0],endings,dbg=dbg)
  b2tab = aorist3_make_btab(b1[1],endings,dbg=dbg)
  btab = join_arrays(b1tab,b2tab)
 if [root,theclass,pada] == ['sAmaya','10','p']:
  btab = map(lambda x: x.replace('samaya','samayA'),btab)
  print err0,"KLUDGE"
 return btab

def aorist3_make_btab(b1,endings,dbg=False):
 """
 """
 if dbg:
  err0 = "aorist3_make_btab(%s,%s)" %(b1,endings)
  print err0
 n = len(endings)
 btab=[]
 for i in xrange(0,n):
  ending = endings[i]
  efirst = ending[0]
  x = b1
  # Kale 547. final 'o' is dropped
  #Kale 547: final 'i' changes to 'iy' and final 'u' changes to 'uv
  # before the appended 'a'
  if x.endswith('o'):
   x = x[0:-1]
  elif x.endswith('i'):
   x = x[0:-1] + 'iy'
  elif x.endswith('u'):
   x = x[0:-1] + 'uv'
  # In this aorist, 'a' is added to the root;
  # this 'a' is
  #   dropped before 1S [a m]
  #   dropped before 3P [a n]
  #   dropped before 3P [a n t a]
  #   lengthened before 'v' and 'm'
  if efirst == 'a':
   # no further action required.
   pass
  elif efirst in 'vm':
   x = conjugation_join(x,'A')
  else:
   x = conjugation_join(x,'a')
  y = augment_a(x) # prefix augment 'a'
  z = conjugation_join(y,ending)
  btab.append(z)
 return btab

def reduplicate(inval,wparts=None,dbg=False):
 """ USED by aorist3_main
 """
 err0 = "reduplicate(%s,%s)" % (inval,wparts)
 try:
  ans = reduplicate_join(reduplicative_pfx(inval,wparts),inval)
 except:
  print err0,"ERROR"
  #raise NameError("fatal error")
  exit(1)
 return ans

def aorist_causal_base_irreg(root,theclass,pada,upasargas,Eng_def=None,dbg=False):
 """
 """
 err0 = "aorist_causal_base_irreg(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,Eng_def)
 if dbg:
  print err0
 d = {'sUc':'susUc', 
   'cur':'cUcur', # p. 344
   # the rest from Kale 556
   'Irzy':['erzizy','erzyiy'], 
   'UrRu':'orRUnav',
   'gaR':['jagaR','jIgaR'],
   'GrA':['jiGrap','jiGrip'],
   'cakAs':['cIcakAs','cacakAs'],
   'dyut':'dudyut',
   'pA':['pIpy','pIpal'],  # drink, protect
   'sTA':'tizWip',
   'sPur':'pusPar'}
 if root in d:
  return d[root]
 if (root == 'i') and (upasargas == ['aDi']): # study, remember (last)
  return ['apip','jIgap','jIgam']
 return None

def aorist_causal_base(root,theclass,pada,upasargas,Eng_def=None,dbg=False):
 """
 ; get the base for aorist3 for the causal of the given dhaatu
 ; Also gets the base for class-10 roots
 ; Returns a list of token arrays.
 """
 err0 = "aorist_causal_base(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,Eng_def)
 if dbg:
  print err0
 tok = root
 lc = tok[-1:] 
 (parts,types) = word_parts(tok)
 if types == 'cvc':
  pc = parts[1][0] # the vowel
 else:
  pc = None
 # get cb = the causal base
 if theclass == '11':
  cb = causal_base(root,theclass,pada,upasargas,None) 
 else:
  cb = class10_base(root)
 if dbg:
  print "chk: before: cb==",cb
 cb0 = cb # used later
 # implement Kale 550
 if not isinstance(cb,list):
  cb = [cb]
 # in case of aMs,10,p.  cb=[['aMsay'],'aMsApay']. Thus, we need to flatten
 cb = flatten(cb)
 cbnew = []
 if dbg:
  print "chk: cb=",cb
 for cb1a in cb:
  cb1 = aorist3_alter10(cb1a,root,dbg=dbg)
  if dbg:
   print "after aorist3_alter10(%s,%s), cb1=%s" %(cb1a,root,cb1)
  if lc in 'uU':
   cb2 = aorist3_alter10a(cb1)
   if dbg:
    print "after aorist3_alter10a: cb1=%s, cb2=%s" %(cb1,cb2)
   if root in ['sru','Sru','dru','pru','plu','cyu']:
    # Kale 550(a)
    cb1 = [cb1,cb2]
   else:
    cb1 = cb2
  if not (cb1 in cbnew):
   cbnew.append(cb1)
 cb = flatten(cbnew) # Not in Elisp, but seems like a good idea
 cb = solution(cb)
 if dbg:
  print "chk: after 550: cb=",cb
 # implement Kale 551.
 if root in ["BrAj","BAs","BAz","dIp","jIv","mIl","pIq","kaR","caR","raR",
              "BaR","vaR","SraR","lup","hew","heW","hve","luw","luW","lup"]:
  if not isinstance(cb0,list):
   cb0=[cb0]
  cb1 = cb0[0][0:-2]
  cbnew = reduplicate(cb1)
  if causal_603_P(tok,root):
   # lengthen the final vowel
   # this logic is required for agreement with Kale 551 in examples
   # kaN, chaN , raN, shraN
   cbnew = aorist3_lengthen_final_vowel(cbnew)
  elif root in ['lup','luw','luW']:
   # based on examples, the initial 'u' is lengthened in the first form
   #print "chk: cb=",cb
   #cb = cb.replace('u','U')
   # Note: In case of 'luw', cb is now 'lUluw', so that first 'u' already long.
   cb = cb[0] + 'U' + cb[2:]
  cb = [cb,cbnew]
  if root == 'hve':
   # 'hve' is exceptional (it takes samprasaaraNa somehow). Kale 553.
   cb = ['jUhav','juhAv']
 if root == 'Svi':
  #  'shvi' is exceptional (it optionally takes samprasaaraNa)
  cb = cb.append('SuSav')
 #
 b = tok  # the root
 nb = len(b)
 ans=[]
 # pc is penultimate char
 if nb == 1:
  pc = None
 else:
  pc = b[-2:-1]
 if kale_400_P(root) and (root != 'aMs'):
  # kale 554: preserve vowel unchanged (no 'i' substituted)
  if not isinstance(cb,list):
   all = [cb]
  else:
   all = cb
  ans1 = []
  for this in all:
   # replace first vowel in 'this' with 'a'
   for i in xrange(0,len(this)):
    v = this[i]
    if v in init.vowel_set:
     this = changestringitem(this,i,'a')
     break # only change first vowel
   ans1.append(this)
  cb = ans1
  ans = cb
 elif (tok[0] in init.vowel_set) and (types == 'vc'):
  # initial vowel
  i = 'i'
  if root in ['Un','aNk','aNg','anD','aMs','arT']:
   # Kale 549 (c) p. 342
   i = 'a'
  if (len(parts[1]) == 2) and\
   ((pc in init.nasal_set) or (pc == 'M') or (pc in 'dr')):
   # Kale 549 (b)
   c1 = parts[1] # conjunct consonant
   v = parts[0]  # the vowel
   #print "chk1:lc=",lc
   c2 = reduplicate_cons(lc)
   #print "chk2:lc=",lc,"c2=",c2
   if (pc == 'M') or (not (pc in init.nasal_set)):
    n1 = pc
   else:
    n1 = nasal_match(c2[0])
   try:
    ans = v + c1[0:-2] + n1 + c2 + i + c1[1:]
   except:
    print err0,"ERROR: (%s,%s,%s,%s,%s,%s)"%(v,c1[0:-2],n1,c2,i,c1[1:])
    print "parts=",parts,"types=",types
    print "pc=",pc,"lc=",lc,"c2=",c2
    exit(1)
  elif len(parts[1]) == 1: # simple consonant
   # Kale 549 (a)
   ans = tok[0:-1] + reduplicate_cons(lc) + i + tok[-1:]
  else:
   # ends with some other conjunct consonant
   # Kale 549 (a)
   c1 = parts[1]
   v = parts[0] # the vowel
   c2 = reduplicate_cons(lc)
   ans = v + c1[0:-1] + c2 + i + c1[-1:] 
 elif (types == 'cvc') and (parts[1] in 'fF'):
  # Kale 548(c) p. 341
  # Roots having a penultimate 'Ri' or 'RI' optionally preserve it,
  # with 'RI' being changed to 'Ri'
  b0 = b
  b = b[0:-2] + 'f' + b[-1:]
  b = b + 'ay'  # base preserving 'f'
  b = aorist3_alter10(b,dbg=dbg)
  if dbg:
   print "Kale 548(c), after aorist3_alter10. b0=%s,b=%s" % (b0,b)
  # two options
  ans = [cb,b]
 else:
  ans = cb
 if not isinstance(ans,list):
  ans = [ans]
 return ans
 # CURRENT POINT

def nasal_match(c):
 """
 ; Find nasal matching the class of 'c' 
 ; e.g., if 'c' is dental, return 'n'
 ; If no match is found, return 'M'
 """
 if c in init.guttural_set:
  return 'N'
 elif c in init.palatal_set:
  return 'Y'
 elif c in init.cerebral_set:
  return 'R'
 elif c in init.dental_set:
  return 'n'
 elif c in init.labial_set:
  return 'm'
 else:
  return 'M'

def aorist3_alter10(tok,root=None,dbg=False):
 """
 ; carry out the alterations of Kale 548(a), applicable to
 ; roots of the 10th class and of causals. Includes reduplication
 ; Assumes, if class 10, the conjugation-10 base already reflected in 'tok'.

  ; Kale 548 b.
  ; If the vowel of the reduplicative syllable is 'a',
  ; it is changed to 'i' if the syllable following it
  ; be short and not prosodially long (i.e., not followed by conj cons. p.14)
  ; This having been checked, if the reduplicative 
  ; syllable has 'i' for its vowel, and if the following
  ; syllable is not followed by either a long syllable or
  ; a conjunct consonant, then
  ; the 'i' of the reduplicative syllable is lengthened to 'ii'
  ; Note: based on Whitney, this also applies when the reduplicative
  ; syllable is 'u'
  ; Kale examples:
  ;  [bh aa v a y] -> [b ii bh a v] (causal of 'bhuu')
  ;  [ch e t a y] -> [ch ii ch i t] (causal of 'chi')
  ;  [s kh a l a y] -> [ch i s kh a l] (causal of 'skhal')
  ;  [s p a n d a y] -> [p a s p a n d] (causal of 'spand')
  ;  [v a r t a y] -> [v a v a r t]
  ;  [v Ri t a y] -> [v ii v Ri t]
  ;  [k ii r t a y] -> [ch i k ii r t] : this is an exception,
  ;    implemented above
 """
 global aorist_id
 b = aorist3_reduplicate(tok,root)
 err0 = "aorist3_alter10(%s,%s)" %(tok,root)
 if dbg:
  print err0," after aorist3_reduplicate, b=",b
 (parts,types) = word_parts(b)
 ntypes = len(types)
 if (5 <= ntypes) and (types.startswith('cvcvc')):
  ivowel = 1
 elif (4 <= ntypes) and (types.startswith('vcvc')):
  ivowel = 0
 else:
  ivowel = None
 if dbg:
  print "chk: parts=%s, types=%s, ivowel=%s" %(parts,types,ivowel)
 if ivowel:
  jvowel = ivowel + 2
  if (parts[ivowel] == 'a') and\
     (parts[jvowel][0] in init.shortsimplevowel_set) and\
     (not (prosodially_long_P(parts,jvowel))):
   parts[ivowel] = 'i'
  if (parts[ivowel] == 'i') and\
     (parts[jvowel][0] in init.shortsimplevowel_set) and\
     (not (prosodially_long_P(parts,jvowel))) and\
     (not (prosodially_long_P(parts,ivowel))):
   parts[ivowel] = 'I'
   if dbg:
    print "chk: parts now =%s" % parts
  if (parts[ivowel] == 'u') and\
     (parts[jvowel][0] in init.shortsimplevowel_set) and\
     (not (prosodially_long_P(parts,jvowel))) and\
     (not (prosodially_long_P(parts,ivowel))):
   parts[ivowel] = 'U'
 b1 = ''.join(parts)
 ans = b1
 # Let's assume aorist_id is always 3.
 #aorist_id = '3'
 if (aorist_id == '3') and \
    (root in ['smf','df','tvar','praT','mrad','stF','spaS','vezw','cezw']):
  # Kale 552. Definitely or optionally change the vowel
  # in the redup syllable to 'a'
  b2 = b1
  for i in xrange(0,len(b2)):
   v = b2[i]
   if v in init.vowel_set:
    b2 = changestringitem(b2,i,'a')
    break # only change first vowel
  if root in ['vezw','cezw']:
   ans = [b1,b2]
  else:
   ans = b2
 return ans

def aorist3_reduplicate(tok,root=None,dbg=False):
 """
 """
 b = tok[0:-2] # remove the ending 'ay'
 # change long vowel
 # Note: 'kRIt' , which enters here as [k ii r t], does not
 # shorten the 'ii', based on examples in Kale p. 341 and dhaatukosha
 # Other examples (cheShT veShT from Kale 552)
 if root not in ["SAs", "ej", "kAS", "krIq", "kzIv", "KAd", "Kel", "QOk", "tAy",
                 "dAS", "dev", "nAT", "proT", "bAD", "yAc", "yoD", "rAD", "rAj",
                 "lAG", "lep", "lok", "loc", "vep", "vel", "SlAG", "Slok", 
                 "sek", "sev", "hez"]:
  # Kale 555. These roots do not shorten penultimate
  if (not ((2 < len(b)) and (b[-1:] in init.consonant_set) and\
     (b[-2:-1] in init.consonant_set))):
   #print "aorist3-reduplicate chk: b before=%s" % b
   b = aorist3_shorten_1st_vowel(b)
   #print "aorist3-reduplicate chk: b  after=%s" % b
 # reduplicate in 'usual' way
 b1 = reduplicate(b)
 return b1

def aorist3_alter10a(tok,dbg=False):
 """ implements Kale 550
 """
 ans = tok
 # find j as index of first vowel in tok
 j = -1 # no vowel
 for i in xrange(0,len(tok)):
  if tok[i] in init.vowel_set:
   j = i
   v = tok[j]
   break
 if (j != -1) and (v in 'iI'):
  if v == 'i':
   v1 = 'u'
  else:
   v1 = 'U'
  nc = tok[j+1] # next character after v; could fail if j == (len(tok)-1)
  if not (((nc in init.labial_set) or\
          (nc in init.semivowel_set) or
          (nc=='j'))and\
          (tok[j+2] in 'aA')):
   ans = changestringitem(ans,j,v1)
 return ans

def aorist3_shorten_1st_vowel(tok,dbg=False):
 """
 """
 ans = tok
 for i in xrange(0,len(tok)):
  v = tok[i]
  if v in init.vowel_set:
   v1 = aorist3_shorten_vowel(v)
   ans = changestringitem(ans,i,v1)
   break
 return ans

def aorist3_shorten_vowel(v):
 """
 """
 allv = init.vowel_set # 'aiufxAIUFXeEoO'
 shortv = 'aiufxaiufxiiuu' # not e,E->i, oO -> u
 ans = v
 i = allv.index(v)
 if i>=0:
  ans = shortv[i]
 return ans

def aorist3_lengthen_final_vowel(tok,dbg=False):
 """
 """
 ans = tok
 for j in xrange(len(tok)-1,-1,-1): 
  # if len(tok)==5, j is 4,3,2,1,0
  v = tok[j]
  if v in init.vowel_set:
   v1 = aorist3_lengthen_vowel(v)
   ans = changestringitem(ans,j,v1)
   break
 return ans

def aorist3_lengthen_vowel(v):
 """
 """
 allv = init.vowel_set # 'aiufxAIUFXeEoO'
 longv = 'AIUFXAIUFXEOoU'
 ans = v
 i = allv.index(v)
 if i>=0:
  ans = longv[i]
 return ans

def prosodially_long_P(parts,ivowel):
 """ parts comes from (parts,types) = word_parts(someword)
     ivowel is the index of a vowel part
 ; Kale #11, p. 14
 ; a short vowel followed by a conjunct consonant is
 ; prosodially long.
 """
 n = len(parts)
 if (ivowel < (n-1)) and\
    (parts[ivowel] in init.shortsimplevowel_set) and\
    (1 < len(parts[ivowel+1])):
  return True
 else:
  return False

 # CURRENT POINT
def reduplicate_cons(c):
 """
 ; c assumed to be a token representing a single consonant
 ; a consonant-token is returned
 """
 r = reduplicate(c+'a')
 ans = r[0]
 return ans

def conjugation_tab_aorist4(upasargas,theclass,pada,root,dbg=False):
 """assumes validity of aorist-tok, aorist-id, aorist_passive_P
 """
 global aorist_sym,aorist_passive_P,aorist_pada
 err0 = "conjugation_tab_aorist4(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 if dbg:
  print err0
 aorist_sym = 'luN4'
 if aorist_passive_P:
  ans = conjugation_tab_aorist4_main(upasargas,theclass,'a',root,dbg=dbg)
  aorist_pada = pada
 else:
  ans = conjugation_tab_aorist4_main(upasargas,theclass,pada,root,dbg=dbg)
 return ans

def conjugation_tab_aorist4_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ 
 """
 global aorist_sym,aorist_voice
 err0 = "conjugation_tab_aorist4_main(%s,%s,%s,%s,%s,%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 if dbg:
  print err0
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 # 5b. get table of base-sew codes (bitab)
 b = atok
 nb = len(b)
 lc = b[-1:]
 if types == "vc":
  pc = parts[0][0]  # the vowel
 elif types == "cvc":
  pc = parts[1][0]  # the vowel
 elif types == "cv":
  pc = parts[0][0]  # the consonant
 else:
  pc = None
 # step1a: All aorists use augment 'a'
 btab=[] 
 # alter base 'b' into 'x'. Get xalt (usu. nil)
 x = b
 xalt = []
 lcalt = None
 if root in ['sfj','dfS']:
  #; Kale p. 348, footnote 1
  #; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
  # is changed to 'ra' before
  # a consonantal strong termination in the general tenses.
  b = b[0:-2] + 'ra' + b[-1:]
  pc = 'a'
 elif root == 'vas':
  # Antoine2#220  p. 155.  'vas' becomes 'vat' in the aorist
  # Without this, the result differs in 3D 2D and 2P, namely
  # 'avaastaam' instead of 'avaattaam', etc.
  # Kale (footnote p. 350, argues a derivation
  # 'avaas' + 'staam' -> 'avaattaam' by 480; however, by 567,
  # the ending would have been modified to 'taam', so I don't agree
  # with the derivation. Thus, the inclusion of this special case.
  b = 'vat'
  lc = 't'
 if pada == 'p':
  x = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale 566a
 elif pada == 'a':
  if root == 'mI':
   x = 'mA' # Kale example p.352
  elif lc in 'iIuU':
   x = aorist_gunate_final_vowel(b) # gunate. Kale 566b
  elif lc == 'F':
   # see also function 'kale-394 in gram2.el
   x = b[0:-1]
   if (pc in init.labial_set) or (pc == 'v'):
    x = x + 'Ur'
   else:
    x = x + 'Ir'
  elif (root in ['dA','DA','sTA']) or (kale_459_P(root,theclass,pada)):
   # Kale 568
   x = b[0:-1] + 'i'
  elif root == 'han':
   x = b[0:-1]  # Kale 569
  elif (root == 'yam') and (not (upasargas == ['upa'])):
   # Kale 570
   x = b[0:-1]
 # end of pada == 'a' cases
 if ((sew_gen == 'aniw') or (root == 'tfp')) and\
    (pc == 'f') and (types == 'cvc') and (pada == 'p'):
  # Kale 566c
  xalt = parts[0] + 'rA' + parts[2]
  lcalt = xalt[-1:]
 elif (root == 'gam') and (pada == 'a'):
  # Kale 570
  xalt = x[0:-1] # drop nasal optionally
  lcalt = xalt[-1:]
 elif (root == 'yam') and (pada == 'a') and (upasargas == ['upa']):
  # Kale 570
  xalt = x[0:-1] # drop nasal optionally
  lcalt = xalt[-1:]
 elif (root == 'i') and (pada == 'a') and (upasargas == ['aDi']):
  # Kale p. 352
  xalt = 'gI'
  lcalt = xalt[-1:]
 # reset lc as last letter of 'x'
 lc = x[-1:]
 if not isinstance(xalt,list):
  xalt = [xalt]
 # loop through combining base with endings, and prefixing 'a'
 if dbg:
  print "chk: lc=",lc,"lcalt=",lcalt,"x=",x,"xalt=",xalt
 for i in xrange(0,n):
  ending0 = endings[i]
  ending = ending0
  if (aorist_voice == 'active') and (ending.startswith(('st','sT'))) and\
     ((lc == 'h') or (lc in init.shortsimplevowel_set) or\
      (lc in init.consonant_set) and (not (lc in init.nasal_set)) and\
       (not (lc in init.semivowel_set))):
   # Kale 567
   ending = ending[1:]
  y = augment_a(x) # prefix augment 'a'
  z = aorist_join(y,'aniw',ending,root,'s',i_insert,dbg=dbg)
  if dbg:
   print "chk: i=%s,y=%s,ending=%s,root=%s,i_insert=%s => z=%s" %(i,y,ending,root,i_insert,z)
  thisbtab = [z]
  for x1 in xalt:
   ending = ending0
   if (aorist_voice == 'active') and (ending.startswith(('st','sT'))) and\
      ((lcalt == 'h') or (lcalt in init.shortsimplevowel_set) or\
       (lcalt in init.consonant_set) and (not (lcalt in init.nasal_set)) and\
        (not (lcalt in init.semivowel_set))):
    # Kale 567
    ending = ending[1:]
   y = augment_a(x1) # prefix augment 'a'
   z = aorist_join(y,'aniw',ending,root,'s',i_insert)
   thisbtab.append(z)
  # some over-rides to thisbtab
  if (root == 'pad') and (pada == 'a'):
   if i == 0:
    # Kale #575, p. 351
    thisbtab = ['apAdi'] 
   elif i == 5:
    # Kale p. 352.  (to correct apadDvam)
    thisbtab = ['apaDvam']
  elif (root == 'buD') and (pada == 'a'):
   if i == 0:
    # Kale #571 p. 351
    thisbtab.append('aboDi')  # another variant
   elif i == 5:
    thisbtab = ['aBudvam']  # was aBuDDvam
  # install thisbtab into btab
  thisbtab = solution(thisbtab)
  btab.append(thisbtab)
 return btab

def conjugation_tab_aorist5(upasargas,theclass,pada,root,dbg=False):
 """assumes validity of aorist-tok, aorist-id, aorist_passive_P
 """
 global aorist_sym,aorist_passive_P,aorist_pada
 err0 = "conjugation_tab_aorist5(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 aorist_sym = 'luN5'
 if aorist_passive_P:
  ans = conjugation_tab_aorist5_main(upasargas,theclass,'a',root)
  aorist_pada = pada
 else:
  ans = conjugation_tab_aorist5_main(upasargas,theclass,pada,root)
 return ans

def conjugation_tab_aorist5_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ Uses aorist_sym
 """
 global aorist_sym
 err0 = "conjugation_tab_aorist5_main(%s,%s,%s,%s,%s.%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 # 5b. get table of base-sew codes (bitab)
 b = atok
 nb = len(b)
 lc = b[-1:]
 if types == "vc":
  pc = parts[0][0]  # the vowel
 elif types == "cvc":
  pc = parts[1][0]  # the vowel
 elif types == "cv":
  pc = parts[0][0]  # the consonant
 else:
  pc = None
 # step1a: All aorists use augment 'a'
 btab=[] 
 # alter base 'b' into 'x'. Get xalt (usu. nil)
 x = b
 xalt = []
 if root in ['sfj','dfS']:
  #; Kale p. 348, footnote 1
  #; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
  # is changed to 'ra' before
  # a consonantal strong termination in the general tenses.
  b = b[0:-2] + 'ra' + b[-1:]
  pc = 'a'
 elif root == 'vas':
  # Antoine2#220  p. 155.  'vas' becomes 'vat' in the aorist
  # Without this, the result differs in 3D 2D and 2P, namely
  # 'avaastaam' instead of 'avaattaam', etc.
  # Kale (footnote p. 350, argues a derivation
  # 'avaas' + 'staam' -> 'avaattaam' by 480; however, by 567,
  # the ending would have been modified to 'taam', so I don't agree
  # with the derivation. Thus, the inclusion of this special case.
  b = 'vat'
  lc = 't'
 elif root == 'han':
  b = 'vaG' # Kale 569
 #
 if pada == 'p':
  if root == 'daridrA':
   x = b[0:-1]  # Kale 578
  elif root == 'grah':
   # Kale 474. The augment 'i' as added to 'grah' is long
   # in all non-conjugational tenses, except in the Perfect
   x = x + 'i'
  elif root == 'guh':
   # Kale p. 249 ftnote
   # The penultimate 'u' of 'guh' is lengthened in the
   # special tenses and before a strong termination beginning
   # with a vowel
   x = 'gUh'
  elif root == 'vij':
   pass # Kale 466, and example p. 354
  elif root in ['kzaR','Svas','Svi','kaw','caw','cat','cad',
                'paT','maT','lag','has','hlas']:
   x = aorist_gunate_final_vowel(b)  # guna. Kale 574d
  elif lc in 'hmy':
   x = aorist_gunate_final_vowel(b)  # guna. Kale 574d
  elif lc in init.vowel_set:
   x = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale 574a
   if root == 'UrRu':
    # Kale 577
    xalt = ['UrRav','UrRu']
  elif (pc == 'a') and (lc in 'rl'):
   x = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale 574a
  elif root in ['vad','vraj']:
   x = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale 574a
  elif root == 'mfj':
   x = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale p. 354 ex.
  elif (types == 'cvc') and (pc == 'a') and (len(parts[2]) == 1) and\
       (not (lc in 'rl')) and (not (root == 'han')): # by example Kale p.354
   xalt = aorist_gunate_final_vowel(b,vfdDiP=True) # vrddhi. Kale 574c
  elif pc and (pc in init.shortsimplevowel_set):
   # the 'pc and' clause is in case pc == None. In this case, Python
   # complains about None in 'xyz'
   x = aorist_gunate_final_vowel(b)  # guna. Kale 574b
 elif pada == 'a':
  if (root == 'vij'):
   # Kale 466, and example p. 354
   pass
  elif (root == 'grah'):
   x = x + 'i'
  elif (root == 'guh'):
   # Kale p. 249 ftnote
   # The penultimate 'u' of 'guh' is lengthened in the
   # special tenses and before a strong termination beginning
   # with a vowel
   x = 'gUh'
  elif (root == 'UrRu'):
   # Kale 577
   xalt = ['UrRu']
  else:
   x = gunate_final_vowel(b) # Only applies to final and short
   if (root == 'vf') or (lc == 'F'):
    #; Kale 475. The intermediate 'i' is optionally lengthened in the
    #; case of 'vRi' and roots ending in 'RI' except
    #; in the Perfect, the Benedictive A and the aorist P
    #; In particular, it is optionally lengthened in the aorist 'A'.
    #; Since the endings in the 5th form all start with 'i',
    #; and since 'x' has been gunated, we can force the lengthening
    #; by adding 'i' to 'x'.
    xalt = x + 'i'
 #elif pada == 'a':
 if aorist_passive_P and (theclass in ['10','11']):
  # Kale 598
  xalt = x + 'ay'
 if root in ['gup','DUp','vicC','paR','pan','kam','ft']:
  xalt = kale_461_alt(root)
 # reset lc as last letter of 'x'
 lc = x[-1:]
 if not isinstance(xalt,list):
  xalt = [xalt]
 # loop through combining base with endings, and prefixing 'a'
 for i in xrange(0,n):
  ending = endings[i]
  y = augment_a(x) # prefix augment 'a'
  z = aorist_join(y,'aniw',ending,root,'s',i_insert)
  w = z
  thisbtab = [w]
  """
  if len(xalt) != 0:
   thisbtab = [w]
  """
  for x1 in xalt:
   y = augment_a(x1)
   z = aorist_join(y,'aniw',ending,root,'s',i_insert)
   thisbtab.append(z)
  if (pada == 'a') and (root in ['dIp','jan','pUr','tAy','pyAy']) and (i == 0):    # Kale 575. The roots mentioned optionally substitute 'i' for
   # the ending 'iShTa' (third pers. sing. Atm)
   ending1 = 'i'
   y = augment_a(x)
   z = aorist_join(y,'aniw',ending1,root,'s',i_insert)
   thisbtab.append(z)
  if (pada == 'a') and (theclass == '8') and (lc in 'Rn') and (i in [0,3]):
   # Jale 576
   if i == 0:
    ending1 = 'ta'
   else: # i == 3
    ending1 = 'TAH'
   y = augment_a(b[0:-1])
   z = aorist_join(y,'aniw',ending1,root,'s',i_insert)
   thisbtab.append(z)
  thisbtab = solution(thisbtab)
  btab.append(thisbtab)
 return btab

def kale_461_alt(root,dbg=False):
 """
   Kale 461. The roots 'gup' 'dhuup' 'vichCh' 'paN' 'pan'
   'kam' and ? 'Rit' preserve their conjugational bases optionally
   Note: I am not sure of 'Rit'. The form given works for
   aorist passive example on p. 366.
 """
 err0 = "kale_461_alt(%s)" % root
 d = {'gup':'gopAy', 'DUp':'DUpAy','vicC':'vicCAy',
      'paR':'paRAy', 'pan':'panAy','kam':'kamAy',
      'ft':'ftiy'}
 if root in d:
  return d[root]
 else:
  return None

def conjugation_tab_aorist6(upasargas,theclass,pada,root,dbg=False):
 """
 """
 global aorist_sym #,aorist_tok
 err0 = "conjugation_tab_aorist6(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 aorist_sym = 'luN6'
 #tok = aorist_tok
 sew_code = 'aniw'
 ans = conjugation_tab_aorist6_main(upasargas,theclass,pada,root,sew_code)
 """ Why different from others?
 if aorist_passive_P:
  ans = conjugation_tab_aorist6_main(upasargas,theclass,'a',root)
  aorist_pada = pada
 else:
  ans = conjugation_tab_aorist6_main(upasargas,theclass,pada,root)
 """
 return ans

def conjugation_tab_aorist6_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):

 """ Uses aorist_sym
 """
 global aorist_sym
 err0 = "conjugation_tab_aorist6_main(%s,%s,%s,%s,%s.%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 tense_sym = aorist_sym
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # CURRENT POINT
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 # 5b. get table of base-sew codes (bitab)
 b = atok
 nb = len(b)
 lc = b[-1:] # last char
 # set pc, penultimate char
 if nb == 1:
  pc = None
 else:
  pc = b[-2:-1]
 # -- step1: modify 'b' as appropriate for this aorist
 if pada == 'passive':
  # do no adjustments to 'b'. They have been done already
  pass
 elif lc in init.vowel_set:
  # Kale 559 examples
  b = b[0:-1] + 'A'
 btab=[] 
 for i in xrange(0,n):
  ending = endings[i]
  x = b
  y = augment_a(x) # prefix augment 'a'
  z = conjugation_join(y,ending) # Note not aorist_join
  thisans = z
  btab.append(thisans)
 return btab

def conjugation_tab_aorist7(upasargas,theclass,pada,root,dbg=False):
 """assumes validity of aorist-tok, aorist-id, aorist_passive_P
 """
 global aorist_sym,aorist_passive_P,aorist_pada
 err0 = "conjugation_tab_aorist7(%s,%s,%s,%s)" %(upasargas,theclass,pada,root)
 aorist_sym = 'luN7'
 sew_code = 'aniw'
 if aorist_passive_P:
  ans = conjugation_tab_aorist7_main(upasargas,theclass,'a',root,sew_code)
  aorist_pada = pada
 else:
  ans = conjugation_tab_aorist7_main(upasargas,theclass,pada,root,sew_code)
 return ans

def conjugation_tab_aorist7_main(upasargas,theclass,pada,root,sew_code=None,i_insert=None,dbg=False):
 """ Uses aorist_sym
 """
 global aorist_sym
 err0 = "conjugation_tab_aorist7_main(%s,%s,%s,%s,%s.%s)" %(upasargas,theclass,pada,root,sew_code,i_insert)
 tense_sym = aorist_sym
 if not i_insert:
  i_insert = 'i'
 endings = aorist_endings()
 n = len(endings)
 atok = root
 (parts,types) = word_parts(atok)
 # sew_gen
 if sew_code:
  temp = sew_code
 else:
  temp = construct_sew_code1a(root,theclass,pada,upasargas)
 sew_gen = solution(temp)
 # table of base-sew codes
 b = atok
 nb = len(b)
 lc = b[-1:]
 pc = b[-2:-1]  # penultimate char of base. Will be empty string if len(b)==1
 # step1a: All aorists use augment 'a'
 btab=[] 
 for i in xrange(0,n):
  ending = endings[i]
  x = b
  y = augment_a(x) # prefix 'a' augment
  z = aorist_join(y,sew_gen,ending,root,'s',i_insert)
  w = z
  thisans = w
  if (pada == 'a') and (root in ['duh','dih','lih','guh']) and\
    (i in [0,3,5,7]):  # 3S, 2S, 2P, 1D
   #  Kale 563. optionally drop the initial 'sa' or 'sA' of ending
   if (2 <= len(ending)):
    # in passive this will be overwritten
    ending = ending[2:] # drop 1st two chars ('sa' or 'sA')
   z = aorist_join(y,sew_gen,ending,root,'s',i_insert)
   w1 = z
   thisans = [w,w1]
  btab.append(thisans)
 return btab

def aorist_join(base,sew_code,sup,root,strength,i_insert,dbg=False):
 """
 """
 err0 = "aorist-join(%s,%s,%s,%s,%s,%s)" %(base,sew_code,sup,root,strength,i_insert)
 if isinstance(sup,list):
  return map(lambda x: aorist_join(base,sew_code,x,root,strength,i_insert),sup)
 if isinstance(base,list):
  return map(lambda x: aorist_join(x,sew_code,sup,root,strength,i_insert),base)
 if (sew_code == 'vew'):
  return map(lambda x: aorist_join(base,x,sup,root,strength,i_insert),['sew','aniw'])
 return aorist_join1(base,sew_code,sup,root,strength,i_insert,dbg=dbg)
 
def aorist_join1(base,sew_code,sup,root,strength,i_insert,dbg=False):
 """ based on conjugation-join.
    sew_code is either None, sew, or aniw
 """
 err0 = "aorist-join1(%s,%s,%s,%s,%s,%s)" %(base,sew_code,sup,root,strength,i_insert)
 if dbg:
  print err0
 # insert 'i' if needed
 # ending may be either a string, or a list of 2 strings
 if sew_code == 'sew':
  ending = conjugation_join(i_insert,sup)
 else:
  ending = sup
 efirst = ending[0]
 y = aorist_join1_adjust_y(base,efirst,root)
 ny = len(y)
 ylast = y[-1:] # last char
 yfirst = first_cons(y)
 if dbg:
  print "y = %s ylast=%s ending= %s efirst=%s"%(y,ylast,ending,efirst)
 ans = None
 if efirst in init.vowel_set:
  ans = aorist_join1_vowel(y,ending,root)
 if not ans:
  if (efirst == 'D') and (ylast in init.vowel_set) and (not (ylast in 'aAi')):
   # 3rd special sandhi rule for future (Antoine2#110)
   ans = y + 'Q' + ending[1:]
 if not ans:
  if efirst in 'tT':
   ans = aorist_join1_t_th(y,ending,root)
 if not ans:
  if efirst == 'D':
   ans = aorist_join1_dh(y,ending,root)
 if not ans:
  if efirst == 's':
   ans = aorist_join1_s(y,ending,root)
   #print "ans from aorist_join1_s(%s,%s,%s) => %s" %(y,ending,root,ans)
 if not ans:
  if (efirst == 'm') and (ylast == 'c'):
   ans = y + ending # otherwise, 'c' is changed to 'j'
 if not ans:
  if (efirst in 'mv') and (ylast == 'm'):
   #Kale p. 321 (footnote)
   #Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
   #note 'n' may be changed to 'N' by sandhi-single (see below)
   ans = y[0:-1] + 'n' + ending
 if not ans:
  if (root == 'guh') and (sew_code == 'sew'):
   # the 'u' is lengthened, rather than gunated (Kale example p. 304)
   ans = 'gUh' + ending
 if not ans:
  sandhiget = SandhiGet(['Antoine72-4','Antoine72-5'])
  x = sandhiget.sandhi_pair(y,ending,'internal','join')
  if len(x)==1:
   ans = x[0][0]
 if not ans:
  x = sandhiget.sandhi_pair(y,ending,None,'join')
  if len(x)==1:
   ans = x[0][0]
 if not ans:
  ans = y + ending
 ans1 = sandhi_single(ans,False)
 if ans1:
  ans = ans1
 return ans

def aorist_join1_adjust_y(y,efirst,root,dbg=False):
 """
 """
 if (root == 'naS') and (efirst in init.consonant_set) and (not (efirst in init.semivowel_set)):
  #Kale 476. nash
  # 'n' is inserted before the ending consonant of 'nash' when
  # it is followed by any consonant except a nasal or a semi-vowel.
  # NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
  y = y[0:-1] + 'M' + y[-1:]
 elif (root == 'masj') and (efirst in init.consonant_set) and (not (efirst in init.semivowel_set)):
  # Kale 476. masj
  # 'n' is inserted before the ending consonant and
  # the  's' is dropped when they are followed by any consonant
  # except a nasal or a semi-vowel. In particular this is applicable
  # to all the periphrastic future forms:  ma~Nktaa.
  # When the 'n' is not dropped, the 's' is changed to 'j': mamajja
  y = y[0:-2] + 'Y' + y[-1:]
 elif (root == 'jaB') and (efirst in init.vowel_set):
  # Kale p.320 footnote
  # 'jabh' inserts a nasal when its final is followed by a vowel
  y = y[0:-1] + 'm' + y[-1:]
 elif (root == 'raB') and (efirst in init.vowel_set):
  # Kale p.320 footnote
  # 'rabh' inserts a nasal when its final is followed by a vowel;
  # however, 'rabh' does not do it
  #  (a) in the Aorist
  #  (b) when it takes 'i', except in the Perfect
  # In this case, (luT lRiT lRi~N), the only way 'efirst' is a vowel
  # is if it is an 'i', presetn because seT-code is 'seT'. Since 
  # the tense is not Perfect, no nasal is inserted
  #(setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
  pass  # no change made to y
 return y

def aorist_join1_vowel(y,ending,root,dbg=False):
 """
 ; when 1st char of ending is a vowel
 ; returns 'nil' when not applicable
  Python returns None when not applicable
  Note:  In Elisp code,There are many completely irrelevant tests involving 
   aorist_sym, such as "if aorist_sym == 'luw'".  This is impossible as
   luw is Perfect.  These tests have been omitted .  With these omissions,
   there is no need to reference the aorist_sym global in this function
 """
 ans = None
 ylast = y[-1:]
 #if (ylast in 'iIf') and (aorist_sym == 'luw'): # test omitted
 if ylast in 'uUF':
  # 2nd special sandhi rule for future (Antoine2#110)
  y0 = y[0:-1]
  if ylast == 'u':
   ans = y0 + 'uv' + ending
  elif ylast == 'U':
   ans = y0 + 'uv' + ending
  if ylast == 'F':
   ans = y0 + 'ar' + ending
 return ans

def aorist_join1_t_th(y,ending,root,dbg=False):
 """ when 1st char of ending is 't' or 'T'
     returns None if not applicable
 """
 global aorist_sym
 efirst = ending[0]
 ylast = y[-1:]
 yfirst = first_cons(y)
 #y0 = y[0:-1]
 ny = len(y)
 if (2<=ny) and (y.endswith(('ar','Ar'))):
  # this rule so [ch a k a r] + [th a]
  # becomes [ch a k a r th a] rather than [ch a k a s th a], which
  # is what 'sandhi-pair' does
  ans = y + ending
 elif (2 < ny) and (y.endswith(('cC','Sc','rj','kz','sj','jj'))):
  # pracC, vraSc, mfj, akz
  y0 = y[0:-2]
  if y.endswith('rj'):
   y0 = y[0:-1]
  if y0.endswith('Y'):
   y0 = y0[0:-1] + 'N'
  if (efirst == 't'):
   efirst = 'w'
  else:
   efirst = 'W'
  ans = y0 + 'z' + efirst + ending[1:]
 elif ylast == 'S':
  #Kale p. 321. Example of klish
  y0 = y[0:-1]
  if (efirst == 't'):
   efirst = 'w'
  else:
   efirst = 'W'
  ans = y0 + 'z' + efirst + ending[1:]
 elif root in ['vah','sah']:
  # Kale #506. p. 317
  # When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
  # is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
  #  vavah + tha =
  #   uvah + tha =
  #   uvaDh + Dha (by #416-3,4) =
  #   uvaDh + Dha =
  #   uvoDha
  if (2 <= len(y)):
   ans = y[0:-2] + 'oQ' + ending[1:]
  else:
   # for 'tvA' of 'vah', y = 'U'
   ans = y + 'Q' + ending[1:]
 # next skipped, as it applies to 'luw' (perfect), not aorist
 #elif (root in ['muh','druh','snih','snuh']) and (efirst == 't') and (aorist_sym == 'luw'):
 elif (2 < ny) and (y.endswith(('ah','Ah'))):
  # 'dah' : dagdhaa (luT), adaagdham (aorist4 p. 349)
  # 'nah' : naddhaasmi (luT)
  if yfirst == 'd':
   ylast1 = 'g'
  elif yfirst == 'n':
   ylast1 = 'd'
  else:
   ylast1 = ylast
  if yfirst == 'g':
   # gAh
   ans = y[0:-1] + 'Q' + ending[1:]
  else:
   # dah
   ans = y[0:-1] + ylast1 + 'D' + ending[1:]
 elif (root in ['dih','duh']) and (aorist_sym == 'luN7'):
  # digDa Kale p. 346
  if yfirst == 'd':
   ylast1 = 'g'
  elif yfirst == 'n':
   ylast1 = 'd'
  else:
   ylast1 = ylast
  ans = y[0:-1] + ylast1 + 'D' + ending[1:]
 elif (root in ['lih','guh']) and (aorist_sym == 'luN7'):
  # digDa Kale p. 346
  y0 = y[0:-1]
  ylast = y0[-1:]
  y0 = y0[0:-1] + lengthen_vowel(ylast)
  ans = y0 + 'Q' + ending[1:]
 elif ylast == 'h':
  # Kale p. 322. Example of 'muh', 'druh', 'snih', 'snuh'
  y0 = y[0:-1]
  ans = y0 + 'Q' + ending[1:]
 elif ylast in 'jc':
  # this rule [bh a j] + [th a] -> [bh a k th a]
  # rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
  # but [bh a ~n j] + [th a] -> [bh a ~N k th a]
  if (2 < ny) and (y.endswith(('aj','Aj'))):
   # yaj, sfj (has been changed to sraj)
   y0 = y[0:-1]
   if efirst == 't':
    efirst = 'w'
   else:
    efirst = 'W'
   ans = y0 + 'z' + efirst + ending[1:]
  elif (2 < ny) and (y.endswith('Yj')):
   # BaYj, masj
   ans = y[0:-2] + 'Nk' + ending
  else:
   ans = y[0:-1] + 'k' + ending
 elif (ylast in 'DB'):
  # so [v i v y a dh] + [th a] -> [v i v y a d dh a]
  # sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
  y0 = y[0:-1]
  ans = y0 + de_aspirate(ylast) + 'D' + ending[1:]
 elif (ylast in 'mn'):
  # For 'gam', sandhi-pair gives 'jagaMtha', but
  # Kale and Antoine both show 'jagantha'
  # Similaraly, for 'han' we want 'jaghantha' rather than 'jagaMtha'
  ans = y[0:-1] + 'n' + ending
 else:
  ans = None
 return ans

def aorist_join1_s(y,ending,root):
 """ when 1st char of ending is 's'
     returns None if not applicable
 """
 efirst = ending[0]
 ylast = y[-1:]
 yfirst = first_cons(y)
 ny = len(y)
 if ylast in 'S':
  # Kale p. 321. based on example of 'aS'
  ans = y[0:-1] + 'kz' + ending[1:]
 elif ylast in 'Bb':
  # case of 'laB': Kale p. 301
  ans = y[0:-1] + 'p' + ending
 elif ylast in 'Dd':
  # case of 'vfD': Kale p. 301
  # case of 'banD': Kale p. 303 : previous 'b' gets the aspiration
  y0 = y[0:-1]
  if ylast == 'D':
   y0 = aspirate_first_cons(y0)
  ans = y0 + 't' + ending
 elif ylast == 'h':
  # Examples:
  # 'nah' : natsyaami
  # 'vah' : vakShyaami
  # 'muh' : mokShyaami
  # 'tRih' : tarkShyati
  # The following also aspirate the first consonant: 
  # 'dah' : dhakShyaami
  # 'duh' : dhokShyaami
  # 'guh' : ghokShyaami
  # 'gaah' : ghaakShyate
  # 'gRih' : gharkShyate
  y0 = y[0:-1]
  if yfirst in 'n':
   ans = y0 + 't' + ending
  elif yfirst in 'vmt':
   ans = y0 + 'kz' + ending[1:]
  elif yfirst in 'dg':
   ans = y0 + 'kz' + ending[1:]
   ans = aspirate_first_cons(ans)
  else:
   ans = y0 + 'kz' + ending[1:]
 elif ylast == 's':
  # Kale 480. 't' is substituted for the ending 's' of a root
  # when followed by any non-conjugational termination
  # beginning with 's'
  y0 = y[0:-1]
  ans = y0 + 't' + ending
 elif (2 < ny) and (y.endswith(('cC','Sc'))):
  # from example of 'prachCh' (Antoine2 p. 89)
  ans = y[0:-2] + 'kz' + ending[1:]
 elif ylast in 'jcz':
  # from example of 'sRij' (Antoine2 p. 89),
  # and of 'kRiSh'
  # takSh (p. 304)
  y0 = y[0:-1]
  if y0.endswith('Y'):
   y0 = y0[0:-1] + 'N'
  elif y0.endswith(('k','j','s')):
   y0 = y0[0:-1]  # drop the last penultimate letter
  ans = y0 + 'kz' + ending[1:]
 else:
  ans = None
 return ans
 
def aorist_join1_dh(y,ending,root,dbg=False):
 """ when 1st char of ending is 'D' (slp1, e.g. 'Dvam')
     returns None if not applicable
 """
 efirst = ending[0]
 ylast = y[-1:]
 yfirst = first_cons(y)
 ny = len(y)
 y0 = y[0:-1]
 if root in ['dih','duh']:
  ans = y0 + 'g' + ending
  ans = aspirate_first_cons(ans)
 elif root in ['lih','guh']:
  ylast = y0[-1:]
  y0 = y0[0:-1] + lengthen_vowel(ylast)
  ans = y0 + 'Q' + ending[1:]
  ans = aspirate_first_cons(ans)
 elif root in ['vah','sah']:
  # Kale #506. p. 317
  # When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
  # is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
  #  vavah + tha =
  #   uvah + tha =
  #   uvaDh + Dha (by #416-3,4) =
  #   uvaDh + Dha =
  #   uvoDha
  ans = y[0:-2] + 'oQ' + ending[1:]
 elif ylast in 'cj':
  # lu~N4. pach -> apagdhvam
  ans = y0 + 'g' + ending
 elif ylast == 'r':
  ans = y + 'Q' + ending[1:]
 elif ylast == 'h':
  y0 = aspirate_first_cons(y0)
  ans = y0 + 'Q' + ending[1:]
 else:
  ans = None
 return ans

def aorist_endings(tense_sym=None,dbg=False):
 """ Usually called with no parameters, so uses global aorist_sym
 """
 global aorist_sym,aorist_pada,aorist_passive_P
 err0 = "aorist_endings(%s)" % tense_sym
 if not tense_sym:  
  tense_sym = aorist_sym
 pada = aorist_pada
 conj_class = '1'
 if aorist_passive_P:
  apada = 'a'
 else:
  apada = pada
 endings = conj_endings(tense_sym,conj_class,apada)
 if aorist_passive_P:
  endings[0] = 'i' # this may be duplicated elsewhere
 return endings

def aorist_gunate_final_vowel(tok,vfdDiP=False,dbg=False):
 """
 """
 ans = tok
 m = len(tok)
 j = m
 while (0 < j):
  j = j - 1
  v = tok[j]
  if v in init.vowel_set:
   if vfdDiP:
    v1 = vfdDi1(v)
   else:
    v1 = guna(v)
   ans = tok[0:j] + v1 + tok[j+1:]
   break
 return ans
######################################################################## 
# ppfactn
def sl_ppfactn(root,theclass,evoice,dtype=None,dbg=False):
 """
  construct periphrastic action noun, if one is applicable
  May return None
 """
 if evoice == 'a':
  pada = 'p'
 else:
  pada = 'a'
 if not periphrastic_liw_P(root,theclass,dtype,dbg):
  return None
 ans1 = periphrastic_base(root,theclass,pada,dtype,dbg)
 if ans1:
  #print "check: root=%s,theclass=%s,pada=%s,ans1=%s" %(root,theclass,pada,ans1)
  pass
 ans1 = flatten(ans1)
 ans=[]
 for ans2 in ans1:
  ppfactn = ans2 + 'Am'
  ans.append(ppfactn)
 #ans.reverse()
 return ans

######################################################################## 

######################################################################## 
# causal
#######################################################################
""" causal.py
    June 23, 2016  Conversion of causal.el in ElispSanskrit
; causal.el  
; begun 08-17-03
; Based upon Kale, p.368 ff

(defun causal-doc-600 ()
 "Kale 600.
   Any root belonging to one of the Conjugational classes
   may have a causal form, which is conjugated like a root of
   the 10th class.
 "
)
(defun causal-doc-601 ()
 "Kale 601.
   The Causal of a root implies that a person or a thing causes
   or makes another person or thing to perform the action, or to
   be in the condition, denoted by the root.  It is also employed,
   sometimes, to convert an intransitive verb into a transitive one.
 "
)
(defun causal-doc-602 ()
 "Kale 602. 
   The Causal Base of a root is formed like that of a root of
   the 10th class.  In the case of roots of the 10th class,
   the causal form is identical with the primitive.  The
   Causal form takes either pada. For example: 
   'budh' has causal base 'bodhay': 'bodhayati, -te': he causes to know
   'kShubh' -> 'kShobhayati' : he shakes or agitates
   'gaN' -> 'gaNayati' : he causes to count
   'nii' -> 'naayayati' : he makes another lead or carry
   'kRi' -> 'kaarayati' : he causes to do
   'kRI' -> 'kaarayati' : he causes to scatter
   'kRIt' -> 'kiirtayati' : he causes to glorify
   Note 1 : the function 'dhaatu-a~Nga-10' in 'gram2.el' does this
            except for the roots in Section 400.
   Note 2 : The function 'class10-base' (in causal.el) includes
            the exceptions in Section 400. 
   Note 3 : They are many cases where this basic method is modified.
            This is detailed in the subsequent sections.
   Note 4 : Class 10 roots are handled by 'class10-base': this
     logic takes precedence over the other categories mentioned in
     subsequent sections regarding the causal.
 "
)
(defun causal-doc-603 ()
 "Kale 603.
  For several roots, the vowel takes its guna substitute:
   1. roots ending in 'am', except
    a. 'am' to go (class = 1) has causal base 'aamay'
       and the root 'am' to be ill (class = 10) has causal base 'aamay',
       since it is class 10
    b.  'kam' (to love)
    c. 'cham' (to eat)
    d. 'sham' when it means 'to see' (class = 10)
    e. 'yam' when it does not mean 'to eat' (Note: this is odd,
       I cannot find 'eat' as a definition for 'yam'.
   2. roots marked with an indicatory 'm' 
     NOTE: In some cases, the choice of 'a' or 'aa' form depends on
     the intended meaning of the causal.  The code as written notes
     such cases by call (indicatory-m1-P dhaatu). Thus, the distinction
     is lost by this code (the 'meaning' is unused).
   3. Six roots with short 'a' have an optional root lengthening
      when not used with a preposition; with a preposition, only
      the short-a form is used.
 "
)
(defun causal-doc-604 ()
 "Kale 604.
  Roots ending in 'aa' insert a 'p' before the [a y].
  Roots that often change to 'aa' (kale-459-P dhaatu class pada )
   do so here, and then insert a 'p' before the [a y].
  For the following roots, the vowel is gunated, then they also
   insert a 'p' before the [a y]
    'ri' (to go)
    'hrii' (to be ashamed)
    'rii'  (to go, to flow)
    'vlii' (to go , to choose)
  
 "
)
(defun causal-doc-605 ()
 "Kale 605.
  a. The following roots insert 'p' after changing their final vowel to 'aa':
   'mi' (throw) , 'mii' (destroy), 'dii' (perish), 'ji' (conquer),
   'krii' (buy)
  b. The following roots shorten their vowel (to 'a') and insert 'p' when
   not preceded by a preposition:
    'kShai' (wane) , 'shraa'(cook), 'shrai' ( cook)
    'j~naa' (to slay = causal meaning)
   NOTE: by #603, j~naa can have to forms (j~naapay, j~napay).
   Thus, the code excludes j~naa from this list
  c. The following roots have an optional form in which the vowel
     is shortened and 'p' added;  the other form changes the final
     vowel to 'aa' and adds 'p':
     'glai' (be weary), 'snaa' (bathe)
 "
)
(defun causal-doc-606 ()
 "Kale 606.
  a. The following insert 'y' instead of 'p' after changing the
     final vowel to 'aa'
     sho (pare, sharpen)  Cho (cut) so (finish)
     hve (call)  vye (cover) ve (weave)
     sai (waste away)
     paa (drink : class = 1)
    b. paa (protect : class = 2) inserts 'l'
     ve (to shake) inserts 'j' after changing vowel to 'aa'
    NOTE: 'shake' is not a meaning of 've' (acc. to Apte).
     Since 've' appears in both parts, I return both forms
 "
)
(defun causal-doc-607 ()
 "Kale 607.
  The roots 'jabh' , 'radh', 'rabh', and 'labh' 
  insert a nasal before their final consonant
 "
)
(defun causal-doc-608 ()
 "Kale 608. The following roots have two forms,
   the first is that constructed by 'class10-base',
   the second ends in [aa y] instead of [a y]
     gup vichCh dhuup paN pan Rit
 "
)
(defun causal-doc-609 ()
 "Kale 609.
   The following roots drop their final vowel before 'ay':
    diidhii vevii daridraa
 "
)
(defun causal-doc-610 ()
 "Kale 610.
   A number of roots form their Causal base anomalously.
   The function 'kale-610' has the details.
 "
)
(defun causal-doc-611 ()
 "Kale 601.
   
 "
)
"""
from test2 import *

def causal_conjtab1a(root,theclass,pada,upasargas,tense,voice=None,dbg=False):
 """  from construct.el
 """
 err0 = "causal_conjtab1a(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 if dbg:
  print err0
 if tense in ['law','laN','low','viDiliN']:
  return causal_conjtab1a_spcltense(root,theclass,pada,upasargas,tense,voice,dbg)
 santensesd = sl_all_tense_dict()
 santensekeys=santensesd.keys()
 santenses = [santensesd[k] for k in santensekeys]
 if tense in santenses:
  # other known tenses
  return causal_conjtab1a_gentense(root,theclass,pada,upasargas,tense,voice,dbg)
 raise NameError(err0 + (' Unknown tense=%s'%tense))

def causal_conjtab1a_spcltense(root,theclass,pada,upasargas,tense,voice=None,dbg=False):
 """  from construct.el
 """
 err0 = "causal_conjtab1a_spcltense(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 nilpada=''
 bases = causal_base1b(root,theclass,nilpada,upasargas,voice,dbg=dbg)
 causalclass='10'
 ans=[]
 for b in bases:
  ctab = conjugation_tab(b,tense,causalclass,pada,root,voice)
  ans.append(ctab)
 ans1 = join_arrays_many(ans)
 return ans1

def causal_conjtab1a_bases (root,theclass,pada,upasargas,tense,voice=None,dbg=False):
 """  from construct.el
 """
 err0 = "causal_conjtab1a_bases(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 bases = causal_conjtab1a_bases_a(root,theclass,pada,upasargas,tense,voice,dbg=dbg)
 return bases

def causal_conjtab1a_gentense(root,theclass,pada,upasargas,tense,voice=None,dbg=False):
 """  from construct.el
 """
 err0 = "causal_conjtab1a_gentense(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 if dbg:
  print err0
 if not (voice in ['active','passive']):
  voice = 'active'
 lc = tense[-1:] 
 if lc in '1234567':
  lcnum = int(lc)
 else:
  lcnum = 0
 bases = causal_bases_gentense(root,theclass,pada,upasargas,tense,voice,dbg=dbg)
 if dbg:
  print err0,"bases = ",bases
 if tense in ['liw-p','luw','lfw','lfN','ASIrliN','luN3']:
  causalclass = '11'
  ctabs = []
  for base in bases:
   if (tense == 'liw-p'): # pft = periphrastic perfect
    ctab = conjugation_tab_liw_p(upasargas,causalclass,pada,base,voice,dbg)
   elif (tense == 'luw'): # pft = periphrastic future
    ctab = conjugation_tab_luw(upasargas,causalclass,pada,base,voice,dbg)
   elif (tense == 'lfw'): # fut = simiple future
    ctab = conjugation_tab_lfw(upasargas,causalclass,pada,base,voice,dbg)
   elif (tense == 'lfN'): # con = conditional
    ctab = conjugation_tab_lfN(upasargas,causalclass,pada,base,voice,dbg)
   elif (tense == 'ASIrliN'): # ben = benedictive
    ctab = conjugation_tab_ASIrliN(upasargas,causalclass,pada,base,voice,dbg)
   elif tense == 'luN3':
    # Note 'root' is used here
    ctab = conjugation_tab_aorist(upasargas,causalclass,pada,root,lcnum,voice)
   if dbg:
    print "base=%s => ctab=%s\n" %(base,ctab)
   ctabs.append(ctab)
  #print "causal_conjtab1a_gentense: ctabs=\n",ctabs
  ans = join_arrays_many(ctabs)
  return ans
 else:
  err = err0+ "ERROR 3"
  raise NameError(err)

def causal_conjtab1a_bases_a(root,theclass,pada,upasargas,tense,voice=None,dbg=False):
 """  from construct.el
 """
 err0 = "causal_conjtab1a_bases_a(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 if tense in ['law','laN','low','viDiliN']:
  bases = causal_base1b(root,theclass,pada,upasargas,voice,dbg=dbg)
 else:
  bases = causal_bases_gentense(root,theclass,pada,upasargas,voice,dbg=dbg)
 if tense in ['laN','lfN']:
  # use prefix 'a' for those tenses that still require it
  #  Note the 'luN3' already has it.
  bases = map(lambda x: 'a'+x,bases)
 return bases


def causal_bases_gentense(root,theclass,pada,upasargas,tense,voice,dbg=False):
 """  from construct.el
 """
 err0 = "causal_bases_gentense(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,tense,voice)
 if dbg:
  print err0
 causalclass='11'
 if not (voice in ['active','passive']):
  voice = 'active'
 lc = tense[-1:] 
 if lc in '1234567':
  lcnum = int(lc)
 else:
  lcnum = 0
 lc = root[-1:]
 nilpada = None
 if (lc in init.vowel_set) and (tense == 'luN3'):
  # raise an error. Logic is known to be faulty
  # In Python version, I skip this. and compute anyway
  bases=[]
 elif (voice == 'active') and (tense == 'luN3'):
  bases0 = aorist_causal_base(root,theclass,pada,upasargas,None)
  bases = map(lambda x: 'a'+x,bases0)
 elif (voice == 'passive') and (tense == 'luN3'):
  ctab = conjugation_tab_aorist(upasargas,causalclass,pada,root,'3',voice,dbg=dbg)
  ctab = solution(ctab) # June 26, 2016
  ctab3s = ctab[0]  # 3rd singular
  tok = ctab3s[0:-1] # drop the final 'i' of passive
  bases = [tok]
  #print "causal_bases_gentense: bases = ",bases
 elif (voice == 'active'):
  bases = causal_base1b(root,theclass,nilpada,upasargas,voice,dbg=dbg)
 elif (voice == 'passive'):
  if tense in ['liw-p','luw','lfw','lfN','ASIrliN']:
   voice1 = 'active'
   bases =  causal_base1b(root,theclass,nilpada,upasargas,voice1,dbg=dbg)
  else:
   bases = causal_base1b(root,theclass,nilpada,upasargas,voice,dbg=dbg)
 # In some Kale shows an alternate form with the causal 'ay' dropped
 #if dbg:
 #  print " from causal-base1b, bases=%s\n" % bases
 if (voice == 'passive'):
  if tense in ['luw','lfw','lfN','ASIrliN']:
   bases1 = bases
   bases = []
   for base in bases1:
    bases = append_if_new(bases,base)
    bases = append_if_new(bases,base[0:-2])
 elif (voice == 'active'):
  if (tense == 'ASIrliN') and (pada == 'p'):
   bases1 = bases
   bases = []
   for base in bases1:
    tok = base[0:-2]
    if tok not in bases:
     bases.append(tok)
 #print "causal_bases_gentense: returns bases=",bases
 return bases

def causal_base1b(root,theclass,pada,upasargas,voice,dbg=False):
 """  from construct.el
 """
 err0 = "causal_base1b(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,voice)
 bases = causal_base(root,theclass,pada,upasargas)
 if voice == 'passive':
  bases = map(lambda x: x[0:-2] + 'y',bases)
 return bases

def causal_base1a(root,theclass=None,pada=None,upasargas=None,eng_def=None,dbg=False):
 """ In Python, same as causal_base
 """
 return causal_base(root,theclass,pada,upasargas,eng_def,dbg)


def causal_base(root,theclass=None,pada=None,upasargas=None,eng_def=None,dbg=False):
 """
 ; returns a list of token arrays
 ; 'pada' is unused.
 ; 'Eng-def' is used via 'class10-base'
 """
 tok = root
 ans1 = kale_610(root,theclass)
 if ans1:
  ans = ans1
 elif (not upasargas) and (root in ['vam','nam','van','jval','hval','hmal']):
  # Kale 603 (a)
  b = gunate_final_vowel(tok) + 'ay'
  b1 = class10_base(tok,eng_def) # a list
  ans = [b]+b1  # concatenate lists
 elif root in ['mi','mI','dI','ji','krI']:
  # Kale 605a
  ans = tok[0:-1] + 'Apay'
 elif (not upasargas) and (root in ['kzE','SrA','SrE']):  # jYA (?)
  # Kale 605b
  ans = tok[0:-1] + 'apay'
 elif (not upasargas) and (root in ['glE','snA']):
  # Kale 605c
  b = tok[0:-1] + 'apay'
  b1 = tok[0:-1] + 'Apay'
  ans = [b1,b]
 elif root == 've':
  # Kale 606a,b
  b = tok[0:-1] + 'Ayay'
  b1 = tok[0:-1] + 'Ajay'
  ans = [b,b1]
 elif (root in ['So','Co','so','hve','vye','sE']) or\
      ([root,theclass] == ['pA','1']):
  # Kale 606a
  ans = tok[0:-1] + 'Ayay'
 elif [root,theclass] == ['pA','2']:
  # Kale 606b
  ans = tok[0:-1] + 'Alay'
 elif root == 'raD':
  ans = tok[0:-2] + 'n' + tok[-1:] + 'ay'
 elif root in ['jaB','raB','laB']:
  ans = tok[0:-2] + 'm' + tok[-1:] + 'ay'
 elif root in ['gup','vicC','DUp','paR','pan','ft']:
  b = class10_base(tok) # a list with 1 elt.
  b = solution(b) # that elt.
  b1 = b[0:-2] + 'Ay'
  ans = [b,b1]
 elif causal_603_P(tok,root):
  ans = gunate_final_vowel(tok) + 'ay'
 elif indicatory_m1_P(root):
  b = gunate_final_vowel(tok) + 'ay'
  b1 = class10_base(tok,eng_def)
  ans = [b] + b1
 elif root in ['dIDI','vevI','daridrA']:
  # Kale 609 : To get 'daridraa' right, this precedes next 604
  ans = tok[0:-1] + 'ay'
 elif kale_459_P(root,theclass) or (tok.endswith('A')):
  # Kale 604
  ans = tok[0:-1] + 'Apay'
 elif root in ['f','hrI','rI','vlI']:
  # Kale 604
  ans = gunate_final_vowel(tok) + 'pay'
 elif root in ['mi','mI','dI','ji','krI']:
  # Kale 605a
  ans = tok[0:-1] + 'Apay'
 elif theclass == '10':
  ans = class10_base(tok,eng_def)
 else:
  # the general case
  ans = class10_base(tok,eng_def)
  if root == 'prI':
   # Whitney
   ans = ans + ['pUray']
 #
 if not isinstance(ans,list):
  ans = [ans]
 b = ans
 ans = []
 for b1 in b:
  b2 = causal_adjust(b1,root,theclass,pada,upasargas,eng_def)
  ans.append(b2)
 return ans

def causal_adjust(b,root,theclass,pada,upasargas,eng_def,dbg=False):
 """
 ; b is a token string ending in [a y]. If the previous
 ; letter is 'aa' or 'a', a 'p' is inserted
 ; experience shows that 'b' may be a singleton list of strings
 """
 b = solution(b)
 try:
  if b[-3:-2] in 'Aa':
   ans = b[0:-2] + 'p' + b[-2:]
  else:
   ans = b
 except:
  err0="causal_adjust(%s,%s,%s,%s,%s,%s)" %(b,root,theclass,pada,upasargas,eng_def)
  print err0,"ERROR",b[-3:-2]
  exit(1)
 return ans

def kale_610(root,theclass=None,upasargas=None,dbg=False):
 """
 "Anomalous causals. The anomalous base(s) is returned,
 either as a string, or a list of strings.
 'None' is returned when 'dhaatu', etc are not considered by this function
 "
 """
 # Represent the cases involving only the root as a Python dictionary
 # for root 'i':
 # adhi-i adhi-aapay
 # prati-i praty-aayay
 # Kale shows [g a m a y] as default causal base ;
 #  but since Whitney, MW do not, I do not
 d = {'i':['Apay','Ayay'],
      'knU':'knopay', 'knUy':'knopay', # cause to sound
      'kzmAy':'kzmApay', # cause to tremble
      'gUh':'gUhay', #cause to conceal
      'ci':['cApay','cAyay',  # class=5
            'capay','cayay'], # class=10
      'duz':['dUzay','dozay'], # cause to sin, corrupt or make depraved
      'DU':'DUnay', # cause to shake
      'prI':'prIRay', #cause to please
      'BA':['BAyay', # frightens with
            'BApay','BIzay'], # inspires fear
      'Brasj':['Barjay','Brajjay'], #cause to fry
      'mfj':'mArjay', # cause to wipe
      'raYj':['raYjay', # dyes or paints, propitiates or satisfies
              'rajay'], # hunts deer
      'ruh':['rohay','ropay'], # plant, cause to grow
      'lA':['lAlay','lApay'], # embrace, melt
      'li':['lInay','lApay'], # embrace, melt
      'vA':['vApay', # cause to blow or move
            'vAjay'],# shake
      'smi':['smAyay','smApay'],
      'vI':['vApay','vAyay'],
      'Sad':['SAtay', # cause to fall, cut down
             'SAday'], # cause to go
      'siD':['sADay', #accomplish or prepare
             'seDay'], # makes perfect a sacred rite
      'sPAy':'sPAvay', # cause to swell
      'sPur':['sPoray','sPAray'], # make tremble
      'han':'GAtay', # cause to strike
      'pad':'pAday', # cause to go
      'pat':['pAtay','patay'], # ref Whitney
     }
 if root in d:
  return d[root]
 else:
  return None
 # In Elisp code, as in this Python code, the next lines are never
 # reached.
 if (root == 'ci') and (theclass == '5'):
  return ['cApay','cAyay']
 elif (root == 'ci') and (theclass == '10'):
  return['capay','cayay']
 return None

def causal_603_P(tok,root,dbg=False):
 """ returns a boolean
 """
 if root in ['am','kam','cam','Sam']:
  # 603 not applicable. Sam may need class=10
  return False
 elif indicatory_m_P(root):
  return True
 elif (2 < len(tok)) and tok.endswith('am'):
  return True
 else:
  return False

def indicatory_m_P(root):
 return indicatory_m_P_1(root)

def indicatory_m1_P(root):
 return (root in ['Cad','jYA','Sam','yam'])

def indicatory_m_P_1(root):
 roots = ["Gaw", "vyaT", "praT", "pras", "mrad", "svad", "kzaYj", "dakz",
          "krand", "kland", "tvar", "jvar", "gaq", "heq", "vaw", "Baw", "naw",
          "stak", "cak", "kaK", "rag", "lag", "hrag", "lag", "sag", "stag",
          "kag", "ak", "ag", "kaR", "raR", "caR", "SaR", "SraR", "SraT",
          "knaT", "kraT", "klaT", "can", "van", "jval", "hval", "hmal", "smf",
          "dF", "nF", "SrA", "cal", "laq", "mad", "Dvan", "svan", "jan", "jF",
          "knas", "raYj", "ram", "gam", "PaR", "kram"]
 return (root in roots)

def indicatory_m_P_2(root,theclass,pada,upasargas,eng_def,dbg=False):
 definition = modify_eng_def(eng_def)
 if (root == 'Cad') and ('live' in definition):
  return True
 elif (root == 'jYA') and ('kill' in definition):
  return True
 else:
  return False

def kale_400_P(root):
 roots = ["aG", "kaT", "kzap", "gaR", "var", "Dvan", "mah", "rac", "ras",
          "rah", "stan", "svar", "pat", "pad", "vaw", "karR", "cap", "SraT",
          "vyay", "spfh", "mfg", "guR", "kuh", "sPuw", "suK", "SaW", "paw",
          "kal", "laj", "vas", "puw", "SaW", "paw", "kal", "laj", "vas",
          "puw", "aMs", "kfp"]
 return (root in roots)

# ----------------------------------------------------------------------------
# PARTICIPLES
# ----------------------------------------------------------------------------

def construct_ippart1a_tvA(root,theclass,pada,upasargas,dtype=None,dbg=False):
 """
 """
 err0 = "construct_ippart1a_tvA(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,dtype)
 #if root == 'dam':
 # dbg=True
 if dbg:
  print err0
 ending = 'tvA'
 lc = root[-1:]
 pc = root[-2:-1]
 fc = root[0:1]
 sew_tvA = tvA_sew_code(root,theclass,pada,upasargas,dbg=dbg)
 if dbg:
  print "chk: sew_tvA = ",sew_tvA
 if theclass == '10':
  bases = class10_base(root)
  if not isinstance(bases,list):
   bases = [bases]
  bases = flatten(bases)
  # each base ends in 'ay', which is keyp. 'i' is inserted
  #print err0,"bases=",bases
  ans = map(lambda b: b + 'i' + ending, bases)
 elif dtype == 'c': # causal
  # law is present tense
  bases = causal_conjtab1a_bases(root,theclass,pada,upasargas,'law',dbg=dbg)
  if not isinstance(bases,list):
   bases = [bases]
  bases = flatten(bases)
  # each base ends in 'ay', which is keyp. 'i' is inserted
  ans = map(lambda b: b + 'i' + ending, bases)
 elif sew_tvA == 'sew':
  if root == 'ku':
   # satisfies kale-463-P, but is gunated
   bases = [gunate_final_vowel(root)]
  elif (root in ['mfq','mfd','guD','kuz','muz']) or\
       (kale_463_P(root)) or\
       ([root,theclass]==['vij','7']):
   # Kale 746(b)
   bases = [root]
  elif root in ['tfz','mfz','kfz','ft']:
   # Kale 746(a)
   bases = [root,gunate_final_vowel(root)]
  else:
   # Kale 746
   b = gunate_final_vowel(root)
   bases = [b]
   if (fc in init.consonant_set) and (pc in 'iu') and (not (lc in 'yv')):
    # Kale 750
    bases = [root,b]
  ending = 'i' + ending
  ans = map(lambda b: conjugation_join(b,ending),bases)
 elif sew_tvA == 'vew':
  # the first part repeats all but the 'ku' clause of the 'sew' case above
  if (root in ['mfq','mfd','guD','kuz','muz']) or\
       (kale_463_P(root)) or\
       ([root,theclass]==['vij','7']):
   # Kale 746(b)
   bases = [root]
  elif root in ['tfz','mfz','kfz','ft']:
   # Kale 746(a)
   bases = [root,gunate_final_vowel(root)]
  else:
   # Kale 746
   b = gunate_final_vowel(root)
   bases = [b]
   if (fc in init.consonant_set) and (pc in 'iu') and (not (lc in 'yv')):
    # Kale 750
    bases = [root,b]
  ending = 'i' + ending
  ans = map(lambda b: conjugation_join(b,ending),bases)
  # The 'vew' case also has a form without 'i'. Use method based on kta
  ans2 = construct_ippart1a_tvA_basic (root,theclass,pada,upasargas,dbg=dbg)
  if dbg:
   print "chk: bases=",bases," + ending=",ending," -> ans=",ans,"ans2=",ans2
  if not isinstance(ans2,list):
   ans2 = [ans2]
  for a in ans2:
   if a not in ans:
    ans.append(a)
 else:
  ans = construct_ippart1a_tvA_basic (root,theclass,pada,upasargas,dbg=dbg)
 if not dtype:
  # exceptions
  ans1 = construct_ippart1a_tvA_exception(root,theclass,pada,upasargas,dbg=False)
  if ans1:
   ans = ans1
 if dbg:
  print err0,"returns ans=",ans," solution(ans)=",solution(ans)
 ans = solution(ans)
 
 return ans

def construct_ippart1a_tvA_basic(root,theclass,pada,upasargas,dbg=False):
 """
 """
 err0="construct_ippart1a_tvA_basic(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 ktas = construct_pppart1a(root,theclass,pada,upasargas,dbg=dbg)
 if not isinstance(ktas,list):
  ktas=[ktas]
 ktas = flatten(ktas)
 ans = []
 for kta in ktas:
  ending = 'tvA'
  tok = kta
  pc = tok[-2:-1]
  if pc in 'DtQ':
   base = tok[0:-1]
   ending = ending[1:]
   thisans = base + ending
  elif tok[-3:] in ['nna','RRa']:
   base = tok[0:-3]
   thisans = tvA_join1_t_th(base,ending,root)
  else:
   base = tok[0:-2]
   thisans = tvA_join1_t_th(base,ending,root)
  if dbg:
   print "chk: kta=",kta,"thisans=",thisans
  if thisans not in ans:
   ans.append(thisans)
 ans = solution(ans)
 return ans

def  tvA_sew_code(root,theclass,pada,upasargas,dbg=False):
 """
 """
 err0 = "tvA_sew_code(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 lc = root[-1:]
 sew_code = construct_sew_code1a(root,theclass,pada,upasargas,dbg=dbg)
 sew_kta = kta_sew_code(root,theclass,pada,upasargas,dbg=dbg)
 if dbg:
  print err0," sew_code=",sew_code," sew_kta=",sew_kta
 if (root == 'traSc'):
  ans = 'sew'
 elif (root == 'svf'):
  ans = 'aniw'
 elif (root in ['mfz','kfz','kuz']):
  ans = 'sew'  # see Kale 746(a,b)
 elif root == 'kzuD':
  ans = 'vew'  # example p. 441
 elif root == 'ku':
  ans = 'sew'  # Kale 746
 elif [root,theclass] == ['vij','7']:
  ans = 'sew'  # Kale 746
 elif [root,theclass] == ['pU','7']:
  ans = 'sew'  # Kale 746
 elif (sew_code == 'vew'):
  ans = 'vew'
 elif root in ['iz','sah','luB','riz','ruz']:
  ans = 'vew'
 elif indicatory_u_P(root,theclass,pada):
  ans = 'vew'
 elif root in ['Svi','qI','SI','pU','jF']:
  ans = 'sew'
 elif (sew_code == 'sew') and (lc in init.consonant_set):
  ans = 'sew'
 elif theclass in ['10','11']: # 11 == causal
  ans = 'sew'
 else:
  ans = sew_kta
 return ans

def indicatory_u_P(root,theclass,pada,dbg=False):
 """
 ; Kale p. 442 footnote. 
 ; The following are the more important of the roots
 ; marked with 'u'. 
 """
 err0 = "indicatory_u_P(%s,%s,%s)" %(root,theclass,pada)
 if dbg:
  print err0
 roots = [
  "ac", "aYc", 
  "fR", "kam", "kuj", 
  "kam", "klam", "kzaR", "kziR", "kziv", 
  "kzIv", "kzed", "Kan", "gfD", "gras", 
  "gruc", "gluc", "gluYc", "GfR", "Gfz", 
  "caYc", "cam", "Cfd", "jaB", "jas", 
  "taYc", "tan", "tfR", "damB", 
  "div", 
  "DAv", "DvaMs", "BraMs", "man", "mfz", 
  "mruc", "mruYc", "mluc", "mluYc", "yas", 
  "yup", "rup", "lup", 
  "van", "vas", 
  "viz", "vft", "vfD", "vfz", "Sam", 
  "Sas", "SaMs", "SAs", "SfD", "SramB", 
  "Sram", "Sriz", "Sliz", "san", "sDi", 
  "zWiv", "skamB", "stamB", "svam", "sraMs", 
  "sriv", "hfz"]
 if root in roots:
  return True
 if preserve_elisp_errors:
  if root == 'dam':
   return True # case dam, class 9 returns True in Elisp.
 if [root,theclass] in [["as","4"],["dam","4"],["vaYc","1"]]:
  return True
 return False

def construct_ippart1a_tvA_exception(root,theclass,pada,upasargas,dbg=False):
 """
 """
 err0 = "construct_ippart1a_tvA_exception(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 if [root,pada] == ['hA','p']:
  ans = 'hitvA'
 elif root == 'ad':
  ans = 'jagDvA'
 elif  [root,theclass] == ['vas','1']:
  ans = 'uzitvA'
 elif  [root,theclass] == ['vas','2']:
  ans = 'vasitvA'
 elif  [root,theclass] == ['vas','4']:
  ans = ['vasitvA','vastvA']
 elif  [root,theclass] == ['vas','10']:
  ans = 'vasayitvA'
 elif root == 'mfj':
  ans = ['mArjitvA','mfzwvA']
 elif root == 'pU':
  ans = ['pavitvA','pUtvA']
 elif root == 'guh':
  ans = ['guhitvA','gUhitvA','gUQvA']
 elif  [root,theclass] == ['gup','10']:
  ans = ['gopAyitvA','gopitvA']
 elif  [root,theclass] == ['gup','1']:
  ans = ['gupitvA','guptvA']
 elif root == 'aYc':
  ans = ['aYcitvA','aktvA']
 elif root == 'div':
  ans = ['devitvA','dyUtvA']
 elif root == 'kram':
  ans = ['kramitvA','krantvA','krAntvA']
 elif root == 'Sam':
  ans = ['SamitvA','SAntvA']
 elif root == 'Cid':
  ans = 'CittvA'
 elif root == 'grah':
  ans = 'gfhItvA'
 else:
  ans = None
 return ans
 raise NameError(err0 + " NOT IMPLEMENTED")

def tvA_join1_t_th(y,ending,root,dbg=False):
 """
 """
 err0 = "tvA_join1_t_th(%s,%s,%ss)" %(y,ending,root)
 if dbg:
  print err0
 ylast = y[-1:]
 yfirst = y[0:1]
 pc = y[-2:-1]
 if ylast == 'r':
  ans = y + ending
 else:
  ans = kta_join1(y,'aniw',ending,root)
 return ans

def construct_pppart1a(root,theclass,pada,upasargas,dtype=None,dbg=False):
 """
 """
 err0 = "construct_pppart1a(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,dtype)
 if dbg:
  print err0
 ending = 'ta'
 lc = root[-1:]
 pc = root[-2:-1]
 if (kale_414_P(root)) or (root == 'jyA') or\
    (kale_692_P(root,theclass,pada,upasargas)) or (root=='tF'):
  # Kale 690. The roots given at #414 (9th class roots ending in long vowel)
  # and the root 'jyaa' substitute 'na' for 'ta'.
  if (root != 'pU'):
   ending = 'na'
 if (lc in 'AeEo') and (root[0] in init.consonant_set) and\
    (root[1:2]  in init.semivowel_set) and\
    (not (root in ['KyA','DyE','vye','hve'])):
  # Kale 689. Roots ending in 'aa' (or 'e ai o' changeable to 'aa')
  # and beginning with a conjunct consonant containing a semi-vowel
  # substitute 'na' for 'ta'
  # Exceptions are 
  #  'khyaa' (to name) etc,
  #  'dhyai' (to contemplate)
  #  'vye' and 'hve'
  ending = 'na'
 sew_kta = kta_sew_code(root,theclass,pada,upasargas,dtype,dbg=dbg)
 base = construct_pppart1a_helper(root,theclass,pada,upasargas,sew_kta,dtype,dbg=dbg)
 ans = kta_join(base,sew_kta,ending,root)
 if dbg:
  print "chk: base=%s, sew_kta=%s, ending=%s, root=%s => ans=%s" %(base,sew_kta,ending,root,ans)
 if (not (dtype in ['c'])):
  # exceptions
  ans1 = construct_pppart1a_exception(root,theclass,pada,upasargas)
  if ans1:
   ans = ans1
 ans = solution(ans)
 return ans

def kale_692_P(root,theclass,pada=None,upasargas=None):
 """
  ; Kale 692. The following roots substitute 'na' for 'ta'
  ; in the past passive participle
 """
 if root in [
  "QI", #  4A to fly
  "dU", #  torment
  "DI", #  hold, accomplish
  "lI", #  4A melt
  "mI", #  4A give pain
  "dI", #  perish, waste
  "rI", #  hurt
  "hA", #  go (3A) hAna# abandon (3P) hIna
  "vij", 
  "vraSc", #
  "sPurj", # 
  "vE", #  to dry, be languid 'vAna'
  "vrI", #  4A move, cover
  "BaYj", #  break 'bhagna'
  "masj", #  6P bathe, sink 'magna'
  "lasj", #  1A be-ashamed 'lagna'
  "skand", #  with 'vi': 'viskanna',  with 'pari': pariskanna, pariShkaNNa
  ]:
  return True
 if [root,theclass] in [
  ["suu","4"], #","4A bring-forth, produce.  Note 2A has 'suuta'
  ["bhuj","6"], #","6P bend 'bhugna'
  ["ruj","6"], # break 'rugna'
  ["laj","6"], # be-ashamed 'lagna'
  ["vid","4"], # vinna  
  ]: 
  return True
 return False

def kta_sew_code(root,theclass,pada,upasargas,dtype=None,dbg=False):
 """
 """
 err0 = "kta_sew_code(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,dtype)
 if dbg:
  print err0
 sew_code = construct_sew_code1a(root,theclass,pada,upasargas,dtype,dbg=dbg)
 lc = root[-1:]
 if dtype == 'c':
  # ; don't apply special cases to causal. 
  ans = sew_code
 elif root in ['riz','ruz']:
  ans = 'aniw' # riSh dhaatukosha
 elif root == 'luB':
  if theclass in ['1','4']:
   ans = 'aniw'
  elif theclass == '6':
   ans = 'sew'
  else:
   ans = 'vew'
 elif [root,theclass] == ['vas','1']:
  ans = 'sew'
 elif root == 'vas':  # redundant?
  ans = 'sew'
 elif [root,theclass] == ['kzuB','1']: 
  ans = 'sew'
 elif root == 'kzuB':
  ans = 'vew' # classes 4,9
 elif [root,theclass] == ['puz','4']: 
  ans = 'vew'
 elif root == 'puz':
  ans = 'aniw' # classes 4,9
 elif [root,theclass] == ['vid','2']:
  ans = 'sew'
 elif [root,theclass] == ['vid','4']: 
  ans = 'aniw'
 elif [root,theclass] == ['vid','7']: 
  ans = 'aniw'
 elif [root,theclass] == ['vid','6']: 
  ans = 'vew'
 elif [root,theclass] == ['Guz','1']:
  ans = 'aniw'
 elif [root,theclass] == ['tfh','7']:
  ans = 'sew'
 elif root == 'aS':
  if theclass == '5':
   ans = 'aniw'
  else:
   ans = 'sew'
 elif root in ['kzuD','svid','kuz']:
  ans = 'sew'
 elif [root,theclass] == ['GUz','1']:
  ans = 'aniw'
 elif root in [
   "cit",   "vfD",   "vft",   "DvaMs",   "sraMs",
   "vfz",   "SaMs",   "pfc",   "vfj",   "as",
   "iz",   "fD",   "kfS",   "klam",   "dIp",
   "BraMS",   "mad",   "tras",   "damB",   "Dfz",
   "rAD",   "stamB",   "juz",   "aYj",   "inD",
   "kzaR",   "kziR",   "tan",   "van",   "kam",
   "Kan",   "kram",   "cam",   "Gfz",   "yat",
   "vam",   "jan",   "Sram",   "gras",   "dam",
   "akz"]:
  ans = 'aniw'
 elif root in [
   "Svas",   "kliS",   "pU",   "kzuB",   "puz",
   "dam",   "Sam",   "pur",   "das",   "spaS",
   "Cad",   "Sap",   "ruz",   "am"]:
  ans = 'vew'
 elif [root,theclass,upasargas] == ['Guz','1',['sam']]:
  ans = 'vew'
 elif [root,upasargas] == ['svan',['A']]:
  ans = 'vew'
 elif (root == 'hfz') and (theclass in ['1','4']) and (pada == 'p'):
  # Kale 686(a). When used with 'loman' (the air), or when it
  # means 'to be surprised or disappointed'
  ans = 'vew'
 elif root in [
   "tfz",   "tvar",   "Dfz",   "Pral",   "Bid",
   "murcC",   "sPurC",   "sPurj",   "kzcid",   "kzvid",
   "svid"]:
  #Kale 686(c)
  # These roots admit 'i' optionally before 'ta' when the
  # P.P. Participle is used impersonally, or conveys the sense of
  # beginning to perform the action or to under the state
  # expressed by the root. When the participle is not used in this
  # sense, these roots reject 'i'.
  # NOTE :There are some class-pada conditions that
  # I have not implemented here
  ans = 'vew'
 elif root == 'aYc':
  # Kale 687(a). 'a~nch' in the sense of 'to worship' takes 'i'
  # in the sense of 'to go', it rejects 'i'
  ans = 'vew'
 elif root in ['Dfz','Sas']:
  # Kale 687(b). 'dhriSh' and 'shas' reject 'i' when they express
  # the idea of immodesty or rudeness;
  # When 'dhRiSh' means 'to overpower', it accepts 'i'.
  # When 'shas' meas 'to torment', it accepts 'i'
  ans = 'vew'
 elif sew_code == 'vew':
  # Kale 684
  ans = 'aniw'
 elif (sew_code == 'aniw') and (lc in init.consonant_set):
  # Kale 684
  ans = 'aniw'
 elif (lc in init.vowel_set) and (not dtype):
  # Kale 684
  # 12-24-04: don't override when derived type code is present . 
  # motivating example: causal of 'sthaa' should be 'seT'
  ans = 'aniw'
 else:
  ans = sew_code
 return ans

def construct_pppart1a_helper(root,theclass,pada,upasargas,sew_kta=None,dtype=None,dbg=False):
 """
 """
 err0 = "construct_pppart1a_helper(%s,%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,sew_kta,dtype)
 if dbg:
  print err0
 lc = root[-1:]
 pc = root[-2:-1]
 if not sew_kta:
  sew_kta = kta_sew_code(root,theclass,pada,upasargas,dtype,dbg=dbg)
 if theclass == '10':
  base = construct_conjbase1a(root,theclass,pada,upasargas)
  if not isinstance(base,list):
   base = [base]
  base = flatten(base)
  base = map(lambda (b): b[0:-2],base)
  base = solution(base)
 elif dtype == 'c':
  # causal. law is present tense
  base = causal_conjtab1a_bases(root,theclass,pada,upasargas,'law',dbg=dbg)
  if not isinstance(base,list):
   base = [base]
  base = flatten(base)
  base = map(lambda (b): b[0:-2],base) # remove the 'ay'
  base = solution(base)
 elif samprasarana_P(root,theclass):
  # three examples from Kale p. 422 have special handling
  if (root == 've'): 
   base = 'u'
  elif (root == 'vah'):
   base = 'Uh'
  elif (root == 'Brasj'):
   base = root
  else:
   base = samprasarana(root)
 elif kale_584_P(root) or (root in ['DvaMs','SaMs','stamB']):
  # Kale 682. penultimate radical nasal dropped (generally)
  base = root[0:-2] + root[-1:]
 elif (lc == 'F'):
  # Kale 690. 
  # See also function kale_394
  x = root[0:-1]
  if (pc in init.labial_set) or (pc == 'v'):
   base = x + 'Ur'
  else:
   base = x + 'Ir'
 elif lc in 'eEo':
  base = root[0:-1] + 'A'
 elif kale_696_P(root):
  base = root[0:-1] # drop final nasal
  if root in ['Kan','jan','san']:
   # Kale 697
   pc = lengthen_vowel(pc)
   base = base[0:-1] + pc
 elif root in ["Sam","kram","kzam","kam","cam", "vam","klam","Sram","dam"]:
  # Kale 696(a). Many roots ending in a nasal don't lengthen the vowel
  # Thus, I just give a list of those which do
  pc = lengthen_vowel(pc)
  base = root[0:-2] + pc + lc
 #?   ((equal dhaatu 'guh) (setq base [g uu h]))
 #?  ((equal dhaatu 'lih) (setq base [l ii h]))
 else:
  base = root
 return base

def kale_696_P(root):
 return root in [
   "man",   "han",   "ram",   "gam",   "tan",
   "kzaR",   "fR",   "nam",   "yam",   "van",
   "GfR",   "tfR",   "van",   "Kan",   "jan",
   "san",   "kziR"]

def kta_join(base,sew_code,sup,root,dbg=False):
 """
 """
 if isinstance(sup,list):
  return map(lambda x: kta_join(base,sew_code,x,root,dbg=dbg),sup)
 elif isinstance(base,list):
  return map(lambda x: kta_join(x,sew_code,sup,root,dbg=dbg),base)
 elif sew_code == 'vew':
  return map(lambda x: kta_join(base,x,sup,root,dbg=dbg),['sew','aniw'])
 else:
  return kta_join1(base,sew_code,sup,root,dbg=dbg)

def kta_join1(base,sew_code,sup,root,dbg=False):
 """
 ; based on 'conjugation-join'
 ; seT-code is either nil, 'seT' or 'aniT'
 """
 y = base
 ending0 = sup
 # insert 'i' if needed
 if sew_code == 'sew':
  ending = conjugation_join('i',ending0)
 else:
  ending = ending0
 # prepare for sandhi
 sandhiget = SandhiGet(['Antoine72-4'])
 ylast = y[-1:]
 yfirst = y[0:1]
 pc = y[-2:-1]
 efirst = ending[0]
 if (y == 'nanaS') and (efirst in init.consonant_set) and\
    (not (efirst in init.semivowel_set)):
  #Kale 476. nash
  # 'n' is inserted before the ending consonant of 'nash' when
  # it is followed by any consonant except a nasal or a semi-vowel.
  # NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
  # NOTE 2: The logic is put here, because other changes, e.g.,
  #  before 'tha', are required. This logic applies to other
  #  forms than the perfect
  y = 'nanaMS'
 if (efirst == 't') and (root == 'sah'):
  # Kale #506. p. 317
  # When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
  # is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
  #  vavah + tha =
  #   uvah + tha =
  #   uvaDh + Dha (by #416-3,4) =
  #   uvaDh + Dha =
  #   uvoDha
  ans = y[0:-2] + 'oQ' + ending[1:]
 elif (efirst == 't') and (root in ['dih','duh']):
  #  digdha Kale p. 346
  if yfirst == 'd':
   ylast1='g'
  elif yfirst == 'n':
   ylast1 = 'd'
  else:
   ylast1 = ylast
  ans = y[0:-1] + ylast1 + 'D' + ending[1:]
 elif (efirst == 't') and (root in ['lih','guh']):
  #  digdha Kale p. 346
  y0 = y[0:-1]
  ylast = y0[-1:]
  y0 = y0[0:-1] + lengthen_vowel(ylast)
  ans = y0 + 'Q' + ending[1:]
 elif (efirst == 't') and (ylast in 'jc'): 
  # this rule [bh a j] + [th a] -> [bh a k th a]
  # rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
  # but [bh a ~n j] + [th a] -> [bh a ~N k th a]
  y0 = y[0:-1]
  ans = y0 + 'k' + efirst + ending[1:]
 elif (efirst == 't') and (ylast == 'd'):
  # Kale 688. 'na' is substituted for 'ta' when it immediately
  # follows a final 'd'. This final 'd' is also changed to 'n'
  y0 = y[0:-1]
  ans = declension_join(y0,'nn'+ending[1:])
  if ans[len(y0)] == 'R': # example kzud -> kzuRRa
   ans = changestringitem(ans,1+len(y0),'R')
 elif False and (efirst=='t') and (ylast == 'A') and\
  (y[0] in init.consonant_set) and (y[1] in init.semivowel_set) and\
  (not (root in ['KyA','DyE','vye','hve'])):
  # NOTE: This Elisp case is odd because the 'False' condition means 
  # it never holds!
  # Kale 689. Roots ending in 'aa' (or 'e ai o' changeable to 'aa')
  # and beginning with a conjunct consonant containing a semi-vowel
  # substitute 'na' for 'ta'
  # Exceptions are 
  #  'khyaa' (to name) etc,
  #  'dhyai' (to contemplate)
  #  'vye' and 'hve'
  y0 = y[0:-1]
  ans = declension_join(y0,'An' + ending[1:])
 elif (efirst in 'tT') and aorist_join1_t_th(y,ending,root):
  ans = aorist_join1_t_th(y,ending,root)
 else:
  ans = declension_join(y,ending)
 ans = solution(ans)
 return ans

def construct_pppart1a_exception(root,theclass,pada,upasargas,dbg=False):
 """
 """
 ans = None  # not an exception
 if (root == 'SAs'):
  ans='Sizwa'
 elif (root == 'muh'):
  ans=["mugDa","mUQa"] # Kale p.423
 elif (root == 'Brasj'):
  ans='Brazwa'
 elif (root == 'mfj'):
  ans='mfzwa'
 elif (root == 'sic'):
  ans='sikta'
 elif (root == 'vah'):
  ans='UQa'
 elif (root == 'SI'):
  ans='Sayita' # Kale 684(a)
 elif (root == 'jAgf'):
  ans='jAgarita'
 elif (root == 'sTA'):
  ans='sTita'
 elif (root == 'daridrA'):
  ans='daridrita'
 elif (root == 'sasj'):
  ans='sajjita'
 elif [root,theclass] == ['as','2']:
  ans = 'BUta'
 elif (root == 'yaj'):
  ans='izwa'
 elif (root == 'DA'):
  ans='hita'
 elif (root == 'dA'):
  ans='datta'
 elif (root == 'druh'):
  ans=["drugDa","drUQa"]
 elif root == 'vid':
  # Kale 688(a). 'vid' (6 P A) takes 'ta' in the sense of
  # 'fit for enjoyment' or 'famous'. It takes 'na' in other cases
  if theclass == '7':
   ans = ['vitta','vinna']
  elif theclass == '6':
   ans = ['vidita','vitta']
  elif theclass == '4':
   ans = 'vinna' # Kale 692
 elif (root == 'Suz'):
  ans='Suzka'
 elif (root == 'siv'):
  ans='syUta'
 elif (root == 'ruz'):
  ans='razwa'
 elif (root == 'so'):
  ans='sita'
 elif (root == 'snih'):
  ans='snigDa'
 elif (root == 'sAD'):
  ans='sadDa'
 elif (root == 'sfj'):
  ans='sfzwa'
 elif (root == 'Cad'):
  ans=["CAdita","Canna"]
 elif (root == 'Sap'):
  ans=["Sapita","Sapta"]
 elif (root == 'tfz'):
  ans=["tfzita","tfzwa"]
 elif (root == 'BaYj'):
  ans='Bagna'
 elif (root == 'mad'):
  ans='matta'
 elif (root == 'vij'):
  ans='vigna'
 elif (root == 'jyA'):
  ans='jIna'
 elif (root == 'kzi'):
  ans=["kzita","kzIRa"]
 elif (root == 'kzan'):
  ans=["kzata"] #Whitney   
 elif (root == 'grah'):
  ans='gfhIta'
 elif (root == 'du'):
  ans=["duta","dUna"]
#   elif (root == 'su'):
  #ans=["sUna"]
 elif (root == 'sU'):
  ans=["sUna","sUta"] # Kale 692
 elif [root,pada] == ['hA','p']:
  ans = 'hIna'
 elif [root,pada] == ['hA','a']:
  ans = 'hAna'
 elif (root == 'vraSc'):
  ans='vfkRa'
 elif (root == 'sPurj'):
  ans='sPUrgRa'
 elif (root == 'sPUrj'):
  ans='sPUrgRa'
 elif (root == 'vE'):
  ans='vAna'
 elif [root,theclass] == ['Buj','6']:
  ans = 'Bugna'
 elif (root == 'masj'):
  ans='magna'
 elif (root == 'f'):
  # Kale 693(a). 'Ri' when it means 'to incur debt' substitutes 'na'#
  # when it means go, 'ta' is used.
  ans = ['fRa','fta']
 elif (root == 'div'):
  ans=["dyUta","dyUna"]
 elif (root == 'vA'):
  ans=["vAna","vAta"]
 elif (root == 'vyE'):
  ans=["SyAna","SIna","SIta"]
 elif (root == 'nud'):
  ans=["nunna","nutta"]
 elif (root == 'und'):
  ans=["unna","utta"]
 elif (root == 'tryE'):
  ans=["trARa","trAta"]
 elif (root == 'GrA'):
  ans=["GrARa","GrAta"]
 elif (root == 'hrI'):
  ans=["hrIRa","hrIta"]
 elif (root == 'pyAy'):
  ans=["pIna","pyAna"]
 elif (root == 'hA'):
  ans=["hAna","hIna"]
 elif (root == 'dU'):
  ans=["dUta","dUna"]
 elif (root == 'granT'):
  ans=["graTita","granTita"]
 elif (root == 'manT'):
  ans=["maTita","manTita"]
 elif (root == 'gE'):
  ans='gIta' 
 elif (root == 'si'):
  ans=["sita","sina"]
 elif (root == 'tvar'):
  ans=["tvarita","tUrRa"]
 elif (root == 'trE'):
  ans=["trAta","trARa"]
 elif (root == 'ad'):
  ans=["jagDa","anna"]
 elif (root == 'ard'):
  ans=["arRRa","arRa"]
 elif (root == 'UT'):
  ans='uta'
 elif (root == 'kaz'):
  ans=["kazwa","kazita"]
 elif (root == 'kfS'):
  ans=["karSita"] # 5/23/05 ref. Whitney; removed 'kfSa'
 elif (root == 'kzIv'):
  ans='kzIva'
 elif (root == 'knUT'):
  ans='knUta'
 elif (root == 'kzmAy'):
  ans=["kzmAyita","kzmAta"]
 elif (root == 'kzE'):
  ans='kzAma'
 elif (root == 'Co'):
  ans=["CAta","Cita"]
 elif (root == 'jyo'):
  ans='jIta'
 elif (root == 'do'):
  ans='dita'
 elif (root == 'dfh'):
  ans=["dfQa","dadfmhita"]
 elif (root == 'DA'):
  ans='hita'
 elif (root == 'DAv'):
  ans=["DOta","DAvita"]
 elif (root == 'De'):
  ans='DIta'
 elif (root == 'pac'):
  ans='pakva'
 elif [root,theclass] == ['pA','1']:
  ans = 'pIta' # to dring
 elif (root == 'pUT'):
  ans='pUta'
 elif (root == 'Pal'):
  ans=["Palita","Pulla"]
 elif (root == 'mav'):
  ans=["mavita","mUta"]
 elif (root == 'mA'):
  ans='mita'
 elif (root == 'me'):
  ans='mita'
 elif (root == 'murcC'):
  ans=["mUrta","mUrCita"]
 elif (root == 'murC'):
  ans=["mUrta","mUrCita"]
 elif (root == 'mUrcC'):
  ans=["mUrta","mUrCita"]
 elif (root == 'mUrC'):
  ans=["mUrta","mUrCita"]
 elif (root == 'lAG'):
  ans='lAGa' # with 'ut'
 elif (root == 'So'):
  ans=["SAta","Sita"]
 elif (root == 'bfh'):
  ans=["bfhita","bfQa"]
 elif (root == 'bfMh'):
  ans=["bfMhita","bfQa"]
 elif (root == 'vfh'):
  ans=["vfhita","vfQa"]
 elif (root == 'vfMh'):
  ans=["vfMhita","vfQa"]
 elif (root == 'sriv'):
  ans='sruta'
 elif (root == 'hlAd'):
  ans=["hlanna","hlAdita"]
 elif (root == 'SrA'):
  ans=["Sfta","SrARa","Srapita"]
 # stambh with 'prati' or 'ni' -> pratistabhda, nistabhda
 # The 's' does not change to 'Sh' here. (Kale p. 430)
 elif (root == 'sPAy'):
  ans='sPIta'
 # Kale p. 430. 'stiita' and 'stiima' with 'pra' : 'sounded'
 elif (root == 'styE'):
  ans=["styAna","stIta","stIma"]
 # can have other forms ('niShNaata' and 'nadiiShNa') with some meanings
 elif (root == 'snA'):
  ans='snAta'
 elif (root == 'cyu'):
  ans='cyuta'
 elif (root == 'qI'):
  ans=["qayita","qIna"]
 elif (root == 'pyE'):
  ans='pIna'
 elif (root == 'buD'):
  ans=["budDa","boDita"]
 elif (root == 'Bram'):
  ans=["BrAnta","Bramita"]
 elif (root == 'lasj'):
  ans='lajjita'
 elif (root == 'SuB'):
  ans='SoBita'
 elif (root == 'Scut'):
  ans=["Scutita","Scotita"] # Antoine2-gloss
 elif (root == 'Svi'):
  ans='SUna' # Antoine2-gloss
 elif (root == 'yat'):
  ans=["yatita","yatta"]
 elif (root == 'bru'):
  ans='ukta'
 elif (root == 'ruh'):
  ans='rUQa'
 elif (root == 'zWiv'):
  ans='zWyUta'
 elif (root == 'DU'):
  ans=["DUta","DUna"] # Antoine2-gloss, Apte
 elif (root == 'svid'):
  ans=["svidita","svinna"] # Apte
 elif (root == 'pU'):
  ans=["pavita","pUta"]
 elif (root == 'fz'):
  ans=["fzwa"] # was '(RiShita)
 return ans

def construct_ippart1a_ya(root,theclass,pada,upasargas=None,dtype=None,dbg=False):
 """ indeclineable pas participles for prefixed verbs.
 """
 err0 = "construct_ippart1a_ya(%s,%s,%s,%s,%s)" %(root,theclass,pada,upasargas,dtype)
 #if (root == 'sAmaya'):
 # dbg=True
 if dbg:
  print err0
 ending = 'ya'
 tok = root
 if  samprasarana_P(root,theclass) and (root not in ['ve','jyA','vye']):
  tok = samprasarana(root)
 lc = tok[-1:]
 pc = tok[-2:-1]
 fc = tok[0:1]
 (c1,v,c2,thetype)= dhAtu_parts(tok+'y')
 if kale_584_P(root):
  # drop penultimate nasal
  tok = tok[0:-2] + lc
 if dbg:
  print err0," tok=",tok
 ans = []
 if theclass == '10':
  # Kale 758
  toks = class_a_base(root,theclass,pada,dbg=dbg)
  if preserve_elisp_errors:
   if root == 'sAmaya':
    toks = ['sAmayaay']
  if dbg:
   print err0,"class_a_base=",toks
  for thetok in toks:
   # thetok ends in 'ay'
   # Following code assumes form of 'thetok' before 'ay' is
   # [... vowel cons]
   thetok = thetok[0:-2] # remove 'ay'
   pc = thetok[-2:-1] # presumably a vowel
   if pc in init.shortsimplevowel_set:
    thisans = thetok + 'ay' + ending
   else:
    thisans = thetok + ending
   ans = append_if_new(ans,thisans)
 elif dtype == 'c':
  # causal. Except for using causal_base instead of class_a_base,
  # this code is same as for theclass==10
  # Kale 758
  toks = causal_base(root,theclass,pada,upasargas,dbg=dbg)
  for thetok in toks:
   # thetok ends in 'ay'
   # Following code assumes form of 'thetok' before 'ay' is
   # [... vowel cons]
   thetok = thetok[0:-2] # remove 'ay'
   pc = thetok[-2:-1] # presumably a vowel
   if pc in init.shortsimplevowel_set:
    thisans = thetok + 'ay' + ending
   else:
    thisans = thetok + ending
   ans = append_if_new(ans,thisans)
 elif ((theclass == '8') and (lc in init.nasal_set) and (root!='san'))or\
      (root in ['man','van','han']):
  # Kale 753. drop final nasal. The ending is then 'a', so by
  # 751 'tya' is appended
  ans = tok[0:-1] + 'tya'
  if root == 'man':
   # 11-24-04 By Whitney, for "man" also manya is acceptable
   ans1 = ans # matya
   ans2 = tok[0:-1] + 'nya' # manya
   ans = [ans1,ans2]
 elif root in ['gam','nam','yam','ram']:
  # optionally drop final nasal
  ans = [tok+ending,tok[0:-1] + 'tya']
 elif root in ['Kan','jan','san']:
  # Kale 754
  ans = [tok+ending,tok[0:-2] + 'A' + ending]
 elif root == 'kzi':
  # Kale 755
  ans = tok[0:-1] + 'I' + ending
 elif root == 'jAgf':
  # Kale 755
  ans = (gunate_final_vowel(tok)) + ending
 elif v == 'F': 
  w = kale_394(c1,v,c2,thetype)
  ans = c1 + w + c2 + ending[1:]
 elif (lc in 'AeEo') or (root in ['mI','mi','dI']):
  # Kale 459. Roots ending in 'e', 'ai', and 'o' are treated as
  # roots ending in 'aa'
  # Kale 459. The roots 'mi' (5 U 'to throw'),
  # 'mii' (9 U 'to kill'), and 'dii' (4 A 'to perish')
  # are treated as roots ending in 'aa' before a termination
  # causing guna or vrddhi.
  ans = tok[0:-1] + 'A' + ending
 elif (lc in init.shortsimplevowel_set):
  ans = tok + 'tya'
 elif (v in 'iufx') and (0<len(c2)):
  w = kale_395(c1,v,c2,thetype)
  ans = c1 + w + c2 + ending[1:]
 elif root == 'lI':
  # Kale 459. The root 'lii' (9 P, 4 A 'to adhere or cling to') changes
  # its vowel optionally to 'aa' before a termination causing
  # guna or vrddhi.
  ans = [tok+ending,tok[0:-1]+'A'+ending]
 else:
  ans = conjugation_join(tok,ending)
 # exceptions
 ans1 = construct_ippart1a_ya_exception(root,theclass,pada,upasargas,dbg=dbg)
 if ans1:
  ans = ans1
 ans = solution(ans)
 return ans

def construct_ippart1a_ya_exception(root,theclass,pada,upasargas,dbg=False):
 """
 """
 if (root == 'vye') and (upasargas in ['pari','sam']):
  # Kale 756
  ans = ['vyAya','vIya']
 else:
  ans=None
 return ans

def construct_papart1a(root,theclass,pada,upasargas,dbg=False):
 """ Ref Deshpande, p. 176
 """
 err0="construct_papart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 ktas = construct_pppart1a(root,theclass,pada,upasargas,dbg=dbg)
 if not isinstance(ktas,list):
  ktas=[ktas]
 ktas = flatten(ktas)
 ans = []
 for kta in ktas:
  ans.append(kta + 'vat')
 return ans

def construct_prapart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of stems, e.g., gamat, for present active participle.
     returns [] if pada is not 'p' (parasmaipada)
 """
 err0="construct_prapart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 if pada != 'p':
  return []
 ans1=construct_prespart_base_alt_P(root,theclass,pada,upasargas,dbg=dbg)
 ans=[]
 for (x,femtype) in ans1:
  ans.append(x + 't')
 if preserve_elisp_errors:
  # there is something wrong with cas upAr 3a. 
  # force an agreement with Elisp
  if (root == 'upAr') and (theclass == '3'):
   ans = ['UpArat']
 return ans

def construct_prespart_base_alt_P(root,theclass,pada,upasargas,dbg=False):
 """
 """
 err0="construct_prapart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 if pada != 'p':
  return []
 tense = 'law' # present
 voice = 'active'
 ctabs = construct_conjtab1a(root,theclass,pada,upasargas,tense,voice,dbg=dbg)
 ctabs = solution(ctabs)
 # Assume that ctabs is a 9-dimensional array, the conjugation table,
 # ordered so that ctabs[2] is the 3rd pl.
 # array of 3rd pl.
 if not isinstance(ctabs,list):
  return []
 #if root in ['vac','upAr']:
 # print err0,"\nctabs=",ctabs
 if len(ctabs) != 9:
  # assume it is a list of same
  ctabs = join_arrays_many(ctabs)
 try:
  ctabelt = ctabs[2]
  ctabelt = solution(ctabelt)
  if not isinstance(ctabelt,list):
   # assume a string, and if there is a ',', split it
   ctabelt = ctabelt.split(',')
 except:
  print 'ctabs=',ctabs
  raise NameError(err0," ctabelt ERROR")
 if ctabelt == ['']: # case of vac,2,p(active)
  return []
 ans = construct_prespart_base_alt_P_1(ctabelt,root,theclass,dbg=dbg)
 return ans

def construct_prespart_base_alt_P_1(ctabelt,root,theclass,dbg=False):
 """
; Note: AntoineI-#83 says
; (1) Verbs taking parasmaipada terminations form their present
;   participle active in 'at'. To form it, the termination
;   'anti' of the 3rd pers. plur of the present active is
;   replaced by 'at'.
; (2) Verbs taking atmanepada terminations form their present
;   participle active in 'maana'. To form it, the termination
;   'te' of the 3rd pers. sing. of the present tense is replaced
;   by 'maana'.
; (2) is correct only for the a-conjugations.
; For the non-a conjugations, the ending 'aana' replaces the
; termination 'ate' of the 3rd pers. PLURAL of the present tense.
; An equivalent alternate of (2) for the 'a' conjugations is
; to replace the termination 'nte' of the 3rd pers. plur.
; of the present tense with 'maana'.
; Since the 3rd pers PLURAL of the present tense can be made
; to work with either, and is required for non-a conjugations,
; that is chosen for both a and non-a conjugations
; This is consistent with Goldman (weak stem of non-a conjugations)
; and Kale (#669, p. 418) says to use 3P of present tense.
 """
 err0="construct_prespart_base_alt_P_1(%s,%s,%s)" %(ctabelt,root,theclass)
 if dbg:
  print err0
 ans=[]
 if not isinstance(ctabelt,list):
  ctabelt=[ctabelt]
 for conjelt in ctabelt:
  # conjelt is 3rd pers PLUR of present active
  # this construction follows Goldman (15.7)
  tok = conjelt
  if conjelt.endswith('uH'):
   # this encountered with vid (class 2)
   # one form of its 3P present tense is viduH
   # I want to pretend this is vidanti,
   # so remove the [u H] and replace with [a n t i]
   tok = tok[0:-2] + 'anti'
  stok = tok[0:-1] # strong base, remove ending 'i'
  # stok ends in either 'ant' or 'at' or
  # 'aant' ('yaa' and similar class 2 ending in 'aa', but not 'daridraa')
  # In the case 'ant' and 'aant', we remove the last 3 letters
  # In the case 'at', we also remove the last 2 letters.
  if stok.endswith(('ant','Ant')):
   wtok = stok[0:-2] # remove 'nt'
  else:
   wtok = stok[0:-1] # remove 't'. class=3, daridrA, etc.
  strong = stok
  weak = wtok
  # now get femtype .
  # 's' = strong, 'w' = weak, 'v' = ?
  if theclass in ['1','4','10']:
   femtype = 's'
  elif theclass == '3':
   femtype = 'vw'
  elif theclass == '6':
   femtype = 'sw'
  elif root in ['SAs','jakz','cakAs','daridrA','jAgf']:
   femtype = 'vw'
  elif theclass == '2':
   if stok.endswith('Ant'):
    femtype = 'sw'
   else:
    femtype = 'w'
  else:
   femtype = 'w'
  thisans = [weak,femtype]
  ans.append(thisans)
 return ans

def construct_prmpart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of stems, e.g., gamyamAna, for present middle participle.
     returns [] if pada is not 'a' (parasmaipada)
 """
 err0="construct_prmpart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 if pada != 'a':
  return []
 ans1 = construct_prespart_base_alt_A(root,theclass,pada,upasargas,dbg=dbg)
 ans=[]
 for x in ans1:
  ans.append(x + 'a')
 return ans

def construct_prespart_base_alt_A(root,theclass,pada,upasargas,dbg=False):
 err0="construct_prespart_base_alt_A(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 tense = 'law' # present
 voice = 'active'
 ctabs = construct_conjtab1a(root,theclass,pada,upasargas,tense,voice,dbg=dbg)
 ctabs = solution(ctabs)
 # Assume that ctabs is a 9-dimensional array, the conjugation table,
 # ordered so that ctabs[2] is the 3rd pl.
 # array of 3rd pl.
 if not isinstance(ctabs,list):
  return []
 #if root in ['vac','upAr']:
 # print err0,"\nctabs=",ctabs
 if len(ctabs) != 9:
  # assume it is a list of same
  ctabs = join_arrays_many(ctabs)
 try:
  ctabelt = ctabs[2]
  ctabelt = solution(ctabelt)
  if not isinstance(ctabelt,list):
   # assume a string, and if there is a ',', split it
   ctabelt = ctabelt.split(',')
 except:
  print 'ctabs=',ctabs
  raise NameError(err0," ctabelt ERROR")
 if ctabelt == ['']: # case of vac,2,p(active)
  return []
 ans1 = construct_prespart_base_alt_A_1(ctabelt,root,theclass,dbg=dbg)
 return ans1

def construct_prespart_base_alt_A_1(ctabelt,root,theclass,dbg=False):
 """
 """
 err0="construct_prespart_base_alt_A_1(%s,%s,%s)" %(ctabelt,root,theclass)
 if dbg:
  print err0
 ans=[]
 if not isinstance(ctabelt,list):
  ctabelt=[ctabelt]
 for conjelt in ctabelt:
  # conjelt is 3rd pers plural of present active
  # this construction follows Goldman (15.8) , Kale (#669)
  # conjelt assumed to end in 'ante' for a-conjugations
  # and 'ate' for non-a-conjugations.
  tok = conjelt
  if theclass in ['1','4','6','10']:
   # conjelt ends in 'ante'. Remove 'nte', leave the 'a'
   stok = tok[0:-3]
   stok = conjugation_join(stok,'mAna')
   stok = stok[0:-1]  # remove final 'a'
  elif theclass in ['5','8','9','2','7']:
   # conjelt ends in 'ate'. Remove this. add 'Ana', and remove final 'a
   stok = tok[0:-3]
   stok = conjugation_join(stok,'Ana')
   stok = stok[0:-1]
  elif theclass == '3':
   # conjelt ends in 'ate'. Remove this. add 'Ana', and remove final 'a
   stok = tok[0:-3]
   if root == 'As':
    # Kale 670(a)
    stok = conjugation_join(stok,'Ina')
   else:
    stok = conjugation_join(stok,'Ana')
   stok = stok[0:-1]
 try:
  ans = [stok]
 except:
  raise NameError (err0 + " stok not defined")
 return ans

def construct_prppart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns list of stems for present passive participles,
     e.g., ['gamyamAna'] for 'gam'
 """
 err0="construct_prppart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 pratipadikas = construct_conjpassbase1a(root,theclass,pada,upasargas)
 if not isinstance(pratipadikas,list):
  pratipadikas = [pratipadikas]
 ans=[]
 for x in pratipadikas:
  # x ends in 'y', like 'buDy'
  # apped amAna to get the base
  b = conjugation_join(x,'amAna')
  ans.append(b)
 if preserve_elisp_errors:
  # without this, hIyamAna for middle voice ('a') and 
  # hAyamAna for active voice ('p')
  if [root,theclass,pada] == ['hA','3','a']:
   ans = ['hAyamAna']
  elif [root,theclass,pada] == ['hA','3','p']:
   ans = ['hIyamAna']

 return ans

def construct_potpart1a(root,theclass,pada,upasargas=None,dbg=False):
 """ potential passive participles (gerundives)
 """
 err0 = "construct_potpart1a(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ans = construct_potpart1a_irregular(root,theclass,pada)
 if ans:
  return ans
 ans=[]
 ans = ans + construct_potpart1a_tavya(root,theclass,pada,upasargas,dbg=dbg)
 ans = ans + construct_potpart1a_anIya(root,theclass,pada,upasargas,dbg=dbg)
 ans = ans + construct_potpart1a_yat(root,theclass,pada,upasargas,dbg=dbg)
 ans = ans + construct_potpart1a_kyap(root,theclass,pada,upasargas,dbg=dbg)
 ans = ans + construct_potpart1a_Ryat(root,theclass,pada,upasargas,dbg=dbg)
 return ans

def construct_potpart1a_irregular(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type irregular
 """
 err0 = "construct_potpart1a_irregular(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 if root == 'vaD':
  return ['vaDya']
 if root == 'vaS':
  # 02-19-05 (MB 3262001)
  return ['vaSya']
 if root == 'sah':
  # 02-28-05 (Kale 716) 
  return ['sahya']
 return []

def construct_potpart1a_tavya(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type tavya
 """
 err0 = "construct_potpart1a_tavya(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ending = 'tavya'
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 ans=[]
 if theclass == '10':
  # drop the 'ay' of conjugational base
  base = construct_conjbase1a(root,theclass,pada,upasargas,dbg=dbg)
  if not isinstance(base,list):
   base = [base]
  for b in base:
   ans.append(b + 'i' + ending)
 elif lc in 'AeEo':
  base = tok[0:-1] + 'A'
  ans = potpart_join1(base,ending,root)
  ans = [ans]
 else:
  base = gunate_final_vowel(tok)
  sew = construct_sew_code1a(root,theclass,pada,upasargas,dbg=dbg)
  if (sew == 'sew'):
   ans = potpart_join1(base,'i' + ending,root)
  elif (sew == 'aniw'):
   ans = potpart_join1(base,ending,root)
  elif (sew == 'vew'):
   ans1 = potpart_join1(base,'i' + ending,root)
   ans2 = potpart_join1(base,ending,root)
   ans=[ans1,ans2]
 if not isinstance(ans,list):
  ans=[ans]
 ans = flatten(ans)
 ans1 = construct_potpart1a_tavya_exception(root,theclass,pada,upasargas)
 if ans1:
  ans = ans1
 if not isinstance(ans,list):
  ans=[ans]
 return ans

def construct_potpart1a_tavya_exception(root,theclass,pada,upasargas=None):
 """ exceptions for gerundive of type tavya
 """
 if root == 'buD':
  return ['boDitavya','bodDavya']
 elif root == 'mfj':
  return ['mArzwavya']
 elif root == 'sfj':
  return ['srazwavya']
 elif root == 'Brasj':
  return ['Barzwavya','Brazwavya']
 elif root == 'Bid':
  return ['Bettavya']
 elif root == 'guh':
  return ['gUhitavya','goQavya']
 else:
  return []

def construct_potpart1a_anIya(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type anIya
 """
 err0 = "construct_potpart1a_anIya(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ending = 'anIya'
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 ans=[]
 if theclass == '10':
  # drop the 'ay' of conjugational base
  base = construct_conjbase1a(root,theclass,pada,upasargas,dbg=dbg)
  if not isinstance(base,list):
   base = [base]
  for b in base:
   thisans = potpart_join1(b[0:-2],ending) # drop the 'ay'
   ans.append(thisans)
 elif lc in 'AeEo':
  base = tok[0:-1] + 'A'
  ans = potpart_join1(base,ending,root)
  ans = [ans]
 elif lc in init.vowel_set:
  base = gunate_final_vowel(tok)
  ans = potpart_join1(base,ending)
  ans = [ans]
 else: # is identical to prior elif clause
  base = gunate_final_vowel(tok)
  ans = potpart_join1(base,ending)
  ans = [ans]
 if not isinstance(ans,list):
  ans=[ans]
 ans = flatten(ans)
 ans1 = construct_potpart1a_anIya_exception(root,theclass,pada,upasargas)
 if ans1:
  ans = ans1
 if not isinstance(ans,list):
  ans=[ans]
 return ans

def construct_potpart1a_anIya_exception(root,theclass,pada,upasargas=None):
 """ exceptions for gerundive of type tavya
 """
 if root == 'mfj':
  ans = ['mArjanIya']
 elif root == 'Brasj':
  ans = ['BarjanIya','BrajjanIya']
 elif root == 'guh':
  ans = ['gUhanIya']
 else:
  ans = []
 return ans

def construct_potpart1a_yat(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type yat
 """
 err0 = "construct_potpart1a_yat(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ending = 'ya'
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 ans=[]
 if lc in 'AeEo':
  base = tok[0:-1] + 'e'
  ans = [potpart_join1(base,ending)]
 elif lc in init.vowel_set:
  base = gunate_final_vowel(tok)
  if (root == 'BU'):
   base = 'BU' # Kale 726(a)
  ans = [potpart_join1(base,ending)]
 elif (pc == 'a') and (lc in init.labial_set):
  base = tok
  ans = [potpart_join1(base,ending)]
 elif root in ['tak','Sas','cat','yat','jan','Sak','sah']:
  base = tok
  ans = [potpart_join1(base,ending)]
 elif ((not upasargas) or (upasargas == ['A'])) and\
      (root in ['gad','mad','car','yam']):
  base = tok
  ans = [potpart_join1(base,ending)]
 elif root in ['vad','paR']:
  # Kale 719
  base = tok
  ans = [potpart_join1(base,ending)]
 elif [root,theclass] == ['vf','9']:
  # Kale 719
  base = tok
  ans = [potpart_join1(base,ending)]
 elif root in ['vah','f']:
  # Kale 720
  base = tok
  ans = [potpart_join1(base,ending)]
 elif [root,upasargas] == ['sf',['upa']]:
  # Kale 720
  base = tok
  ans = [potpart_join1(base,ending)]
 elif root == 'jF':
  # Kale 721
  base = tok
  ans = [potpart_join1(base,ending)]
 elif root == 'han':
  # Kale 722
  base = 'vaG'
  ans = [potpart_join1(base,ending)]
 else:
  # root does not take 'yat'
  ans=[]
 return ans
 if not isinstance(ans,list):
  ans=[ans]
 ans = flatten(ans)
 ans1 = construct_potpart1a_yat_exception(root,theclass,pada,upasargas)
 if ans1:
  ans = ans1
 if not isinstance(ans,list):
  ans=[ans]
 return ans

def construct_potpart1a_yat_exception(root,theclass,pada,upasargas=None):
 """ exceptions for gerundive of type yat.  Currently no cases
 """
 return []

def construct_potpart1a_kyap(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type kyap
 """
 err0 = "construct_potpart1a_kyap(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ending = 'ya'
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 ans=[]
 if (pc == 'f') and (root not in ['kfp','cft']):
  ans = tok + ending
 elif root in ['i','stu','SAs','vf','df','juz']:
  if lc in init.vowel_set:
   ans = tok + 't' + ending
  else: 
   ans = tok + ending
  if root == 'SAs':
   ans = 'Siz' + ending
 elif root in ['SaMs','duh','guh']:
  if root == 'SaMs':
   ans = 'Sas' + ending
  else:
   ans = tok + ending
 elif root == 'mfj':
  ans = tok + ending
 elif root == 'BU':
  ans = tok + ending
 elif root == 'vad':
  ans = [tok+ending, 'ud' + ending] # Kale 726(b), 729
 elif root == 'Kan':
  ans = 'Ke' + ending
 elif root == 'Bf':
  ans = tok + 't' + ending
 elif root == 'sf':
  ans = 'sUr' + ending
 elif [root,theclass] == ['sU','6']:
  ans = 'sUr' + ending
 elif root in ['kuz','pac','vyaT','Bid','ujJ','puz','siD']:
  ans = tok + ending
 elif root == 'gup':
  ans = 'kup' + ending # ?
 elif (root in ['pU','nI','ji']) and (upasargas == ['vi']):
  if root == 'ji':
   ans = tok + 't' + ending
  else:
   ans = tok + ending
 elif root == 'gfh':
  ans = tok + ending
 elif root == 'kf':
  ans = tok + 't' + ending
 elif root == 'yuj':
  ans = 'yug' + ending
 else:
  ans=[]  # root does not take this form
 if not isinstance(ans,list):
  ans=[ans]
 ans = flatten(ans)
 return ans

def construct_potpart1a_Ryat(root,theclass,pada,upasargas=None,dbg=False):
 """ gerundive of type Ryat
 """
 err0 = "construct_potpart1a_Ryat(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ending = 'ya'
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 ans=[]
 if root == 'han':
  ans = 'GAt' + ending
 elif root == 'vad':
  ans = ['vAd'+ending,'ud'+ending]
 elif root == 'vas':
  ans = ['vAs'+ending,'vas'+ending]
 elif root == 'yuj':
  ans = ['yoj'+ending,'yog'+ending]
 elif root == 'pac':
  ans = ['pAk'+ending,'pAc'+ending]
 elif root == 'vac':
  ans = ['vAk'+ending,'vAc'+ending]
 elif root == 'vaYc':
  ans = ['vaNkya',tok + ending]
 elif root == 'Buj':
  ans = ['Bog'+ending,'Boj'+ending]
 elif root == 'nI':
  ans = ['ne'+ending,'nAy' + ending]
 elif lc == 'f':
  base = gunate_final_vowel(tok,True) # vrddhi
  ans = base + ending
 elif lc in 'uU':
  base = tok[0:-1] + 'Av'
  ans = conjugation_join(base,ending)
 elif root == 'ram':
  ans = [] # Ryat->rSmya, not in Whitney
 elif lc in init.consonant_set:
  if [root,upasargas] == ['vac',['pra']]:
   pass
  elif root in ['yaj','yAc','ruc','tyaj','fc']:
   pass
  elif lc == 'c':
   tok = tok[0:-1] + 'k'
  elif lc == 'j':
   tok = tok[0:-1] + 'g'
  if (pc == 'a') or (root == 'mfj'):
   base = gunate_final_vowel(tok,True) # vrddhi
  else:
   base = gunate_final_vowel(tok) # guna
  ans = base + ending
 else:
  ans=[]  # root does not take this form
 if not isinstance(ans,list):
  ans=[ans]
 ans = flatten(ans)
 return ans

def potpart_join1(base,ending,root=None,dbg=False):
 """
 """
 return kta_join1(base,'aniw',ending,root,dbg=dbg)

def construct_rppart1a_stem(root,theclass,pada,upasargas,dbg=False):
 """ This is not in Elisp.  It calls construct_rppart1a, which IS in elisp,
     then construct a stem from the result 
 """
 err0 = "construct_rppart1a_stem(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 val = construct_rppart1a(root,theclass,pada,upasargas,dbg=dbg)
 ans = []
 for v in val:
  if pada == 'p':
   # v is a pair, strong/weak
   s = v[0] # strong, ending in 'v'
   thisans = s+'as'
   ans.append(thisans)
  else:
   # v is a string, to which we append 'a'
   thisans = v + 'a'
   ans.append(thisans)
 return ans

def construct_rppart1a(root,theclass,pada,upasargas,dbg=False):
 """ 
; reduplicative perfect participle
; when pada = 'P', return a list of pairs of symbols,
;    the first symbol of a pair ends in 'v', the second ends in 'uSh'
; when pada = 'A', return a list of symbols (ending in 'aan' or 'aaN')
 """
 err0 = "construct_rppart1a(%s,%s,%s,%s" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 ans1 = construct_rppart1b(root,theclass,pada,upasargas,dbg=dbg)
 if not ans1:
  ans = []
 elif pada == 'a':
  ans = ans1
 else:
  ans = []
  for a in ans1:
   ans.append(construct_rppart1a_mod(a))
 return ans

def construct_rppart1a_mod(x,dbg=False):
 """
 ; x is a symbol ending in 'v'.
 ; return a list of 2 symbols, the 1st is 'x'
 ; the 2nd is the prefix for weak endings (ends in 'uSh')
 ; compare to logic in 'declension-general-vas-ADJ'
 """
 tok = x
 ptok1 = tok[0:-1]
 if ptok1.endswith('i'):
  wtok = ptok1[0:-1] + 'uz'
 else:
  wtok = declension_join(ptok1,'uz')
 return [x,wtok]

def construct_rppart1b(root,theclass,pada,upasargas,dbg=False):
 """ 
; reduplicative perfect participle
; The form returned is a list of symbols, where
;  when pada = 'P', the symbol ends in 'v', so when 'as' is 
;  appended, the form ending in 'vas' needed by 'declension-general-vas-ADJ'
;  is available.
;  when pada = 'A', the symbol ends in 'aan', the form
;   needed for the declension ending in 'a'.
 """
 err0 = "construct_rppart1b(%s,%s,%s,%s)" %(root,theclass,pada,upasargas)
 if dbg:
  print err0
 if not reduplicative_liw_P(root,theclass,dbg=dbg):
  return [] # there is no reduplicative perfect particple
 tok = root
 lc = tok[-1:]
 pc = tok[-2:-1]
 # establish base
 if (tok.startswith('a')) and (pc in init.nasal_set):
  root1 = tok[0:-2] + lc # drop the penultimate nasal
 elif (lc == 'F') and (pada == 'p'):
  # Kale 676
  x = tok[0:-1] # drop the 'F'
  fc = tok[0:1] # first char
  if (fc in init.labial_set) or (pc == 'v'):
   y = x + 'Ur'
  else:
   y = x + 'Ir'
  root1 = y
 else:
  root1 = root
 # i-insertion. Compute base as a certain list 
 bitab = liw_main_get_bitab(upasargas,theclass,pada,root1)
 x = bitab[2] # 3rd pers. plur., a list of pairs
 y = [z[0] for z in x] #
 if dbg:
  print err0," y=",y," bitab=",bitab
 if (lc == 'F') and (pada == 'a'):
  # Kale 676
  (parts,types) = word_parts(tok)  
  y = reduplicate_perfect(tok,(parts,types))
  x = y[0] # assume 'y' has only one form in this case
  x = x[0:-1] # drop the final 'F'
  fc = tok[0:1] # irst char
  if (fc in init.labial_set) or (pc == 'v'):
   z = x + 'ur'
  else:
   z = x + 'ir'
  y = [z]
 base = y
 # 
 if dbg:
  print err0,"base=",base
 ans = []
 for x in base:
  if pada == 'p':
   ending = 'va'
   sew_code = 'aniw'
   (parts,types) = word_parts(x)
   if dbg:
    print err0," parts=",parts," types=",types
   if types in ['v','vc','cv','cvc']: # 1 syllable
    sew_code = 'sew'
   elif lc in 'Aeio':  # check this. usually have AeEo
    sew_code = 'sew'
   y = perfect_join1(x,sew_code,ending)
   y = y[0:-1] # drop the final 'a'
  elif pada == 'a':
   ending = 'Ana'
   sew_code = 'aniw'
   y = perfect_join1(x,sew_code,ending)
   y = y[0:-1] # drop the final 'a'
  ans.append(y)
 # exceptions
 if pada == 'p':
  if root == 'BU':
   ans = ['baBUv']
  elif root == 'jan':
   ans = ['jajanv']
  elif root == 'Kan':
   ans = ['caKanv']
  elif root == 'gam':
   ans = ['jagmiv','jaganv']
  elif root == 'han':
   ans = ['jaGniv','jaGanv']
  elif root == 'vid':
   ans = ['vividv','vividiv']
  elif root == 'dfS':
   ans = ['dadfSiv','dadfSv']
  elif root == 'viS':
   ans = ['viviSiv','viviSv']
 return ans

def construct_fapart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of stems, e.g., gamizyat, for future active participle.
     returns [] if pada is not 'p' (parasmaipada)
 """
 err0="construct_fapart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 if pada != 'p':
  return []
 tense = 'law' # present
 voice = 'active'
 ctabs = conjugation_tab_lfw(upasargas,theclass,pada,root,'active',dbg=dbg)
 if not isinstance(ctabs,list):
  return []
 if len(ctabs) != 9:
  # assume it is a list of same
  ctabs = join_arrays_many(ctabs)
 try:
  ctabelt = ctabs[2]
  ctabelt = solution(ctabelt)
  if not isinstance(ctabelt,list):
   # assume a string, and if there is a ',', split it
   ctabelt = ctabelt.split(',')
 except:
  print 'ctabs=',ctabs
  raise NameError(err0," ctabelt ERROR")
 if ctabelt == ['']: # case of vac,2,p(active)
  return []
 ans=[]
 for x in ctabelt:
  # e.g., x = gamizyanti. -> gamizyat
  ans.append(x[0:-3]+'t')
 if preserve_elisp_errors:
  pass
 return ans

def construct_fmpart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of stems, e.g., gamizyamARa, for future middle participle.
     returns [] if pada is not 'a' (atmanepada)
 """
 err0="construct_fmpart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 if pada != 'a':
  return []
 tense = 'law' # present
 voice = 'active'
 ctabs = conjugation_tab_lfw(upasargas,theclass,pada,root,'active',dbg=dbg)
 if not isinstance(ctabs,list):
  return []
 if len(ctabs) != 9:
  # assume it is a list of same
  ctabs = join_arrays_many(ctabs)
 try:
  ctabelt = ctabs[0] # 3rd singular
  ctabelt = solution(ctabelt)
  if not isinstance(ctabelt,list):
   # assume a string, and if there is a ',', split it
   ctabelt = ctabelt.split(',')
 except:
  print 'ctabs=',ctabs
  raise NameError(err0," ctabelt ERROR")
 if ctabelt == ['']: # case of vac,2,p(active)
  return []
 ans=[]
 for x in ctabelt:
  # e.g., x = gamizyate. -> gamizyamARa
  y = x[0:-2] # gamizya
  thisans = conjugation_join(y,'mAna')
  ans = append_if_new(ans,thisans)
 if preserve_elisp_errors:
  pass
 return ans

def construct_fppart1a(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of stems, e.g., gamizyamARa, for future passive participle.
     returns [] if cannot find such
 """
 err0="construct_fppart1a(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 ctabs = conjugation_tab_lfw(upasargas,theclass,pada,root,'passive',dbg=dbg)
 if not isinstance(ctabs,list):
  return []
 if len(ctabs) != 9:
  # assume it is a list of same
  ctabs = join_arrays_many(ctabs)
 try:
  ctabelt = ctabs[0] # 3rd singular
  ctabelt = solution(ctabelt)
  if not isinstance(ctabelt,list):
   # assume a string, and if there is a ',', split it
   ctabelt = ctabelt.split(',')
 except:
  print 'ctabs=',ctabs
  raise NameError(err0," ctabelt ERROR")
 if ctabelt == ['']: # case of vac,2,p(active)
  return []
 ans=[]
 for x in ctabelt:
  # e.g., x = gamizyate. -> gamizyamARa
  y = x[0:-2] # gamizya
  thisans = conjugation_join(y,'mAna')
  ans = append_if_new(ans,thisans)
 if preserve_elisp_errors:
  pass
 return ans

def sl_participle(root,theclass,evoice,partcode,dtype=None,upasargas=None,dbg=False):
 """
 construct various participle stems based on partcode; main work parcelled
 out to other routines, depending on partcode.
 partcode also follows the conventions of Scharf.

  Assume root and evoice are symbols using
  SLP conventions, class is a number, dtype, if present is one of
  the dervived type symbols. 
 """
 err0 = "sl_participle(%s,%s,%s,%s,%s,%s)" %(root,theclass,evoice,partcode,dtype,upasargas)
 if dbg:
  print err0
 #upasargas = None
 if evoice == 'a':
  pada = 'p' #parasmaipada
 else:
  pada = 'a' # atmanepada
 if partcode == 'ppp':
  vals = construct_pppart1a(root,theclass,pada,upasargas,dtype,dbg=dbg)
 elif partcode == 'ipp':
  vals = construct_ippart1a_tvA(root,theclass,pada,upasargas,dtype,dbg=dbg)
 elif partcode == 'ippa':
  vals = construct_ippart1a_ya(root,theclass,pada,upasargas,dtype,dbg=dbg)
 elif partcode == 'pap':
  vals = construct_papart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prap':
  vals = construct_prapart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prmp':
  vals = construct_prmpart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prpp':
  vals = construct_prppart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'potp':
  vals = construct_potpart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'rpp':
  vals = construct_rppart1a_stem(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fap':
  vals = construct_fapart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fmp':
  vals = construct_fmpart1a(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fpp':
  vals = construct_fppart1a(root,theclass,pada,upasargas,dbg=dbg)
 else:
  print err0, "ERROR. unknown partcode=",partcode
  exit(1)
 if not isinstance(vals,list):
  vals = [vals]
 vals = flatten(vals)
 return vals

# ----------------------------------------------------------------------------
# participle declensions
# ----------------------------------------------------------------------------
def construct_prapart1a_decl(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of declension records
 """
 err0="construct_prapart1a_decl(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 #if [root,theclass] == ['ard','6']:
 # dbg=True
 if dbg:
  print err0
 if pada != 'p':
  return []
 ans1 = construct_prespart_base_alt_P(root,theclass,pada,upasargas,dbg=dbg)
 ans=[] 
 if dbg:
  print "ans1=",ans1
 def fjoindelt(d):
  """ converts a declension table element to a string.
  """
  if isinstance(d,list):
   return '(' + (' '.join(d)) + ')'
  else: # assume a string
   return d

 for (x,femtype) in ans1:
  #ans.append(x + 't')
  
  for g in ['m','f','n']:
   dtab = declension_pres_part_alt_P((x,femtype),g,theclass,pada,dbg=dbg)
   
   if g in 'mn':
    key1 = x + 't'
   else:
    key1 = dtab[0]
   # there could be variants, expressed as a list
   # Example "ard-6, feminine"
   # For comparison with the Elisp program output, these are 'joined',
   # however, it would be better if they were separate.
   key1 = fjoindelt(key1) 
   key2=key1
   outline_pfx=":prap %s-%s %s:%s:%s" % (root,theclass,g,key1,key2)
   dtab1 = map(fjoindelt,dtab)
   dtab1 = "[" + (' '.join(dtab1)) + "]"
   outline = "%s:%s" %(outline_pfx,dtab1)
   if preserve_elisp_errors:
    # there is something wrong with case upAr 3a. 
    # force an agreement with Elisp
    if (root == 'upAr') and (theclass == '3'):
     outline = outline.replace('uu','U')
   ans.append(outline)
 
 return ans

def declension_pres_part_alt_P(v,g,theclass,pada,dbg=False):
 """ v == (x,femtype), an element in the list returned by
     construct_prespart_base_alt_P
 """
 err0 = "declension_pres_part_alt_P(%s,%s,%s,%s)" %(v,g,theclass,pada)
 if dbg:
  print err0
 code = v[1]
 form = 'prap-%s' % code
 endings = declension_get_sups(g,form)
 weak = v[0]
 if dbg: 
  print "weak=",weak,"endings=",endings
 def fjoin(e1):
  if not isinstance(e1,list):
   e1 = e1.split(',')
  y = map(lambda e: weak+e,e1)
  z = solution(y)
  return z
 ans = map(fjoin,endings)
 return ans

def construct_prmpart1a_decl(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of declension records
 """
 err0="construct_prmpart1a_decl(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 #if [root,theclass] == ['ard','6']:
 # dbg=True
 if dbg:
  print err0
 if pada != 'a':
  return []
 ans1 = construct_prespart_base_alt_A(root,theclass,pada,upasargas,dbg=dbg)
 ans=[] 
 if dbg:
  print "ans1=",ans1
 def fjoindelt(d):
  """ converts a declension table element to a string.
  """
  if isinstance(d,list):
   return '(' + (' '.join(d)) + ')'
  else: # assume a string
   return d

 for x in ans1:
  #ans.append(x + 't')
  for g in ['m','f','n']:
   dtab = declension_pres_part_alt_A(x,g,theclass,pada,dbg=dbg)
   if g in 'mn':
    key1 = x + 'a'
   else:
    key1 = dtab[0]
   # there could be variants, expressed as a list
   # Example "ard-6, feminine"
   # For comparison with the Elisp program output, these are 'joined',
   # however, it would be better if they were separate.
   key1 = fjoindelt(key1) 
   key2=key1
   outline_pfx=":prmp %s-%s %s:%s:%s" % (root,theclass,g,key1,key2)
   dtab1 = map(fjoindelt,dtab)
   dtab1 = "[" + (' '.join(dtab1)) + "]"
   outline = "%s:%s" %(outline_pfx,dtab1)
   if preserve_elisp_errors:
    pass
    # there is something wrong with case upAr 3a. 
    # force an agreement with Elisp
    #if (root == 'upAr') and (theclass == '3'):
    # outline = outline.replace('uu','U')
   ans.append(outline)
 return ans

def declension_pres_part_alt_A(x,g,theclass,pada,dbg=False):
 """ x an element in the list returned by
     construct_prespart_base_alt_A
 """
 err0 = "declension_pres_part_alt_A(%s,%s,%s,%s)" %(x,g,theclass,pada)
 if dbg:
  print err0
 form = 'prmp'
 endings = declension_get_sups(g,form)
 weak = x
 if dbg: 
  print "weak=",weak,"endings=",endings
 def fjoin(e1):
  if not isinstance(e1,list):
   e1 = e1.split(',')
  y = map(lambda e: weak+e,e1)
  z = solution(y)
  return z
 ans = map(fjoin,endings)
 return ans

def construct_prppart1a_decl(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of declension records
 """
 err0="construct_prppart1a_decl(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 #if [root,theclass] == ['ard','6']:
 # dbg=True
 if dbg:
  print err0
 ans1 = construct_passpart_base_alt(root,theclass,pada,upasargas,dbg=dbg)
 ans=[] 
 if dbg:
  print "ans1=",ans1
 def fjoindelt(d):
  """ converts a declension table element to a string.
  """
  if isinstance(d,list):
   return '(' + (' '.join(d)) + ')'
  else: # assume a string
   return d

 for x in ans1:
  for g in ['m','f','n']:
   dtab = declension_pass_part_alt(x,g,theclass,pada,dbg=dbg)
   if g in 'mn':
    key1 = x + 'a'
   else:
    key1 = dtab[0]
   # there could be variants, expressed as a list
   # Example "ard-6, feminine"
   # For comparison with the Elisp program output, these are 'joined',
   # however, it would be better if they were separate.
   key1 = fjoindelt(key1) 
   key2=key1
   cp = "%s%s" %(theclass,pada)
   outline_pfx=":prpp %s-%s %s:%s:%s" % (root,cp,g,key1,key2)
   dtab1 = map(fjoindelt,dtab)
   dtab1 = "[" + (' '.join(dtab1)) + "]"
   outline = "%s:%s" %(outline_pfx,dtab1)
   if preserve_elisp_errors:
    pass
    # there is something wrong with case upAr 3a. 
    # force an agreement with Elisp
    #if (root == 'upAr') and (theclass == '3'):
    # outline = outline.replace('uu','U')
   ans.append(outline)
 return ans

def construct_passpart_base_alt(root,theclass,pada,upasargas,dbg=False):
 err0="construct_passpart_base_alt(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 if dbg:
  print err0
 ans1 =  construct_prppart1a(root,theclass,pada,upasargas,dbg=dbg)
 ans=[]
 for x in ans1:
  # drop the final 'a' 
  ans.append(x[0:-1])
 return ans

def declension_pass_part_alt(x,g,theclass,pada,dbg=False):
 """ x an element in the list returned by
     construct_passpart_base_alt
 """
 err0 = "declension_pass_part_alt(%s,%s,%s,%s)" %(x,g,theclass,pada)
 if dbg:
  print err0
 form = 'prpp'
 endings = declension_get_sups(g,form)
 weak = x
 if dbg: 
  print "weak=",weak,"endings=",endings
 def fjoin(e1):
  if not isinstance(e1,list):
   e1 = e1.split(',')
  #y = map(lambda e: weak+e,e1)
  y = map(lambda e: declension_join(weak,e),e1)
  z = solution(y)
  return z
 ans = map(fjoin,endings)
 return ans

def construct_fapart1a_decl(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of declension records
     Refer to AntoineII, section 193. also Deshpande p. 168
 """
 err0="construct_fapart1a_decl(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 #if [root,theclass] == ['ard','6']:
 # dbg=True
 if dbg:
  print err0
 if pada != 'p':
  return []
 ans1 = construct_fapart1a(root,theclass,pada,upasargas,dbg=dbg)
 ans=[] 
 if dbg:
  print "ans1=",ans1
 def fjoindelt(d):
  """ converts a declension table element to a string.
  """
  if isinstance(d,list):
   return '(' + (' '.join(d)) + ')'
  else: # assume a string
   return d

 for x0 in ans1:
  # remove the final 't'
  x = x0[0:-1]
  femtype = 'sw' 
  for g in ['m','f','n']:
   dtab = declension_pres_part_alt_P((x,femtype),g,'6','p',dbg=dbg)
   if g in 'mn':
    key1 = x + 't'
   else:
    key1 = dtab[0]
   # there could be variants, expressed as a list
   # Example "ard-6, feminine"
   # For comparison with the Elisp program output, these are 'joined',
   # however, it would be better if they were separate.
   key1 = fjoindelt(key1) 
   key2=key1
   outline_pfx=":fap %s-%s %s:%s:%s" % (root,theclass,g,key1,key2)
   dtab1 = map(fjoindelt,dtab)
   dtab1 = "[" + (' '.join(dtab1)) + "]"
   outline = "%s:%s" %(outline_pfx,dtab1)
   if preserve_elisp_errors:
    # there is something wrong with case upAr 3a. 
    # force an agreement with Elisp
    if (root == 'upAr') and (theclass == '3'):
     outline = outline.replace('uu','U')
   ans.append(outline)
 
 return ans

def construct_fmpart1a_decl(root,theclass,pada,upasargas,dbg=False):
 """ returns a list of declension records
     Refer to AntoineII, section 193. also Deshpande p. 168
 """
 err0="construct_fmpart1a_decl(%s,%s,%s,%s)" % (root,theclass,pada,upasargas)
 #if [root,theclass] == ['ard','6']:
 # dbg=True
 if dbg:
  print err0
 if pada != 'a':
  return []
 ans1 = construct_fmpart1a(root,theclass,pada,upasargas,dbg=dbg)
 ans=[] 
 if dbg:
  print "ans1=",ans1
 def fjoindelt(d):
  """ converts a declension table element to a string.
  """
  if isinstance(d,list):
   return '(' + (' '.join(d)) + ')'
  else: # assume a string
   return d

 for x0 in ans1:
  x = x0[0:-1] # remove final 'a'
  for g in ['m','f','n']:
   # can reuse present middle participle declension for future middle
   dtab = declension_pres_part_alt_A(x,g,theclass,pada,dbg=dbg)
   if g in 'mn':
    key1 = x + 'a'
   else:
    key1 = dtab[0]
   # there could be variants, expressed as a list
   # Example "ard-6, feminine"
   # For comparison with the Elisp program output, these are 'joined',
   # however, it would be better if they were separate.
   key1 = fjoindelt(key1) 
   key2=key1
   outline_pfx=":fmp %s-%s %s:%s:%s" % (root,theclass,g,key1,key2)
   dtab1 = map(fjoindelt,dtab)
   dtab1 = "[" + (' '.join(dtab1)) + "]"
   outline = "%s:%s" %(outline_pfx,dtab1)
   if preserve_elisp_errors:
    pass
    # there is something wrong with case upAr 3a. 
    # force an agreement with Elisp
    #if (root == 'upAr') and (theclass == '3'):
    # outline = outline.replace('uu','U')
   ans.append(outline)
 
 return ans

def sl_participle_decl(root,theclass,evoice,partcode,dtype=None,upasargas=None,dbg=False):
 """
 construct various participle declensions based on partcode; main work parcelled
 out to other routines, depending on partcode.
 partcode also follows the conventions of Scharf.

  Assume root and evoice are symbols using
  SLP conventions, class is a number, dtype, if present is one of
  the dervived type symbols. 
 """
 err0 = "sl_participle_decl(%s,%s,%s,%s,%s,%s)" %(root,theclass,evoice,partcode,dtype,upasargas)
 if dbg:
  print err0
 #upasargas = None
 if evoice == 'a':
  pada = 'p' #parasmaipada
 else:
  pada = 'a' # atmanepada
 if partcode == 'ppp':
  vals = construct_pppart1a_decl(root,theclass,pada,upasargas,dtype,dbg=dbg)
 elif partcode == 'pap':
  vals = construct_papart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prap':
  vals = construct_prapart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prmp':
  vals = construct_prmpart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'prpp':
  vals = construct_prppart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'potp':
  vals = construct_potpart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'rpp':
  vals = construct_rppart1a_stem_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fap':
  vals = construct_fapart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fmp':
  vals = construct_fmpart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 elif partcode == 'fpp':
  vals = construct_fppart1a_decl(root,theclass,pada,upasargas,dbg=dbg)
 else:
  print err0, "ERROR. unknown partcode=",partcode
  exit(1)
 if not isinstance(vals,list):
  vals = [vals]
 vals = flatten(vals)
 return vals

def sl_inf(root,theclass,evoice,dtype,dbg=False):
 """
construct infinitive. Assume root and evoice are symbols using
  SLP conventions, class is a number, dtype, if present is one of
  the dervived type symbols. 
  The algorithm recomputes the conjugation table of the periphrastic
  future, then replaces the 'A' of the 3s with 'um'
 """
 err0 = "sl_inf(%s,%s,%s,%s)" %(root,theclass,evoice,dtype)
 if dbg:
  print err0
 sltense = 'pft'  # periphrastic future
 upasargas = None
 ctab = sl_conjtab(root,theclass,evoice,upasargas,sltense,dtype,dbg=dbg)
 ctab= solution(ctab)
 if not isinstance(ctab,list):
  return []
 infs = []
 x = ctab[0] # 3rd singular
 if not isinstance(x,list):
  x = [x]
 for u in x:
  if u.endswith('A'): #usual case
   y= u[0:-1] + 'um'  # replace final 'A' with 'um'
   infs.append(y)
  else:
   print err0," ERROR. 3s of pft doesn't end in 'A'",ctab
   exit(1)
 return infs

def fjoin(tup):
  """ how to join a tuple of alternates """
  tup = flatten(tup) # July 1, 2016
  if not isinstance(tup,(tuple,list)):
   if tup == '': # special handling of missing value. Example is vac, 3p 
    tup = 'nil' 
    return tup
   # ASSUME tup is a string.
   # IT MAY BE (example vid, ipv, 2, a) that this string is a comma-delimited
   #  set of alternates. 
   parts = tup.split(',')
   if len(parts) > 1:
    return fjoin(parts)
   # tup is now a string
   # Do some adjustment for comparison 
   if preserve_elisp_errors:
    tup = re.sub('^uu','U',tup)  # probably bogus upAr,3.
   return tup # a string
  # a list or tup.
  # for perfect, we sometimes (why?) have extra level of lists
  # e.g.  [[u'Ada'], ['jaGAsa']]   (ad 2 a prf) (July 1, 2016)
  #tup = map(solution,tup)
  if len(tup) == 1:
   # case of a tuple or list of length 1
   return fjoin(tup[0])
  else:
   ans = ('('+' '.join(tup) + ')')
   if preserve_elisp_errors:
    data = [
     ('(cacagDa cacaKTa ceGiTa)',
      '(((cacagDa) (cacaKTa)) ceGiTa)'), #caG
     ('(jajambDa jajamPTa jajamBiTa)', 
      '(((jajambDa) (jajamPTa)) jajamBiTa)'), #jaB 1 a prf
     ('(dadagDa dadaKTa deGiTa)',
      '(((dadagDa) (dadaKTa)) deGiTa)'), # daG 5/4a prf
     ('(nanabDa nanaPTa neBiTa)',
      '(((nanabDa) (nanaPTa)) neBiTa)'), # naB prf 1/4/9a
     ('(yayabDa yayaPTa yeBiTa)',
      '(((yayabDa) (yayaPTa)) yeBiTa)'), # yaB prf 1a
     ('(rarabDa raraPTa reBiTa)',
      '(((rarabDa) (raraPTa)) reBiTa)'), # raB prf 1a
     ('(lalawTa lalawWa leqiTa)',
      '(((lalawTa) (lalawWa)) leqiTa)'), # laq prf 1a
     ('(lalabDa lalaPTa leBiTa)',
      '(((lalabDa) (lalaPTa)) leBiTa)'), # laB prf 1a
     ('(sasagDa sasaKTa seGiTa)',
      '(((sasagDa) (sasaKTa)) seGiTa)'),  # saG prf 5a
    ]
    for (old,new) in data:
     ans1 = ans.replace(old,new)
     if ans1 != ans:
      ans = ans1
      break
   return ans
 # end of fjoin

def v_file_init_alt1_pre_helper_adjust(ans,root,theclass,voice,tense,dtype,dbg=False):
 """make a string identical with that generated by the Elisp program
   Handles multiple forms
 """
 if not ans:
  return ans
 if not isinstance(ans,list):
  return ans
 #print "v_file_init_alt1_pre_helper_adjust: ans=",ans
 if not dtype:
  outline_pfx = ":%s %s %s%s" % (root,tense,theclass,voice)
 else:
  outline_pfx = ":%s %s %s%s %s" % (root,tense,theclass,voice,dtype)

 # sometimes, as when there are two bases, ans is an array of conjugation
 # tables: [tab1,tab2]....  (gam,1,a,pre) is an example
 # for comparison with Elisp, it is desired to join these parallel arrays
 # START HERE
 # We assume that ans is either 
 # (a) a single table (a list of length 9) or
 # (b) a list of such tables.
 # The next logic assumes that in case (b), the list is NOT of length 9
 if len(ans) == 9:
  ans1 = [ans]  # case (a)
 else:
  ans1 = ans  # case (b)
 ans2 = zip(*ans1)  
 # this is some Python magic.
 # ans2 is a list of 9 tuples, where x1[i] is a tuple of corresponding
 # i-th elements of the lists in ans1
 ctab = map(fjoin,ans2)
 # now, change this list into a string
 ctab1 = ' '.join(ctab)
 ctab2 = "[%s]" % ctab1
 return "%s:%s" %(outline_pfx,ctab2)

def v_file_init_alt1_pre_helper(root,theclass,voice,tense,dtype=None,dbg=False):
 """ functional equivalent of Elisp
   (v-file-init-alt1-pre-helper root theclass voice tenses &optional dbg)

 """
 #dbg=True
 if dbg:
  tmp="ENTER v_file_init_alt1_pre_helper(%s,%s,%s,%s)" %(root,theclass,voice,tense)
  print tmp
 upas = []
 #dtype= None
 theclass1 = elisp_string_to_number(theclass)
 ans=sl_conjtab(root,theclass1,voice,upas,tense,dtype,dbg)
 if dbg:
  tmp="v_file_init_alt1_pre_helper: ans=\n%s"%ans
  print tmp
 # take care of djustment 
 ansadj = ans
 try:
  ans1 = v_file_init_alt1_pre_helper_adjust(ansadj,root,theclass,voice,tense,dtype,dbg)
 except:
  ans1="Adjustment problem:%s" % ans
 return ans1

def elisp_string_to_number(x):
 """  The Elisp function converts a string to a number-type value.
      For instance "23"-> 23, which is like int("23") in Python.
      However, in future tense testing, sometimes this function is applied
      to strings like "2/4"-> 2.  But Python int("2/4") raises a ValueError.
      So, this Python function is a partial mimicing of the Elisp functionality.
      However, it always returns a string of digits, rather than a python integer.
 """
 m = re.search(r'^([0-9]+)',x)
 if not m:
  return 0
 else:
  return m.group(1)

def test_v_file_init_alt1_pre_helper(filein,dbg=False):
 import codecs
 with codecs.open(filein,"r","utf8") as f:
  teststring=f.read()
 npass=0
 nfail=0
 for x in teststring.splitlines():
  if not (':' in x):
   continue
  if x.startswith(';'):
   continue
  x=x.rstrip('\r\n')
  #(xinput,xanswer)=x.split(':')
  (xinput,xanswer)=x.split('=')
  
  #(root,theclass,voice,tense)=xinput.split(',')
  parts = xinput.split(',')
  (root,theclass,voice,tense) = parts[0:4]
  if len(parts) == 5:
   dtype = parts[4]  # derived type. Currently, only 'c' is known
   dtypestr = ' ' + dtype
  else:
   dtype = None
   dtypestr = ''
  answer = xanswer
  compute=v_file_init_alt1_pre_helper(root,theclass,voice,tense,dtype,dbg)
  if compute == answer:
   npass=npass+1
   status = "PASS"
   #out="v_file_init_alt1_pre_helper(%s,%s,%s,%s) %s" %(root,theclass,voice,tense,status)
   out="v_file_init_alt1_pre_helper(%s) %s" %(xinput,status)
  else:
   nfail = nfail+1
   status = "FAIL"
   if compute == None:
    compute ='NONE'
   if answer == None:
    answer ='NONE'
   #out=("v_file_init_alt1_pre_helper(%s,%s,%s,%s) %s" %(root,theclass,voice,tense,status)) + "\n" +\
   out=("v_file_init_alt1_pre_helper(%s) %s" %(xinput,status)) + "\n" +\
       ("computed=\n%s\n" % compute) + ("Elisp=\n%s\n" % answer)
  print out
 print filein,"#pass=%s, #fail=%s" %(npass,nfail)


def test2(filein,dbg=False):
 test_v_file_init_alt1_pre_helper(filein,dbg)


if __name__ == "__main__":
 import sys
 if len(sys.argv)>1:
  filein = sys.argv[1]
 else:
  filein = "conjugation_test.txt"
 dbg=False
 if len(sys.argv) > 2:
  tmp = sys.argv[2]
  if tmp == '-d':
   dbg=True
 test2(filein,dbg)
