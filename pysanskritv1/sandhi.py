""" sandhi.py
"""

import init
import re

# word_parts is also defined in test1.py and test2.py
def word_parts(citation):
 """ from gram2.el
   return a pair (x,y) where
   x is a list of strings, each of which is either all vowels or all 
    consonants, and whose concatenation is citation and
   y is a string of 'v' and 'c' corresponding.
   Examples: rAma -> [['r','A','m','a'],'cvcv']
   akza -> [['a','kz','a'],'vcv']
   strI -> [['str','I'],'cv']
 """
 parts=[]
 types=[]
 def typeof(x):
  if x in init.consonant_set:
   return "c"
  else:
   return "v"
 prevtype=None
 prevpart=''
 for x in citation:
  t = typeof(x)
  if not prevtype:
   prevpart=x
   prevtype=t
  elif prevtype == t:
   prevpart = prevpart+x
  else:
   parts.append(prevpart)
   types.append(prevtype)
   prevpart=x
   prevtype=t
 # last one
 parts.append(prevpart)
 types.append(prevtype)
 return [parts,''.join(types)]


class SandhiGet(object):
 """  ref sandhi.el  
 """
 def __init__(self,skipreflist):
  """ a list of references, used by method sandhi_pair_main below
  """
  self.skipreflist = skipreflist
 def sandhi_pair(self,t1,t2,condition,action,dbg=False):
  """ Returns empty list when inputs are inappropriate 
  """
  if dbg:
   print "sandhi_pair: %s,%s,%s,%s"%(t1,t2,condition,action)
  n1 = len(t1)
  n2 = len(t2)
  tokans = []
  if (n1==0) or (n2==0):
   return tokans
  v = t2[0] # first character
  if v not in init.SandhiLength.letterdict:
   return tokans
  sandhilens = init.SandhiLength.letterdict[v]
  len1 = sandhilens.len1
  len2 = sandhilens.len2
  ilen1 = 1
  while (ilen1<=len1) and (ilen1<=n1):
   i1 = n1 - ilen1
   ilen1 = ilen1+1
   y1 = t1[i1:]
   x1 = t1[0:i1]
   ilen2 = 1
   while (ilen2<=len2) and (ilen2<=n2):
    i2 = ilen2
    ilen2 = ilen2+1
    y2 = t2[i2:]
    x2 = t2[0:i2]
    v1 = y1
    v2 = x2
    thisans = self.sandhi_pair_main(t1,t2,n1,n2,v1,v2,x1,y2,condition,action)
    if dbg:
     print "sandhi_pair_main(%s,%s,%s,%s,%s,%s)->%s" %(n1,n2,v1,v2,x1,y2,thisans)
    for thisans1 in thisans:
     if thisans1 not in tokans:
      tokans.append(thisans1)
  return tokans

 def sandhi_pair_main(self,t1,t2,n1,n2,v1,v2,x1,x2,condition,action):
  """ returns a list, each element of which is a
    list of strings. Such an element has either 1 string (when
    action = 'join'), or two strings (when action = 'nojoin')
    May return an empty list.
  """
  ans=[]
  pair = "%s-%s" %(v1,v2)
  if pair not in init.SandhiPair.pairdict:
   return ans
  items = init.SandhiPair.pairdict[pair].items 
  # filter items
  for item in items:
   thisans = None
   if item.ref in self.skipreflist:
    continue
   if item.condition != condition:
    continue
   if item.action != action:
    continue
   # x1,w1,w2,x2 are assumed to be strings
   w1 = item.out1
   w2 = item.out2
   if action == 'join':
    thisans = [x1 + w1 + w2 + x2]
   elif action == 'nojoin':
    thisans = [x1 + w1, w2+x2]
   else: # check for internal consistency in value of 'action'
    print "sandhi_pair_main: unknown action",action
    # thisans remains None
   if thisans:
    ans.append(thisans)
  return ans

def lengthen_vowel(tok):
 """
; By Kale (section 19) there is no long form of 'x' (vocalic 'l'), hence
; the long form of 'f' is used when a long form of 'x' is needed.
; Note that Apte has one word beginning with 'X' in fact just the
; one letter, 'X', meaning 'a mother', or 'Shiva'.
 """
 s = init.shortsimplevowel_set 
 # l = init.longsimplevowel_set  # just the uppercase of shortsimplevowel_set
 ans = tok # in case no match, just return the arg
 if tok in s:
  ans = tok.upper()  
 return ans

def lengthen_diphthong(x):
 """ from sandhi.el: lengthen-diphthong
 """
 if x in ['e','E']:
  return 'E'
 elif x in ['o','O']:
  return 'O'
 else:
  return x

def shorten_vowel(x):
 l = init.longsimplevowel_set
 s = init.shortsimplevowel_set
 i = l.find(x)
 if i != -1:
  return s[i]
 else:
  return x

def corresponding_letter(x,s1,s2):
 """ assume s1 and s2 are strings, and that 'x' is a character
     If x appears at index 'i' of s1, return the corresponding letter of s2.
     Otherwise, empty string
 """
 i = s1.find(x)
 if (i != -1) and (i < len(s2)):
  return s2[i]
 else:
  return ''

def sandhi_internal_diphthong_A(word,dbg=False):
 """ 'word' is a string
 """
 x = word
 n = len(x)
 # v is last char of word; if word is empty string, so is v
 v = x[-1:] 
 if v == 'e':
  ans = x[0:-1]+'ay'
 elif v == 'o':
  ans = x[0:-1]+'av'
 elif v == 'E':
  ans = x[0:-1]+'Ay'
 elif v == 'O':
  ans = x[0:-1]+'Av'
 else:
  ans = x
 return ans

def sandhi_internal_a_simplevowel(w1,w2,dbg=False):
 """ w1 and w2 are strings
 """
 n1 = len(w1)
 n2 = len(w2)
 if (n1==0)or(n2==0):
  return(w1 + w2)
 v1 = w1[-1:] # last char of w1
 v2 = w2[0:1] # first char of w2
 if not (v1 in 'aA'):
  return(w1 + w2)
 if not (v2 in init.simplevowel_set):
  return(w1 + w2)
 x1 = w1[0:-1] # drop last char of w1
 x2 = w2[1:] # drop first char of w2
 v = guna(v2)
 return x1 + v + x2

def sandhi_one_final_cons(xin):
 """Antoine 72-1. No Sanskrit word can end with more than one
  consonant. A final compound consonant must be reduced to its
  first member. In particular, this applies to Nominative Singular
  ending 's' joined to nouns ending in consonants.
  Routine returns None unless some change is made in xin.
 """
 n0 = len(xin)
 n = n0
 n2 = None
 more = (1 < n)
 ans=None
 while more:
  n = n - 1
  if (n0<=n) or (n < 0): # Oct 4, 2016
   break
  x = xin[n] # n-th character 
  if x in init.consonant_set: 
   if not n2:
    n2 = n
   n1 = n
  else:
   more = False
 # 
 #if xin == 'prAvfws':
 # print "sandhi-one-final-cons: tokar=%s, n0=%s,n1=%s,n2=%s" %(xin,n0,n1,n2)
 if not n2:
  ans = None # xin does not end in consonant cluster
 elif n1 == n2:
  ans = None # xin ends in a single consonant
 else:
  # n1 < n2
  if xin[n1] == 'r':
   # keep the initial 'r' and next consonant
   if (n1+2)<n0: 
    ans = xin[0:n1+2]
  else:
   # xin[n1] != 'r'.  keep the initial consonant
   ans = xin[0:n1+1]
 return ans

def sandhi_nR(xin,nR_parm=None):
 """ Antoine 17
  When, in the same word, 'n' is preceded by 'f', 'F', 'r', or 'z' and
  followed by a vowel or by 'n', 'm', 'y', or 'v', then it is changed to
  'R' even when there are letters between the preceding 'f' (etc) and 'n'
  provided these intervening letters be vowel, gutturals, labials, 
  'y', 'v', h', or 'M' (anusvAra).
  Returns None if no change
 """
 ifirst = nR_parm
 if not ifirst:
  ifirst = 0
 changed = False
 tokar = xin
 n = len(tokar)
 i = 0
 while ( i < n):
  x1 = tokar[i]
  i = i+1
  if x1 in 'fFrz':
   i1 = i
   i2 = None
   ok = False
   while (i < n):
    x2 = tokar[i]
    i = i+1
    if  (x2 == 'n') and (i < n):
     x3 = tokar[i]
     if (x3 in init.vowel_set) or (x3 in 'nmyv'):
      i = i - 1
      i2 = i
      i = n # break inner while loop
   i = i1
   if i2:
    # found a subsequent "n". Now check intervening letters
    ok = True
    while ok and (i < i2):
     y = tokar[i]
     if (y in init.vowel_set) or (y in init.guttural_set) or (y in init.labial_set) or (y in 'yvhM'):
      i = i + 1
     else:
      ok = False # breaks while loop
    if ok:
     if (ifirst <= i2):
      # make the change
      changed = True
      # recall tokar is a string so next does not work in Python
      #tokar[i2] = 'R'
      # rather:
      tokar = tokar[:i2]+'R'+tokar[i2+1:]
      i = i2 + 1
 if changed:
  return tokar
 else:
  return None

def sandhi_single(x,nR_parm,condition=None,dbg=False):
 """ same as sandhi-single-main of sandhi.el.
  Returns either an adjusted string, or else None if no adjustments made
 """
 ans = None
 if not x:
  return ans
 if isinstance(x,list) or isinstance(x,tuple):
  return ans
 dbg = False
 #if x == 'prAvfws':
 # dbg=True
 if dbg:
  print "sandhi_single enters:",x,nR_parm,condition
 tok = sandhi_nR(x,nR_parm)
 if dbg:
  print "sandhi_nR returns",tok
 if tok:
  changed=True
 else:
  changed=False
  tok = x # copy x into tok
 tok1 = sandhi_one_final_cons(tok)
 if dbg:
  print "sandhi_one_final_cons(%s) returns %s"%(tok,tok1)
 if tok1:
  changed=True
  tok = tok1
 n1 = len(tok)
 v1 = tok[-1:] # last letter of tok
 x1 = tok[0:-1] # all but last letter of tok
 if dbg:
  print "sandhi_single: v1,x1 = ",v1,x1
 if v1 == 's':
  tokans = x1 + 'H' # change final 's' to visarga 'H'
 elif changed:
  tokans = tok
 else:
  tokans = None
 if dbg:
  print "sandhi_single returns:",tokans
 return tokans

def guna(c):
 if c in init.simplevowel_set:
  return init.guna_map[c]
 else:
  return c

def vfdDi1(c):
 if c in init.vfdDi_map:
  return init.vfdDi_map[c]
 else:
  return c

def vfdDi(c):
 if c in init.simplevowel_set:
  return vfdDi1(c)
 else:
  return c

def gunate_final_vowel(word,vfdDiP=False,parts=None):
 """ in sandhi.el
   ; when vrddhiP is nil (or absent) gunaP = t (so we gunate vowel)
   ; when vrddhiP is 't', gunaP = nil (so we vrddhate vowel)  
 """
 if not parts:
  (parts,types) = word_parts(word)
 gunaP = not vfdDiP
 if types.endswith('v'):
  p1 = parts[0:-1] # all but last
  p2 = parts[-1:]  # last
  # presumably, p2 is a list with 1 element, a string which spells a vowel
  v0 = p2[0]
  if gunaP:
   v = guna(v0)
  else:
   v = vfdDi(v0)
  ans = ''.join(p1) + v
 elif types.endswith('vc'):
  p1 = parts[0:-2] # all but the last 2, list of strings
  p2 = parts[-2:-1] # penultimate, a list with 1 string
  v0 = p2[0] # the string (vowel)
  p3 = parts[-1:][0]  # last element, a string (final consonant cluster)
  # only gunate when v0 is a short simple vowel AND
  # the final consonant is not compound
  if (len(p3) == 1) and (v0 in init.shortsimplevowel_set):
   if gunaP:
    v = guna(v0)
   else:
    v = vfdDi(v0)
   v0 = v
  # reconstruct the word
  ans = ''.join(p1) + v0 + p3
 else:
  ans = word
 return ans

def aspirate(a):
 """given an unaspirated character , return
  the corresponding aspirated character
  If 'a' is not found to be an unaspirated consonant, return 'a' unchanged
 """
 # These '_set' values are assumed here to be strings, with the
 # hard and soft letters in the same order
 nonaspirate_set = init.hardnonaspirate_set + init.softnonaspirate_set
 aspirate_set = init.hardaspirate_set + init.softaspirate_set
 i = nonaspirate_set.find(a)
 if i != -1:
  # found an unaspirated character
  return aspirate_set[i]
 return a

def de_aspirate(a):
 """given an aspirated character , return
  the corresponding unaspirated character
  If 'a' is not found to be an aspirated consonant, return 'a' unchanged
hardaspirate_set = 'KCWTP'
softaspirate_set = 'GJQDB'
  Note: in the Elisp version, aspirated consonants (in ITRANS) are 
  those spelled 'Xh' for some X, and the deaspirated spelling drops the 'h'.
  However, the hardpalatal 'ch' (SLP 'c') is not aspirated, and the
  cerebral sibilant 'Sh' (SLP 'z') is not aspirated,
  and the de-aspiration of 'Ch' (SLP 'C') is 'ch'.
  Thus, in addition to the two sets shown above, there is the palatal
  sibilant 'S' (SLP1) (itrans 'sh') which is considerated aspirated (with
  de-aspiration being 's').
 """
 # These '_set' values are assumed here to be strings, with the
 # hard and soft letters in the same order
 nonaspirate_set = init.hardnonaspirate_set + init.softnonaspirate_set + 's'
 aspirate_set = init.hardaspirate_set + init.softaspirate_set + 'S'
 i = aspirate_set.find(a)
 if i != -1:
  # found an aspirated character
  return nonaspirate_set[i]
 return a

def sandhi_legalise_final_cons(base):
 """
 Antoine 72.3. A Sanskrit word (i.e., a verb with its terminations
  or a nominal stem with its case endings) can end only with a vowel or
  with one of the eight following consonants: k T t p ~N n m H.
  All other final consonants must be reduced to one of these eight.
  1. 'h' and palatals are reduced to 'k' or 'T' :
      vaach -> vaak ; samraaj -> samraaT
  2. Cerebrals are reduced to 'T' : praavRiSh -> praavRiT
  3. Dentals are reduced to 't' : suhRid -> suhRit
  4. Labials are redueced to 'p' : kakubh -> kakup
  5. 's' and 'r' are reduced to H: kavis -> kaviH ; pitar -> pitaH

  This function returns a list of strings
 """
 #dbg=True
 ans = sandhi_one_final_cons(base)
 if not ans:
  ans = base
 #if dbg:
 # print "sandhi_legalise_final_cons. base=%s, sandhi_one_final_cons=%s" %(base,ans)
 pfx = ans[0:-1]
 e = ans[-1:] # last char
 if not (e in init.consonant_set):
  pass
 elif e in 'kwtpNnmH':
  pass
 elif e in 'sr':
  ans = pfx + 'H'
 elif (e == 'h') or (e in init.palatal_set):
  ans = [pfx + 'k',pfx + 'w'] # only multiple ans
 elif (e in init.guttural_set):
  ans = pfx + 'k'
 elif (e in init.cerebral_set):
  ans = pfx + 'w'
 elif (e in init.dental_set):
  ans = pfx + 't'
 elif (e in init.labial_set):
  ans = pfx + 'p'
 if not isinstance(ans,list):
  ans = [ans]
 return ans

def duplicate_sandhi_one_final_cons(base):
 """
 Antoine 72-1. No Sanskrit word can end with more than one
  consonant. A final compound consonant must be reduced to its
  first member. In particular, this applies to Nominative Singular
  ending 's' joined to nouns ending in consonants.
  Routine returns None unless some change is made in tokar.
 """
 ans = None
 n0 = len(base)
 n = n0
 more = (1 < n) # if n = 1 or 0, no changes
 n2 = None
 while more:
  n = n - 1
  x = base[n]
  if x in init.consonant_set:
   if not n2:
    n2 = n 
    n1 = n
  else:
   more = False
 if not n2: 
  # does not end in consonant
  ans = None
 elif n1 == n2:
  # ends a single consonant
  ans = None
 else:
  # n1 < n2
  if base[n1] == 'r':
   # keep the initial 'r' and next cons
   if (n1 + 2) < n0:
    ans = base[0:n1+2]
  else:
   # keep the initial consonant
   ans = base[0:n1+1]
 return ans

def reduplicative_pfx(tok,wparts=None):
 """
 ;Antoine2#70. 
 ;Reduplication consists in repeating before a verbal root that
 ;initial portion of it which ends with its first vowel.
 ;Reduplication is subject to special rules:
 ;1. An initial aspirate loses its aspiration in reduplication
 ;  Note: In dealing with the perfect, one encounters reduplication
 ;  of roots starting with 'sh'.  Published examples show that in these
 ;  cases, the 'sh' does not lose its aspiration.
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
 ; (3) Antoine gives an example [s m Ri] -> [s a s m Ri].  My logic
 ;     provides [s i s m Ri]. 
 """
 if wparts:
  (parts,types) = wparts
 else:
  (parts,types) = word_parts(tok)
 if types[0] == 'c':
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
  # 8. 'Ri' and 'RI' become 'i' in reduplication
  if (2 == len(ctok)) and (c in init.sibilant_set) and (ctok[1] in init.hard_set):
   c = ctok[1]
  # 1. initial aspirate loses aspiration
  if c != 'S':
   c = de_aspirate(c)
  # 2. initial guttural changed to palatal. initial 'h' -> 'j'
  if c == 'h':
   c = 'j'
  elif c in init.guttural_set:
   c = corresponding_letter(c,init.guttural_set,init.palatal_set)
  v = vtok[0]
  # 5. shorten the vowel
  v = shorten_vowel(v)
  if (types=='cvc') and (v=='e'): 
   # medial 'e' -> 'i'
   v = 'i'
  elif (types=='cvc') and (v in 'oO'):
   # medial o,O -> u
   v = 'u'
  elif (types=='cv') and (v in 'eEoO'):
   # final e,o,O -> 'a'    (QUESTION: E also?)
   v = 'a'
  elif v in 'fF':
   # f,F -> i
   v = 'i'
  ans = c + v
 elif types[0] == 'v':
  # v or vc
  # 8. 'Ri' and 'RI' become 'i' in reduplication
  # For the conj-7 verb 'Ri', we return [i y]
  vtok = parts[0] 
  v = vtok[0]  # this is just first char of a string. So, normally v == vtok
  if (tok == 'f'):
   ans = 'iy'
  elif v in 'fF':
   ans = 'i'
  else:
   ans = v
 else:
  print "reduplicative_pfx: UNEXPECTED FORM: %s, parts=%s, types=%s" %(tok,parts,types)
  ans = tok
 return ans

def sandhi_internal_diphthong_a(xin):
 """ from sandhi.el. 
 """
 x = xin
 n = len(x)
 v = x[-1:]  # last character
 x0 = x[0:-1]
 if v == 'e':
  return x0 + 'a' + 'y'
 if v == 'o':
  return x0 + 'a' + 'v'
 if v == 'E':
  return x0 + 'A' + 'y'
 if v == 'O':
  return x0 + 'A' + 'v'
 return x # otherwise

 
