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
 if not (voice in ['active','passive']):
  voice = 'active'
 lc = tense[-1:] 
 if lc in '1234567':
  lcnum = int(lc)
 else:
  lcnum = 0
 bases = causal_bases_gentense(root,theclass,pada,upasargas,tense,voice,dbg=dbg)
 if tense in ['liw-p','luw','lfw','lfN','ASIrliN','luN3']:
  causalclass = '11'
  ctabs = []
  for base in bases:
   if (tense == 'liw-p'): # pft = periphrastic perfect
    ctab = conjugation_tab_luw(upasargas,causalclass,pada,root,voice,dbg)
   elif (tense == 'luw'): # pft = periphrastic future
    ctab = conjugation_tab_luw(upasargas,causalclass,pada,root,voice,dbg)
   elif (tense == 'lfw'): # fut = simiple future
    ctab = conjugation_tab_lfw(upasargas,causalclass,pada,root,voice,dbg)
   elif (tense == 'lfN'): # con = conditional
    ctab = conjugation_tab_lfN(upasargas,causalclass,pada,root,voice,dbg)
   elif (tense == 'ASIrliN'): # ben = benedictive
    ctab = conjugation_tab_ASIrliN(upasargas,causalclass,pada,root,voice,dbg)
   elif tense == 'luN3':
    ctab = conjugation_tab_aorist(upasargas,causalclass,pada,root,lcnum,voice)
   ctabs.append(ctab)
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
 causalclass='11'
 if not (voice in ['active','passive']):
  voice = 'active'
 lc = tense[-1:] 
 if lc in '1234567':
  lcnum = int(lc)
 else:
  lcnum = 0
 lc = root[-1:]
 if (lc in init.vowel_set) and (tense == 'luN3'):
  # raise an error. Logic is known to be faulty
  # In Python version, I skip this. and compute anyway
  pass
 nilpada = ''
 if (voice == 'active') and (tense == 'luN3'):
  bases0 = aorist_causal_base(root,theclass,pada,upasargas,None)
  bases = map(lambda x: 'a'+x,bases0)
 elif (voice == 'passive') and (tense == 'luN3'):
  ctab = conjugation_tab_aorist(upasargas,causalclass,pada,root,3,voice)
  ctab3s = ctab[0]  # 3rd singular
  tok = ctab3s[0:-1] # drop the final 'i' of passive
  bases = [tok]
 elif (voice == 'active'):
  bases = causal_base1b(root,theclass,nilpada,upasargas,voice)
 elif (voice == 'passive'):
  if tense in ['liw-p','luw','lfw','lfN','ASIrliN']:
   voice1 = 'active'
   bases =  causal_base1b(root,theclass,nilpada,upasargas,voice1)
  else:
   bases = causal_base1b(root,theclass,nilpada,upasargas,voice)
 # In some Kale shows an alternate form with the causal 'ay' dropped
 if (voice == 'passive'):
  if tense in ['luw','lfw','lfN','ASIrliN']:
   bases1 = bases
   bases = []
   for base in bases1:
    tok = base[0:-2]
    if tok not in bases:
     bases.append(tok)
 elif (voice == 'active'):
  if (tense == 'ASIrliN') and (pada == 'p'):
   bases1 = bases
   bases = []
   for base in bases1:
    tok = base[0:-2]
    if tok not in bases:
     bases.append(tok)
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
  ans = b + b1
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
 """
 if b[-3:-2] in 'Aa':
  ans = b[0:-2] + 'p' + b[-2:]
 else:
  ans = b
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
