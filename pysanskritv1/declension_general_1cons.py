""" declension_general_1cons.py
 1. declension-pres-part-P and declension-pres-part-A (and their related
 functions) are not implemented. Conjugation tables must be done first.
 2. (rAjan f) constructs nom.sg. as rAjnI. Although this agrees with
    Elisp version, it is wrong, as it should of course be rAjYI.
 3. (June 27, 2016).  See icase=4 for declension_general_1cons_s
    Example: apuMs,m,a-puMs.  Note this should be done like puMs, (which is
    irregular) and then prepend the 'a'.
"""

from sandhi import *
import init
preserve_elisp_errors=True

def declension_get_sups(gender,form):
 """ Utility function. Repeated from test1.py (Ugh! - needs refactoring)
 """
 supkey = "%s-%s" %(gender,form)
 if supkey in init.Sup_data.d:
  sups = init.Sup_data.d[supkey]
  return sups.endings
 err = "declension_get_sups(%s,%s)" %(gender,form)
 raise NameError(err)

def declension_join(base,sup,dbg=False):
 """ functional equivalent of Elisp
  (declension-join base sup) in gram3.el
  Assume base and sup are strings.
 """

 sandhiget = SandhiGet(['Antoine72-4'])
 err0 = 'declension_join(%s,%s)' %(base,sup)
 if base == '':
  err="%s: ERROR. Empty base" % err0
  raise NameError(err)
 dbg=False # Normally don't want this level of detail
 if dbg:
  print err0
 ans = None
 x = sandhiget.sandhi_pair(base,sup,'internal','join')
 if len(x)==1:
  ans = x[0][0]
  if dbg:
   print "declension_join using internal,join,ans=",ans
 if ans == None:
  x=sandhiget.sandhi_pair(base,sup,None,'join')
  if len(x) == 1:
   ans = x[0][0]
   if dbg:
    print "declension_join using None,join,ans=",ans
 if (ans == None) and base.endswith('H'):
  # for nouns ending in 's', e.g. for 'sumanas', use 'nojoin'
  # example base=='sumanas', sup =='BiH'
  x = sandhiget.sandhi_pair(base,sup,None,'nojoin')
  if len(x) == 1:
   # result is pair: ['sumano','BiH']
   # for current purpose, we must join these two parts
   #y = x[0][0]
   y = x[0]
   ans = y[0]+y[1]
   if dbg:
    print "declension_join(%s,%s): x=%s, y=%s, ans=%s" %(base,sup,x,y,ans)
 if (ans == None): 
  # concatenate base and sup
  ans = base+sup
  if dbg:
    print "declension_join uses concatenation: (%s +%s => %s)" %(base,sup,ans)
 #----------------------
 # change 4-17-04. To (hopefully) inhibit unwanted 'n-N' changes.
 # For instance, 'raghunandanaH' should not be changed to
 # 'raghuNandanaH'; this is because of some inhibition of application
 # of this sandhi rule across compound boundaries - the details are
 # not clear but P. Scharf asserts.
 #----------------------
 if base.endswith('SatruGn'):
  nR_parm = len(base)
 else:
  nR_parm = len(base) - 1
 # override this. June 26, 2016.
 # Then, change mind again. It is needed. (e.g. karin, to get the n->R conversions)
 #nR_parm = len(base)
 try:
  ans1 = sandhi_single(ans,nR_parm)
 except:
  ans1 = None
  print err0,": sandhi_single(error):",ans,nR_parm
 if dbg:
  if ans1 != ans:
   print "declension_join(%s,%s), sandhisingle(%s,%s)->%s" %(base,sup,ans,nR_parm,ans1)
 if ans1:
  ans = ans1
 if dbg:
  print "declension-join: %s + %s => %s" %(base,sup,ans)
 return ans

def declension_general_1(base,sup,dbg=False):
 """ functional equivalent of Elisp
  (declension-general_1 base sup) in gram3.el
  Assume base and sup are strings.
  Alternate forms are separated by commas
 """
 dbg = False # normally don't want to see this level of detail, even in dbg.
 err0 = "declension_general_1(%s,%s)" %(base,sup)
 if dbg:
  print err0
 if isinstance(base,list): # June 26, 2016
  altbases = base
 else:
  altbases = base.split(',') # usually 1
 if isinstance(sup,list): # July 16, 2016
  altsups=sup
 else:
  altsups = sup.split(',')
 nosup = "_"
 ans = []
 for altbase in altbases:
  for altsup in altsups:
   temp = declension_join(altbase,altsup,dbg)
   if altsup == nosup:
    tempstr=nosup
   elif temp:
    tempstr = "%s" % temp
   else:
    tempstr = "NONE"
   ans.append(tempstr)
 ans1 = ','.join(ans)
 if dbg:
  print "declension_general_1: %s + %s -> %s" %(base,sup,ans1)
 return ans1

def declension_general_2(pratipadika,sups,irregs,dbg=False):
 """ This function not in Elisp code.
  It is added to avoid some repetitions in the existing Elisp code.
  It constructs declension by joining the pratipadika to all the sups,
  taking into account overrides from the parallel irregs.
  The logic is essentially the same as in declension_general EXCEPT that
  the sups are assumed to have been previously computed
 """
 err0 = "declension_general_2(%s,%s,%s)" %(pratipadika,sups,'irregs') #'irregs')
 if dbg:
  print err0
 nans = len(sups) # 24
 ans = []
 for i in xrange(0,nans):
  sup = sups[i]
  if irregs and (irregs[i]) and (irregs[i]!=''):
   ans1 = irregs[i]
  elif sup == '_':  # code for missing
   ans1 = sup
  else:
   ans1 = declension_general_1(pratipadika,sup,dbg=dbg)
  ans.append(ans1)
 if dbg:
  print "declension_general_2 returns",ans
 return ans

def declension_general_3(basetoks,sups,irregs,dbg=False):
 """ This function not in Elisp code.
  It is added to avoid some repetitions in the existing Elisp code.
  It constructs declension by joining correspond basetoks and sups,
  taking into account overrides from the parallel irregs.
  The logic is essentially the same as in declension_general EXCEPT that
  the sups are assumed to have been previously computed
 """
 err0 = "declension_general_3(%s,%s,%s)" %(basetoks,sups,'irregs')
 if dbg:
  print err0
 nans = len(sups) # 24
 ans = []
 for i in xrange(0,nans):
  if irregs and (irregs[i]) and (irregs[i]!=''):
   ans1 = irregs[i]
  else:
   try:
    ans1 = declension_general_1(basetoks[i],sups[i],dbg)
   except:
    tmp = "ERROR: declension_general_3(%s,%s,%s)\n" %(basetoks,sups,irregs)
    tmp = " Error at i=%s, calling declension_general_1(%s,%s)" % (i,basetoks[i],sups[i])
    raise NameError(tmp)
  ans.append(ans1)
  if dbg:
   print "declension_general_3: i=%s, %s + %s => %s" %(i,basetoks[i],sups[i],ans1)
 if dbg:
  print err0," => ",ans
 return ans

#-------------------------------------------------------
def declension_general_mat(citation,pratipadika,gender,form,irregs,dbg=False):
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = pratipadika
 wtok = tok+"at"
 if pratipadika == "mah":
  stok = tok + "Ant"
 else:
  stok = tok + "ant"
 if gender == 'm':
  nomsing = tok + "Ant"
  basetoks = [
   nomsing, stok, stok,
   stok, stok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   stok, stok, stok
  ]
 elif gender == 'n':
  basetoks = [
   wtok, wtok, stok,
   wtok, wtok, stok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, stok
  ]
 elif gender == 'f':
  basetoks = [  # all weak
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok,
   wtok, wtok, wtok
  ]
 return declension_general_3(basetoks,sups,irregs,dbg)
 """
 ans = []
 for i in xrange(0,nsups):
  sup = sups[i]
  irreg = irregs[i]
  base = basetoks[i]
  if irreg:
   elt = irreg
  else:
   elt = declension_general_1(base,sup,dbg)
  ans.append(elt)
 return ans
 """

def declension_general_vat(c,p,g,f,i,dbg=False):
 return declension_general_mat(c,p,g,f,i,dbg)

def declension_general_vat_adj(c,p,g,f,i,dbg=False):
 return declension_general_mat(c,p,g,f,i,dbg)

def declension_general_mat_adj(c,p,g,f,i,dbg=False):
 return declension_general_mat(c,p,g,f,i,dbg)

def declension_general_in(citation,pratipadika,gender,form,irregs,dbg=False):
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = pratipadika
 wtok = tok+"i"
 stok = tok + "in" #  before ALL vowel case endings 
 if gender == 'm':
  nomsing = wtok
  basetoks = [
   nomsing, stok, stok,
   stok, stok, stok,
   stok, wtok, wtok,
   stok, wtok, wtok,
   stok, wtok, wtok,
   stok, stok, stok,
   stok, stok, wtok,
   stok, stok, stok
  ]
 elif gender == 'n':
  xtok = tok + 'In'
  basetoks = [
   wtok, stok, xtok,
   wtok, stok, xtok,

   stok, wtok, wtok, # same as m.
   stok, wtok, wtok,
   stok, wtok, wtok,
   stok, stok, stok,
   stok, stok, wtok,

   wtok, stok, xtok
  ]
 elif gender == 'f':
  basetoks = [stok]*nsups
 return declension_general_3(basetoks,sups,irregs,dbg)
 """
 ans = []
 for i in xrange(0,nsups): # same as for _mat. Refactor?
  sup = sups[i]
  irreg = irregs[i]
  base = basetoks[i]
  if irreg:
   elt = irreg
  else:
   elt = declension_general_1(base,sup,dbg)
  ans.append(elt)
 return ans
 """

def declension_general_in_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 return declension_general_in(citation,pratipadika,gender,form,irregs,dbg)

def declension_general_vas_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 """ Reduplicated perfect partciple active in 'vas'
     Three stems
     Antoine I, p. 98
 """
 #print "citation,pratipadika = %s,%s" % (citation,pratipadika)
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = citation
 # cakfvas  jagmivas vidvas
 ptok = tok
 ptok1 = ptok[0:-3]  # drop the ending 'vas'
 stok = ptok1 + 'vAMs' # strong
 mtok = ptok1 + 'vat'  # medium
 if ptok1.endswith('i'): # jagmivas
  wtok = ptok1[0:-1] + 'uz' 
 else: #cakfvas, vidvas
  wtok = declension_join(ptok1,'uz')
 if gender == 'm':
  nomsing = ptok1 + 'vAn'
  vocsing = ptok1 + 'van'
  basetoks = [
   nomsing, stok, stok,
   stok, stok, wtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, mtok,
   vocsing, stok, stok
  ]
 elif gender == 'n':
  basetoks = [
   mtok, wtok, stok,
   mtok, wtok, stok,

   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, mtok,

   mtok, wtok, stok
  ]
 elif gender == 'f':
  basetoks = [wtok]*nsups
 return declension_general_3(basetoks,sups,irregs,dbg)

def declension_general_an_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 """
 """
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = citation
 ptok = tok
 ptok1 = ptok[0:-2]  # drop the ending 'an'
 stok = ptok1 + 'An'
 stok1 = stok
 if (citation in ['pUzan','aryaman']) or ptok.endswith('han'):
  # Kale section 111
  stok1 = ptok1 + 'an'
 mtok = ptok1 + 'a'
 wtok1 = ptok
 # the usual weak stem drops the 'a' in 'an'
 # In the masculine, sandhi may occur when
 # the remaining dental 'n' combines with what precedes it, e.g.,
 # rAj + n -> rAjY
 # In the neuter, the [n] is joined without Sandhi. 
 # Reason (03-16-03): Formerly, Sandhi was also applied in the neuter
 # and in all regular neuter nouns in 'an', this means joining [... m] with
 # [n] and there are no sandhi changes applicable.
 # However, (Antoine, Vol II Chapter 5) two irregular neuter nouns
 # (asthi and sakthi) in cases 3-7 before vowel terminations are
 # declined as if they were 'asthan' and 'sakthan', and
 # the sandhi combination 'asT' + 'n' -> 'asDn' , and
 # 'sakT' + 'n' -> 'sakDn'.
 # However, this is incorrect 
 # according to Antoine and Kale (Section 126).
 # The similarly irregular 'akzi', which is declined before
 # vowel endings like 'akzan', is also handled correctly as follows:
 # The combination 'akzn' is joined to (say for case 3S) the 
 # ending 'A', by the routine 'declension-general-1', which converts
 # 'akzn'+'A'  to the correct 'akzRA'
 wtok = None
 if ptok.endswith('han'):
  # Kale p. 76 example
  wtok = ptok1[0:-1] + 'Gn'
 if (wtok == None) and (gender == 'm'):
  sandhiget = SandhiGet([])
  x = sandhiget.sandhi_pair(ptok1,'n',None,'join')
  #print "chk: x=%s,ptok1=%s" %(x,ptok1)
  if x != []:
   try:
    wtok = x[0][0]
   except:
    err = "declension_general_an_adj: problem from sandhi_pair: ptok1=%s, x=%s" %(ptok1,x)
    raise NameError(err)
 #print "chk1: wtok=",wtok
 if (wtok == None):
  wtok = ptok1 + 'n'
 if  (4 < len(ptok)) and ptok.endswith(('man','van')) and (ptok[-4:-3] in init.consonant_set):
  # (4 < len(ptok)) is implicit, but not required in Python 
  # stems ending in 'man' and 'van' preceded by a consonant always
  # form their weak stem in 'an' and not in 'n'
  wtok = wtok1
 if gender == 'm':
  nomsing = ptok1 # 'rAj' + 'A' -> 'rAjA'
  vocsing = ptok  # 'rAjan' + '' -> 'rAjan'
  if wtok == wtok1:
   locsing = wtok
  else:
   locsing = [wtok,wtok1] #"%s,%s" %(wtok,wtok1)  # alternate forms
  basetoks = [
   nomsing, stok1, stok1,
   stok1, stok1, wtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   locsing, wtok, mtok,
   vocsing, stok1, stok1
  ]
 elif gender == 'n':
  if wtok == wtok1:
   locsing = wtok
  else:
   locsing = [wtok,wtok1] # "%s,%s" %(wtok,wtok1)  # alternate forms
  basetoks = [
   mtok, locsing, stok,
   mtok, locsing, stok,

   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   locsing, wtok, mtok,

   mtok, locsing, stok
  ]
 elif gender == 'f':
  basetoks = [wtok]*nsups
 return declension_general_3(basetoks,sups,irregs,dbg)

def declension_general_an(citation,pratipadika,gender,form,irregs,dbg=False):
 form = 'an'
 return declension_general_an_adj(citation,pratipadika,gender,form,irregs,dbg)

def declension_general_ac_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 """  adjectives of direction ending in 'ac'.  In MW, these are spelled
   ending in 'aYc'
   Examples, pratyac, anvac, udac

 """
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = citation
 ptok = tok
 ptok1 = ptok[0:-2]  # drop the ending 'ac'
 stok = ptok1 + 'aYc'
 # note: Antoine says middle ends in 'ac'; these are endings
 # preceding consonants; Kale asserts they are nouns with 1 stem
 # ending in 'c' in these spots, and this means we should change
 # 'c' to 'g' here.
 mtok = ptok1 + 'ag'
 if ptok == 'ac':
  wtok = ptok
 elif ptok == 'tiryac':
  wtok = ptok1[0:-1] + 'aSc'
 elif ptok1.endswith('v'): 
  # anvac -> anUc
  wtok = ptok1[0:-1] + 'Uc'
 elif ptok1.endswith('y'):
  # pratyac -> pratIc
  wtok = ptok1[0:-1] + 'Ic'
 else:
  # udac -> udIc
  wtok = ptok1 + 'Ic'
 #
 if gender == 'm':
  nomsing = ptok1 + 'a' #
  vocsing = nomsing  # 
  locplu = ptok1 + 'ak'
  basetoks = [
   nomsing, stok, stok,
   stok, stok, wtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, locplu,
   vocsing, stok, stok
  ]
 elif gender == 'n':
  nomsing = ptok1 + 'a' 
  vocsing = nomsing  # 
  locplu = ptok1 + 'ak'
  basetoks = [
   nomsing, wtok, stok,
   nomsing, wtok, stok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, locplu,
   vocsing, wtok, stok
  ]
 elif gender == 'f':
  basetoks = [wtok]*nsups
 return declension_general_3(basetoks,sups,irregs,dbg)

def declension_general_Ac_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 """ adjectives of direction ending in 'Ac'.  such as pra-ac => prAc. MW 
     spells these ending in 'AYc'
     The endings are the same as for 'ac'.
     The code might be able to be refactored, so that it is more
     similar to the ac_adj code.
 """
 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = citation
 ptok = tok
 ptok1 = ptok[0:-1]  # drop the ending 'c'. prA
 stok = ptok1 + 'Yc' # prAYc
 wtok = ptok1 + 'c'  # prAc
 # note: Antoine says middle  = weak, thus ends in 'c'; these are endings
 # preceding consonants; Kale asserts they are nouns with 1 stem
 # ending in 'ch' in these spots, and this means we should change
 # 'c' to 'g' here.
 mtok = ptok1 + 'g' # prAg
 #
 if gender == 'm':
  nomsing = ptok1  #
  vocsing = nomsing  # 
  locplu = ptok1 + 'k'
  basetoks = [ # identical configuration to ac_adj
   nomsing, stok, stok,
   stok, stok, wtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, locplu,
   vocsing, stok, stok
  ]
 elif gender == 'n':
  nomsing = ptok1
  vocsing = nomsing  # 
  locplu = ptok1 + 'k'
  basetoks = [  # identical configuration to ac_adj
   nomsing, wtok, stok,
   nomsing, wtok, stok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, mtok, mtok,
   wtok, wtok, wtok,
   wtok, wtok, locplu,
   vocsing, wtok, stok
  ]
 elif gender == 'f':
  basetoks = [wtok]*nsups
 return declension_general_3(basetoks,sups,irregs,dbg)

#-----------------------------------------------------------------
def declension_pron(citation,pratipadika,gender,form,dbg=False):
 """ This appears to be used ONLY with citation = Bavat
 """
 if citation == "Bavat":
  # pronoun 'Bavat' is declined like adjective 'Bagavat'
  # It has irregular form 'BoH' in 8s (MW- bhos)
  irregs = [None]*24
  if gender == 'm':
   irregs[21] = "Bavan,BoH"
  ans = declension_general_vat(citation,pratipadika,gender,"vat",irregs,dbg)
  return ans

 raise NameError('declension_pron(%s,%s,%s,%s)'%(citation,pratipadika,gender,form))

def declension_general_Iyas_adj(citation,pratipadika,gender,form,irregs,dbg=False):
 """ Antoine I (79.3, p. 81)
     comparative in Iyas ('Iyasun') CAse of Noun with two stems.
     For a list of these comparatives, see section 93 (p. p4-5)
 """
 if dbg:
  tmp = "ENTER declension_general_Iyas(%s,%s,%s,%s,%s)" % (citation,pratipadika,gender,form,irregs)
  print tmp

 sups = declension_get_sups(gender,form)
 nsups = len(sups)
 tok = pratipadika
 pratipadikatok = tok
 wtok = tok + 'aH'  # before weak cons endings
 stok = tok + 'AMs' # before all strong endings
 vtok = tok + 'as'  # before weak vowel endings
 if gender == 'm':
  nomsing = tok + 'Ant'
  vocsing = tok + 'an'
  basetoks = [
   nomsing, stok, stok,
   stok, stok, vtok,
   vtok, wtok, wtok,
   vtok, wtok, wtok,
   vtok, wtok, wtok,
   vtok, vtok, vtok,
   vtok, vtok, wtok,
   vocsing, stok, stok
  ]
 elif gender == 'n':
  basetoks = [
   wtok, vtok, stok,
   wtok, vtok, stok,

   vtok, wtok, wtok, # same as m.
   vtok, wtok, wtok,
   vtok, wtok, wtok,
   vtok, vtok, vtok,
   vtok, vtok, wtok,

   wtok, vtok, stok
 ]
 elif gender == 'f':
  basetoks = [vtok]*nsups # 
 return declension_general_3(basetoks,sups,irregs,dbg)
 """
 ans = []  # rest is same as for _mat. Refactor ?
 if not irregs:
  irregs=[None]*nsups
 for i in xrange(0,nsups):
  sup = sups[i]
  irreg = irregs[i]
  base = basetoks[i]
  if irreg:
   elt = irreg
  else:
   elt = declension_general_1(base,sup,dbg)
  ans.append(elt)
 return ans
 """
# ------------------------------------------------------------------------
# nouns ending in consonant with 1 stem (_1cons)
# ------------------------------------------------------------------------

def declension_1cons_finish(basetoks,gender,irregs,dbg=False):
 """ The basetoks is a list of 24  base forms, waiting to be joined to
     endings. Picking the endings and doing guiding the joining is the
     task of this function
 """
 err0 = "declension_1cons_finish(%s,%s,%s)" %(basetoks,gender,'irregs')
 if dbg:
  print err0
 sups = declension_get_sups(gender,'1cons')
 ans = declension_general_3(basetoks,sups,irregs,dbg)
 return ans
 """
 nsups = len(sups)
 if not irregs:
  irregs = [None]*nsups
 if dbg:
  print "declension_1cons_finish.sups=\n",sups
 ans = []
 for i in xrange(0,nsups):
  sup = sups[i]
  irreg = irregs[i]
  base = basetoks[i]
  if irreg:
   elt = irreg
  else:
   elt = declension_general_1(base,sup,dbg)
  ans.append(elt)
 return ans
 """

def declension_general_1cons(citation,pratipadika,gender,newform,irregs,dbg=False):
 err0 = "declension_general_1cons(%s,%s,%s,%s,%s)" %(citation,pratipadika,gender,newform,'irregs')
 if dbg:
  print err0
 # details depend on the ending consonant
 lc = citation[-1:]
 if lc in 'tTdD':
  lc = 't'  # all these the same
 elif lc in 'pPbB':
  lc = 'p'
 elif lc in 'kKgG':
  lc = 'k'
 localdict = globals()
 fcnname = "declension_general_1cons_%s" % lc
 if fcnname not in localdict:
  # next follows that of Elisp
  print err0,": unexpected ", citation,gender
  return None
  """
  err = "Function  Not implemented: %s" % fcnname
  err = err + " ; citation=%s" % citation
  raise NameError(err)
  """
 fcn = localdict[fcnname]
 # note that pratipadika and newform parameters are unused
 """
 dbg=False
 if False:
  dbg = True
 """
 return fcn(citation,gender,irregs,dbg=dbg)

def declension_general_1cons_s(citation,gender,irregs,dbg=False):
 """
    ; 77(6) Nouns with stems ending in 's'
    ; Final 's' becomes visarga in 1S and before 'bhyaam', 'bhiH' and 'bhyaH',
    ; and that 'H' follows visarga sandhi rules in combining with the
    ; terminations,
    ; except that final 's' remains before terminations beginning with vowels,
    ; being changed to 'Sh' if preceding vowel is other than 'a' or 'aa'.
    ; M and F nouns in 'as' lengthen the 'a' in 1S.

 """
 last = citation[-1:] # s
 tok1 = citation
 tok2 = citation
 vowel = citation[-2:-1] # penultimate character
 v = None # for case 4
 #gender = gender.lower()
 if (vowel == 'a') and (gender in ['m','f']):
  v = lengthen_vowel(vowel)
  tok2 = tok1[0:-2] + v + 'H'
 else:
  tok2 = tok1[0:-1]+'H' 
 #
 x = tok1
 y = tok2
 icase=0
 if (vowel in ['a','A']) and (gender in ['m','f']):
  icase=1
  # The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
  z = x[0:-1]+'H'
  basetoks = [
       y, x, x,
       x, x, x,
       x, z, z,
       x, z, z,
       x, z, z,
       x, x, x,
       x, x, z,
       x, x, x
  ]
 elif (gender == 'n'):
  icase=2
  # The neuter nouns in 'as', 'is', and 'us' lengthen the 'a', 'i',
  # and 'u' and insert a nasal (anusvaara) in 1P 2P 8P
  # The nouns in 'is' and 'us' become 'iz' and 'uz' before
  # vowel endings (other than 1P 2P 8P)
  v = lengthen_vowel(vowel)
  z = x[0:-1]+'H'
  if (vowel == 'a'):
   last1 = last
  else:
   last1 = 'z'
  w = x[0:-2]+v+'M'+last1
  j = x[0:-2]+vowel + last1
  basetoks = [
       y, j, w,
       y, j, w,
       j, z, z,
       j, z, z,
       j, z, z,
       j, j, j,
       j, j, z,
       x, j, w
  ]
 elif (vowel in init.vowel_set):
  icase=3
  # There do not seem to be many M/F nouns ending in 's' and preceded
  # by a non-a.  The irregular noun 'dos' (m. arm) (Antoine2-#78) is
  # declined in part like a noun with one stem.
  # The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
  z = x[0:-1] + 'H'
  if (vowel == 'a'): #never occurs, due to previous two cases
   last1 = last
  else:
   last1 = 'z'
  j = x[0:-2]+ vowel + last1
  basetoks = [
       y, j, j,
       j, j, j,
       j, z, z,
       j, z, z,
       j, z, z,
       j, j, j,
       j, j, z,
       x, j, j
  ]
 else:
  icase=4
  # not sure what cases go here
  if v == None:
   v = 'nil'  # this is goofy, but is what Elisp does.
  z = x[0:-2]+v+'r'
  if (gender == 'n'): # this case should never occur
   y1 = y
  else:
   y1 = x
  basetoks = [
       y, x, x,
       y1, x, x,
       x, z, z,
       x, z, z,
       x, z, z,
       x, x, x,
       x, x, z,
       y, x, x
  ]
 # now finish
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 if dbg:
  print "declension_general_1cons_s for ",citation,gender
  print " icase=",icase
  print " basetoks=",basetoks
  print " ans=",ans
 return ans

def declension_general_1cons_l(citation,gender,irregs,dbg=False):
 """
 """
 last = citation[-1:]  # last character 
 tok1 = citation[0:-1]  # all but last character
 tok2 = citation
 # Kale#89a, p. 54
 # After final 'l', the 's' of the loc. pl. is changed to 'z'
 # Example 'kamal' (not in MW)
 # This rule impacts the 'sup', not the basetok
 # In this logic, rather than take a different sup list, this routine
 # does the adjustment
 basetoks = [tok2]*24
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 # adjust 20 (loc. pl.)
 ans[20] = tok2 + 'zu'
 return ans

def declension_general_1cons_r(citation,gender,irregs,dbg=False):
 """
 """
 last = citation[-1:]  # last character 
 tok1 = citation[0:-1]  # all but last character
 tok2 = citation
 # Antoine
 # 77(5) Nouns with stems ending in 'r'. Examples dvAr,f.  gir,f.
 # Final 'r' is changed to 'H' (visarga) in 1S.
 # A short vowel preceding 'r' is lengthened in 1S and
 #  before 'ByAm', 'BiH' and 'ByaH'
 vowel = citation[-2:-1] # penultimate character
 v = lengthen_vowel(vowel)
 tok2 = citation[0:-2] + v + 'H'
 x = citation
 y = tok2
 z = citation[0:-2] + v + 'r'
 if (gender == 'n'):
  y1 = y
 else:
  y1 = x
 basetoks = [
       y, x, x,
       y1, x, x,
       x, z, z,
       x, z, z,
       x, z, z,
       x, x, x,
       x, x, z,
       y, x, x
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_c(citation,gender,irregs,dbg=False):
 """
  Note some cases ending in 'j' are mentioned in the logic of this
  function.  Not sure why. Also, some logic is 
 """
 last = citation[-1:]  # last character 
 tok1 = citation[0:-1]  # all but last character
 tok2 = citation
 if tok2.endswith(('vraSc','Brasj','vfSc')):
  # (?) from SL. I think the sibilant disappears in front of consonants
  tok2 = tok1[0:-1] + 'w'
 elif tok2.endswith(('vraSc','Brasj','vfSc','sfj','yaj','rAj','BrAj')):
  # Note first three cases handled by above 'if' clause
  # Also, last cases are presumably inapplicable, as 'citation' ends in 'c',
  # not 'j'
  # Kale 94(b) Pan VIII.2.36
  # The ending 'S' or 'C' of root-nouns and the final
  # of 'vraSc Brasj sfj mfj rAj and BrAj'
  # are changed to 'z' when followed 
  # by any consonant except a nasal or a semi-vowel
  # or by nothing.
  # the 'z' is changed to 'w' or 'q' when at the end of a word,
  # and to 'q' when followed by a soft consonant.
  # The 'j' of 'parivrAj' is similarly changed to 'w' or 'q'.
  # NOTE: this includes 'samraaj parivraaj'
  tok2 = tok1 + 'w'
 else:
  # Antoine1: 77(1) Nouns with stems ending in palatals
  #  Final 'ch' is changed to 'k' in 1S and 7P
  #                        to 'g' before 'bhyaam', 'bhiH' and 'bhyaH'
  # Kale #94(a). 'ch' or 'j' is changed to 'k' when followed by
  # a hard consonant or by nothing, and to 'g' when followed by
  # a soft consonant. (Pan. VIII.2.30)
  tok2 = tok1 + 'k'
 x = citation
 y = tok2
 basetoks = [
       y, x, x,
       x, x, x,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, x
  ]
 try:
  ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 except NameError as err:
  print err0," ERROR from declension_1cons_finish",err
  ans=None
 return ans

def declension_general_1cons_C(citation,gender,irregs,dbg=False):
 """
 """
 last = citation[-1:]  # last character 
 tok1 = citation # citation[0:-1]  # all but last character
 tok2 = citation
 if citation[-2:-1] in init.vowel_set:
  # penultimate character is a vowel
  tok2 = tok2[0:-1] + 'w' # replace last character ('C') with 'w'
  tok1 = tok2[0:-1] + 'cC'
 else:
  # penultimate character not a vowel
  tok2 = tok2[0:-2] + 'w' # Example?
 x = tok1
 y = tok2
 basetoks = [  # same configuration as for '_c'
       y, x, x,
       x, x, x,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, x
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_j(citation,gender,irregs,dbg=False):
 """  See the comments for 1cons_c above
 """
 #last = citation[-1:]  # last character 
 tok1 = citation  #citation[0:-1]  # all but last character
 tok2 = citation
 # Final 'j' is changed to 'k' or 'T' in 1S and 7P
 #                      to 'g' or 'q' before 'ByAm', 'BiH' and 'ByaH'
 if tok2.endswith(('Brasj','Bfsj')):
  # 'sj' looks wrong; as no MW words end in 'sj'. Not sure what it should be
  #  also drop the sibilant
  tok2 = tok1[0:-2]+'w'
  # (?) acc. to SL: change 'sj' to 'jj' before vowel endings
  tok1 = tok1[0:-2] + 'jj'
 elif tok2.endswith(('vraSc','Brasj','sfj','mfj','yaj','rAj','BrAj')):
  # see note in 1cons_c re Kale 94(b) Pan VIII.2.36
  tok2 = tok1[0:-1] + 'w'
 elif tok2.endswith('sj'):
  # see comment above, no such headwords in MW
  tok2 = tok1[0:-2] + 'k'
  tok1 = tok1[0:-2] + 'jj'
 else:
  # Kale #94(a). 'ch' or 'j' is changed to 'k' when followed by
  # a hard consonant or by nothing, and to 'g' when followed by
  # a soft consonant. (Pan. VIII.2.30)
  tok2 = tok1[0:-1]+'k'
 #
 x = tok1
 y = tok2
 basetoks = [ # same configuration as 1cons_c
       y, x, x,
       x, x, x,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, x
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_S(citation,gender,irregs,dbg=False):
 """
 """
 err0="declension_general_1cons_S(%s,%s,%s)" %(citation,gender,'irregs')
 if dbg:
  print err0
 #last = citation[-1:]  # last character 
 tok1 = citation  #citation[0:-1]  # all but last character
 tok2 = citation
 # Final 'j' is changed to 'k' or 'T' in 1S and 7P
 #                      to 'g' or 'q' before 'ByAm', 'BiH' and 'ByaH'
 if tok2.endswith(('diS','dfS','spfS','mfS')):
  # Kale #94(c) p. 56.
  # The 'S' of the root-nouns  is 'diS','dfS','spfS','mfS' is
  # changed to 'k'. 
  tok2 = tok1[0:-1] + 'k'
 elif tok2.endswith(('driS','spriS','mriS')):
  # THIS IS FAULTY to facilitate comparisons to Elisp
  # Kale #94(c) p. 56.
  # The 'S' of the root-nouns  is 'diS','dfS','spfS','mfS' is
  # changed to 'k'. 
  tok2 = tok1[0:-1] + 'k'
 elif tok2.endswith('naS'):
  # The 'S' of 'naS' is optionally changed to 'w' or 'k'
  # this is a new wrinkle, having tok2 as a list
  # The way to handle is to make it a comma-delimited string
  tok2 = ','.join([tok1[0:-1]+'w',tok1[0:-1]+'k'])
 elif tok2.endswith('RaS') and preserve_elisp_errors:
  # The string-match function used in gram3.el is USED SO THAT IT
  # IS CASE-INSENSITIVE.  In ITRANS, the spelling of 'arRaS' is
  # 'arNash'. so there is a match here.  This occurs in 'maDv-arRaS',
  # which seems to have nothing to do with 'naS' (to perish)
  tok2 = ','.join([tok1[0:-1]+'w',tok1[0:-1]+'k'])
 else:
  # Kale #94(b). The ending 'S' of root-nouns changes to 'z'
  # when followed by any consonant except a nasal or a semi-vowel or
  # by nothing. The 'z' changes to 'w' or 'q' when at the end of a
  # word, and to 'q' when followed by a soft consonant.
  tok2 = tok1[0:-1] + 'w'
 x = tok1
 y = tok2
 y1 = y
 if citation.endswith('dfS'):
  # ? SL. Not sure why this is done. An optional form?
  #y1 = ','.join([y,y[0:-1]+'N'])
  y1 = [y,y[0:-1]+'N']
 # for comparison to Elisp, the following (wrong) code is inserted
 elif citation.endswith('driS'):
  #y1 = ','.join([y,y[0:-1]+'N'])
  y1 = [y,y[0:-1]+'N']
 if citation.endswith(('driS','spriS','mriS')):
  print err0," WARNING:  'riS' code is wrong"
 if dbg:
  print "x=%s,y=%s,y1=%s" %(x,y,y1)
 basetoks = [ # same configuration as 1cons_c, except 1s and 8s
       y1, x, x,
       x, x, x,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y1, x, x
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_z(citation,gender,irregs,dbg=False):
 """
 """
 err0 = "declension_general_1cons_z(%s,%s,%s)" %(citation,gender,'irregs')
 if dbg:
  print err0
 tok1 = citation
 tok2 = citation
 if tok2.endswith(('takz','gorakz')):
  # Kale #94(c) p. 56.
  # The 'kz' of 'takz','gorakz'' are optionally changed to 'w' or 'k'
  tok2 = ','.join([tok1[0:-2]+'w',tok1[0:-2]+'k'])
 elif tok2.endswith('kz'):
  # Kale #94(c) p. 56.
  # 'kz' of such words as 'vipakz' is changed to 'k'
  tok2 = tok1[0:-2]+'k'
 elif tok2.endswith('daDfz'):
  # Kale #94(c) p. 56.
  # The 'z' of 'daDfz' (bold or impudent man) is changed to 'k'
  # 'kz' of such words as 'vipakz' is changed to 'k'
  # NOTE: (?) SL has 'k' for masc., 'w' for neut.
  if gender == 'n':
   tok2 = tok1[0:-1] + 'w'
  else:
   tok2 = tok1[0:-1] + 'k'
 else:
  # Antoine1:77(2) Nouns with stems ending in cerebrals
  #  Final 'z' is changed to 'w' in 1S and 7P
  #                        to 'q' before 'ByAm', 'BiH' and 'ByaH'
  tok2 = tok1[0:-1] + 'w'
 x = citation
 y = tok2
 if gender == 'n':
  y1 = y
  y2 = x[0:-1] + 'M' + x[-1:]
 else:
  y1 = x
  y2 = x
 basetoks = [
       y, x, y2,
       y1, x, y2,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, y2
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_h(citation,gender,irregs,dbg=False):
 """
 """
 err0 = "declension_general_1cons_h(%s,%s,%s)" %(citation,gender,'irregs')
 if dbg:
  print err0
 tok1 = citation 
 tok2 = citation
 #Antoine1 77(7) Nouns with stems ending in 'h'
 # Usually, final 'h' becomes 'k' in 1S and 7P
 #                   'g' before 'bhyaam', 'bhiH' and 'bhyaH'
 # the appearance of 'dh' is explained by the following sandhi:
 # When 'gh', 'dh', 'bh', or 'h' loses aspiration owing to combination
 # with following consonants, the preceding consonant becomes aspirated,
 # if possible
 # In nouns ending with 'lih', final 'h' becomes 'T' in 1S and 7P,
 # and final 'h' becomes 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
 # For the noun 'upaanah', the final 'h becomes 't' and 'd'
 #  The following matching has a subtle difference in Elisp code and
 #  this Python code, due to the fact the the Elisp code assumes 
 #  ITRANS spelling  while this Python version assumes SLP1 spelling.
 #  Thus, in Python version when tok2 = 'dfh'(SLP1), there is a
 #  match with "d[a-zA-Z]h$"; but in Elisp the spelling is 'dRih', so there
 #  is no matching.  
 #  I'm unsure which is correct, but will follow the Elisp for now
 if preserve_elisp_errors:
  # use vowels whose ITRANS spelling is one character:
  vowels = 'aiueo' 
 else:
  vowels = init.vowel_set # = 'aiufxAIUFXeEoO'
 if re.search(r"d[%s]h$"%vowels,tok2):
  tok2 = tok2[0:-3]+'D' + tok2[-2:-1] + 'k'
 elif re.search(r"q[%s]h$"%vowels,tok2) and preserve_elisp_errors:
  # example anaquh.  in Elisp (ITRANS) this is 'anaDuh'.  Since
  # the string matching in Elisp is done with case INSENSITIVITY,
  # this matches.  It is likely wrong, of course. But for comparison
  # to Elisp results, it is maintained.
  tok2 = tok2[0:-3]+'D' + tok2[-2:-1] + 'k'
 elif re.search(r"g[%s]h$"%vowels,tok2):
  tok2 = tok2[0:-3]+'G' + tok2[-2:-1] + 'w'
 elif tok2.endswith('upAnah'):
  tok2 = tok2[0:-1] + 't'
 elif tok2.endswith('uzRih'):
  tok2 = tok2[0:-1] + 'k'
 elif tok2.endswith(('muh','snuh','snih')):
  # ; Kale#95(a), p.57 
  tok2 = ','.join([tok2[0:-1] + 'w',tok2[0:-1] + 'k'])
 elif tok2.endswith('druh'):
  # Kale#95(a), p.57
  # Not sure why 'd' is aspirated before consonant endings
  # but Kale (p. 59 example) and SL show this.
  tok2 = tok2[0:-4] + 'Druh'
  tok2 = ','.join([tok2[0:-1] + 'w',tok2[0:-1] + 'k'])
 elif tok2.endswith(('turAsAh','turAzAh')):
  # Kale#98 p. 60.
  # The 's' of 'turAsAh' (Indra) is changed to 'Sh' before the
  # consonantal terminations (Pan VIII.3.56)
  # Note: MW cites this is 'turAzAh'
  tok2 = tok2[0:-3] + 'zAw'
 else:
  tok2 = tok2[0:-1] + 'w'
 x = tok1
 x1 = x
 y = tok2
 if (gender == 'n'):
  y1= y
  y2 = x[0:-1] + 'M' + x1[-1:]
 else:
  y1 = x1
  y2 = x1
 y3 = y2
 if citation.endswith(('vah','vAh')):
  # MW says 'Svetavah' is variant of 'shvetavAh'.
  # Kale#100, p. 60. says:
  # The 'vA' of root-nouns ending in 'vAh' is changed to 'U' before
  # the vowel terminations beginning with 2P.
  # REF: Pan VI.4.132, VI.1.108
  # 'A' and 'U' combine into 'O' by Pan VI.1.89.
  if len(tok1) < 4:
   return None
  w = declension_general_1(tok1[0:-3],'U')
  if tok1[-4:-3] in ['a','A']:
   w = w[0:-1]+ 'O' # very special sandhi
  tok1 = w + 'h'
  tok2 = tok2[0:-2]+ 'A' + 'w'
  x = tok1
  x1 = tok2[0:-2] + 'A' + 'h'
  y = tok2
  if (gender == 'n'):  # same setting of y1, y2 as before
   y1= y
   y2 = x[0:-1] + 'M' + x1[-1:]
  else:
   y1 = x1
   y2 = x1
  y3 = x  # this is different 
 # Now ready to set basetoks 
 basetoks = [
       y, x1, y2,
       y1, x1, y3,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x1, y2
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_v(citation,gender,irregs,dbg=False):
 """
 """
 tok1 = citation
 tok2 = citation
 if tok2.endswith('div'):
  # 'div' f. (sky) : Antoine2-#80. See irreg.el also
  # This is declined like a noun with 1 consonant, except (in M/F)
  # (a) 1S is 'dyauH'
  # (a)' 8S is 'dyauH'
  # (b) 2S is optionally 'dyaam'
  # (c) before terminations beginning with a consonant it uses stem 'dyu'
  #    (this is accomplished in algorithm, by providing two praatipadikas
  #     ('div' and 'dyu')
  tok2 = tok2[0:-3]+'dyu'
 #
 x = tok1
 y = tok2
 z = y
 if gender == 'n':
  y1 = y
 else:
  y1 = x
 basetoks = [
       y, x, x,
       y1, x, x,
       x, z, z,
       x, z, z,
       x, z, z,
       x, x, x,
       x, x, z,
       y, x, x
 ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 # some post adjustments for 'div' 
 if citation.endswith('div') and (gender in 'mf'):
  tmp1 = citation[0:-3] + 'dyOH'
  ans[0] = tmp1
  ans[21] = tmp1
  if gender == 'f':
   tmp1 = citation[0:-3] + 'dyAm'
   # provide optional form for acc. sg.
   ans[3] = ans[3] + "," + tmp1
 return ans

def declension_general_1cons_t(citation,gender,irregs,dbg=False):
 """ Handles t,T,d,D
 """
 tok1 = citation
 tok2 = citation
 if tok2.endswith('buD'):
  # (?) I think the sibilant disappears in front of consonants
  tok2 = tok2[0:-3]+ 'But'
 else:
  # Antoine#1.77(3) Nouns with stems ending in dentals
  #  Final dental other than 'n' is changed to 't' in 1S and 7P
  #                       to 'd' before 'bhyaam', 'bhiH' and 'bhyaH'
  tok2 = tok2[0:-1] + 't'
 x = tok1
 y = tok2
 if gender == 'n':
  y1 = y
  y2 = x[0:-1] + 'n' + x[-1:]
 else:
  y1 = x
  y2 = x
 basetoks = [ # same configuration as in 1cons_z
       y, x, y2,
       y1, x, y2,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, y2
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_p(citation,gender,irregs,dbg=False):
 """ for p,P,b,B
 """
 tok1 = citation
 tok2 = citation
 # Antoine#1. 77(4) Nouns with stems ending in labials
 #  Final labial is changed to 'p' in 1S and 7P
 #                       to 'b' before 'bhyaam', 'bhiH' and 'bhyaH'
 tok2 = tok2[0:-1] + 'p'
 #
 x = tok1
 y = tok2
 if gender == 'n':
  y1 = y
  y2 = x[0:-1] + 'm' + x[-1:]
 else:
  y1 = x
  y2 = x
 basetoks = [ # same configuration as in 1cons_t
       y, x, y2,
       y1, x, y2,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, y2
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

def declension_general_1cons_k(citation,gender,irregs,dbg=False):
 """
 """
 tok1 = citation
 tok2 = citation
 tok2 = tok2[0:-1] + 'k'
 #
 x = tok1
 y = tok2
 if gender == 'n':
  y1 = y
  y2 = x[0:-1] + 'M' + x[-1:]
 else:
  y1 = x
  y2 = x
 basetoks = [ # same configuration as in 1cons_t
       y, x, y2,
       y1, x, y2,
       x, y, y,
       x, y, y,
       x, y, y,
       x, x, x,
       x, x, y,
       y, x, y2
  ]
 ans = declension_1cons_finish(basetoks,gender,irregs,dbg)
 return ans

