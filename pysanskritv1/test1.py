"""
  Note1: See note of 'June 27, 2016.' for a bug correction. We leave the bug in
  place for now, to facilitate comparison of Python results and Elisp results.
  Example: a-cala-BrAtf
  Note2:  senAnI,m,senAnI  is wrong in comparison to Huet.
"""
import init
import re
from sandhi import *
from declension_general_1cons import *
# from declension_general_1cons import declension_general_1cons
preserve_elisp_errors=True

def subanta_base(citation,gender,form,dbg=False):
 """ find the 'prAtipadika', the raw form of the citation to
     be used in forming declension.
     inputs are strings. citation coded in SLP1
     returns a string (THE prAtipadika), or a sequence.
     gender should be m,f or n
     form varies. Often (for nouns), the significant part of the ending
     Can return None, indicating an error
 """
 err0 = "subanta_base(%s,%s,%s)" %(citation,gender,form)
 gf = "%s,%s" %(gender,form)
 if dbg:
  print err0
 if len(citation) <= 1:
  return None
 #-------------------------------
 if form in ['manC','manV']:
  # base drops last 2 characters 'an'  (??)
  return citation[0:-2]
 #-------------------------------
 if gf in ['m,I' , 'm,I1']:
  # Antoine2, Section 12.
  # Kale #77 p.43
  s = citation[0:-1] # remove I
  if len(s) == 1:
   t = s + "iy"  # example?
  elif gf == 'm,I1':
   if s.endswith(('n','R')):
    # senAnIH -> senAny, agraRIH -> agraRy
    t = s + "y"
   else:
    t = s + "iy"
  else:
   # suDIH -> suDiy
    t = s + "iy"
  return [s,t]
 #-------------------------------
 if gf in ['f,I1' , 'f,I2', 'f,U1' , 'm,U1' , 'm,U']:
  # assumes 'monosyllabic for F' has been checked.
  # See function 'subanta-convert-form' in 'construct.el'.
  # Antoine2, Section 24. Monosyllabic Feminine nouns in 'ii' and 'uu' are 
  # referenced as of type 'ii1' or 'uu1'. They are declined
  # (a) before terminations beginning with a vowel, like stems
  #     ending with a consonant 
  # (b) and before consonant terminations like 'nadii' or 'vadhuu'
  # However, for cases 4-7 before terminations beginning with a vowel, they
  # have an optional form using the consonantal stem with the
  # terminations of 'nadii' or 'vadhuu'
  # I give the citation form as 'dhii' or 'bhuu'; Apte dictionary gives the
  # citation form as the nominative singular (e.g. 'dhiiH' or 'bhuuH')
  # Section 25. monosyllaabic feminine noun 'strii' (type ii2)
  # This has no visarga in 1S. It takes the terminations of 'nadii' with
  # the consonantal stems before terminations beginning with vowels. However,
  # it has optional forms in cases 2S and 2P. 
  s = citation[0:-1] # remove final I or U
  if citation.endswith('I'):
   t = s+"iy"
  elif citation.endswith('U'):
   t = s+"uv"
  else:
   print "ERROR 1:",citation,gender,form,"s=",s
   return None
  return [s,t]
 #-------------------------------
 if (form=='irr') and (citation in ['dos','ASis']):
  # Antoine volume 2: Chapter 8
  return (citation[0:-1]+'H')
 if (form=='irr') and (citation in ['ap']):
  # Antoine volume 2: Chapter 8
  return 'Ap'
 if (citation == 'go'):
  # Antoine2, Section 54: go 
  # (Kale) vrddhi ('au') is substituted for  'o'  of words ending
  # in 'o' in the first five inflections (and in 8th), except for
  # 2S and 2P, where 'aa' is substuted. 
  # Nouns ending in 'au' are regularly declined.
  # The 'normal' endings are used.
  return 'g'
 if (citation in ['sat','asat']) and (gender=='adj'):
  return [citation,citation[0:-1]+"nt","W"]
 if (form == 'at'):
  # note that order of tests is significant here
  return citation[0:-2] # remove 'at' suffix
 for i in [2,1,4,3]:
  # most cases fall here.
  if (gf in init.Gender_form_data.records[i]) and ((i+1)<=len(citation)):
   return citation[0:-i]
 # Last resort. Return citation
 return citation
 # unresolved 
 #print "SUBANTA BASE NOT READY for",(citation,gender,form)
 #return None

def test_subanta_base():
 teststring="""
nI,m,I:n,niy
nI,m,I1:n,niy
senAnI,m,I1:senAn,senAny
agraRI,m,I1:agraR,agraRy
suDI,m,I1:suD,suDiy
DI,f,I1:D,Diy
BU,f,U1:B,Buv
dos,n,irr:doH
ASis,f,irr:ASiH
ap,f,irr:Ap
;not sure of 'form' ('o') for 'go'
go,f,o:g
go,m,o:g
sat,adj,at:sat,sant,W
asat,adj,at:asat,asant,W
jagat,n,at:jag
rAma,m,a:rAm
"""
 npass = 0
 nfail = 0
 for x in teststring.splitlines():
  if not (':' in x):
   continue
  if x.startswith(';'):
   continue
  x=x.rstrip('\r\n')
  (xinput,xanswer)=x.split(':')
  (citation,gender,form)=xinput.split(',')
  answer = xanswer.split(',')
  if len(answer)==1:
   answer=answer[0]
  #print citation,gender,form
  compute=subanta_base(citation,gender,form)
  if compute == answer:
   status = "PASS"
   npass = npass + 1
  else:
   status = "FAIL % s" % compute
   nfail = nfail + 1
  out = "subanta-base(%s,%s,%s)=>%s [%s]" %(citation,gender,form,answer,status)
  print out
 #
 print "%s cases PASS, %s cases FAIL" %(npass, nfail)

class MyException(Exception):
 """ suggested by 
   http://stackoverflow.com/questions/1319615/proper-way-to-declare-custom-exceptions-in-modern-python
 """
 pass

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

def test_word_parts():
 tests= [
  ['akza',[['a','kz','a'],'vcv']],
  ['rAma',[['r','A','m','a'],'cvcv']],
  ['strI',[['str','I'],'cv']],
 ]
 for (citation,answer) in tests:
  compute = word_parts(citation)
  if compute == answer:
   status = "PASS"
  else:
   status = "FAIL % s" % compute
  out = "word_parts(%s)=>%s [%s]" %(citation,answer,status)
  print out

def subanta_convert_form(citation,formin,dbg=False):
 """ adaptation of Elisp
  (subanta-convert-form subanta formin)
 """
 if dbg:
  tmp="ENTER subanta_convert_form(%s,%s)" %(citation,formin)
  print tmp
 form = formin
 if citation in init.Subanta_type_data.d:
  thetype = init.Subanta_type_data.d[citation].val
 else:
  thetype = None
 if dbg:
  tmp="subanta_convert_form: thetype=%s" % thetype
  print tmp
 # 
 lc = citation[-1] # last character
 l = len(citation)
 if thetype:
  if (lc == 'a') and (formin in ['m','f','n','adj']) and (thetype in ['c','d']):
   thetype = 'a'
  elif (citation == 'sama') and (formin == 'adj'):
   thetype = None
 if not thetype:
  (parts,types) = word_parts(citation)
  monosyllabicp = (len(types)==2)  # Example, "BU",
  if (form in ['m','f']) and (lc == 'f'):
   (thetype,form)=('f-A','noun')
  elif (form in ['n']) and (lc == 'f'):
   (thetype,form)=('f','noun')
  elif (form == 'ord') and (citation in ['praTama','dvitIya','tftIya']):
   (thetype,form)=('orda','pron')
  elif (form == 'ord') and  (citation in ['turIya','turya']):
   (thetype,form)=('ordb','pron')
  elif (form == 'ord'):
   (thetype,form)=('ord','pron')
  elif (form == 'card'):
   (thetype,form)=('card','pron')
  elif (form == 'adji'):
   (thetype,form)=('ai','adj')
  elif (form == 'adjt'):  # examples?
   (thetype,form)=('at','adj')
  elif (4 <= l) and (citation.endswith(('man','van'))):
   if (citation[-4:-3] in init.consonant_set):
    thetype='manc'
   else:
    thetype='manv'
  elif (2 <= l) and (citation.endswith(('in','an','ac','Ac'))):
   thetype = citation[-2:] # last two characters
  elif (3 <= l) and (citation.endswith(('mat','vat'))):
   thetype = 'mat'
  elif (5 <= l) and (citation.endswith('mahat')):
   thetype = 'mat'
  elif (3 <= l) and (citation.endswith('vas')) and (not (citation in ['Dvas','viSravas'])):
   thetype = 'vas'
  elif (4 <= l) and ((citation.endswith(('Iyas','eyas','Uyas'))) or (citation == 'jyAyas')):
   # 4-19-04: vihaayas does not decline like this, I think, but as '1cons'
   # for 'jyAyas', see Antoine 1, p. 103.
   # There are some adjectives in 'DAyas', I think those are '1cons'.
   thetype = 'Iyas'
  elif (form in ['m','f','n']) and (lc in init.consonant_set):
   thetype = '1cons'
  elif (citation.endswith(('RI','nI','DI','krI','hrI','BI','SrI'))) and\
    ( ((form == 'f') and monosyllabicp) or (form == 'm')):
   thetype = 'I1' # root nouns ending in 'I' ('m' or 'f')
  elif (form == 'm') and (lc == 'I'):
   thetype = 'I1' # kOmudI
  elif (citation.endswith(('BU','brU','lU'))) and\
    ( ((form == 'f') and monosyllabicp) or (form == 'm')):
   thetype = 'U1' # root nouns ending in 'U' ('m' or 'f')
  elif (lc in init.vowel_set):
   thetype = lc
  else:
   thetype = None
 #
 if (form == 'card'):
  form = 'pron'
  if (thetype != 'irr'):
   thetype = 'card'
 if thetype:
  ans = [form,thetype]
 else:
  ans = None
 if dbg: 
  tmp = "EXIT subanta_convert_form(%s,%s) => %s" %(citation,formin,ans)
  print tmp
 return ans
def test_convert_subanta_lexinfo():
 tests = [
  [['rAma','m'],[['a','m']]],
  [['idam','pron'],[[]]]
 ]
 for (inputs,answer) in tests:
  (citation,fg) = inputs
  compute = convert_subanta_lexinfo(citation,fg)
  if compute == answer:
   status = "PASS"
  else:
   status = "FAIL % s" % compute
  out = "convert_subanta_lexinfo(%s,%s)=>%s [%s]" %(citation,fg,answer,status)
  print out


def convert_subanta_lexinfo(citation,fg,dbg=False):
 """ arguments same as in init_genform. fg is lowercase
  adaptation of Elisp
   (convert-subanta-lexinfo subanta lexinfo)
  Returns a list of [form,gender] pairs
 """
 if dbg:
  print "Enter convert_subanta_lexinfo(%s,%s)" %(citation,fg)
 formin = fg
 forms=[]
 formsin=[]
 if re.search(r'^[mfn]+$',formin):  # a string of genders
  formsin = list(formin)
 elif formin == 'adjt':
  formsin = [formin]
 elif formin == 'adj':
  tmp = subanta_convert_form(citation,formin,dbg)
  if tmp:
   # next is confusing. The 'thetype' variable of helper2 is
   # here in helper1 called 'form'
   (theform,thetype)=tmp
   form = thetype
   forms = [[form,formin]]
   formsin = [] # inhibit next phase attempt (?)
  else:
   formsin = ['m','f','n']
 else:
  formsin = [formin]
 #
 for formin in formsin:
  tmp = subanta_convert_form(citation,formin)
  if formin in ['m','f','n']:
   gender = formin
  else:
   gender = tmp[0]
  form = tmp[1]
  forms.insert(0,[form,gender])
 if len(forms) == 0:
  print "convert_subanta_lexinfo ERROR: no forms citation=%s,fg=%s" %(citation,fg)
  raise NameError('convert_subanta_lexinfo')
 if dbg:
  print "convert_subanta_lexinfo(%s,%s) => %s " %(citation,fg,forms)
 return forms

def init_genform(citation,fg,dbg=False):
 """ adaptation of Elisp
     (s-file-init-genform subanta fg) in forms.el
  fg should be a 'form or gender', e.g.
   m,f,n or any (2) of them
   adj or adji
   pron, card, ord
   Note:  lowercase 'fg' 
 """
 err0 = "init_genform(%s,%s)" % (citation,fg)
 if dbg:
  print err0
 fg1 = fg.lower()
 forms1 = convert_subanta_lexinfo(citation,fg1,dbg)
 #print "forms1=",forms1
 ans=[]
 for (form,gender) in forms1:
  if form.lower() in ['manc','manv']: # capitalization technical concern
   form = 'an'
  elif gender.lower() == 'adjt':
   gender='adj'
  elif form == 'at':
   form='mat'
  newform = [form,gender]
  ans.append(newform)
 if dbg:
  print err0 + (" returns %s" % ans)
 return ans

def test_init_genform():
 tests= [
  [('rAma','m'),[['a','m']]],
  [('caru','mf'),[['u','m'],['u','f']]],
  [('caru','fm'),[['u','f'],['u','m']]],
  [('jagat','n'),[['1cons','n']]]
 ]
 for ((citation,fg),answer) in tests:
  compute = init_genform(citation,fg)
  if compute == answer:
   status = "PASS"
  else:
   status = "FAIL % s" % compute
  out = "init_genform(%s,%s)=>%s [%s]" %(citation,fg,answer,status)
  print out

def  sl_construct_subanta1_m1p_opt(subanta):
 """
 Kale 156, p. 190. In this case, the nom. plu. is optionally formed
 like that of pronouns. 'AH' is optionally replaced by 'e'
 Note: This applies only in M case (I think)
 """
 if (subanta in ['praTama','carama','alpa','arDa','katipaya']):
  ans = subanta[0:-1]+'e' 
 elif subanta[-4:] == 'taya':
  ans = subanta[0:-1]+'e' 
 else:
  ans=None
 return ans

def construct_subanta2_adj_at(citation,genderin,form):
 """ functional equivalent of Elisp
  ( construct-subanta2-ADJ-at citation-sym  genderin form)  in construct.el,
  which in turn is actually implemented (with ITRANS) by
  (construct-subanta1 subanta gender form)
 """
 genderin = "adj"
 form = "at"
 weak = citation
 strong = citation[0:-1]+"nt" # replace final 't' with 'nt'
 sw = "s" # strong
 pratipadikas = [[weak,strong,sw]]
 otherinfo = []
 for gender in ['m','f','n']:
  iform = "%s-%s" % (gender,form)  # m-at, f-at, n-at
  irregs=None
  xnew = [pratipadikas,gender,"at-adj",irregs]
  otherinfo.append(xnew)
 return otherinfo


def construct_subanta2(citation,genderin,form,dbg=False):
 """ functional equivalent of Elisp
  ( construct-subanta2 citation-sym  genderin form)  in construct.el
 """
 if dbg:
  tmp = "ENTER construct_subanta2(%s,%s,%s)" %(citation,genderin,form)
  print tmp
 if (citation == 'Bavat') and (genderin.lower() == 'pron'):
  pratipadikas ="Bav"
 else:
  pratipadikas = subanta_base(citation,genderin,form,dbg=dbg)
 if dbg:
  tmp = "construct_subanta2. pratipadikas=%s" %pratipadikas
  print tmp
 if genderin in ['m','f','n']:
  genders = [genderin]
 else:
  genders=['m','f','n']
  form = "%s-%s" %(form,genderin)
  if (form == 'vat-pron'): # case of Bavat
   form = 'vat'
 otherinfo=[]
 for gender in genders:
  iform = "%s-%s" %(gender,form)
  irregkey = "%s.%s" % (citation,iform)
  # capitalization and ITRANS will cause problems here. 
  if irregkey in init.Subanta_irreg_data.d:
   irregs = init.Subanta_irreg_data.d[irregkey].vals
  else:
   irregs=None
  xnew = [pratipadikas,gender,form,irregs]
  otherinfo.append(xnew)
 if dbg:
  tmp = "construct_subanta2(%s,%s,%s) => %s" % (citation,genderin,form,otherinfo)
  print tmp
 return otherinfo

def declension_general(pratipadika,gender,form,irregs,dbg=False):
 """ functional equivalent of Elisp
  (declension-general praatipadika gender form irregs) in gram3.el,
  Refactored .
  Returns None if there are no sups
 """
 err0 = "declension_general(%s,%s,%s,%s)" %(pratipadika,gender,form,'irregs')
 if dbg:
  print err0
 try:
  sups = declension_get_sups(gender,form)
 except:
  print err0," sups not found for",gender,form
  return None
 ans = declension_general_2(pratipadika,sups,irregs,dbg=dbg)
 if dbg:
  tmp = "declension_general. ans=%s" % ans
  print tmp
 return ans


def declension_general_m_U(citation,pratipadika,gender,form,irregs,dbg=False):
 err0 = "declension_general_m_U(%s,%s,%s,%s,%s)" % (citation,pratipadika,gender,form,'irregs')
 if dbg:
  print err0
 last = citation[-1:]
 pratipadikas = subanta_base(citation,gender,form,dbg)
 #print "pratipadikas=", pratipadikas
 if not isinstance(pratipadikas,list):
  pratipadikas = [pratipadikas,pratipadikas]
 if (len(pratipadikas) == 1):
  pratipadikas = pratipadikas + pratipadikas
 if (len(pratipadikas) == 2):
  (sym1,sym2) = pratipadikas
  sym2 = sym2[0:-1] #solution(sym_delete_last(sym2))
  pratipadikas = [sym1,sym2]
 sups = declension_get_sups(gender,form)
 ptok = pratipadikas[0]
 if last == 'I':
  wtok = ptok + 'I'
 else:
  wtok = ptok + 'U'
 stok = pratipadikas[1]
 if citation == 'KalapU':
  wtok = ptok
  stok = ptok
 if gender in 'mf':
  basetoks = [
   wtok, stok, stok,
   stok, stok, stok,
   stok, wtok, wtok,
   stok, wtok, wtok,
   stok, wtok, wtok,
   stok, stok, stok,
   stok, stok, wtok,
   wtok, stok, stok,
  ]
 else:
  raise NameError(err + " Cannot handle gender " + gender)
 ans = declension_general_3(basetoks,sups,irregs,dbg)
 if (gender == 'm') and (citation.endswith('nI')):
  # special case of locative singular
  # Kale p.45, footnote
  i = 18
  sup = sups[i]
  sup = sup[0:-1] + 'Am'  # from yi to yAm
  basetok = basetoks[i]
  ans[i] = declension_general_1(basetok,sup)
 return ans


def declension_general_cons(citation,pratipadika,gender,newform,irregs,dbg=False):
 if dbg:
  tmp = "ENTER declension_general_cons(%s,%s,%s,%s,%s)" % (citation,pratipadika,gender,newform,'irregs')
  print tmp
 localdict = globals()
 # newform may have '-' in the name, like 'Iyas-adj'. In Python,
 # function names cannot have the '-' character, so we change '-' to '_'
 newform1 = newform.replace('-','_')
 fcnname = "declension_general_%s" % newform1
 if fcnname not in localdict:
  err = "declension_general_cons: function not implemented: %s" %fcnname
  err = err + "\n (%s,%s,%s,%s,%s)" % (citation,pratipadika,gender,newform,irregs)
  raise NameError(err)
 # otherwise, call fcn 
 fcn = localdict[fcnname]
 #if not irregs:
 # irregs=[None]*24
 if dbg:
  print "declension_general_cons: fcnname=",fcnname
 return fcn(citation,pratipadika,gender,newform,irregs,dbg)
 #if newform == '1cons':
 # return declension_general_1cons(citation,pratipadika,gender,newform,irregs,dbg)


def declension_citation1(citation,pratipadika,gender,form,irregs,dbg=False):
 """ functional equivalent of Elisp
  (declension-citation1 citation-sym praatipadika gender form irregs) in gram3.el,
   This just passes the buck to a more specialized function
 """
 err0 = "declension_citation1(%s,%s,%s,%s, %s)" %(citation,pratipadika,gender,form,'irregs')

 if dbg:
  print err0
 #pratipadika = pratipadikas[0]  # assume 1 one-element list
 if form in ['ac','Ac']:
  #formtmp = 'ac' # May 6, 2016. The sups are the same for 'Ac' and 'ac'
  formtmp = form  # May 10, 2016
  newform = "%s-adj" % formtmp
  return declension_general_cons(citation,pratipadika,gender,newform,irregs,dbg)
 #if dbg:
 # print "declension_general_cons:init.declension_cons_forms = ",init.declension_cons_forms
 if form in init.declension_cons_forms:
  if dbg:
   print "declension_citation1 calls declension_general_cons"
  return declension_general_cons(citation,pratipadika,gender,form,irregs,dbg)
 if (gender in ['m','f']) and (form in ['U1','I1']):
  return declension_general_m_U(citation,pratipadika,gender,form,irregs,dbg=dbg)
 if (gender in ['m']) and (form in ['U']):
  if isinstance(pratipadika,list):
   # Why do we throw away multiples here? Example dyU,m  has
   # pratipadika = ['dy','dyuv'], but we throw away 'dyuv'. 
   p = pratipadika[0]
  else:
   p = pratipadika
  return declension_general(p,gender,form,irregs,dbg=dbg)
 #if dbg:
 # print "declension_citation1 calls declension_general"
 return declension_general(pratipadika,gender,form,irregs,dbg=dbg)
 
 
def sl_construct_subanta1(sl_subanta,gender,form,dbg=False):
 """ functional equivalent of Elisp
  (SL-construct-subanta1 SL-subanta SL-gender  SL-form) in construct.el,
  which in turn is actually implemented (with ITRANS) by
  (construct-subanta1 subanta gender form)
 """
 if dbg:
  tmp = "ENTER sl_construct_subanta1(%s,%s,%s)" %(sl_subanta,gender,form)
  print tmp
 if preserve_elisp_errors:
  parts = sl_subanta.split('-')
  subanta= parts[0]
 m1p_opt = sl_construct_subanta1_m1p_opt(subanta)
 if dbg:
  tmp = " sl_construct_subanta1: m1p_opt=%s" % m1p_opt
  print tmp
 if (gender == 'adj') and (form == 'at'):
  info = construct_subanta2_adj_at(subanta,gender,form)
 else:
  info = construct_subanta2(subanta,gender,form,dbg=dbg)
 if gender in ['m','f','n']:
  thetype='noun'
 else:
  thetype = gender
 ans=[]
 for info1 in info:
  if dbg:
   tmp = " sl_construct_subanta1: info1=%s" % info1
   print tmp
  [pratipadikas,g,f,irregs] = info1
  if not irregs:
   irregs = [None]*24
  if (gender == 'pron') and (subanta == 'Bavat'):
   #print "chk: subanta,gender,form=%s,%s,%s" % ( subanta,gender,form)
   #print "chk: info1=%s" % info1
   #raise NameError("pronoun Bavat")
   dtab = declension_pron(subanta,pratipadikas,g,f,dbg)
  else:
   dtab = declension_citation1(subanta,pratipadikas,g,f,irregs,dbg)
   # could have None.
   if dtab == None:
    return None
  if dbg:
   tmp = " sl_construct_subanta1. dtab=%s"%dtab
   print tmp
  if m1p_opt:
   old = dtab[2]
   # may need to convert 'old' to a list.  This is confusing. Class needed?
   # if old is not a list, old = [old]
   if not isinstance(old,list):
    old = [old]
   old.append(m1p_opt)
   dtab[2]=old
  thisans = [g,dtab]
  if dbg:
   tmp = " sl_construct_subanta1. thisans=%s"%thisans
  ans.append(thisans)
 theans = [(thetype,form),ans]
 if dbg:
  print "sl_construct_subanta1 returns %s" % theans
 return theans


def s_file_init_alt_helper_adjust(ans,key1,fg,key2):
 """make a string identical with that generated by the Elisp program
   Handles multiple forms
 """
 if not ans:
  return ans
 outarr=[]
 (s_type,s_form)= ans[0]
 s_form0=s_form
 # for purposes of comparison, do some adjustments
 s_form_replacements=[
  ('irr','IRR'),
  ('A','aa'),
  ('I1','ii1'),
  ('I0','ii0'),
  ('I','ii'),
  ('U1','uu1'),
  ('U','uu'),
  ('f-r','Ri-R'),
  ('f-A','Ri-A'), # svasf
  ('Iyas','iiyas'),
  ('ac','ach'),
  ('Ac','aach'),
  ('O','au'),
  ('f','Ri'),
  ('E','ai'),
  ('ai','aI'),
  ('ord','ORD'),
  ('card','CARD'),
  ('orda','ORDa'),
  ('ordb','ORDb'),
  #('Ri-R','Ri-A')
 ]
 for (pyform,elispform) in s_form_replacements:
  if s_form == pyform:
   s_form = elispform
   break
 if key1 == 'anuBrAtfxxx':
   print "s_form0=",s_form0," => s_form=",s_form

 #outarr.append(':%s %s' % (s_type,s_form))
 ans1 = ans[1]
 for icase in xrange(0,len(ans1)):
  (g,dtab) = ans1[icase]
  #out1 = '%s:%s:%s' %(key1,fg,key2)
  if not dtab:
   dtab = [None]*24
  for j in xrange(0,len(dtab)):
   val = dtab[j]
   if not val:
    val = "_"  # symbol for missing
    dtab[j] = val
   else:
    if isinstance(dtab[j],list):
     parts = dtab[j]
    else:
     parts = dtab[j].split(',')
    newparts=[]
    #print "preserve_elisp_errors=",preserve_elisp_errors,"j=",j,parts
    for part1 in parts:
     if  preserve_elisp_errors:
      part1 = part1.replace('au','O')
      part1 = part1.replace('aU','Ou')
      part1 = part1.replace('picCaA','picCAa') # picCaAnga(?)
      part1 = re.sub(r'^dh','D',part1) # dhA (?)
      part1 = re.sub(r'^rabdh','rabD',part1) # rabdhA (?)
      part1 = re.sub(r'^siMhoddh','siMhodD',part1) # siMhoddhonnatA (?)
      part1 = part1.replace('vihewh','viheW') # vihewheWa (?)
      part1 = part1.replace('vaiz','vEz') # vaizayikI
      part1 = re.sub(r'^sOraBeyatantr','sauraBeyatantr',part1)
     newparts.append(part1)
    parts = newparts
    if len(parts)>1:
     # replace x,y by (x y), the form of Elisp.
     dtab[j] = '('+' '.join(parts) + ')'
    else:
     dtab[j] = parts[0]
  dtab1 = ' '.join(dtab)
  #out = '%s:%s:%s:[%s]\n' %(fg,key1,key2,dtab1)
  dtab2 = "[%s]" % dtab1
  # For purposes of comparison, replace _ with nil
  dtab2 = dtab2.replace('_','nil')
  headarr=[]
  headarr.append("%s %s %s" %(s_type,s_form,g))
  headarr.append(key1)
  headarr.append(key2)
  headarr.append(dtab2)
  out = ':'.join(headarr)
  out = ':' + out # for agreement with Elisp

  outarr.append(out)
 return ';'.join(outarr)

def s_file_init_alt_helper(key1,fg,key2,dbg=False):
 """ functional equivalent of Elisp
    (s-file-init-alt-helper subanta fg mw-word) in forms.el
 """
 #global preserve_elisp_errors
 #print " preserve_elisp_errors=",preserve_elisp_errors
 if dbg:
  tmp="ENTER s_file_init_alt_helper(%s,%s,%s)" %(key1,fg,key2)
  print tmp
 (key1orig,key2orig) = (key1,key2)
 if preserve_elisp_errors:
  if (key1,key2)==('naa','naa'):
   key1='nA'
   key2='nA'
  elif (key1,key2)==('manaU','manaU'):
   key1='manOu'
   key2='manOu'
 subwords = key2.split('-')
 if key1 != ''.join(subwords):
  err =  "s_file_init_alt_helper: key1 and key2 inconsistent\n"
  err += "key1=%s, key2=%s" %(key1,key2)
  #raise MyException(err)
  subwords = [key1] # this is what Elisp does 
 lword=subwords[-1]
 # June 27, 2016.  Although 'lword' is probably the 'right' parameter to 
 # use in the call to init_genform, nonetheless, I pass key1 for the
 # purpose of comparison to the Elisp computations.
 #forms = init_genform(lword,fg,dbg)
 forms = init_genform(key1,fg,dbg)
 if dbg:
  tmp="s_file_init_alt_helper: forms=%s" % forms
  print tmp
 # this logic June 26, 2016. For consistency with ELisp.
 # When multiple genders occur.
 outlines = [] 
 for (form,gender) in forms:
  if dbg:
   print "form,gender=",form,gender
  ans=sl_construct_subanta1(lword,gender,form,dbg)
  if dbg:
   print "ans=",ans
  if not ans:  # could have ans = None
   continue
  ansadj = s_file_init_helper1(subwords,ans)
  if dbg:
   print "ansadj=",ansadj
  ans1 = s_file_init_alt_helper_adjust(ansadj,key1,fg,key2)
  if preserve_elisp_errors:
   if (key1orig,key2orig) == ('naa','naa'):
    ans1 = ans1.replace('nA:nA:','naa:naa:')
   elif (key1orig,key2orig) == ('manaU','manaU'):
    ans1 = ans1.replace('manOu','manaU')
  if dbg:
   print "ans1=",ans1
  outlines.append(ans1)
 #print "outlines length=",len(outlines)
 if len(outlines) == 0:
  return None
 ans = solution(outlines)
 if isinstance(ans,list):
  ans = ';'.join(ans)
 return ans
 """
 (form,gender) = forms[0]
 ans=sl_construct_subanta1(lword,gender,form,dbg)
 if dbg:
  tmp="s_file_init_alt_helper: ans=\n%s"%ans
  print tmp
 # take care of the adjustment in case key1 != key2
 ansadj = s_file_init_helper1(subwords,ans)
 ans1 = s_file_init_alt_helper_adjust(ansadj,key1,fg,key2)
 return ans1
 """

def solution(ans):
 while True:
  if isinstance(ans,list) and (len(ans) == 1):
   ans=ans[0]
  else:
   break
 return ans

def s_file_init_helper1(subwords,ans):
 """ Refer to s_file_init_alt_helper_adjust for structure of ans,
   which is a list with 2 elements.
 """
 if not ans:
  return ans
 if len(subwords) == 1:
  return ans # nothing to do
 #print 's_file_init_helper1 enter'
 #print 'subwords=',subwords
 #print 'ans=',ans
 pfx = ''.join(subwords[0:-1])
 new = []
 new.append(ans[0])
 ans1 = ans[1]
 newans1 = []
 #print "len(ans1)=",len(ans1)
 for icase in xrange(0,len(ans1)):
  # ans1[icase] is a 2-element list.
  # first element is a gender
  # second element is a declension table 
  (g,dtab) = ans1[icase] 
  dtabnew = []
  for j in xrange(0,len(dtab)):
   if isinstance(dtab[j],list):
    parts = dtab[j]
   else:
    parts = dtab[j].split(',')
   newparts = []
   for part in parts:
    if part == '_':  # case of irregulars when missing
     part1 = part
    else:
     part1 = pfx + part
    newparts.append(part1)
   newpart = ','.join(newparts)
   dtabnew.append(newpart)
  newans1.append([g,dtabnew])
 new.append(newans1)
 #print 'new=',new
 return new

def test_s_file_init_alt_helper(filein,dbg=False):
 import codecs
 with codecs.open(filein,"r","utf8") as f:
  teststring=f.read()
 npass = 0
 nfail = 0
 for x in teststring.splitlines():
  if not (':' in x):
   continue
  if x.startswith(';'):
   continue
  x=x.rstrip('\r\n')
  #(xinput,xanswer)=x.split(':')
  (xinput,xanswer)=x.split('=')
  (key1,fg,key2)=xinput.split(',')
  #answer = xanswer.split(',')
  answer = xanswer
  #if len(answer)==1:
  # answer=answer[0]
  compute=s_file_init_alt_helper(key1,fg,key2,dbg)
  if compute == None:
   compute = '::'

  if compute == answer:
   status = "PASS"
   npass = npass + 1
   out="s_file_init_alt_helper(%s,%s,%s) %s" %(key1,fg,key2,status)
  else:
   status = "FAIL"
   nfail = nfail + 1
   if answer == None:
    answer = ''
   if compute == None:
    compute = ''
   out=("s_file_init_alt_helper(%s,%s,%s) %s" %(key1,fg,key2,status)) + "\n" +\
       ("computed=%s\n" %compute) + "\n" +\
       ("Elisp=%s\n" % answer) + "\n"
  print out
 print "%s cases PASS, %s cases FAIL" %(npass, nfail)

def test_sandhi():
 pairs = ['f-f']
 for pair in pairs:
  if pair not in init.SandhiPair.pairdict:
   print "test_sandhi, NOT FOUND:",pair
   continue 
  items = init.SandhiPair.pairdict[pair].items  # SandhiItem list
  for item in items:
   print "test_sandhi: %s -> %s" %(pair,item)
  
def test_subanta_convert_form():
 tests = [
  [['rAma','m'],['m', 'a']],
  [['idam','pron'],['pron', 'irr']]
 ]
 for (inputs,answer) in tests:
  (citation,formin) = inputs
  compute = subanta_convert_form(citation,formin)
  if compute == answer:
   status = "PASS"
  else:
   status = "FAIL % s" % compute
  out = "subanta_convert_form(%s,%s)=>%s [%s]" %(citation,formin,answer,status)
  print out

def test1():
 test_sandhi()
 #test_subanta_base()
 test_subanta_convert_form()
 test_convert_subanta_lexinfo()
 print "-"*60
 test_s_file_init_alt_helper("declension_test.txt")
 #test_word_parts()
 test_init_genform()

if __name__ == "__main__":
 #test1()
 import sys
 if len(sys.argv)>1:
  filein = sys.argv[1]
 else:
  filein = "declension_test.txt"
 dbg=False
 if len(sys.argv) > 2:
  tmp = sys.argv[2]
  if tmp == '-d':
   dbg=True
 test_s_file_init_alt_helper(filein,dbg)

