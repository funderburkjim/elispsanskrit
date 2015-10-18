; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; kta.el  
; begun 09-10-03
; Construction of the past passive participle
; based upon Kale, p. 421-431.
; kta-join1 is similar to 'perfect-join1' in gram2-lit.el
; Also, construction of indeclineable past participles,
; based on Kale p.440-445 (tvaa and lyap)
; Also, infinitive ('inf')
; Also, potential participles (Kale 432-440) (potpart)
(defun kta-doc ()
 "Past Passive Participles.
 Kale 679. The Past Passive Participle is formed by the addition
  of the affix 'ta' to the root.
 Kale 680. Roots capable of taking samprasaarana take it before
  the affix 'ta'.
 Kale 682. The penultimate radical nasal is generally dropped.
  NOTE: Kale 584 gives a list, which is used
 Kale 683. The augment 'i' may be prefixed to 'ta'
 Kale 684. As a general rule, the following roots do not insert 'i':
   all roots ending in a vowel which are 'veT' and
   all roots ending in a consonant which are 'aniT'.
 Kale 685. 'seT' roots (subject to 684) generally admit of 'i'
  All derived roots admit of 'i'
  Roots of the 10th class reject their final 'ay'.
    NOTE: The root undergoes the 10th class conjugational changes
  Causals reject their final 'ay'.
  Parasmaipada Frequentatives reject their final 'a'.
  Atmanepada Frequentatives reject their final 'y'
 Kale 696(a)
  Roots ending in a nasal lengthen their penultimate vowel before
  - a weak termination  beginning with any consonant
    (except a nasal or a semivowel)
  - the affix 'kvip'. 
  e.g., 'sham' -> 'shaanta', 'kram' -> 'kraanta'
 Kale 696(b)
  The following roots drop their final nasal before
  a weak termination  beginning with any consonant
    (except a nasal or a semivowel).
  - 'aniT' roots ending in a nasal,
  - 'the root 'van' (1P),
  - the eight roots of the 8th class ending in 'n'
  - some others (see p. 354)
 Kale 697.
  The roots 'khan jan and san' drop their nasal and at the same time
  lengthen their vowel.
 "
)
(defun tvaa-doc ()
 "Kale 744. The Indeclinable Past Participles are of the nature of
   gerunds. They fall under two heads: (1) Those derived by affixing
   'tvaa' to the simple root; and (2) Those derived by means of 
   'ya' affixed to the root compounded with prepositions or words
   used prepositionally. For example:
   'gam' -> 'gatvaa' (having gone)
   'bhuu' with 'anu' -> 'anubhuuya' (having experienced)
 Kale 745.
  The Indeclinable Past Participle or gerund in 'tvaa' is formed of
  all roots or derivative verbal bases to which no preposition (nor
  a prepositional word) is prefixed.  The 'tvaa' is of the same nature
  as the 'ta' of the Past Passive Participle; so that all the changes
  that take place before 'ta' generally also take place before 'tvaa'.
  In other words, form the P.P. Ptc. of a root, separate the 'ta' or
  'na' from it, add 'tvaa' instead, and this will be the form of the
  gerund in 'tvaa' (generally).
 Kale 746. When the intermediate 'i' is inserted, the preceding vowel
  takes its guNa substitute:
  'shi' -> 'shayitvaa', 'ku' -> 'kavitvaa', 'jaagRi' -> 'jaagaritvaa'
 Kale 746(a) The following roots take guNa optionally:
  'tRiSh mRiSh kRiSh Rit':
  'mRiSh' -> 'mRiShitvaa' or 'marShitvaa'
  'kRiSh' -> 'kRiShitvaa' or 'karShitvaa' ; not clear why accepts 'i'
  'Rit' -> 'Rititvaa' or 'artitvaa'
 Kale 746(b). But the following roots do not take guNa:
  - 'mRiD mRid gudh kuSh muSh klish' ('klish' is 'veT')
  - the roots mentioned at #463 (kale-463-P in gram2-liT.el)
  - the root 'vij' (class 7)
   'mRiD' -> 'mRiDitvaa' (having rejoiced)
   'gudh' -> 'gudhitvaa' (having covered)
   'kuSh' -> 'kuShitvaa' 
   'muSh' -> 'muShitvaa'
   'klish' -> 'klishitvaa kliShTvaa'
   'kuT' -> 'kuTitvaa'
   'vij 7' -> 'vijitvaa'
 Kale 747. The following roots admit 'i' optionally before 'tvaa':
  - 'veT' roots except
    1. 'trashch' takes 'i' necessarily
    2. 'svRi sRi dhuu' reject 'i'
  - the five roots given under #472, namely
    'iSh sah (1A) lubh riSh ruSh'
  - roots marked with an indicatory 'u' (see 'indicatory-u-P' below)
 Kale 748. The following roots admit 'i' (definitely) before 'tvaa':
  - The roots 'shvi Dii shii puu jRI'
  - 'seT' roots ending in consonants
  - roots of the 10th class and all derivative verbs
 Kale 748(cont). Roots of the 10th class preserve their 'ay' 
  before 'tvaa'
 Kale 749(a) The roots 'skand syand' do not drop their nasals:
  'skand' -> 'skantvaa', 'syand' -> 'syantvaa syanditvaa'
 Kale 749(b) The penultimate nasal of the following roots are 
   optionally dropped before 'tvaa':
  - roots ending in 'th' or 'ph'
  - 'va~nch' (to roam abroad, to deceive)
  - 'lu~nch' (to tear out)
   'granth' -> 'granthitvaa grathitvaa'
   'gumph' -> 'gumphitvaa guphitvaa'
   'va~nch' -> 'va~nchitvaa vachitvaa vaktvaa' (indicatory-u so 'veT')
   'lu~nch' -> 'lu~nchitvaa luchitvaa'
 Kale 749(c) The penultimate nasal of the following roots are 
   optionally dropped before 'tvaa'
  - roots ending in 'j' (such as 'bha~nj ra~nj sa~nj sya~nj')
  - the root 'ta~nch'
   'bha~nj' -> 'bhaMktvaa bhaktvaa'
   'ra~nj' -> 'raMktvaa raktvaa'
   'a~nj' -> 'a~njitvaa a~Nktvaa aktvaa'
 Kale 749(d) The following roots insert a nasal optionally before 'tvaa':
   'masj' -> 'maktvaa maMktvaa' (note 's' is dropped)
   'nash' -> 'nashitvaa naShTvaa naMShTvaa'
 Kale 750. Guna is optionally substituted in the following case when
  'tvaa' has 'i' prefixed to it:
  - The vowel is 'i' or 'u' and 
    the root begins with any consonant and
    the root ends in any consonant except 'y' or 'v'
   'likh' -> 'likhitvaa lekhitvaa'
   'klid' -> 'kliditvaa kleditvaa klitvaa'
   'lubh' (6 P) -> 'lubhitvaa lobhitvaa'
   'dyut' -> 'dyutitvaa dyotitvaa'
   'riSh' -> 'riShitvaa reShitvaa riShTvaa'
   but 'div' -> 'devitvaa'
"
)
(defun lyap-doc ()
 "Kale 751.
  When a root is compounded with one or more prepositions or words
  prefixed to verbs like prepositions, the Indeclinable participle
  is formed by affixing 'ya' immediately to it. The 'ya' is changed
  to 'tya' after a short radical vowel (even when this vowel combines
  with the final vowel of a preposition to a long vowel).
 Kale 752. 
  Several types of root modification are applicable:
   - Kale 394: 'RI' may become 'ir iir u uur' 
   - Kale 395: penultimate 'i u Ri Li'
   - Kale 459: roots ending in 'e ai o'
   - Kale 502: samprasaaraNa
   - Kale 587: ?? I think this may have been '584' (drop penultimate nasal)
   -     
 Kale 753.
  Some roots drop a final nasal:
  - Roots of the 8th class, except 'san', necessarily drop a final nasal
  - The roots 'man van han' necessarily drop a final nasal
  - The roots 'gam nam yam ram' optionally drop a final nasal
    'tan' with 'vi' -> 'vitatya' , 'man' with 'ava' -> 'avamatya'
    'yam' with 'ni' -> 'niyamya' or 'nimatya'
    'ram' with 'vi' -> 'viramya' or 'viratya'
 Kale 754. Some other cases of final nasals:
  - 'khan' with 'ni' -> 'nikanya nikaaya'
  - 'jan' with 'pra' -> 'prajanya prajaaya'
  - 'san' with 'pra' -> 'prasanya prasaaya'
 Kale 755. 
  - 'kShi' lengthens its 'i' before 'ya': with 'pra'-> 'prakShiiya'
  - 'jaagRi' gunates its final vowel: with 'pra' -> 'prajaagarya'
 Kale 756.
  - 've' and 'jyaa' do not take samprasaaraNa: 'pravaaya', 'prajyaaya'
  - 'vye' generally does not take samprasaaraNa: 'upavyaaya'
  - 'vye' with 'pari' and 'sam' takes samprasaaraNa optionally:
    'parivyaaya pariviiya', 'saMvyaaya saMviiya'
 Kale 757. 
  The roots mentioned in #486 do not change their 'aa' to 'ii'.
  These are 'daa' (3 U 1 P), 'dhaa do de dhe maa sthaa',
    and 'gaa' (substituted for 'i' (2 P) and 'i' with 'adhi'
   - with 'pra', 'daa' -> 'pradaaya', 'dhaa' -> 'pradhaaya',
       'maa' -> 'pramaaya':
 Kale 758.
  Roots of the 10th class and causals preserve their 'ay' before 'ya'
   if it be preceded by a short penultimate vowel; before a long
   penultimate vowel the 'ay' is dropped before 'ya'.
   'pra' with 'chur' -> 'prachorya'
   'pra' with causal of 'budh' -> 'prabodhya'
   'vi' with causal of 'kRI' -> 'vikaarya'
   'aa' with causal of 'nii' -> 'aanaayya'
   'vi' with 'gaN' -> 'vigaNayya'
   'pra' with 'nam' -> 'praNamayya' (note: why not optionally 'praNaamya'?)
   'pra' with 'bhid' -> 'prabebhidayya' ???
   NOTE: It appears that the conjugational base is used for class 10 roots,
   and the causal base for causal roots.
 Kale 759. The causal of 'aap' may retains its 'ay' optionally:
    with 'pra' -> 'praapya praapayya'
 Kale 760. 
  Desiderative bases add 'ya' immediately.
     'prabubodhiShya'
  Frequentative bases add 'ya' after dropping their 'ya' when
   it is preceded by a consonant and after dropping the final 'aa'
   when it is preceded by a vowel.
     'bodh' -> 'prabobudhya'
     'bhuu' -> 'prabobhuuya'
"
)
(defun inf-doc ()
 "Kale 776.
  The Infinitive is formed by the affix 'tum' with the
  same effect as the 'taa' of the future.
  Note: The code takes the '3S' of the 'luT', and
  replaces the ending 'aa' with 'um'.
 "
)
(defun potpart-doc ()
 "Kale 711.
   The Potential Participle is formed by means of the affixes
   'tavya aniiya ya' and rarely 'elim', added to a root or 
   derivative verb.  This is Passive when the verb is transitive
   and impersonal when the verb is intransitive.  It is also used
   like an adjective when denoting fitness, etc.
 Kale 712.
  The affixes 'tavya aniiya' are added to roots or derivative bases
  in the sense of 'must be, fit to be, etc.'. 
  Before 'tavya' or 'aniiya', the ending vowel and the 
   penultimate short vowel of a root take their guNa substitute.
  Before 'tavya', 'seT' roots take 'i', 'aniT' roots do not, and
   'veT' roots take it optionally.
  Before 'aniiya', penultimate 'Ri' is always changed to 'ar', 
   and not to 'r' (as it sometimes does).
 Kale 713.
  The final 'ay' of roots of the 10th class is dropped before 'aniiya'.
  The final 'ay' of causals  is dropped before 'aniiya'.
  The final 'a' of Atmanepadi Frequentative Bases, when the 'ya' is
   preceded by a vowel,  is dropped before 'aniiya'.
  The final 'ya' of Atmanepadi Frequentative bases, when the 'ya' is
   preceded by a consonant, is dropped before 'aniiya'.
  To Desiderative bases, 'aniiya' is added without any change.
 "
)
(defun potpart-yat-doc ()
 "
 Kale 714.
  The affix 'ya' ('yat') is added on to roots ending in a vowel in the
  sense of 'fit for, fit to be, ought to be'.
  Before this 'ya', the radical vowel undergoes guNa substitute.
  Before this 'ya', final 'aa' (and 'e ai o' changeable to 'aa') is
   changed to 'e'
 Kale 715.
  Roots having 'a' for their penultimate and ending in a consonant of
  the labial class take the affix 'ya':
   'shap' -> shapya, labh -> labhya, ram -> ramya
 Kale 716.
  The roots 'tak' (to laugh at), 'shas' (to kill), 'chat' (to look for,
  to ask), 'yat' (to strive), 'jan', 'shak', and 'sah' take the affix
  'ya'.
 Kale 717.
  The roots 'gad mad char yam' take the affix 'ya' when not preceded by
  a preposition, or when preceded by the preposition 'aa'.
  Note: 'aa' with 'char' takes 'ya' provided the meaning is NOT 'preceptor'
  'aa' with 'char' takes 'Nyat' (becoming 'aachaarya') when meaning
  'preceptor'.
 Kale 718.
  Some roots, when having certain meanings, take 'ya':
  - 'vad' when implying reproach; otherwise 'kyap'
  - 'paN' meaning 'to transact business'; otherwise 'Nyat'
  - 'vRi' 9 A , not denoting limit or restraint; otherwise 'kyap'
 Kale 719.
  - 'vah', when not denoting an instrument for carrying, takes 'ya';
     otherwise, 'Nyat'
  - 'Ri' in the sense of 'a master or a Vaishya' takes 'ya';
     otherwise, 'Nyat'
 Kale 720.
  - 'sRi' with 'upa' in the sense of 'being conceived' takes 'ya';
    otherwise 'sRi' with 'upa' takes 'Nyat'
 Kale 721.
  - 'jRI' with the negative particle 'a' and qualifying 'saMgataM' 
    (friendship) takes 'ya'.
    When qualifying something else, it takes the affix 'tRi'
     (ajaritaa kambalaH : new blanket)
 Kale 722.
  - 'han' takes the affix 'yat' optionally, before which 'vagh' is
    substituted for it: 'han' + 'ya' = 'vaghya' (what ought to be killed).
  - 'han' optionally takes 'Nyat', which which case 'ghaat' is 
    substituted for it: 'han' + 'Nyat' = 'ghaatya'
 "
)
(defun potpart-kyap-doc ()
 "Kale 723. The following roots take the affix 'kyap (ya)' in the
  same sense as 'yat':
  - the root 'i' (1,2 P) (to go)
  - roots 'stu shaas vRi' (5 P)
  - roots 'dRi juSh'
  - roots having 'Ri' for their penultimate, except 'kRip chRit'
  When a root ends in a short vowel, 't' is inserted between the final
  vowel and the affix 'ya': e.g., 
   - 'i' -> 'itya' (worthy of being approached)
   - 'stu' -> 'stutya' (deserving praise)
   - 'shaas' -> 'shiShya' (fit to be instructed)
   - 'vRit' -> 'vRitya' 
   - 'juSh' -> 'juShya' (worthy of being served)
   - 'vRidh' -> 'vRidhya' (fit to be increased as wealth)
 Kale 724.
  According to Vaamana, the roots 'shams duh guh' take this affix optionally.
  - 'shaMs' -> 'shasya' (praiseworthy)
  - 'duh' -> 'duhya'
  - 'guh' -> 'guhya'
  Optionally, they take the affix 'Nyat':
  - 'shaMs' -> 'shaMsya'
  - 'duh' -> 'dohya'
  - 'guh' -> 'gohya'
 Kale 725. 
  'mRij' takes this affix optionally:
   'mRijya' (wheat is fit or ought to be cleansed.
  Optionally, it takes 'Nyat', before which the final 'j' is changed to 'g':
   'maargya'
 Kale 726(a)
  'bhuu', when it has a Subanta prefixed to it and is without a 
   preposition takes the affix 'kyap' impersonally:
    'brahmaNo bhaavaH' -> 'brahmabhuuyam' (identity with Brahman)
  When no Subanta is used prepositionally with 'bhuu', it takes
  the affix 'yat': 'bhavya'
 Kale 726(b)
  'vad' under similar circumstances takes either 'kyap' or 'Nyat' in
  a passive sense or impersonally:
  'vadya' (kyap) ('brahmavadyam' = expounding the Veda)
  'udya' (Nyat) ('brahmodyam' = expounding the Veda)
 Kale 727. 'khan' takes 'kyap', but drops its 'n' and adds 'ii' to
  its penultimate vowel:
  khan + ya = kha + ya = kha + ii + ya = khe + ya = kheya (what is fit
    or ought to be dug or excavated)
 Kale 728. 'bhRi' (class 1) takes this affix when the participle so
  derived does not form a name:
    'bhRitya' (those who ought to be nourished or maintained = servants)
  'bhRi' takes 'Nyat' when a name is meant:
    'bhaarya' = 'bhRi' + 'Nyat' (a class of Kshatriyas)
  When 'sam' precedes 'bhRi', either of 'kyap' or 'Nyat' is added optionally:
   'saMbhRitya' , 'saMbhaarya'
  Note: the word 'bhaaryaa' (a wife) ought to be derived from 'bhRi'
   of the 3rd class and affix 'Nyat'
 Kale 729. The following 7 words are irregularly formed with the affix
  'kyap' in the sense given in each case:
  - 'raajabhuuyaH' (raajan + bhuu + kyap)
  - 'suuryaH' (sRi + kyap) or (suu (6 P) + kyap)
  - 'mRiShodya' (mRiShaa + vad + kyap) = falsehood
  - 'kupyam' (any base metal) from 'gup + kyap' (??)
  Note: 'gup' in other cases takes the affix 'Nyat':
   'gup' + 'Nyat' = 'gopya' (what ought to be concealed)
  - 'pachya' (pach + kyap) (growing in cultivated ground)
  - 'avyathya' = 'a + vyath + kyap' (not feeling pain)
 Kale 730(a)
  The following two words, which are the names of two rivers,
  are derived by means of the affix 'kyap':
  - 'bhinatti kuulaM bhidyaH' from 'bhid + kyap'
  - 'ujjhatyudakaM ujjhyaH' from 'ujjh + kyap'
 Kale 730(b)
  Similarly, the words 'puShyaH' and 'sidhyaH', both the names of the
  constellation Pushya, are derived from 'puSh' and 'sidh' respectively
  with the affix 'kyap'
 Kale 731.
  The roots 'puu nii ji', preceded by 'vi', take 'kyap' when the are
  connected with the words 'mu~nja kalpa hala', respectively:
  - vipuuyo mu~njaH : the Munja grass to be prepared for weaving into a rope
  - niniiyaH kalkaH : sin which ought to be destroyed
  - jityo haliH : the plough to be pulled with great force
 Kale 732.
  The following words are derived from the root 'grah' with affix 'kyap':
  - 'avagRihyam', 'pragRihyam' : two technical terms in grammar
  - 'gRihyakaaH' : birds restrained from free motion, such as parrots
  - 'graamagRihyaa senaa' : an army stationed outside a village
  - 'aaryagRihyaH' : siding with the noble
 Kale 733. 
  The roots 'kRi' and 'vRi' take both 'kyap' and 'Nyat':
  'kRitya', 'kaarya' : what ought to be done
  'vRiShya', 'vaarya' (not sure of 1st)
 Kale 734.
  The root 'yuj' in the sense of 'to be harnessed' takes the affix 'kyap'
  and changes its final to 'g':
  'yugyaH gauH' : a bull to be harnessed to the yoke
  In other senses, it takes the affix 'Nyat': yojya
 "
)
(defun potpart-Nyat-doc ()
 "Kale 735. 
  Roots ending in 'Ri' and those ending in a consonant take the affix
  'Nyat' ('ya') in the same sense as 'yat'.
  Before this affix,
   - the ending 'ch' of a root is changed to 'k'
   - the ending 'j' of a root is changed to 'g'
   - the final vowel and the penultimate 'a' take vrddhi
   - any penultimate vowel but 'a' generally takes guna.
  Examples:
   - 'kRi' -> 'kaarya' : what ought to be done
   - 'dhRi' -> 'dhaarya' : what ought to be worn
   - 'grah' -> 'graahya' : what ought to be held
   - 'dabh' -> 'daabhya' : what ought to be impelled
   - 'vach' -> 'vaakya' : what is arranged, a sentece
   - 'pach' -> 'paakya' : what is to be cooked
   - 'mRij' -> 'maargya' : what is to be purified
 Kale 736.
  The root 'vas', when preceded by 'amaa' (with) takes the affix 'Nyat'
  before which the penultimate 'a' is optionally changed to vrddhi.
  'amaavasyaa' or 'amaavaasyaa': the day on which the sun and moon
    are with each other, i.e., are in conjunction (e.g. eclipse?)
  The root 'sRij' when preceded by 'paaNi' or the preposition 'samava'
  takes 'Nyat': 'sargya'
 Kale 737(a)
  The roots 'yaj yaach ruch pravach tyaj pach' do not change
  their final consonant to 'k' or 'g' before 'Nyat'.
   e.g. yaajya, yaachya, rochya, pravaachya, archya, tyaajya, paachya
  Note 1: 'archya' must come from 'Rich' (praise, shine)
  Note 2: In #735, 'pach' is shown as 'paakya', so both forms must be ok.
 Kale 737(b)
  - 'vach' does not change its 'ch' to 'k' before 'Nyat' when the meaning
     is 'what ought to be spoken out, speech' : vaachya
  - 'vach' changes its 'ch' to 'k' otherwise: 'vaakyam' (a sentence)
 Kale 737(c)
  - 'va~nch' -> 'va~nchya' when it means 'to go'
  - 'va~nch' -> 'va~Nkya' when it means 'to bend'
 Kale 737(d)
  - 'yuj' when it is preceded by 'pra' and 'ni' takes the affix 'Nyat'
    in the sense of 'what is possible or capable of',
    and does not change its 'j' to 'g'
 Kale 737(e)
  - 'bhuj' -> 'bhojyam' (food)
  - 'bhuj' -> 'bhogya' (what is fit to be enjoyed)
 Kale 738. 
  Roots ending in 'u uu' take the affix 'Nyat' in the sense of
  'what ought to or must necessarily be done'.
  - 'luu' -> 'laavyam' (what must necessarily be cut off)
  - 'suu' (with 'aa') -> 'saavyam'
  - 'yu' -> 'yaavya' (to mix)
 Kale 738(a)
  The roots 'vap rap lap bhap chap' also do the same:
  - 'vap' -> 'vaapya' (what must necessarily be sown)
  - 'rap' -> 'raapya' (what must be spoken of distinctly)
  - 'lap' -> 'laapya'
  - 'bhap' -> 'bhaapya'  (could not find this root)
  - 'chap' -> 'chaapya'
 Kale 739. The following words are irregularly derived by means of
  the affix 'Nyat':
  - aanaayyaH : (aa nii + Nyat) : what ought to be brought from the
    Gaarhapatya, ie, the Dakshinaagni
    But 'aaneya' in other cases.
  - praNaayyaH : (pra nii + Nyat) : a jar, disgusted with worldly pleasures ??
 Kale 740 ??
  - 'paayam' a measure. From 'maa' ??
 Kale 741 ??
 "
)
(defun construct-pppart1a-helper (dhaatu class pada upasargas 
   &optional seT-kta dtype)
 (let (ending tok base ntok lc pc)
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (if (not seT-kta) 
   (setq seT-kta (kta-seT-code dhaatu class pada upasargas tok dtype))
  )
  (cond
   ((equal class 10)
    ; symbol or list
    (setq base (construct-conjbase1a dhaatu class pada upasargas))
    (if (not (listp base)) (setq base (list base)))
    (setq base (mapcar 
     (lambda (b)
      (substring (car (ITRANS-parse-words-1 (symbol-name b))) 0 -2)
     )
     base
    ))
    (setq base (solution base))
   )
   ((equal dtype 'c)  ; causal
    ; laT is present tense
    (setq base (causal-conjtab1a-bases dhaatu class pada upasargas 'laT))
    (if (not (listp base)) (setq base (list base)))
    (setq base (mapcar  ; remove the 'ay'
     (lambda (b)
      (substring (car (ITRANS-parse-words-1 (symbol-name b))) 0 -2)
     )
     base
    ))
    (setq base (solution base))
   )
   ((samprasaaraNa-P dhaatu class)
    (cond
     ((equal dhaatu 've) (setq base [u])) ; Kale p. 422 example
     ((equal dhaatu 'vah) (setq base [uu h])) ; Kale p. 422 example
     ((equal dhaatu 'bhrasj) (setq base tok)) ; Kale p. 422 example
     (t (setq base (samprasaaraNa tok)))
    )
   )
   ((or (and  (kale-584-P dhaatu)) ; see gram2-future.el (equal class 9)
	(member dhaatu '(dhvaMs shaMs staMbh))
    )
    ; Kale 682. penultimate radical nasal dropped (generally)
    (setq base (vconcat (substring tok 0 -2) (substring tok -1)))
   )
   ((equal lc 'RI)
    ; Kale 690.
    ; see also function 'kale-394 in gram2.el
    (let (x)
     (setq x (substring tok 0 -1))
     (if (or (labial-P pc) (equal pc 'v))
      (setq x (vconcat x [uu r]))
      (setq x (vconcat x [ii r]))
     )
     (setq base x)
    )
   )
   ((member lc '(e ai o))
    (setq base (vconcat (substring tok 0 -1) [aa]))
   )
   ((kale-696-P dhaatu)
    (setq base (substring tok 0 -1)) ; drop final nasal
    (when (member dhaatu '(khan jan san)) ; Kale 697
     (setq pc (lengthen-vowel pc))
     (setq base (vconcat (substring base 0 -1) (vector pc)))
    )
   )
   ((member dhaatu '(sham kram kSham kam cham vam klam shram dam))
    ; Kale 696(a). Many roots ending in a nasal don't lengthen the vowel
    ; Thus, I just give a list of those which do
    (setq pc (lengthen-vowel pc))
    (setq base (vconcat (substring tok 0 -2) (vector pc lc)))
   )
;   ((equal dhaatu 'guh) (setq base [g uu h]))
;   ((equal dhaatu 'lih) (setq base [l ii h]))
   (t (setq base tok)
   )
  )
  base
 )
)

(defun construct-pppart1a (dhaatu class pada upasargas &optional dtype)
 (let (ans ending tok base seT-kta ntok lc pc ans1)
  (setq ending [t a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (when (or (kale-414-P dhaatu)
	    (equal dhaatu 'jyaa)
	    (kale-692-P dhaatu class pada upasargas)
	    (equal dhaatu 'tRI)
    )
    ; Kale 690. The roots given at #414 (9th class roots ending in long vowel)
    ; and the root 'jyaa' substitute 'na' for 'ta'.
    (when (not (member dhaatu '(puu)))
     (setq ending [n a])
    )
  )
  (when (and (member lc '(aa e ai o))
	     (consonant-P (elt tok 0))
	     (semivowel-P (elt tok 1))
	     (not (member dhaatu '(khyaa dhyai vye hve)))
	)
    ; Kale 689. Roots ending in 'aa' (or 'e ai o' changeable to 'aa')
    ; and beginning with a conjunct consonant containing a semi-vowel
    ; substitute 'na' for 'ta'
    ; Exceptions are 
    ;  'khyaa' (to name) etc,
    ;  'dhyai' (to contemplate)
    ;  'vye' and 'hve'
   (setq ending [n a])
  )
  (setq seT-kta (kta-seT-code dhaatu class pada upasargas tok dtype))
  (setq base
    (construct-pppart1a-helper dhaatu class pada upasargas seT-kta dtype))
  (when nil 
   (fol-msg (format "chk: %s %s %s %s \n" dhaatu base seT-kta ending))
  )
  (setq ans (kta-join base seT-kta ending dhaatu))
  (when ans (setq ans (sym-without-space ans)))
  (when nil
   (fol-msg (format "chk: %s %s %s -> %s\n" dhaatu base seT-kta ans))
  )
  (when (not (member dtype '(c)))
   ; exceptions
   (setq ans1 
    (construct-pppart1a-exception dhaatu class pada upasargas))
   (if ans1 (setq ans ans1))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun construct-pppart1a-exception (dhaatu class pada upasargas)
 (let (ans)
  (cond
   ((equal dhaatu 'shaas) (setq ans 'shiShTa))
   ((equal dhaatu 'muh) (setq ans '(mugdha muuDha))) ; Kale p.423
   ((equal dhaatu 'bhrasj) (setq ans 'bhraShTa))
   ((equal dhaatu 'mRij) (setq ans 'mRiShTa))
   ((equal dhaatu 'sich) (setq ans 'sikta))
   ((equal dhaatu 'vah) (setq ans 'uuDha))
   ((equal dhaatu 'shii) (setq ans 'shayita)) ; Kale 684(a)
   ((equal dhaatu 'jaagRi) (setq ans 'jaagarita))
   ((equal dhaatu 'sthaa) (setq ans 'sthita))
   ((equal dhaatu 'daridraa) (setq ans 'daridrita))
   ((equal dhaatu 'sasj) (setq ans 'sajjita))
   ((and (equal dhaatu 'as) (equal class 2)) (setq ans 'bhuuta))
   ((equal dhaatu 'yaj) (setq ans 'iShTa))
   ((equal dhaatu 'dhaa) (setq ans 'hita))
   ((equal dhaatu 'daa) (setq ans 'datta))
   ((equal dhaatu 'druh) (setq ans '(drugdha druuDha)))
   ((equal dhaatu 'vid)
    (cond
    ; Kale 688(a). 'vid' (6 P A) takes 'ta' in the sense of
    ; 'fit for enjoyment' or 'famous'. It takes 'na' in other cases
     ((equal class 7) (setq ans '(vitta vinna)))
     ((equal class 6) (setq ans '(vidita vitta)))
     ((equal class 4) (setq ans 'vinna)) ; Kale 692
    )
   )
   ((equal dhaatu 'shuSh) (setq ans 'shuShka))
   ((equal dhaatu 'siv) (setq ans 'syuuta))
   ((equal dhaatu 'ruSh) (setq ans 'raShTa))
   ((equal dhaatu 'so) (setq ans 'sita))
   ((equal dhaatu 'snih) (setq ans 'snigdha))
   ((equal dhaatu 'saadh) (setq ans 'saddha))
   ((equal dhaatu 'sRij) (setq ans 'sRiShTa))
   ((equal dhaatu 'Chad) (setq ans '(Chaadita Channa)))
   ((equal dhaatu 'shap) (setq ans '(shapita shapta)))
   ((equal dhaatu 'tRiSh) (setq ans '(tRiShita tRiShTa)))
   ((equal dhaatu 'bha~nj) (setq ans 'bhagna))
   ((equal dhaatu 'mad) (setq ans 'matta))
   ((equal dhaatu 'vij) (setq ans 'vigna))
   ((equal dhaatu 'jyaa) (setq ans 'jiina))
   ((equal dhaatu 'kShi) (setq ans '(kShita kShiiNa)))
   ((equal dhaatu 'kShan) (setq ans '(kShata))) ;Whitney   
   ((equal dhaatu 'grah) (setq ans 'gRihiita))
   ((equal dhaatu 'du) (setq ans '(duta duuna)))
;   ((equal dhaatu 'su) (setq ans '(suuna)))
   ((equal dhaatu 'suu) (setq ans '(suuna suuta))) ; Kale 692
   ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hiina)) ; abandon
   ((and (equal dhaatu 'haa) (equal pada 'A)) (setq ans 'haana)) ; abandon
   ((equal dhaatu 'vrashch) (setq ans 'vRikNa))
   ((equal dhaatu 'sphurj) (setq ans 'sphuurgNa))
   ((equal dhaatu 'sphuurj) (setq ans 'sphuurgNa))
   ((equal dhaatu 'vai) (setq ans 'vaana))
   ((and (equal dhaatu 'bhuj) (equal class 6)) (setq ans 'bhugna))
   ((equal dhaatu 'masj) (setq ans 'magna))
   ((equal dhaatu 'Ri)
    ; Kale 693(a). 'Ri' when it means 'to incur debt' substitutes 'na';
    ; when it means go, 'ta' is used.
    (setq ans '(RiNa Rita)))
   ((equal dhaatu 'div) (setq ans '(dyuuta dyuuna)))
   ((equal dhaatu 'vaa) (setq ans '(vaana vaata)))
   ((equal dhaatu 'vyai) (setq ans '(shyaana shiina shiita)))
   ((equal dhaatu 'nud) (setq ans '(nunna nutta)))
   ((equal dhaatu 'und) (setq ans '(unna utta)))
   ((equal dhaatu 'tryai) (setq ans '(traaNa traata)))
   ((equal dhaatu 'ghraa) (setq ans '(ghraaNa ghraata)))
   ((equal dhaatu 'hrii) (setq ans '(hriiNa hriita)))
   ((equal dhaatu 'pyaay) (setq ans '(piina pyaana)))
   ((equal dhaatu 'haa) (setq ans '(haana hiina)))
   ((equal dhaatu 'duu) (setq ans '(duuta duuna)))
   ((equal dhaatu 'granth) (setq ans '(grathita granthita)))
   ((equal dhaatu 'manth) (setq ans '(mathita manthita)))
   ((equal dhaatu 'gai) (setq ans 'giita)) 
   ((equal dhaatu 'si) (setq ans '(sita sina)))
   ((equal dhaatu 'tvar) (setq ans '(tvarita tuurNa)))
   ((equal dhaatu 'trai) (setq ans '(traata traaNa)))
   ((equal dhaatu 'ad) (setq ans '(jagdha anna)))
   ((equal dhaatu 'ard) (setq ans '(arNNa arNa)))
   ((equal dhaatu 'uuth) (setq ans 'uta))
   ((equal dhaatu 'kaSh) (setq ans '(kaShTa kaShita)))
   ; removed 5/23/05. ref. Whitney. result: ans=(karshita)
   ((equal dhaatu 'kRish) (setq ans '(karshita))) ; 5/23/05 ref. Whitney
;   ((equal dhaatu 'kRish) (setq ans 'kRisha)) ; removed 5/23/05
   ((equal dhaatu 'kShiiv) (setq ans 'kShiiva))
   ((equal dhaatu 'knuuth) (setq ans 'knuuta))
   ((equal dhaatu 'kShmaay) (setq ans '(kShmaayita kShmaata)))
   ((equal dhaatu 'kShai) (setq ans 'kShaama))
   ((equal dhaatu 'Cho) (setq ans '(Chaata Chita)))
   ((equal dhaatu 'jyo) (setq ans 'jiita))
   ((equal dhaatu 'do) (setq ans 'dita))
   ((equal dhaatu 'dRih) (setq ans '(dRiDha dadRimhita)))
   ((equal dhaatu 'dhaa) (setq ans 'hita))
   ((equal dhaatu 'dhaav) (setq ans '(dhauta dhaavita)))
   ((equal dhaatu 'dhe) (setq ans 'dhiita))
   ((equal dhaatu 'pach) (setq ans 'pakva))
   ((and (equal dhaatu 'paa) (equal class 1)) (setq ans 'piita)) ; drink
   ((equal dhaatu 'puuth) (setq ans 'puuta))
   ((equal dhaatu 'phal) (setq ans '(phalita phulla)))
   ((equal dhaatu 'mav) (setq ans '(mavita muuta)))
   ((equal dhaatu 'maa) (setq ans 'mita))
   ((equal dhaatu 'me) (setq ans 'mita))
   ((equal dhaatu 'murchCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'murCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'muurchCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'muurCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'laagh) (setq ans 'laagha)) ; with 'ut'
   ((equal dhaatu 'sho) (setq ans '(shaata shita)))
   ((equal dhaatu 'bRih) (setq ans '(bRihita bRiDha)))
   ((equal dhaatu 'bRiMh) (setq ans '(bRiMhita bRiDha)))
   ((equal dhaatu 'vRih) (setq ans '(vRihita vRiDha)))
   ((equal dhaatu 'vRiMh) (setq ans '(vRiMhita vRiDha)))
   ((equal dhaatu 'sriv) (setq ans 'sruta))
   ((equal dhaatu 'hlaad) (setq ans '(hlanna hlaadita)))
   ((equal dhaatu 'shraa) (setq ans '(shRita shraaNa shrapita)))
   ; stambh with 'prati' or 'ni' -> pratistabhda, nistabhda
   ; The 's' does not change to 'Sh' here. (Kale p. 430)
   ((equal dhaatu 'sphaay) (setq ans 'sphiita))
   ; Kale p. 430. 'stiita' and 'stiima' with 'pra' : 'sounded'
   ((equal dhaatu 'styai) (setq ans '(styaana stiita stiima)))
   ; can have other forms ('niShNaata' and 'nadiiShNa') with some meanings
   ((equal dhaatu 'snaa) (setq ans 'snaata))
   ((equal dhaatu 'chyu) (setq ans 'chyuta))
   ((equal dhaatu 'Dii) (setq ans '(Dayita Diina)))
   ((equal dhaatu 'pyai) (setq ans 'piina))
   ((equal dhaatu 'budh) (setq ans '(buddha bodhita)))
   ((equal dhaatu 'bhram) (setq ans '(bhraanta bhramita)))
   ((equal dhaatu 'lasj) (setq ans 'lajjita))
   ((equal dhaatu 'shubh) (setq ans 'shobhita))
   ((equal dhaatu 'shchut) (setq ans '(shchutita shchotita))) ; Antoine2-gloss
   ((equal dhaatu 'shvi) (setq ans 'shuuna)) ; Antoine2-gloss
   ((equal dhaatu 'yat) (setq ans '(yatita yatta)))
   ((equal dhaatu 'bru) (setq ans 'ukta))
   ((equal dhaatu 'ruh) (setq ans 'ruuDha))
   ((equal dhaatu 'ShThiv) (setq ans 'ShThyuuta))
   ((equal dhaatu 'dhuu) (setq ans '(dhuuta dhuuna))) ; Antoine2-gloss, Apte
   ((equal dhaatu 'svid) (setq ans '(svidita svinna))) ; Apte
   ((equal dhaatu 'puu) (setq ans '(pavita puuta)))
   ((equal dhaatu 'RiSh) (setq ans '(RiShTa))) ; was '(RiShita)
;   ((equal dhaatu ') (setq ans '))
  )
  ans
 )
)
(defun kta-seT-code (dhaatu class pada upasargas &optional tok dtype)
 (let (seT-code ans lc pc)
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas dtype))
;  (fol-msg (format "seT-code chk: %s\n" seT-code))
  (if (not tok)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  )
  (setq lc (elt (substring tok -1) 0)) ; last char
  (cond
   ((member dtype '(c C))
     ; don't apply special cases to causal. 
     (setq ans seT-code)
   )
   ((member dhaatu '(riSh ruSh)) (setq ans 'aniT)) ; riSh dhaatukosha
   ((equal dhaatu 'lubh)
    (cond
     ((member class '(1 4)) (setq ans 'aniT))
     ((equal class 6) (setq ans 'seT))
     (t (setq ans 'veT))
    )
   )
   ((and (equal dhaatu 'vas) (equal class 1)) (setq ans 'seT))
   ((equal dhaatu 'vas) (setq ans 'seT))
   ((and (equal dhaatu 'kShubh) (equal class 1)) (setq ans 'seT))
   ((equal dhaatu 'kShubh) (setq ans 'veT)) ; classes 4,9
   ((and (equal dhaatu 'puSh) (equal class 4)) (setq ans 'veT))
   ((equal dhaatu 'puSh) (setq ans 'aniT)) ; classes 1,9
   ((and (equal dhaatu 'vid) (equal class 2)) (setq ans 'seT))
   ((and (equal dhaatu 'vid) (equal class 4)) (setq ans 'aniT))
   ((and (equal dhaatu 'vid) (equal class 7)) (setq ans 'aniT))
   ((and (equal dhaatu 'vid) (equal class 6)) (setq ans 'veT))
   ((and (equal dhaatu 'ghuSh) (equal class 1)) (setq ans 'aniT))
   ((and (equal dhaatu 'tRih) (equal class 7)) (setq ans 'seT))
   ((equal dhaatu 'ash)
    (if (equal class 5)
     (setq ans 'aniT)
     (setq ans 'seT)
    )
   )
   ((member dhaatu '(kShudh svid kuSh))
    (setq ans 'seT)
   )
   ((and (equal dhaatu 'ghuuSh) (equal class 1)) (setq ans 'aniT))
   ((member dhaatu '(chit vRidh vRit dhvaMs sraMs vRiSh
		     shaMs  pRich vRij as iSh Ridh
		     kRish klam diip bhraMsh mad tras dambh
		     dhRiSh raadh stambh juSh a~nj indh
		     kShaN kShiN tan van kam khan kram
		     cham   ghRiSh yat vam jan shram
		     gras dam akSh
		    ))
    (setq ans 'aniT)
   )
   ((member dhaatu '(shvas klish puu kShubh puSh
		     dam sham pur das spash Chad shap
		     ruSh am
		    ))
    (setq ans 'veT)
   )
   ((and (equal dhaatu 'ghuSh) (equal class 1) (equal upasargas '(sam)))
    (setq ans 'veT)
   )
   ((and (equal dhaatu 'svan) (equal upasargas '(aa)))
    (setq ans 'veT)
   )
   ((and (equal dhaatu 'hRiSh) (member class '(1 4)) (equal pada 'P))
    ; Kale 686(a). When used with 'loman' (the air), or when it
    ; means 'to be surprised or disappointed'
    (setq ans 'veT) 
   )
   ((member dhaatu '(tRiSh tvar dhRiSh phral bhid
		      murchCh sphurCh sphurj kShchid kShvid svid))
    ;Kale 686(c)
    ; These roots admit 'i' optionally before 'ta' when the
    ; P.P. Participle is used impersonally, or conveys the sense of
    ; beginning to perform the action or to under the state
    ; expressed by the root. When the participle is not used in this
    ; sense, these roots reject 'i'.
    ; NOTE :There are some class-pada conditions that
    ; I have not implemented here
    (setq ans 'veT)
   )
   ((equal dhaatu 'a~nch)
    ; Kale 687(a). 'a~nch' in the sense of 'to worship' takes 'i'
    ; in the sense of 'to go', it rejects 'i'
    (setq ans 'veT)
   )
   ((member dhaatu '(dhRiSh shas))
    ; Kale 687(b). 'dhriSh' and 'shas' reject 'i' when they express
    ; the idea of immodesty or rudeness;
    ; When 'dhRiSh' means 'to overpower', it accepts 'i'.
    ; When 'shas' meas 'to torment', it accepts 'i'
    (setq ans 'veT)
   )
   ((equal seT-code 'veT)
    ; Kale 684
    (setq ans 'aniT)
   )
   ((and (equal seT-code 'aniT) (consonant-P lc))
    ; Kale 684
    (setq ans 'aniT)
   )
   ((and (vowel-P lc) (not dtype))
    ; Kale 684
    ; 12-24-04: don't override when derived type code is present . 
    ; motivating example: causal of 'sthaa' should be 'seT'
    (setq ans 'aniT)
   )
   (t (setq ans seT-code)
   )
  )
  ans
 )
)
(defun kale-696-P (dhaatu)
 (if (member dhaatu '(
     man ; to think
     han ; to strike
     ram ; to sport
     gam ; to go
     tan ; to spread
     kShaN ; to hurt
     RiN ; to go
     nam ; to bow
     yam ; to restrain
     van ; 1P to sound, to serve
     ghRiN ; to shine
     tRiN ; to graze
     van ; to beg
     khan ; to dig
     jan ; to be born
     san ; to love
     kShiN
     ))
  t
 )
)
(defun kale-692-P (dhaatu class pada upasargas)
  ; Kale 692. The following roots substitute 'na' for 'ta'
  ; in the past passive participle
 (if (or
      (member dhaatu '(
       Dii ; 4A to fly
       duu ; torment
       dhii ; hold, accomplish
       lii ; 4A melt
       mii ; 4A give pain
       dii ; perish, waste
       rii ; hurt
       haa ; go (3A) haana; abandon (3P) hiina
       vij 
       vrashch
       sphurj ;
       vai ; to dry, be languid 'vaana'
       vrii ; 4A move, cover
       bha~nj ; break 'bhagna'
       masj ; 6P bathe, sink 'magna'
       lasj ; 1A be-ashamed 'lagna'
       skand ; with 'vi': 'viskanna'
             ; with 'pari': pariskanna, pariShkaNNa
      ))
      (member (list dhaatu class) '(
       (suu 4) ; 4A bring-forth, produce.  Note 2A has 'suuta'
       (bhuj 6) ; 6P bend 'bhugna'
       (ruj 6) ; break 'rugna'
       (laj 6) ; be-ashamed 'lagna'
       (vid 4) ; vinna
      ))
     )
  t
 )   
)
(defun kta-join (base-tok seT-code sup dhaatu)
 (cond 
  ((listp sup)
   (mapcar (lambda (x) (kta-join base-tok seT-code x dhaatu)) sup))
  ((listp base-tok)
   (mapcar (lambda (x) (kta-join x seT-code sup dhaatu)) base-tok))
  ((equal seT-code 'veT)
   (mapcar (lambda (x) (kta-join base-tok x sup dhaatu)) '(seT aniT))
  )
  (t (kta-join1 base-tok seT-code sup dhaatu))
 )
)
(defun kta-join1 (y seT-code ending0 dhaatu)
 ; based on 'conjugation-join'
 ; seT-code is either nil, 'seT' or 'aniT'
 (let (ans skiprefs yfirst ylast efirst ending y0 ny pc)
  ; insert 'i' if needed
  (setq ending
   (if (equal seT-code 'seT)
    (conjugation-join [i] ending0)
    ending0
   )
  )
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ny (length y))
  (when (< 0 ny)
   (setq ylast (elt (substring y -1) 0)) ;last char
   (setq yfirst (elt y 0))
  )
  (if (<= 2 ny)
   (setq pc (elt y (- ny 2))) ;penultimate char of 'y'
  )
  (setq efirst (elt ending 0))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  ;Kale 476. nash
  ; 'n' is inserted before the ending consonant of 'nash' when
  ; it is followed by any consonant except a nasal or a semi-vowel.
  ; NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
  ; NOTE 2: The logic is put here, because other changes, e.g.,
  ;  before 'tha', are required. This logic applies to other
  ;  forms than the perfect
  (when (and (equal y [n a n a sh])
	     (consonant-P efirst)
	     (not (semivowel-P efirst)))
   (setq y [n a n a M sh])
  )
  (cond
   ((and (equal efirst 't) (member dhaatu '(sah)))
    ; Kale #506. p. 317
    ; When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
    ; is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
    ;  vavah + tha =
    ;   uvah + tha =
    ;   uvaDh + Dha (by #416-3,4) =
    ;   uvaDh + Dha =
    ;   uvoDha
    (setq ans (vconcat (substring y 0 -2) [o Dh] (substring ending 1)))
   )
   ((and (member dhaatu '(dih duh))
	 (equal efirst 't)
    )
    ; digdha Kale p. 346
    (let (ylast1)
     (setq ylast1 (cond
      ((equal yfirst 'd) [g])
      ((equal yfirst 'n) [d])
      (t (vector ylast))
     ))		
     (setq ans (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
    )
   )
   ((and (member dhaatu '(lih guh))
	 (equal efirst 't)
    )
    ; digdha Kale p. 346
    (setq y0 (substring y 0 -1))
    (setq ylast (elt (substring y0 -1) 0))
    (setq y0 (vconcat (substring y0 0 -1)
		      (vector (lengthen-vowel ylast))))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))    
   )
   ((and (equal efirst 't) (member ylast '(j ch)))
    ; this rule [bh a j] + [th a] -> [bh a k th a]
    ; rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
    ; but [bh a ~n j] + [th a] -> [bh a ~N k th a]
;    (fol-msg (format "y=%s ending=%s\n" y ending))
     (setq y0 (substring y 0 -1))
     (setq ans (vconcat y0 [k] (vector efirst) (substring ending 1)))
   )
   ((and (equal efirst 't) (equal ylast 'd))
    ; Kale 688. 'na' is substituted for 'ta' when it immediately
    ; follows a final 'd'. This final 'd' is also changed to 'n'
    (setq y0 (substring y 0 -1))
    (setq ans (declension-join y0 (vconcat [n n] (substring ending 1))))
    (when (equal (elt ans (length y0)) 'N)
     (aset ans (1+ (length y0)) 'N)
    )
   )
   ((and nil (equal efirst 't)
	 (equal ylast 'aa) ; or 'e ai o' changeable to 'aa'
	 (consonant-P (elt y 0))
	 (semivowel-P (elt y 1))
	 (not (member dhaatu '(khyaa dhyai vye hve)))
    )
    ; Kale 689. Roots ending in 'aa' (or 'e ai o' changeable to 'aa')
    ; and beginning with a conjunct consonant containing a semi-vowel
    ; substitute 'na' for 'ta'
    ; Exceptions are 
    ;  'khyaa' (to name) etc,
    ;  'dhyai' (to contemplate)
    ;  'vye' and 'hve'
;    (fol-msg (format "chk...\n"))
    (setq y0 (substring y 0 -1))
    (setq ans (declension-join y0 (vconcat [aa n] (substring ending 1))))
   ) 
   ((and (member efirst '(t th))
         (setq ans (aorist-join1-t-th y ending dhaatu)))
   )
   (t
    (setq ans (declension-join y ending))
   )
  )
;  (setq ans (or (sandhi-single ans) ans))
  (setq ans (solution ans))
  ans
 )
)

(defun construct-ippart1a (dhaatu class pada upasargas)
 (if upasargas
  (construct-ippart1a-ya dhaatu class pada upasargas)
  (construct-ippart1a-tvaa dhaatu class pada upasargas)
 )
)
(defun construct-ippart1a-tvaa (dhaatu class pada upasargas &optional dtype)
 (let (ans ending tok base ansbasic seT-tvaa ntok lc pc fc ans1 bases)
  (setq ending [t v aa])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (if (< 0 ntok) (setq fc (elt tok 0)))
  (setq seT-tvaa (tvaa-seT-code dhaatu class pada upasargas tok))
  (cond
   ((equal class 10)
    (setq bases (class10-base tok)) ; toks
    (setq ans (mapcar
     (lambda (b)
      ; b ends in 'ay' which is kept. 'i' is added
      (sym-without-space (vconcat b [i] ending))
     )
     bases
    ))
   )
   ((equal dtype 'c) ; causal
    (setq bases (causal-conjtab1a-bases dhaatu class pada upasargas 'laT))
    (setq ans (mapcar
     (lambda (b) 
      ; b ends in 'ay' which is kept. 'i' is added
      ; b is a symbol
      (setq tok (car (ITRANS-parse-words-1 (symbol-name b))))
      (sym-without-space (vconcat tok [i] ending))
     )
     bases
    ))
   )
   ((equal seT-tvaa 'seT)
    (cond
     ((equal dhaatu 'ku)
      ; satisfies kale-463-P, but is gunated
      (setq bases (list (gunate-final-vowel tok)))
     )
     ((or (member dhaatu '(mRiD mRid gudh kuSh muSh))
	 (kale-463-P dhaatu)
	 (and (equal dhaatu 'vij) (equal class 7))
      )
      (setq bases (list tok)) ; Kale 746(b)
     )
     ((member dhaatu '(tRiSh mRiSh kRiSh Rit))
      (setq bases (list tok (gunate-final-vowel tok))) ; Kale 746(a)
     )
     (t
      (setq bases (list (gunate-final-vowel tok))) ; Kale 746
      (if (and (consonant-P fc) (member pc '(i u)) (not (member lc '(y v))))
       ; Kale 750
       (setq bases (cons tok bases))
      )
     )
    )
    (setq ending (vconcat [i] ending))
    (setq ans (mapcar
     (lambda (b)
      (sym-without-space (conjugation-join b ending))
     )
     bases
    ))
   )
   ((equal seT-tvaa 'veT)
    (cond
     ((or (member dhaatu '(mRiD mRid gudh kuSh muSh))
	 (kale-463-P dhaatu)
	 (and (equal dhaatu 'vij) (equal class 7))
      )
      (setq bases (list tok)) ; Kale 746(b)
     )
     ((member dhaatu '(tRiSh mRiSh kRiSh Rit))
      (setq bases (list tok (gunate-final-vowel tok))) ; Kale 746(a)
     )
     (t
      (setq bases (list (gunate-final-vowel tok))) ; Kale 746
      (if (and (consonant-P fc) (member pc '(i u)) (not (member lc '(y v))))
       ; Kale 750
       (setq bases (cons tok bases))
      )
     )
    )
    (setq ending (vconcat [i] ending))
    (setq ans (mapcar
     (lambda (b)
      (sym-without-space (conjugation-join b ending))
     )
     bases
    ))
    ; also has a form without 'i'. Use method based on kta
    (let (ans2)
     (setq ans2 (construct-ippart1a-tvaa-basic dhaatu class pada upasargas))
     (if (not (listp ans2)) (setq ans2 (list ans2)))
     (mapcar
      (lambda (thisans)
       (setq ans (append-if-new ans thisans))
      )
      ans2
     )
    )
   )
   (t
    (setq ans
     (construct-ippart1a-tvaa-basic dhaatu class pada upasargas))
   )
  )
  (when nil
   (fol-msg (format "chk: %s %s %s -> %s\n" dhaatu base seT-tvaa ans))
  )
  (when (not dtype)
   ; exceptions
   (setq ans1 
    (construct-ippart1a-tvaa-exception dhaatu class pada upasargas))
   (if ans1 (setq ans ans1))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun construct-ippart1a-tvaa-basic (dhaatu class pada upasargas)
 (let (ans thisans ending tok base ktas kta ntok lc pc)
  (setq ktas (construct-pppart1a dhaatu class pada upasargas))
  (if (not (listp ktas)) (setq ktas (list ktas)))
  (while ktas
   (setq kta (car ktas))
   (setq ktas (cdr ktas))
   (setq ending [t v aa])
   (setq tok (car (ITRANS-parse-words-1 (symbol-name kta))))
   (setq ntok (length tok))
   ; tok ends in [t a] or [n a] or [N a] or [Dh a]
   (setq pc (elt (substring tok -2) 0))
   (cond
    ((member pc '(dh t Dh))
     (setq base (substring tok 0 -1))
     (setq ending (substring ending 1))
     (setq thisans (vconcat base ending))
    )
    ((and (<= 3 ntok)
	  (member (substring tok -3) '([n n a] [N N a])))
     (setq base (substring tok 0 -3))
     (setq thisans (tvaa-join1-t-th base ending dhaatu))
    )
    (t
     (setq base (substring tok 0 -2))
     (setq thisans (tvaa-join1-t-th base ending dhaatu))
    )
   )
   (when thisans (setq thisans (sym-without-space thisans)))
   (setq ans (append-if-new ans thisans))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun tvaa-join1-t-th (y ending  dhaatu)
 (let (ans ny ylast yfirst pc)
;  (fol-msg (format "%s %s %s\n" y ending dhaatu))
  (setq ny (length y))
  (when (< 0 ny)
   (setq ylast (elt (substring y -1) 0)) ;last char
   (setq yfirst (elt y 0))
  )
  (if (<= 2 ny)
   (setq pc (elt y (- ny 2))) ;penultimate char of 'y'
  )
  (cond
   ((equal ylast 'r)
    (setq ans (vconcat y ending))
   )
   (t 
    (setq ans (kta-join1 y 'aniT ending dhaatu))
   )
  )
  ans
 )
)
(defun construct-ippart1a-tvaa-exception (dhaatu class pada upasargas)
 (when nil ; dbg
  (fol-msg (format "tvaa exception: %s %s %s %s\n"
		   dhaatu class pada upasargas))
 )
 (let (ans)
  (cond
   ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hitvaa))
   ((equal dhaatu 'ad) (setq ans 'jagdhvaa))
      ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hitvaa))
   ((equal dhaatu 'ad) (setq ans 'jagdhvaa))
   ((equal dhaatu 'vas)
    (setq ans (cond
     ((equal class 1) 'uShitvaa)
     ((equal class 2) 'vasitvaa)
     ((equal class 4) '(vasitvaa vastvaa))
     ((equal class 10) 'vasayitvaa)
    ))
   )
   ((equal dhaatu 'mRij) (setq ans '(maarjitvaa mRiShTvaa)))
   ((equal dhaatu 'puu) (setq ans '(pavitvaa puutvaa)))
   ((equal dhaatu 'guh) (setq ans '(guhitvaa guuhitvaa guuDhvaa)))
   ((equal dhaatu 'gup)
    (setq ans (cond
     ((equal class 10) '(gopaayitvaa gopitvaa))
     ((equal class 1) '(gupitvaa guptvaa))
    ))
   )
   ((equal dhaatu 'a~nch) (setq ans '(a~nchitvaa  aktvaa)))
   ((equal dhaatu 'div) (setq ans '(devitvaa dyuutvaa)))
   ((equal dhaatu 'kram) (setq ans '(kramitvaa krantvaa kraantvaa)))
   ((equal dhaatu 'sham) (setq ans '(shamitvaa shaantvaa)))
   ((equal dhaatu 'Chid) (setq ans '(Chittvaa))) ; 10-23-04 Whitney
   ((equal dhaatu 'grah) (setq ans 'gRihiitvaa)) ; 03-20-05 Whitney
   (t nil) ; stop here
  )
  ans
 )
)
(defun tvaa-seT-code (dhaatu class pada upasargas &optional tok)
 (let (seT-code ans lc pc seT-kta)
  (if (not tok)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  )
  (setq lc (elt (substring tok -1) 0)) ; last char
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
  (setq seT-kta (kta-seT-code dhaatu class pada upasargas tok))
  (cond
   ((equal dhaatu 'trashch) (setq ans 'seT))
   ((equal dhaatu 'svRi) (setq ans 'aniT))
   ((member dhaatu '(mRiSh kRiSh kuSh))
    (setq ans 'seT) ; exceptions . see #746(a,b)
   )
;   ((equal dhaatu 'puu) (setq ans 'veT))
   ((equal dhaatu 'kShudh) (setq ans 'veT)) ; example p.441
   ((equal dhaatu 'ku) (setq ans 'seT)) ; Kale#746
   ((and (equal dhaatu 'vij) (equal class 7)) (setq ans 'seT)); Kale 746
   ((and (equal dhaatu 'puu) (equal class 7)) (setq ans 'seT)) ; Kale 746
;   ((equal seT-kta 'veT) (setq ans 'veT))
   ((equal seT-code 'veT) (setq ans 'veT))
   ((member dhaatu '(iSh sah lubh riSh ruSh)) (setq ans 'veT))
   ((indicatory-u-P dhaatu class pada) (setq ans 'veT))
   ((member dhaatu '(shvi Dii shii puu jRI)) (setq ans 'seT))
   ((and (equal seT-code 'seT) (consonant-P lc)) (setq ans 'seT))
   ((member class '(10 11))(setq ans 'seT)) ; 11 = causal?
   (t (setq ans seT-kta)
   )
  )
  ans
 )
)
(defun construct-ippart1a-ya (dhaatu class pada upasargas &optional dtype)
 (let (ans ending tok ntok lc pc ans1 x c1 v c2 type fc)
  (setq ending [y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (if (and (samprasaaraNa-P dhaatu class)
	   (not (member dhaatu '(ve jyaa vye)))
      )
   (setq tok (samprasaaraNa tok)) ; Kale 752, 756
  )
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (if (< 0 ntok) (setq fc (elt tok 0)))
  (setq x (dhaatu-parts (vconcat tok [y])))
  (setq c1 (elt x 0)) (setq v (elt x 1)) (setq c2 (elt x 2))
  (setq type (elt x 3))
  (cond
   ((kale-584-P dhaatu) ; drop penultimate nasal
    (setq tok (vconcat (substring tok 0 -2) (substring tok -1)))
   )
  )
  (cond
   ((equal class 10)
    ; Kale 758.
    (let (toks)
     (setq toks (class-a-base dhaatu class pada))
     (mapcar
      (lambda (tok)
       ; tok ends in [a y].
       ; Following code assumes form of 'tok' before [a y] is
       ; [... vowel cons]
       (setq tok (substring tok 0 -2)) ; remove [a y]
       (setq pc (elt (substring tok -2 -1) 0)) ; presumably, a vowel
       (setq ans (append-if-new ans
	(if (shortvowel-P pc)
	 (vconcat tok [a y] ending)
	 (vconcat tok ending)
	)
       ))
      )
      toks
     )
    )
   )
   ((equal dtype 'c) ; causal
    ; Kale 758.
    (let (toks)
     (setq toks (causal-base dhaatu class pada upasargas nil))
     (mapcar
      (lambda (tok)
       ; tok ends in [a y].
       ; Following code assumes form of 'tok' before [a y] is
       ; [... vowel cons]
       (setq tok (substring tok 0 -2)) ; remove [a y]
       (setq pc (elt (substring tok -2 -1) 0)) ; presumably, a vowel
       (setq ans (append-if-new ans
	(if (shortvowel-P pc)
	 (vconcat tok [a y] ending)
	 (vconcat tok ending)
	)
       ))
      )
      toks
     )
    )
   )
   ((or (and (equal class 8)
	     (nasal-P lc)
	     (not (equal dhaatu 'san))
	)
	(member dhaatu '(man van han))
    )
    ; Kale 753. drop final nasal. The ending is then 'a', so by
    ; 751 'tya' is appended
    (setq ans (vconcat (substring tok 0 -1) [t y a]))
    ; 11-24-04 By Whitney, for "man" also manya is acceptable
    (when (equal dhaatu 'man)
     (let (ans1 ans2)
      (setq ans1 ans) ; [m a t y a]
      (setq ans2 (vconcat (substring tok 0 -1) [n y a])) ; [m a n y a]
      (setq ans (list ans1 ans2))
     )
    )
   )
   ((member dhaatu '(gam nam yam ram))
    ; optionally drop final nasal.
    (setq ans (list
     (vconcat tok ending) 
     (vconcat (substring tok 0 -1) [t y a])
    ))
   )
   ((member dhaatu '(khan jan san))
    ; Kale 754
    (setq ans (list
      (vconcat tok ending)
      (vconcat (substring tok 0 -2) [aa] ending)
    ))
   )
   ((equal dhaatu 'kShi)
    ; Kale 755
    (setq ans (vconcat (substring tok 0 -1) [ii] ending))
   )
   ((equal dhaatu 'jaagRi)
    ; Kale 755
    (setq ans (vconcat (gunate-final-vowel tok) ending))
   )
   ((equal v [RI]) 
    (setq v (kale-394 c1 v c2 type))
    (setq ans (vconcat c1 v c2 (substring ending 1)))
   )
   ((or (member lc '(aa e ai o))
	(member dhaatu '(mii mi dii))
    )
    ; Kale 459. Roots ending in 'e', 'ai', and 'o' are treated as
    ; roots ending in 'aa'
    ; Kale 459. The roots 'mi' (5 U 'to throw'),
    ; 'mii' (9 U 'to kill'), and 'dii' (4 A 'to perish')
    ; are treated as roots ending in 'aa' before a termination
    ; causing guna or vrddhi.
    (setq ans (vconcat (substring tok 0 -1) [aa] ending))
   )
   ((shortvowel-P lc) (setq ans (vconcat tok [t y a])))
   ((and (member (elt v 0) '(i u Ri Li)) (< 0 (length c2)))
    (setq v (kale-395 c1 v c2 type))
    (setq ans (vconcat c1 v c2 (substring ending 1)))
   )
   ((equal dhaatu 'lii)
    ; Kale 459. The root 'lii' (9 P, 4 A 'to adhere or cling to') changes
    ; its vowel optionally to 'aa' before a termination causing
    ; guna or vrddhi.
    (setq ans (list (vconcat tok ending)
		    (vconcat (substring tok 0 -1) [aa] ending)))
   )
   (t
    (setq ans (conjugation-join tok ending))
   )
  )
  (setq ans (sym-without-space ans))
  ; exceptions
  (setq ans1 
   (construct-ippart1a-ya-exception dhaatu class pada upasargas))
  (if ans1 (setq ans ans1))
  (setq ans (solution ans))
  ans
 )
)
(defun construct-ippart1a-ya-exception (dhaatu class pada upasargas)
 (let (ans)
  (cond
   ((and (equal dhaatu 'vye)
	 (member upasargas '((pari) (sam)))
    )
    ; Kale 756
    (setq ans '(vyaaya viiya))
   )
   (t nil) ; stop here
   ((equal dhaatu 'shaas) (setq ans 'shiShTa))
   ((equal dhaatu 'muh) (setq ans '(mugdha muuDha))) ; Kale p.423
   ((equal dhaatu 'bhrasj) (setq ans 'bhraShTa))
   ((equal dhaatu 'mRij) (setq ans 'mRiShTa))
   ((equal dhaatu 'sich) (setq ans 'sikta))
   ((equal dhaatu 'vah) (setq ans 'uuDha))
   ((equal dhaatu 'shii) (setq ans 'shayita)) ; Kale 684(a)
   ((equal dhaatu 'jaagRi) (setq ans 'jaagarita))
   ((equal dhaatu 'sthaa) (setq ans 'sthita))
   ((equal dhaatu 'daridraa) (setq ans 'daridrita))
   ((equal dhaatu 'sasj) (setq ans 'sajjita))
   ((and (equal dhaatu 'as) (equal class 2)) (setq ans 'bhuuta))
   ((equal dhaatu 'yaj) (setq ans 'iShTa))
   ((equal dhaatu 'dhaa) (setq ans 'hita))
   ((equal dhaatu 'daa) (setq ans 'datta))
   ((equal dhaatu 'druh) (setq ans '(drugdha druuDha)))
   ((equal dhaatu 'vid)
    (cond
    ; Kale 688(a). 'vid' (6 P A) takes 'ta' in the sense of
    ; 'fit for enjoyment' or 'famous'. It takes 'na' in other cases
     ((equal class 7) (setq ans '(vitta vinna)))
     ((equal class 6) (setq ans '(vidita vitta)))
     ((equal class 4) (setq ans 'vinna)) ; Kale 692
    )
   )
   ((equal dhaatu 'shuSh) (setq ans 'shuShka))
   ((equal dhaatu 'siv) (setq ans 'syuuta))
   ((equal dhaatu 'ruSh) (setq ans 'raShTa))
   ((equal dhaatu 'so) (setq ans 'sita))
   ((equal dhaatu 'snih) (setq ans 'snigdha))
   ((equal dhaatu 'saadh) (setq ans 'saddha))
   ((equal dhaatu 'sRij) (setq ans 'sRiShTa))
   ((equal dhaatu 'Chad) (setq ans '(Chaadita Channa)))
   ((equal dhaatu 'shap) (setq ans '(shapita shapta)))
   ((equal dhaatu 'tRiSh) (setq ans '(tRiShita tRiShTa)))
   ((equal dhaatu 'bha~nj) (setq ans 'bhagna))
   ((equal dhaatu 'mad) (setq ans 'matta))
   ((equal dhaatu 'vij) (setq ans 'vigna))
   ((equal dhaatu 'jyaa) (setq ans 'jiina))
   ((equal dhaatu 'kShi) (setq ans '(kShita kShiiNa)))
   ((equal dhaatu 'grah) (setq ans 'gRihiita))
   ((equal dhaatu 'du) (setq ans '(duta duuna)))
   ((equal dhaatu 'su) (setq ans '(suuna)))
   ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hiina)) ; abandon
   ((and (equal dhaatu 'haa) (equal pada 'A)) (setq ans 'haana)) ; abandon
   ((equal dhaatu 'vrashch) (setq ans 'vRikNa))
   ((equal dhaatu 'sphurj) (setq ans 'sphuurgNa))
   ((equal dhaatu 'sphuurj) (setq ans 'sphuurgNa))
   ((equal dhaatu 'vai) (setq ans 'vaana))
   ((and (equal dhaatu 'bhuj) (equal class 6)) (setq ans 'bhugna))
   ((equal dhaatu 'masj) (setq ans 'magna))
   ((equal dhaatu 'Ri)
    ; Kale 693(a). 'Ri' when it means 'to incur debt' substitutes 'na';
    ; when it means go, 'ta' is used.
    (setq ans '(RiNa Rita)))
   ((equal dhaatu 'div) (setq ans '(dyuuta dyuuna)))
   ((equal dhaatu 'vaa) (setq ans '(vaana vaata)))
   ((equal dhaatu 'vyai) (setq ans '(shyaana shiina shiita)))
   ((equal dhaatu 'nud) (setq ans '(nunna nutta)))
   ((equal dhaatu 'und) (setq ans '(unna utta)))
   ((equal dhaatu 'tryai) (setq ans '(traaNa traata)))
   ((equal dhaatu 'ghraa) (setq ans '(ghraaNa ghraata)))
   ((equal dhaatu 'hrii) (setq ans '(hriiNa hriita)))
   ((equal dhaatu 'pyaay) (setq ans '(piina pyaana)))
   ((equal dhaatu 'haa) (setq ans '(haana hiina)))
   ((equal dhaatu 'duu) (setq ans '(duuta duuna)))
   ((equal dhaatu 'granth) (setq ans '(grathita granthita)))
   ((equal dhaatu 'manth) (setq ans '(mathita manthita)))
   ((equal dhaatu 'gai) (setq ans 'giita)) 
   ((equal dhaatu 'si) (setq ans '(sita sina)))
   ((equal dhaatu 'tvar) (setq ans '(tvarita tuurNa)))
   ((equal dhaatu 'trai) (setq ans '(traata traaNa)))
   
   ((equal dhaatu 'ard) (setq ans '(arNNa arNa)))
   ((equal dhaatu 'uuth) (setq ans 'uta))
   ((equal dhaatu 'kaSh) (setq ans '(kaShTa kaShita)))
   ((equal dhaatu 'kRish) (setq ans 'kRisha))
   ((equal dhaatu 'kShiiv) (setq ans 'kShiiva))
   ((equal dhaatu 'knuuth) (setq ans 'knuuta))
   ((equal dhaatu 'kShmaay) (setq ans '(kShmaayita kShmaata)))
   ((equal dhaatu 'kShai) (setq ans 'kShaama))
   ((equal dhaatu 'Cho) (setq ans '(Chaata Chita)))
   ((equal dhaatu 'jyo) (setq ans 'jiita))
   ((equal dhaatu 'do) (setq ans 'dita))
   ((equal dhaatu 'dRih) (setq ans '(dRiDha dadRimhita)))
   ((equal dhaatu 'dhaa) (setq ans 'hita))
   ((equal dhaatu 'dhaav) (setq ans '(dhauta dhaavita)))
   ((equal dhaatu 'dhe) (setq ans 'dhiita))
   ((equal dhaatu 'pach) (setq ans 'pakva))
   ((and (equal dhaatu 'paa) (equal class 1)) (setq ans 'piita)) ; drink
   ((equal dhaatu 'puuth) (setq ans 'puuta))
   ((equal dhaatu 'phal) (setq ans '(phalita phulla)))
   ((equal dhaatu 'mav) (setq ans '(mavita muuta)))
   ((equal dhaatu 'maa) (setq ans 'mita))
   ((equal dhaatu 'me) (setq ans 'mita))
   ((equal dhaatu 'murchCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'murCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'muurchCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'muurCh) (setq ans '(muurta muurChita)))
   ((equal dhaatu 'laagh) (setq ans 'laagha)) ; with 'ut'
   ((equal dhaatu 'sho) (setq ans '(shaata shita)))
   ((equal dhaatu 'bRih) (setq ans '(bRihita bRiDha)))
   ((equal dhaatu 'bRiMh) (setq ans '(bRiMhita bRiDha)))
   ((equal dhaatu 'vRih) (setq ans '(vRihita vRiDha)))
   ((equal dhaatu 'vRiMh) (setq ans '(vRiMhita vRiDha)))
   ((equal dhaatu 'sriv) (setq ans 'sruta))
   ((equal dhaatu 'hlaad) (setq ans '(hlanna hlaadita)))
   ((equal dhaatu 'shraa) (setq ans '(shRita shraaNa shrapita)))
   ; stambh with 'prati' or 'ni' -> pratistabhda, nistabhda
   ; The 's' does not change to 'Sh' here. (Kale p. 430)
   ((equal dhaatu 'sphaay) (setq ans 'sphiita))
   ; Kale p. 430. 'stiita' and 'stiima' with 'pra' : 'sounded'
   ((equal dhaatu 'styai) (setq ans '(styaana stiita stiima)))
   ; can have other forms ('niShNaata' and 'nadiiShNa') with some meanings
   ((equal dhaatu 'snaa) (setq ans 'snaata))
   ((equal dhaatu 'chyu) (setq ans 'chyuta))
   ((equal dhaatu 'Dii) (setq ans '(Dayita Diina)))
   ((equal dhaatu 'pyai) (setq ans 'piina))
   ((equal dhaatu 'budh) (setq ans '(buddha bodhita)))
   ((equal dhaatu 'bhram) (setq ans '(bhraanta bhramita)))
   ((equal dhaatu 'lasj) (setq ans 'lajjita))
   ((equal dhaatu 'shubh) (setq ans 'shobhita))
   ((equal dhaatu 'shchut) (setq ans '(shchutita shchotita))) ; Antoine2-gloss
   ((equal dhaatu 'shvi) (setq ans 'shuuna)) ; Antoine2-gloss
   ((equal dhaatu 'yat) (setq ans '(yatita yatta)))
   ((equal dhaatu 'bru) (setq ans 'ukta))
   ((equal dhaatu 'ruh) (setq ans 'ruuDha))
   ((equal dhaatu 'ShThiv) (setq ans 'ShThyuuta))
   ((equal dhaatu 'dhu) (setq ans '(dhuuta dhuuna))) ; Antoine2-gloss
   ((equal dhaatu 'svid) (setq ans '(svidita svinna))) ; Apte
   ((equal dhaatu 'puu) (setq ans '(pavita puuta)))
;   ((equal dhaatu ') (setq ans '))
  )
  ans
 )
)
(defun construct-ippart1a-ya-basic (dhaatu class pada upasargas)
 (let (ans thisans ending tok base ktas kta ntok lc pc)
  (setq ktas (construct-pppart1a dhaatu class pada upasargas))
  (if (not (listp ktas)) (setq ktas (list ktas)))
  (while ktas
   (setq kta (car ktas))
   (setq ktas (cdr ktas))
   (setq ending [y a])
   (setq tok (car (ITRANS-parse-words-1 (symbol-name kta))))
   (setq ntok (length tok))
   ; tok ends in [t a] or [n a] or [N a]
   (setq pc (elt (substring tok -2) 0))
   (cond
    ((member pc '(dh t))
     (setq base (substring tok 0 -1))
     (setq ending (substring ending 1))
     (setq thisans (vconcat base ending))
    )
    ((and (<= 3 ntok)
	  (member (substring tok -3) '([n n a] [N N a])))
     (setq base (substring tok 0 -3))
     (setq thisans (tvaa-join1-t-th base ending dhaatu))
    )
    (t
     (setq base (substring tok 0 -2))
     (setq thisans (tvaa-join1-t-th base ending dhaatu))
    )
   )
   (when thisans (setq thisans (sym-without-space thisans)))
   (setq ans (append-if-new ans thisans))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun indicatory-u-P (dhaatu class pada)
 ; Kale p. 442 footnote. 
 ; The following are the more important of the roots
 ; marked with 'u'. 
 (let (data datum ans)
  (setq data '(
   ach ; 1 U
   a~nch ; 1 10 U
   (as 4) ; 4P
   RiN kam ; 5P 1A
   kuj kam ; 1P
   klam ; 4P
   kShaN ; 8 U
   kShiN ; 8 U
   kShiv ; 1 4 P
   kShiiv ; 1 P
   kShed ; 1 P
   khan ; 1 U
   gRidh ; 4 P
   gras ; 1 A
   gruch ; 1 P
   gluch ; 1 P
   glu~nch ; 1 P
   ghRiN ; 8 U
   ghRiSh ; 1 P
   cha~nch ; 1 P
   cham ; 1 5 P
   ChRid ; 7 U
   jabh ; 1 P
   jas ; 4 P 10 U
   ta~nch ; 1 P
   tan ; 8 U 1 U 10 P
   tRiN ; 8 U
   dambh ; 5 P
   dam 4 P
   div ; 4 P 10 A
   dhaav ; 1 U
   dhvaMs  ; 1 A 4 P
   bhraMs ; 1 A
   man ; 8 A
   mRiSh ; 1 P
   mruch ; 1 P
   mru~nch ; 1 P
   mluch ; 1 P
   mlu~nch ; 1 P
   yas ; 4 P
   yup ; 4 P
   rup ; 4 P
   lup ; 4 P
   (va~nch 1); 1 U
   van ; 8 P
   vas ; 4 P
   viSh ; 1 P
   vRit ; 1 4 A 10 U
   vRidh ; 1 A 10 U
   vRiSh ; 1 P
   sham ; 4 U
   shas ; 1 P
   shaMs ; 1 P
   shaas ; 1 P 2 U
   shRidh ; 1 U
   shrambh ; 1 P
   shram ; 4 P
   shriSh ; 1 4 P
   shliSh ; 1 P
   san ; 1 P 8 U
   sdhi ; 1 4 P
   ShThiv ; 1 4 P
   skambh ; 4 9 P
   stambh ; 4 9 P
   svam ; 1 P
   sraMs ; 1 A
   sriv ; 4 P
   hRiSh ; 1 P
  ))
  (while (and data (not ans))
   (setq datum (car data))
   (setq data (cdr data))
   (cond
    ((symbolp datum) (setq ans (equal dhaatu datum)))
    ((listp datum)
     (setq ans (and (equal dhaatu (elt datum 0))
		    (equal class (elt datum 1))
	       )
     )
    )
   )
  )
  ans
 )
)
(defun construct-inf1a (dhaatu class pada upasargas)
 (let (ans ending tok base ansbasic seT ntok lc pc fc ans1 bases)
  (setq ending [t u m])
  (setq ans
   (construct-inf1a-basic dhaatu class pada upasargas))
  ; exceptions
  (setq ans1 
   (construct-inf1a-exception dhaatu class pada upasargas))
  (if ans1 (setq ans ans1))
  (setq ans (solution ans))
  ans
 )
)
(defun construct-inf1a-basic (dhaatu class pada upasargas)
 (let (ans thisans ending tok base luTs luT ntok lc pc  ctab)
;  (setq ctab (conjugation-tab upasargas 'luT  class pada dhaatu))
   (setq ctab (construct-conjtab1a dhaatu class pada upasargas 'luT))
  (setq luTs (elt ctab 0)) ; 3S
  (when (not (listp luTs)) (setq luTs (list luTs)))
  (while luTs
   (setq luT (car luTs))
   (setq luTs (cdr luTs))
   (setq ending [t u m])
   ; luT is expected to end in 'aa'
   (setq tok (car (ITRANS-parse-words-1 (symbol-name luT))))
   (setq ntok (length tok))
   ; tok ends in [t a a]
   (when (< 0 ntok)
    (setq lc (elt (substring tok -1) 0))
    (cond
     ((equal lc 'aa)
      ; replace 'aa with 'um
      (setq thisans (vconcat (substring tok 0 -1) (substring ending 1)))
     )
     (t ; unexpected. write message
      (fol-msg (format "inf1a err: %s %s %s %s: %s\n"
         dhaatu class pada upasargas luT))
      (setq thisans nil)
     )
    )
   )
   (when thisans (setq thisans (sym-without-space thisans)))
   (setq ans (append-if-new ans thisans))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun construct-inf1a-exception (dhaatu class pada upasargas)
 ; currently, no exceptions
; (fol-msg (format "inf1a chk: %s\n" dhaatu))
 (let (ans)
  (cond
   ((equal dhaatu 'spRish) (setq ans 'spraSTum))
   (t nil) ; stop here
;   ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hitvaa))
   (t nil) ; stop here
  )
  ans
 )
)
(defun construct-potpart1a (dhaatu class pada &optional upasargas)
 (let (ans)
  (setq ans (construct-potpart1a-irregular dhaatu class pada))
  (if (not ans)
   (setq ans
    (append
     (construct-potpart1a-tavya dhaatu class pada upasargas)
     (construct-potpart1a-aniiya dhaatu class pada upasargas)
     (construct-potpart1a-yat dhaatu class pada upasargas)
     (construct-potpart1a-kyap dhaatu class pada upasargas)
     (construct-potpart1a-Nyat dhaatu class pada upasargas)
    )
   )
  )
  ans
 )
)
(defun construct-potpart1a-irregular (dhaatu &optional class pada)
 (cond
  ((equal dhaatu 'vadh) '(vadhya))
  ((equal dhaatu 'vash) '(vashya)) ; 02-19-05 (MB 3262001)
  ((equal dhaatu 'sah) '(sahya)) ; 02-28-05 (Kale 716) 
 )
)
(defun construct-potpart1a-tavya (dhaatu class pada upasargas)
 (let (ans ending tok base ntok lc pc seT ending ans1)
  (setq ending [t a v y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
 ; (setq seT (seT dhaatu class pada upasargas))
  (cond
   ((equal class 10)
    ; drop the [a y] of conjugational base
    ; list of symbols
    (setq base (construct-conjbase1a dhaatu class pada upasargas))
    (if (not (listp base)) (setq base (list base)))
    (setq ans (mapcar 
     (lambda (b)
      (vconcat
       (car (ITRANS-parse-words-1 (symbol-name b))) ; leave the [ay]
       [i] ; class 10 is 'seT'
       ending
      )
     )
     base
    ))
   )
   ((member lc '(aa e ai o))
    (setq base (vconcat (substring tok 0 -1) [aa]))
    (setq ans (potpart-join1 base ending dhaatu))
    (setq ans (list ans))
   )
   (t
    (setq base (gunate-final-vowel tok))
    (setq seT (seT dhaatu class pada upasargas))
    (cond
     ((equal seT 'seT)
      (setq ans (potpart-join1 base (vconcat [i] ending) dhaatu))
     )
     ((equal seT 'aniT)
      (setq ans (potpart-join1 base ending dhaatu))
     )
     ((equal seT 'veT)
      (setq ans (list
       (potpart-join1 base (vconcat [i] ending) dhaatu)
       (potpart-join1 base ending dhaatu)
      ))
     )
    )
    (if (not (listp ans)) (setq ans (list ans)))
   )
  )
  (setq ans (mapcar 'sym-without-space ans))
  (setq ans (flatten ans))
  (setq ans1 (construct-potpart1a-tavya-exception
      dhaatu class pada upasargas))
  (when ans1 (setq ans ans1))
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun construct-potpart1a-tavya-exception (dhaatu class pada upasargas)
 (let (ans)
  (cond
   ((equal dhaatu 'budh) '(bodhitavya boddhavya))
   ((equal dhaatu 'mRij) 'maarShTavya)
   ((equal dhaatu 'sRij) 'sraShTavya)
   ((equal dhaatu 'bhrasj) '(bharShTavya bhraShTavya))
   ((equal dhaatu 'bhid) 'bhettavya)
   ((equal dhaatu 'guh) '(guuhitavya goDhavya ))
  )
 )
)
  
(defun construct-potpart1a-aniiya (dhaatu class pada upasargas)
 (let (ans ending tok base ntok lc pc ending ans1)
  (setq ending [a n ii y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (cond
   ((equal class 10)
    ; drop the [a y] of conjugational base
    (setq base (construct-conjbase1a dhaatu class pada upasargas))
    (if (not (listp base)) (setq base (list base)))
    (setq ans (mapcar 
     (lambda (b)
      (potpart-join1
       (substring (car (ITRANS-parse-words-1 (symbol-name b))) 0 -2) ;drop [ay]
       ending
      )
     )
     base
    ))
   )
   ((member lc '(aa e ai o))
    (setq base (vconcat (substring tok 0 -1) [aa]))
    (setq ans (potpart-join1 base ending))
    (setq ans (list ans))
   )
   ((vowel-P lc)
    (setq base (gunate-final-vowel tok))
    (setq ans (potpart-join1 base ending))
    (setq ans (list ans))
   )
   (t
    (setq base (gunate-final-vowel tok))
    (setq ans (potpart-join1 base ending))
   )
  )
  (if (not (listp ans)) (setq ans (list ans)))
  (setq ans (mapcar 'sym-without-space ans))
  (setq ans (flatten ans))
  (setq ans1 (construct-potpart1a-aniiya-exception
      dhaatu class pada upasargas))
  (when ans1 (setq ans ans1))
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun construct-potpart1a-aniiya-exception (dhaatu class pada upasargas)
 (let (ans)
  (cond
   ((equal dhaatu 'mRij) 'maarjaniiya)
   ((equal dhaatu 'bhrasj) '(bharjaniiya bhrajjaniiya))
   ((equal dhaatu 'guh) 'guuhaniiya)
  )
 )
)
(defun potpart-join1 (base ending &optional dhaatu)
 (kta-join1 base 'aniT ending dhaatu)
)

(defun construct-potpart1a-yat (dhaatu class pada upasargas)
 (let (ending tok base ntok lc pc ans ans1)
  (setq ending [y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
 ; (setq seT (seT dhaatu class pada upasargas))
  (cond
   ((member lc '(aa e ai o))
    (setq base (vconcat (substring tok 0 -1) [e]))
    (setq ans (potpart-join1 base ending))
    (setq ans (list ans))
   )
   ((vowel-P lc)
    (setq base (gunate-final-vowel tok))
    (if (equal dhaatu 'bhuu) (setq base tok)) ; Kale 726(a)
    (setq ans (potpart-join1 base ending))
    (setq ans (list ans))
   )
   ((and (equal pc 'a) (labial-P lc))
    (setq base tok)
    (setq ans (potpart-join1 base ending))
;    (fol-msg (format "construct-potpart1a-yat: %s %s %s %s -> %s %s -> %s\n"
;		     dhaatu class pada upasargas base ending ans))
   )
   ((member dhaatu '(tak shas chat yat jan shak sah))
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((and (or (not upasargas)
	     (equal upasargas '(aa))
	 )
	 (member dhaatu '(gad mad char yam))
    )
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'vad)
    ; Kale 719
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'paN)
    ; Kale 719
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((and (equal dhaatu 'vRi) (equal class 9))
    ; Kale 719
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'vah)
    ; Kale 720
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'Ri)
    ; Kale 720
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((and (equal dhaatu 'sRi) (equal upasargas '(upa)))
    ; Kale 720
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'jRI)
    ; Kale 721
    (setq base tok)
    (setq ans (potpart-join1 base ending))
   )
   ((equal dhaatu 'han)
    ; Kale 722
    (setq base [v a gh])
    (setq ans (potpart-join1 base ending))
   )
   (t
    (setq ans nil) ; root does not take 'yat'
   )
  )
   
  (if (not (listp ans)) (setq ans (list ans)))
  (setq ans (mapcar 'sym-without-space ans))
  (setq ans (flatten ans))
  (setq ans1 (construct-potpart1a-yat-exception
      dhaatu class pada upasargas))
  (when ans1 (setq ans ans1))
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun construct-potpart1a-yat-exception (dhaatu class pada upasargas)
 (let (ans)
  (cond
   (t nil)
;   ((equal dhaatu 'mRij) 'maarjaniiya)
  )
 )
)

(defun construct-potpart1a-kyap (dhaatu class pada upasargas)
 (let (ending tok base ntok lc pc ans)
  (setq ending [y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (cond
   ((and (equal pc 'Ri)
	 (not (member dhaatu '(kRip chRit)))
    )
    (setq ans (vconcat tok ending))
   )
   ((member dhaatu '(i stu shaas vRi dRi juSh))
    (if (vowel-P lc)
     (setq ans (vconcat tok [t] ending))
     (setq ans (vconcat tok ending))
    )
    (if (equal dhaatu 'shaas) (setq ans (vconcat [sh i Sh] ending)))
   )
   ((member dhaatu '(shaMs duh guh))
    (if (equal dhaatu 'shaMs)
     (setq ans (vconcat [sh a s] ending))
     (setq ans (vconcat tok ending))
    )
   )
   ((equal dhaatu 'mRij) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'bhuu) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'vad)
    (setq ans (list
     (vconcat tok ending) ; Kale 726(b)
     (vconcat [u d] ending) ; Kale 729
    ))
   )
   ((equal dhaatu 'khan) (setq ans (vconcat [kh e] ending)))
   ((equal dhaatu 'bhRi) (setq ans (vconcat tok [t] ending)))
   ((equal dhaatu 'sRi) (setq ans (vconcat [s uu r] ending)))
   ((and (equal dhaatu 'suu) (equal class 6))
    (setq ans (vconcat [s uu r] ending))
   )
   ((equal dhaatu 'kuSh) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'pach) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'vyath) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'gup) (setq ans (vconcat [k u p] ending)))
   ((member dhaatu '(bhid ujjh)) (setq ans (vconcat tok ending)))
   ((member dhaatu '(puSh sidh)) (setq ans (vconcat tok ending)))
   ((and (member dhaatu '(puu nii ji))
	 (equal upasargas '(vi))
    )
    (if (equal dhaatu 'ji)
     (setq ans (vconcat tok [t] ending))
     (setq ans (vconcat tok ending))
    )
   )
   ((equal dhaatu 'gRih) (setq ans (vconcat tok ending)))
   ((equal dhaatu 'kRi) (setq ans (vconcat tok [t] ending)))
   ((equal dhaatu 'yuj) (setq ans (vconcat [y u g] ending)))
   (t
    (setq ans nil) ; root does not take this form
   )
  )
   
  (if (not (listp ans)) (setq ans (list ans)))
  (setq ans (mapcar 'sym-without-space ans))
  (setq ans (flatten ans))
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun construct-potpart1a-Nyat (dhaatu class pada upasargas)
 (let (ending tok base ntok lc pc ans)
  (setq ending [y a])
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq ntok (length tok))
  (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
  (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  (cond
   ((equal dhaatu 'han) (setq ans (vconcat [gh aa t] ending)))
   ((equal dhaatu 'vad)
    (setq ans (list
     (vconcat [v aa d] ending)
     (vconcat [u d] ending)
    ))
   )
   ((equal dhaatu 'vas)
    (setq ans (list
     (vconcat [v aa s] ending)
     (vconcat [v a s] ending)
    ))
   )
   ((equal dhaatu 'yuj)
    (setq ans (list
     (vconcat [y o j] ending)
     (vconcat [y o g] ending)
    ))
   )
   ((equal dhaatu 'pach)
    (setq ans (list
     (vconcat [p aa k] ending)
     (vconcat [p aa ch] ending)
    ))
   )
   ((equal dhaatu 'vach)
    (setq ans (list
     (vconcat [v aa k] ending)
     (vconcat [v aa ch] ending)
    ))
   )
   ((equal dhaatu 'va~nch)
    (setq ans (list
      [v a ~N k y a]
     (vconcat tok ending)
    ))
   )
   ((equal dhaatu 'bhuj)
    (setq ans (list
     (vconcat [bh o g] ending)
     (vconcat [bh o j] ending)
    ))
   )
   ((equal dhaatu 'nii)
    (setq ans (list
     (vconcat [n e] ending)
     (vconcat [n aa y] ending)
    ))
   )
   
   ((equal lc 'Ri)
    (setq base (gunate-final-vowel tok t)) ; vrddhi
    (setq ans (vconcat base ending))
   )
   ((member lc '(u uu))
;    (setq base (gunate-final-vowel tok t))
    (setq base (vconcat (substring tok 0 -1) [aa v]))
    (setq ans (conjugation-join base ending))
   )
   ((equal dhaatu 'ram)) ; Nyat->raamya, not in Whitney
   ((consonant-P lc)
    (cond
     ((and (equal dhaatu 'vach) (equal upasargas '(pra))))
     ((member dhaatu '(yaj yaach ruch tyaj Rich)))
     ((equal lc 'ch) (setq tok (vconcat (substring tok 0 -1) [k])))
     ((equal lc 'j) (setq tok (vconcat (substring tok 0 -1) [g])))
    )
    (if (or (equal pc 'a)
	    (equal dhaatu 'mRij)
	)
     (setq base (gunate-final-vowel tok t)) ; vrddhi
     (setq base (gunate-final-vowel tok)) ; guna
    )
    (setq ans (vconcat base ending))
   )
   (t
    (setq ans nil) ; root does not take this form
   )
  )
  (if (not (listp ans)) (setq ans (list ans)))
  (setq ans (mapcar 'sym-without-space ans))
  (setq ans (flatten ans))
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun construct-potpart1a-exception (dhaatu class pada upasargas)
 ; currently, no exceptions
 (let (ans)
  (cond
   (t nil) ; stop here
;   ((and (equal dhaatu 'haa) (equal pada 'P)) (setq ans 'hitvaa))
   
   (t nil) ; stop here
  )
  ans
 )
)
