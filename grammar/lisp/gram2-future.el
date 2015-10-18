; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; gram2-future.el  
; begun 08-08-03
; Based upon Antoine, volume 2 and Kale
; code for the following:
;  1. periphrastic future = First future = luT
;  2. simple future = second future = lRiT
;  3. conditional = lRi~N
;  4. benedictive = aashiirli~N
; Based upon Antoine, volume 2 and Kale
(defun future-doc ()
 "Antoine2#132. There are two forms of the future in Sanskrit:
   1. The periphrastic future (luT) which expresses remote future events.
   2. The simple future (lRiT) which expresses any future event, whether
      immediate or remote.
 "
)
(defun luT-doc-Antoine ()
  "Antoine2#135. The periphrastic future (luT).
   1. A verbal noun formed with the suffix 'tRich' in the nom. masc. sing.
      is prefixed to the the present tense of the auxiliary 'as' in the
      1st and 2nd persons.  In the 3rd person, the verbal noun is not 
      followed by the auxiliary and takes the forms of the nom. sing., dual.
      and plur. masc: -taa -taarau -taaraH
   2. The final vowel and the short medial vowel of the verbal roots
     take guna.  The verbs of the 10th conjugation keep the base of the
     present tense. The result may be called the verbal base.
   3. To the verbal base is added 'i', necessarily, optionally, or not at
     all, depending on whether the root is 'seT', 'veT', or 'aniT'.
     To this is added the ending as describe in 1.
 "
)
(defun luT-doc-Kale ()
 "Kale #468.
  The First Future (luT) is also called the periphrastic future.
  Its Parasmaipada terminations are:
    taa taarau taaraH
    taasi taasyaH taasya
    taasmi taasvaH tassmaH
  Its Atmanepada terminations are:
    taa taarau taaraH 
    taase taasathe taadhve 
    taahe taasvahe taasmahe
 Kale #469.
  To these terminations the augment 'i' 
    is prefixed in the case of 'seT' roots,
    is optionally prefixed in the case of 'veT' roots
    is not prefixed at all in the case of 'aniT' roots.
 Kale #470.
  All these terminations are strong.
  The final vowel and the penultimate short vowel of a root 
  therefore take their 'guNa' substitute before these.
 Kale #471.
  'aniT' roots with a penultimate 'Ri' change it to 'r' optionally
  before a strong termination beginning with any consonant except
  a nasal or a semi-vowel (in particular, before any of the terminations
  of the periphrastic future):
   sRip : sarptaasmi  sraptaasmi

  IRREGULAR BASES
 Kale #472.
  The following roots are 'veT' (admit 'i' optionally) in the First Future:
   iSh
   saH (1 A)
   lubh
   riSh
   ruSh
 Kale #473.
  The root 'kLip' is optionally parasmaipada in the First future, 
  and when so it is 'aniT' (rejects 'i').
 Kale #474.
  The augment 'i' as added to the root 'grah' is long (i.e., 'ii' is 
  added) in all non-conjugational tenses, except in the Perfect.
 Kale #475.
  The intermediate 'i' is optionally lengthened (i.e., 'ii' is added)
  in the case of 'vRi' and roots ending in 'Ri', except
  in the Perfect, the Benedictive atmanepada, and the Aorist parasmaipada.
 Kale #476.
  The root 'masj': This is an 'aniT' root. 
    'n' is inserted before the ending consonant and
    the  's' is dropped when they are followed by any consonant
    except a nasal or a semi-vowel. In particular this is applicable
    to all the periphrastic future forms:  ma~Nktaa.
    When the 'n' is not dropped, the 's' is changed to 'j': mamajja
  The root 'nash': This is a 'veT' root.
    'n' is inserted after the vowel when it is followed by an consonant
    except a nasal or a semi-vowel. In particular this is applicable
    to all the periphrastic future forms: naMShTaa or nashitaa
 Kale #477.
  The root 'aj'.
  The root 'vii' is substituted for the root 'aj' (1 P 'to go') 
    a. necessarily before any non-conjugational termination, and
    b. optionally before any such as begin with any consonant except 'y':
      First Future:  vetaa or ajitaa
      (another example)   veShyati ajiShyati
"
)

(defun lRiT-doc-Antoine ()
)
(defun lRiT-lRi~N-doc-Kale ()
 "Kale 478. 
   NOTE: The endings for the Second Future (lRiT) are in file 'endings.el'.
  Kale 479.
   NOTE: The endings for the Conditional (lRi~N) are in file 'endings.el'.
  Kale 480.
   't' is substituted for the ending 's' of a root when followed by
   any non-conjugational termination beginning with 's'.
   NOTE: All terminations of lRiT and lRi~N begin with 's'
  Kale 481.
   'i' is to be prefixed, or not, or optionally in the terminations,
   according as the root is 'seT aniT or weT'.
   Before the terminations, the final vowel and the penultimate
   short vowel take their guna substitute.
  Kale 482.
   In the Conditional, the augment 'a' is prefixed to the root as
   in the imperfect.
 "
)
(defun future-passive-doc-Antoine ()
 "Antoine2#138. 
 The future passive is identical with the future atmanepada. Thus,
  'jyeShyati' = he will conquer
  'jyeShyate' = he will be conquered
 Roots ending with a vowel and the roots 'han', 'grah', and 'dRish'
 have an optional form in the future passive.
 1. The final vowel takes vrddhi
 2. a final 'aa' is changed to 'aay'
 3. 'han' changes to 'ghan', 
    'grah' changes to 'graah'
    'dRish' changes to 'darsh'
 4. 'i' is inserted, and the atmanepada terminations are added.
 "
)
(defun future-passive-doc-Kale()
 "Kale 596(a)
   The forms of the two Futures, Conditional and the Benedictive
   of the Passive are made up in the same way as those of the Active,
   every root being supposed Atmanepadi.

  Kale 596(b)
   Roots ending in a vowel and the roots 'han grah dRish' optionally
   form the two Futures, the Conditional, and the Benedictive of the
   Passive by changing their vowel to 'vrddhi' and appending the 
   'A' terminations of those tenses with 'i' prefixed to them invariably.
   In the case of roots ending in 'aa' (and in 'e ai o' changeable to 'aa'),
   'y' is inserted between the root and this 'i'.
   Note:
    'nuu' and 'dhuu' take vrddhi in this case though it is usually
    forbidden with them.  See #463. 'dRish' takes 'guna' only.
 "
)
(defun aashiirli~N-doc-Antoine ()
 "Antoine2#142, p. 94-6
 The Benedictive expresses a wish of the speaker or a blessing conferred
 by him on some other person.
 (1) Before parasmaipada terminations
  1. The verbal root is treated as in the passive voice
     (refer Antoine#1 68 and 71(3))
     with the 'y' of the passive being replaced by the terminations.
  2. Roots endinging in 'aa', 'e', 'ai', and 'o' change their final
     vowel to 
     a. 'e' when the preceding consonant is simple
     b. to 'aa' or 'e' when the preceding consonant is conjunct
 (2) Before atmanepada terminations
  1. 'i' is inserted in the case of 'seT' roots
     'i' is inserted optionally in the case of 'veT' roots
  2. The following roots are considered 'veT':
     a. Roots ending in 'Ri' and preceded by a conjunct consonant
     b. 'vRi'
     c. Roots ending in 'RI'
  3. When 'i' is inserted,
     the final vowel and the short medial vowel take guna.
  4. When 'i' is not inserted,
     (a) a short medial vowel remains unchanged
     (b) a final 'Ri' remains unchanged
     (c) a final 'RI' is changed to 'iir' after a non-labial
     (d) a final 'RI' is changed to 'uur' after a labial
  5. The inserted 'i' is lengthened for 'grah'.
 "
)

(defun Kale-459-doc ()
 "Roots ending in 'e', 'ai', and 'o' are to be treated as
  roots ending in 'aa'. 
  Also the roots 'mi' (5 U to throw), 'mii' (9 U to kill), and
  'vii' (4 A to perish) is treated as a root ending in 'aa'
  before a termination causing guna or vrddhi.
 The root 'lii (9 U 4 A to adhere) changes its vowel to 'aa'
  before a termination causing guna or vrddhi.
 "
)
(defun Kale-502-doc ()
 "Kale 502, p. 308
  The change of a semivowel to its corresponding vowel is called
  'samprasaaraNa'.  SamprasaaraNa generally takes place before 
  weak terminations in the case of the following roots:
  vach yaj vap vah
  vas (to dwell)
  ve vye hve shvi
  vad svap jyaa vash vyach prachCh vrashch bhrasj grah and vyadh.
  In the case of the Perfect, the roots 'prachCh vrashCh and bhrasj'
  form an exception.
 "
)
(defun aashiirli~N-doc-Kale ()
 "Parasmaipada Benedictive
  580. The P terminations are weak and therefore occasion no
    guna or vrddhi change in the root.
    All roots reject the augment 'i'
  581. The following changes apply to roots both before
   the P terminations of the Benedictive, and before the 'y'
   of the (ordinary) passive.
   1. The final 'i' or 'u' is lengthened
   2. final 'Ri' is changed to 'ri'
   3. final 'RI' preceded by a non-labial is changed to 'iir'
   4. final 'RI' preceded by a labial or 'v' is changed to 'uur'
  582. 
   1. Final 'Ri' preceded by a conjunct consonant is changed
      to guna: 'smRi' -> smaryaat
   2. the  'Ri' of the root 'Ri' is changed to Guna: 'aryaat'
  583. 
   1. Roots capable of taking samprasaaraNa take it (see #502)
   2. 'shaas' substitutes 'i' for its vowel : 'shiShyaat'
  584. A penultimate nasal is generally dropped. Some of the roots
   dropping their nasal thus are:
   a~nch a~nj bha~nj ra~nj sa~nj sva~nj
   granth manth
   und skand syand
   indh bandh 
   dambh stambh
   daMsh bhraMsh sraMs tRiMh
  585. 
   1. Some roots change their final vowel to 'e':
     'daa', 'dhaa' and roots assuming these forms (???)
      'maa' , 'sthaa', 'gai',
      'paa' (to drink: 1P),
      'haa' (to abandon : 3)
   2. Final 'aa' (original or substituted - See #459), if it be
      preceded by a conjunct consonant, is changed to 'e' optionally:
      e.g. : glaa -> gleyaat or glaayaat
   3. paa (to protect: 2) does not change to 'e': paayaat
    
  Atmanepada Benedictive
  586.
   a. 'i' is prefixed to the terminations after 'seT' roots, and
      optionally after 'veT' roots
   b. Roots ending in 'Ri' preceded by a conjunct consonant,
      the root 'vRi', and roots ending in 'RI' admit 'i' optionally.
  587.
      The atmanepada terminations are strong. Before these,
      the radical vowel takes its guNa substitute, but when the
      intermediate 'i' is not prefixed to the terminations, the
      final 'Ri' remains unchanged, and 'RI' is changed to 'uur' if
      a labial or 'v' precedes, and to 'iir' otherwise. For instance:
       chi -> cheShiiShTa
       dhu -> dhoShiiShTa
       luu -> laviShiiShTa
       stRI -> stariShiiShTa or stiirShiiShTa
       pRI -> pariShiiShTa or puurShiiShTa
      NOTE: Kale actually says 'ur' and 'ir', but his examples, as
      well as the explanation in Antoine, make me think this is an
      error, and that actually 'uur' and 'iir' are meant.
 "
)
(defun future-part-doc ()
 "Kale 709, p. 431
  The participle of the simple future, active as well as passive, is
  formed from the third person singular form of the Second Future of
  a root.  
  The Parasm. Ptc. is formed simply by dropping the final 'i'.
  The Atm. Ptc. is formed by substituting 'maana' for the final 'te'.
  The Passive Ptc. is formed by substituting 'maana' for the final 'te'.
 Kale 710.
  These participles are declined like words ending in 't' and 'a'.
  (The Par. Ptc. is declined like participles ending in 'at')
 "
)
(defun participles-at-doc ()
 "Kale 116(a). Participial bases ending in 'at'.
  The declension of these does not differ from that of nouns in 'mat'
  except in the Nom. sing. mas. in which the 'a' of 'at' is not lengthened.
  In the case of the dual of the Nom. Voc. and Acc. neuter, 'n' is
  inserted before the final 't':
  - necessarily in the case of
    -- the present participles of the roots of classes 1,4 and 10
    -- the present participles of causal, desiderative, and nominal verbs
  - optionally in the case of
    -- the present participles of the roots of class 6
    -- the present participles of roots of class 2 ending in 'aa'
    -- the future participles (in 'syat' or 'Shyat')
  - Not at all in the case of the remaining participles of the present tense
  The Feminine of these ends in 'ii', being the same as the Neuter dual of
  the Nom., and has no declensional peculiarities.
 Kale 116(b)
  'n' is NOT inserted at all in the case of the participles of 
  the present of 
   - roots of the 3rd class
   - the Parasm. frequentative
   - the participles
     -- shaasat 
     -- jakShat
     -- chakaasat
     -- daridrat
     -- jaagrat
     -- diivyat
     -- devyat.
   In the case of the Nom. Voc. and Acc. plu. neu, however, it is
    optionally inserted.
  NOTE (09-27-03): I have not implemented this last exception, e.g.,
    see (sanget2 'x '(dhaatu PRESPART)) for x = daa-3P, jaagRi-2P, etc.
 "
)
(defun future-part-doc-Antoine ()
 "Antoine2#139, p. 92-3
 The future participles ('syatRi syamaana')
  The parasm. and atm. participles of the simple future are formed
  in the same way as the present participles of the 6th conjugation.
  The termination 'syanti' of the 3rd pers. plur. parasm. is replaced
  by 'syat'.
  The termination 'syante' of the 3rd pers. plur. atm. is replaced
  by 'syamaana'
 The declension of the future participle parasm is the same as that
  of verbs of the 6th conjugation, i.e., they optionally keep the
  'n' in their strong forms.
   - Masc: daasyan daasyat, daasyantau daasyatau, daasyantaH daasyataH
   - Neut: daasyat , daasyantii or daasyatii, daasyanti
   - Fem: either daasyantii or daasyatii.
 The future participle is often used in the sense of 'about to' or
  'going to': e.g.,
   - 'gamiShyan' about to go
   - 'ghaaniShyamaaNaa kanyaa' the girl about to be killed
 NOTE: This account differs from that of Kale in the Masculine for
  Parasm. future participle. Namely, Kale does not have the optional
  strong forms with an inserted 'n', e.g.,   'daasyat, daasyatau, daasyataH'.
  I think Antoine just made a mistake here, because his description of
  the present active participles of verbs of class 6 also does not have
  these alternate 'n' forms in the masculine (see Antoine1#83, p. 84).
 "
)
(defvar ForC-sym nil) 
(defun conjugation-tab-lRiT (upa-syms class pada dhaatu &optional voice)
 (setq ForC-sym 'lRiT)
 (conjugation-tab-ForC upa-syms class pada dhaatu voice)
)
(defun conjugation-tab-lRi~N (upa-syms class pada dhaatu &optional voice)
 (setq ForC-sym 'lRi~N)
 (conjugation-tab-ForC upa-syms class pada dhaatu voice)
)
(defun conjugation-tab-luT (upa-syms class pada dhaatu &optional voice)
 (setq ForC-sym 'luT)
 (conjugation-tab-ForC upa-syms class pada dhaatu voice)
)
(defun conjugation-tab-aashiirli~N (upa-syms class pada dhaatu
    &optional voice)
 (setq ForC-sym 'aashiirli~N)
 (conjugation-tab-ForC upa-syms class pada dhaatu voice)
)
(defun conjugation-tab-ForC (upa-syms class pada dhaatu &optional voice)
 ; first future, second future, and conditional
 ; Some  irregular forms are  included here
 (let (ans  tok ylast seT-code types pc parts)
  (let ( wparts)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   (setq ylast (elt (substring tok -1) 0))
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
   (if (equal types "CVC") (setq pc (elt (elt parts 1) 0)))
  )
 
  (cond
   ((equal pada 'PASSIVE)
    (setq seT-code (construct-seT-code1a dhaatu class 'A upa-syms))
   )
   (t
    (setq seT-code (construct-seT-code1a dhaatu class pada upa-syms))
   )
  )
  (setq seT-code (solution seT-code))
  (cond
   ((or (equal pada 'PASSIVE) (equal voice 'PASSIVE))
    (let (ans1 ans2 tok2 dhaatu2 i)
     (setq ans1 (conjugation-tab-ForC upa-syms class 'A dhaatu ))
     (cond
      ((equal ylast 'aa)
       (setq tok2 (vconcat tok [y]))
       (setq dhaatu2 (sym-without-space tok2))
      )       
      ((vowel-P ylast)
       (setq tok2 (vconcat
		   (substring tok 0 -1)
		   (if (equal ylast 'e) [ai] (vrddhi ylast))
		  )
       )
;       (setq tok2 (gunate-final-vowel tok 't)) ;vrddhi
       (setq dhaatu2 (sym-without-space tok2))
      )
      ((equal dhaatu 'han) (setq dhaatu2 'ghaan))
      ((equal dhaatu 'grah) (setq dhaatu2 'graah) )
      ((equal dhaatu 'dRish) (setq dhaatu2 'darsh))
      ((member class '(10))
       ; Kale 598. The 'ay' is optionally dropped in the general tenses,
       ; except the Perfect.
       ; In the benedictive, the 'ay' has already been dropped;
       ; so to get the alternate form, it must be added
       (let (cb)
	(setq cb (causal-base dhaatu class pada upa-syms nil))
	(setq cb (solution cb))
	(cond
	 ((not (arrayp cb))) ; can't handle multiple values for cb now
	 ((equal ForC-sym 'aashiirli~N)
	  (setq tok2 cb)
	  (setq dhaatu2 (sym-without-space tok2))
	 )
	 (t ; drop ay
	  (setq tok2 (substring cb 0 -2)) ; drop 'ay'
	  (setq dhaatu2 (sym-without-space tok2))
	 )
	)
       )
      )
      ((equal class 11)
       (if (equal ForC-sym 'aashiirli~N)
	(let ()
	)
        (setq dhaatu2 dhaatu)
       )
      )
     )
     (when dhaatu2
      (setq ans2
	(conjugation-tab-ForC-main upa-syms class 'PASSIVE dhaatu2 '(seT)))
     )
     (if ans2
      (setq ans (join-arrays ans1 ans2))
      (setq ans ans1)
     )
    )
   )
   ((and (equal dhaatu 'han)
	 (equal pada 'A)
	 (equal ForC-sym 'aashiirli~N)
    )
    ; 'han' is 2P in dhaatukosha.
    ; however, its passive uses the 'A' logic.
    ; In Kale examples on p. 364, the normal passive benedictive is
    ; given as 'vaghiShiiya'.  So this
    ; (a) uses the base 'vagh'
    ; (b) inserts 'i'.
    ; Since 'han' is an 'aniT', this also must be adjusted
    ; By Kale 483, 'han' admits 'i' in lRiT, lRi~N (handled elsewhere)
    (setq ans
	(conjugation-tab-ForC-main upa-syms class 'A 'vagh '(seT)))
   )
   
   ((and (equal dhaatu 'aj) (equal pada 'P))
    ; Kale #477 p. 300
    ; 'vii' (2 P) is substituted for 'aj' (1 P 'to go') necessarily before any
    ; non-conjugational termination, and optionally before such as
    ; begin with any consonant except 'y':
    ;   'vetaa  ajitaa'
    ;   'veShyati ajiShyati'
    (let (ans1 ans2)
     (setq ans1
	 (conjugation-tab-ForC-main upa-syms 2 pada 'vii ))
     ;all forms of 'aj' have optional answers with 'vii'
     (setq ans2
         (conjugation-tab-ForC-main upa-syms class pada dhaatu ))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ; some other irregularites
   ((and (equal dhaatu 'dRish)
	 (equal pada 'A)
	 (equal ForC-sym 'aashiirli~N))
    ; Note2: for the 1 P root dRish, when it appears as 'A', in
    ; formation of passive, the change of Kale 465 (below)
    ; does not occur (Kale p. 364, example)
    (setq ans
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu))
   )
   ((member dhaatu '(sRij dRish))
    ; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
    ; is changed to 'ra' before
    ; a consonantal strong termination in the general tenses.
    ; Note: both are 'aniT'
    (let ( dhaatu2 c1 v c2)
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu2 seT-code))
    )
   )
   ((equal dhaatu 'tRip)
    ; Kale 471. In the example of 'tRip' on p. 303, there are three
    ; forms : an 'i' form with 'Ri' (gunated), and two forms without
    ; 'i' (one with 'Ri' gunated, and one with 'ra' instead of 'Ri').
    ; This logic (along with that in '..-main') achieve these three
    ; forms.
    ; Note: 'tRip' has seT-code 'veT'
    (let (ans1 ans2 dhaatu2 c1 v c2)
     (setq ans1
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu2 '(aniT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal seT-code 'aniT)
	 (equal pc 'Ri)
	 (equal types "CVC")
	 (equal ForC-sym 'luT)
    )
    ; Kale 471.
    ; 'aniT' roots with a penultimate 'Ri' change it to
    ; 'ra' optionally before a strong termination beginning
    ; with any consonant except a nasal or a semi-vowel.
    (let (ans1 ans2 dhaatu2 c1 v c2 ans3)
     (setq ans1
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu seT-code))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu2 seT-code))
     (setq ans (join-arrays ans1 ans2))
     ans
    )
   )
   ((and (equal seT-code 'aniT)
	   (member dhaatu '(kRiSh spRish tRip))
	   (member ForC-sym '(lRiT lRi~N))
    )
    ; Antoine2#137, p.89. gives the optional 'ra' forms for kRiSh, spRish
    ; Kale p. 303 gives the optional 'ra' form for 'tRip'
    ; in simple future
    ; For these roots
    ; 'ra' optionally before a strong termination beginning
    ; with any consonant except a nasal or a semi-vowel.
    (let (ans1 ans2 dhaatu2 c1 v c2)
     (setq ans1
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-ForC-main upa-syms class pada dhaatu2 seT-code))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal ForC-sym 'luT)
	 (or (member dhaatu '(iSh sah lubh riSh ruSh))
	     (and (equal dhaatu 'sah) (equal class 1) (equal pada 'A))
	 )
    )
    ;Kale 472. These roots admit 'i' optionally in the First Future
    (setq ans
	(conjugation-tab-ForC-main upa-syms class pada dhaatu  '(veT)))
   )
   ((and (equal dhaatu 'kLip) (equal pada 'P))
    ; Kale 473. 'kLip' is optionally 'P' in the futures and conditional,
    ; and when so it rejects 'i'
    (setq ans
      (conjugation-tab-ForC-main upa-syms class pada dhaatu  '(aniT)))
   )
   ((equal dhaatu 'grah)
    ; Kale 474. The augment 'i' as added to 'grah' is long
    ; in all non-conjugational tenses, except in the Perfect
    (setq ans
      (conjugation-tab-ForC-main upa-syms class pada dhaatu nil [ii]))
   )
   ((and (or (equal dhaatu 'vRi)
	     (equal ylast 'RI)
	     (and (equal ylast 'Ri) (< 1 (length (elt parts 0))))
	 )
	 (and (equal ForC-sym 'aashiirli~N) (equal pada 'A))
    )
    ; Kale 586 . These roots admit 'i' optionally in benedictive A
    ; For all changes to work properly, it is needed to
    ; call '..-main' with aniT and with seT
    (let (ans1 ans2) 
     (setq ans1
      (conjugation-tab-ForC-main upa-syms class pada dhaatu '(aniT)))
     (setq ans2
      (conjugation-tab-ForC-main upa-syms class pada dhaatu '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal ForC-sym 'aashiirli~N) (equal pada 'A) (equal seT-code 'veT))
    ; For logic to work properly, 'veT' must be separated here
    (let (ans1 ans2) 
     (setq ans1
      (conjugation-tab-ForC-main upa-syms class pada dhaatu '(aniT)))
     (setq ans2
      (conjugation-tab-ForC-main upa-syms class pada dhaatu '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (or (equal dhaatu 'vRi)
	     (equal ylast 'RI)
	 )
	(not (and (equal ForC-sym 'aashiirli~N) (equal pada 'A)))
    )
    ; Kale 475. The intermediate 'i' is optionally lengthened in the
    ; case of 'vRi' and roots ending in 'RI', except in the
    ; Perfect, the Benedictive atmanepada, and the Aorist parasmaipada
    (let (ans1 ans2)
     (setq ans1
       (conjugation-tab-ForC-main upa-syms class pada dhaatu nil [i]))
     (setq ans2
       (conjugation-tab-ForC-main upa-syms class pada dhaatu nil [ii]))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and
     (or (and (equal dhaatu 'gam) (equal pada 'P))
	(equal dhaatu 'han)
	(equal ylast 'Ri)
     )
     (member ForC-sym '(lRiT lRi~N))
    )
    ; Kale 483 p. 301
    ; 'gam P', 'han', and 'aniT' roots ending in 'Ri'
    ; admit 'i' in the Second Future and the conditional.
    ; 'gam P' - also that substituted for 'i' (to go) and
    ;  with 'adhi' (to remember) - also admits it in the Desiderative
    ; svRi is 'seT' in the 2nd Fut and Conditional (Kale p.305)
    ; NOTE: 'han' also admits 'i' in passive (atmanepada)
    ;  of 'aashiirli~N' (p .364 example)
    ;       This is handled elsewhere
    ; however, normal benedictive of 'han' does not admit 'i' (dhaatukosha)
    (when (and (equal ylast 'Ri)
	       (not (equal seT-code 'aniT))
	       ; next are already checked to be ok
	       (not (member dhaatu '(svRi)))
	  )
      (fol-msg (format "ForC-warning: %s %s %s %s\n"
		       dhaatu class pada seT-code))
    )
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu '(seT)))
   )
   ((and (member dhaatu '(kLip vRit vRidh shRidh syand))
	 (equal pada 'A)
	 (member ForC-sym '(lRiT lRi~N))
    )
    ; Kale 484 p. 301
    ; The roots (above) optionally take parasmaipada terminations in
    ; the Second Future, Conditional, and Desiderative.
    ; They reject the augment 'i' when parasmaipada terminations are taken.
    ; Note: the present logic (implicitly) assumes this applies
    ; in the passive voice (whose form is 'A'), as well as active voice
    (let (ans1 ans2)
     (setq ans1 (conjugation-tab-ForC-main upa-syms class pada dhaatu))
     (setq ans2
	   (conjugation-tab-ForC-main upa-syms class 'P dhaatu '(aniT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (member dhaatu '(kRit chRit ChRid tRid nRit))
	 (not (equal ForC-sym 'luT))
    )
    ; Kale 485 p.302
    ; The roots above take 'i' optionally when followed by
    ; an 'aarchadhaatuka' (non-conjugational) termination beginning
    ; with an 's' except in the Aorist
    ; By the examples on p. 305 and on p. 306, I inferred that
    ; this rule does not apply for the 'luT'.
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu '(veT)))
   )
   ((and (equal dhaatu 'nRit) (equal ForC-sym 'luT))
    ; 'nRit' is classed as 'veT'
    ; however, from Kale dhaatukosha, its 'luT' (periphrastic future)
    ; is 'seT' (inserts 'i'). Also, on p. 306, Kale says 'nRit P' is
    ; to be conjugated like 'Chrid'
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu '(seT)))
   )
   ((and (equal dhaatu 'i)
	 (equal upa-syms '(adhi))
	 (equal pada 'A)
	 (equal ForC-sym 'lRi~N)
    )
    ; Kale 486. p. 302
    ; In the case of 'i' with 'adhi', the root 'gaa' is
    ; optionally substituted for 'i' in the conditional and the aorist.
    ; In this case, 'i' is substituted for the final vowel of 'gaa'
    ; before a consonantal weak termination; all terminations added
    ; to 'gaa' for 'i' are weak. This can be due to the establishment
    ; of 'gaa' as a 'seT' verb
    ; NOTE: By the example, actually 'ii' is substituted for 'aa'
    (let (ans1 ans2)
     (setq ans1 (conjugation-tab-ForC-main upa-syms class pada dhaatu))
     (setq ans2 (conjugation-tab-ForC-main upa-syms class pada 'gaa '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((or
     (and (equal dhaatu 'daa) (member class '(3 1)))
     (member dhaatu '(dhaa do de de maa sthaa paa haa so))
    )
    ; Kale 486. 'i' is substituted for the final vowel of these verbs
    ; before a consonantal weak termination.
    ; Note: for lRiT and lRi~N, all terminations are consonantal but
    ;  strong, thus these changes do not apply
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu))
   )
   ((member dhaatu '(a~nj ash))
    ; Kale p. 304. Roots which are 'veT' in luT, lRit, lRi~N
    (setq ans
       (conjugation-tab-ForC-main upa-syms class pada dhaatu '(veT)))
   )
   ((member dhaatu '(gup dhuup vichCh paN pan kam Rit))
    ; Kale 461.
    ; These roots preserved their conjugational bases optionally.
    ; These have present-system conjugational forms that look
    ; like conjugation 10 forms.
    ; When the conjugation-10 form is used, the roots are 'seT',
    ;  which is the conjugation-10 standard.
    ; When the non-conjugation-10-form is used, the roots take
    ; the general seT-code associated with the root.
    (let (ans1 ans2 dhaatu2)
     (setq dhaatu2
      (cond
       ((equal dhaatu 'gup) 'gopaay)
       ((equal dhaatu 'dhuup) 'dhuupaay)
       ((equal dhaatu 'vichCh) 'vichChaay)
       ((equal dhaatu 'paN) 'paNaay)
       ((equal dhaatu 'pan) 'panaay)
       ((equal dhaatu 'kam) 'kaamay)
       ((equal dhaatu 'Rit) 'Ritiiy)
      )
     )
     (setq ans1
       (conjugation-tab-ForC-main upa-syms class pada dhaatu))
     (setq ans2
       (conjugation-tab-ForC-main upa-syms class pada dhaatu2 '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal dhaatu 'dhuu) (equal class 6))
    ; Kale example p. 305
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu '(seT)))
   )
   ((and (equal dhaatu 'dhuu) (equal class 1))
    ; Kale example p. 305
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu '(veT)))
   )
   (t
    (setq ans (conjugation-tab-ForC-main upa-syms class pada dhaatu))
   )
  )
  ans
 )
)
(defun conjugation-tab-ForC-main (upa-syms class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab itab
       bitab wparts parts types tense-sym)
  (when nil
   (fol-msg (format "ForC-main: %s %s %s %s %s %s\n"
		    upa-syms class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym ForC-sym)
  (if (not i-insert) (setq i-insert [i]))
  (let (conj-class sym name apada)
   (setq conj-class 1) 
   (if (equal pada 'PASSIVE)
    (setq apada 'A)
    (setq apada pada)
   )
   (setq name (format "%s-%s-%s" tense-sym conj-class  apada))
   (setq sym (intern-soft name))
   (setq endings (sanget 'Sup sym))
   (setq endings (copy-sequence endings))
   (setq n (length endings))
   (setq name (format "%s-%s-%s-strengths" tense-sym conj-class  apada))
   (setq sym (intern-soft name))
   (setq strengths (sanget 'Sup sym))
   (setq strengths (copy-sequence strengths))
  )
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (setq temp (ForC-seTCode dhaatu class pada upa-syms))
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5a. get default table of i-inserts:
  ; All  endings in the luT, lRiT, and lRi~N begin with a consonant
  ; ('t' or 's'), so it is applicable to insert 'i' if this
  ; is required by seT-code.
  (if (equal pada 'P)
    ; parasmaipada
    (setq itab (vector
     seT-gen seT-gen seT-gen
      seT-gen seT-gen seT-gen
      seT-gen seT-gen seT-gen
    ))
    ; atmanepada
    (setq itab (vector
     seT-gen seT-gen seT-gen
     seT-gen seT-gen seT-gen
     seT-gen seT-gen seT-gen
    ))
  )
  (let (i x)
   (setq i 0)
   (while (< i n)
    (setq x (aref itab i))
    (when (equal x 'veT)
     (aset itab i '(aniT seT))
    )
    (setq i (1+ i))
   )
  )
  ;--- 5b. get table of base-seT codes (bitab)
  ; Usually, each element is the list of elements of btab and itab.
  ; However, for some exceptions, e.g. 'aa' roots, elements are
  ; made idiosyncratically
  (let (b nb lc pc strong)
    (if (and (equal ForC-sym 'aashiirli~N) (equal pada 'P))
     (setq strong nil)
     (setq strong t)
    )
;     (fol-msg (format "ForC-sym = %s, pada=%s, strong=%s\n"
; 		     ForC-sym pada strong))
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (cond
      ((member types '("CVC" "VC"))
       ;e.g. prachCh -> ([[p r] [a] [ch Ch]] "CVC"), pc = 'a'
       (setq pc (elt (elt (substring parts -2 -1) 0) 0))
      )
      (t (setq pc (elt b (- nb 2))) ; penultimate char
      )
     )
    )
    ;-- step1: modify 'b' as appropriate for this general tense
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((equal ForC-sym 'aashiirli~N)
      (setq b (benedictive-base dhaatu class pada upa-syms seT-gen))
     )
     (t
      (setq b (future-base dhaatu class pada upa-syms seT-gen))
     )
    )
    ;-- step1a: In case of 'lRi~N', join prefix 'a' to b
    (when (equal ForC-sym 'lRi~N)
     (let (bfirst b1 allb ansb b0)
      (setq allb (if (not (listp b)) (list b) b))
      (setq b nil)
      (while allb
       (setq b0 (car allb))
       (setq allb (cdr allb))
       (setq b1 (augment-a b0))
       (setq b (append-if-new b b1))
      )
     )
     (setq b (solution b))
    )
    ;-- step2: construct btab and bitab
    (setq btab (vector
      b b b
      b b b
      b b b
    ))
    (setq bitab (ForC-bitab btab itab))
   )

  (when nil ; 't' for debug
   (fol-msg (format "bitab=%s\n" bitab))
   (fol-msg (format "endings=%s\n" endings))
  )
  ;--- 6. combine base and endings to get ans
   (setq ans (ForC-bitab-join bitab endings dhaatu strengths i-insert))
  ;--- 7. Irregularities not yet covered
  ; None of these for luT, lRiT, lRi~N
  (when (and (equal ForC-sym 'aashiirli~N) (equal pada 'A))
   (cond
    ((equal dhaatu 'kRi)
     (aset ans 5 'kRiShiiDhvam) ; was kRiShiidhvam
    )
    ((equal dhaatu 'chi)
     (aset ans 5 'cheShiiDhvam) ; was cheShiidhvam ; Kale p. 358
    )
   )
  )
  ans
 )
)
(defun ForC-bitab (btab itab)
 (let (n i ans b c thisans b1 c1 c0)
  (setq n (length btab))
  (setq ans (make-vector n nil))
  (setq i 0)
  (while (< i n)
   (setq b (elt btab i))
   (setq c (elt itab i))
   (if (not (listp b)) (setq b (list b)))
   (if (not (listp c)) (setq c (list c)))
   (setq c0 c)
   (setq thisans nil)
   (while b
    (setq b1 (car b))
    (setq b (cdr b))
    (setq c c0)
    (while c
     (setq c1 (car c))
     (setq c (cdr c))
     (setq thisans (append thisans (list (list b1 c1))))
    )
   )
   (aset ans i thisans)
   (setq i (1+ i))
  )
  ans
 )
)
(defun ForC-bitab-join (bitab endings dhaatu strengths i-insert)
 (let (ans i n y thisans base ending strength e a seT-code bi thisbi thisans1
	   thisans2 strength)
  (setq n (length bitab))
  (setq ans (make-vector n nil))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq strength (elt strengths i)) ;
    (setq bi (elt bitab i)) ; a list of base-seTcode pairs
    (setq thisans nil)
    (while bi
     (setq thisbi (car bi))
     (setq bi (cdr bi))
     (setq base (elt thisbi 0))
     (setq seT-code (elt thisbi 1))
     (setq thisans1
	   (ForC-join base seT-code ending dhaatu strength i-insert))
     (if (not (listp thisans1)) (setq thisans1 (list thisans1)))
     (while thisans1
      (setq thisans2 (car thisans1))
      (setq thisans1 (cdr thisans1))
      (setq thisans2 (sym-without-space thisans2))
      (setq thisans (append-if-new thisans thisans2))
     )
    )
;    (fol-msg (format "%s: %s (%s %s)\n" i thisans (elt bitab i) ending ))
    (setq thisans (solution thisans)) ; so singletons appear w/o parens
    (aset ans i thisans)
    (setq i (1+ i))
   )
   ans
  )
)
(defun ForC-join (base-tok seT-code sup dhaatu strength i-insert)
 (let (ans1 ans)
  (setq ans
   (cond 
    ((listp sup)
     (mapcar (lambda (x)
	 (ForC-join base-tok seT-code x dhaatu strength i-insert)) sup))
    ((listp base-tok)
     (mapcar (lambda (x)
	   (ForC-join x seT-code sup dhaatu strength i-insert)) base-tok))
    ((equal seT-code 'veT)
     (mapcar (lambda (x)
	   (ForC-join base-tok x sup dhaatu strength i-insert)) '(seT aniT))
    )
    (t (ForC-join1 base-tok seT-code sup dhaatu strength i-insert))
   )
  )
;  (setq ans (flatten ans1))
;  (fol-msg (format "%s -> %s \n" ans1 ans))
  ans
 )
)
(defun ForC-join1 (y seT-code ending0 dhaatu strength i-insert)
 ; based on 'conjugation-join'
 ; seT-code is either nil, 'seT' or 'aniT'
 (let (ans skiprefs ylast efirst ending y0 ny yfirst)
  ; insert 'i' if needed
  ; Note 'ending' may then be either a token, or a list of 2 tokens
  (setq ending
   (if (equal seT-code 'seT)
    (conjugation-join i-insert ending0)
    ending0
   )
  )
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq efirst (elt ending 0))
  ; NOTE 2: The logic is put here, because other changes, e.g.,
  ;  before 'tha', are required. This logic applies to other
  ;  forms than the future
  (cond
   ((and (equal dhaatu 'nash)
	     (consonant-P efirst)
	     (not (semivowel-P efirst)))
    ;Kale 476. nash
    ; 'n' is inserted before the ending consonant of 'nash' when
    ; it is followed by any consonant except a nasal or a semi-vowel.
    ; NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
    (setq y (vconcat (substring y 0 -1) [M] (substring y -1)))
   )
   ((and (equal dhaatu 'masj)
	     (consonant-P efirst)
	     (not (semivowel-P efirst)))
    (setq y (vconcat (substring y 0 -2) [~n] (substring y -1)))
    ; Kale 476. masj
    ; 'n' is inserted before the ending consonant and
    ; the  's' is dropped when they are followed by any consonant
    ; except a nasal or a semi-vowel. In particular this is applicable
    ; to all the periphrastic future forms:  ma~Nktaa.
    ; When the 'n' is not dropped, the 's' is changed to 'j': mamajja
   )
   ((and (equal dhaatu 'jabh)
	 (vowel-P efirst)
    )
    ; Kale p.320 footnote
    ; 'jabh' inserts a nasal when its final is followed by a vowel
    (setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
   )
   ((and (equal dhaatu 'rabh)
	 (vowel-P efirst)
    )
    ; Kale p.320 footnote
    ; 'rabh' inserts a nasal when its final is followed by a vowel;
    ; however, 'rabh' does not do it
    ;  (a) in the Aorist
    ;  (b) when it takes 'i', except in the Perfect
    ; In this case, (luT lRiT lRi~N), the only way 'efirst' is a vowel
    ; is if it is an 'i', presetn because seT-code is 'seT'. Since 
    ; the tense is not Perfect, no nasal is inserted
    ;(setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
   )
  )
  (setq ny (length y))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  (cond
   ((and (vowel-P efirst) (member ylast '(i ii Ri)) (equal ForC-sym 'luT))
    ; 1st special sandhi rule for perfect (Antoine2#110)
    (let (wp wp0 wp1 nw y1)
     (setq wp (word-parts y))
     (setq wp1 (elt wp 1)) ; string like "CVCV"
     (setq wp0 (elt wp 0))
     (setq nw (length wp1))
     (setq y0 (substring y 0 -1)) ; all but last char
     (if (equal y0 []) (setq y0 y)) ; 07-24-03 for dhaatu='i', y='ii'
;     (fol-msg (format "wp=%s, y0=%s\n" wp y0))
     (if (and (< 1 nw) (< 1 (length (elt wp0 (- nw 2)))))
      ; compound consonant precedes the final 'i ii Ri'
      (setq ans
       (cond
        ((equal ylast 'i) (vconcat y0 [i y] ending))
        ((equal ylast 'ii) (vconcat y0 [i y] ending))
        ((equal ylast 'Ri) (vconcat y0 [a r] ending))
       )
      )
     ; word is a single vowel ('i ii Ri') or
     ; a simple constant  precedes final 'i ii Ri'
      (setq ans
       (cond
        ((equal ylast 'i) (vconcat y0 [y] ending))
        ((equal ylast 'ii) (vconcat y0 [y]  ending))
        ((equal ylast 'Ri) (vconcat y0 [r]  ending))
       )
      )
      ans
     )
    )
   )
   ((and (vowel-P efirst)
	 (member ylast '(u uu RI))
;	 (equal ForC-sym 'luT)
    )
    ; 2nd special sandhi rule for future (Antoine2#110)
    (setq y0 (substring y 0 -1))
    (setq ans
     (cond
      ((equal ylast 'u) (vconcat y0 [u v] ending))
      ((equal ylast 'uu) (vconcat y0 [u v] ending))
      ((equal ylast 'RI) (vconcat y0 [a r] ending))
     )
    )
   )
   ((and (equal efirst 'dh) (vowel-P ylast) (not (member ylast '(a aa i))))
    ; 3rd special sandhi rule for future (Antoine2#110)
    (setq ans (vconcat y [Dh] (substring ending 1)))
   )
   ((and (member efirst '(t th)) (< 2 ny)
	 (equal (substring y -2) [a r]))
    ; this rule so [ch a k a r] + [th a]
    ; becomes [ch a k a r th a] rather than [ch a k a s th a], which
    ; is what 'sandhi-pair' does
    (setq ans (vconcat y ending))
   )
   ((and (member efirst '(t th))
	 (< 2 ny)
	 (member (substring y -2) '([ch Ch] [sh ch] [r j] [k Sh] [s j] [j j]))
    )
    ; prachCh , vrashch, mRij, akSh
    (setq y0 (substring y 0 -2))
    (if (equal (substring y -2) [r j])
     (setq y0 (substring y 0 -1))
    )
    (when (equal (substring y0 -1) [~n])
     (setq y0 (vconcat (substring y0 0 -1) [~N]))
    )
    (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
    (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
   )
   ;Kale p. 321. Example of klish
   ((and (member efirst '(t th)) (equal ylast 'sh))
    (setq y0 (substring y 0 -1))
    (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
    (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
   )
   ((and (member efirst '(t th))
	 (member dhaatu '(vah sah))
    )
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
   ((and (equal efirst 't)
	 (equal ForC-sym 'luT) ; redundant
	 (member dhaatu '(muh druh snih snuh))
    )
    ; A few verbs take two forms
    ; muh : mogdhaa moDhaa (Kale p. 305)
    ; druh : drogdhaa droDhaa (Kale, dhaatukosha)
    (let (ans1 ans2 ylast1)
     (setq y0 (substring y 0 -1))
     (setq ans1 (vconcat y0 [Dh] (substring ending 1)))
     (setq ylast1 [g])
     (setq ans2 (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
     (setq ans (list ans1 ans2))
    )
   )
   ((and (member efirst '(t th))
	 (< 2 ny)
	 (equal (substring y -2) [a h]))
    ; 'dah' : dagdhaa (luT)
    ; 'nah' : naddhaasmi (luT)
    (let (ylast1)
     (setq ylast1 (cond
      ((equal yfirst 'd) [g])
      ((equal yfirst 'n) [d])
      (t (vector ylast))
     ))		
     (setq ans (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
    )
   )
   ;Kale p. 322. Example of 'muh', 'druh', 'snih', 'snuh'
   ((and (member efirst '(t th)) (equal ylast 'h))
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))
   )
   ((and (member efirst '(t th)) 
	 (member ylast '(j ch)))
    ; this rule [bh a j] + [th a] -> [bh a k th a]
    ; rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
    ; but [bh a ~n j] + [th a] -> [bh a ~N k th a]
;    (fol-msg (format "y=%s ending=%s\n" y ending))
    (cond
     ((and (< 2 ny) (equal (substring y -2) [a j]))
      ; yaj, sRij (has be changed to 'sraj')
      (setq y0 (substring y 0 -1))
      (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
      (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
     )
     ((and (< 2 ny) (equal (substring y -2) [~n j]))
      ;bha~nj, masj
      (setq ans (vconcat (substring y 0 -2) [~N k] ending))
     )
     (t
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [k] ending))
     )
    )
   )
   ((and (member efirst '(t th))
	 (member ylast '(dh bh)))
    ; so [v i v y a dh] + [th a] -> [v i v y a d dh a]
    ; sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0
		       (vector (de-aspirate ylast))
		       [dh] (substring ending 1)))
   )
   ((and (member efirst '(t th))
	 (member ylast '(m n)))
    ; For 'gam', sandhi-pair gives 'jagaMtha', but
    ; Kale and Antoine both show 'jagantha'
    ; Similaraly, for 'han' we want 'jaghantha' rather than 'jagaMtha'
    (setq ans
     (vconcat (substring y 0 -1) ; all but last char
	      [n]
	      ending))
   )
   ((and (equal efirst 'm) (equal ylast 'ch))
    (setq ans (vconcat y ending)) ; otherwise, 'ch' is changed to 'j'
   )
   ((and (equal ylast 'm) (member efirst '(m v)))
    ;Kale p. 321 (footnote)
    ;Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
    ;note 'n' may be changed to 'N' by sandhi-single (see below)
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [n] ending)) 
   )
   ((and (equal efirst 's)
    (progn
     (when nil
      (fol-msg (format "check:  %s %s %s %s\n" y ny (substring y -2) ylast))
     )
     t
    )
    (cond
     ((member ylast '(sh))
      ; Kale p. 321. based on example of 'ash'
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [k Sh] (substring ending 1))) 
     )
     ((member ylast '(bh b))
      ; case of 'labh': Kale p. 301
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [p] ending))
     )
     ((member ylast '(dh d))
      ; case of 'vRidh': Kale p. 301
      ; case of 'bandh': Kale p. 303 : previous 'b' gets the aspiration
      (setq y0 (substring y 0 -1))
      (if (equal ylast 'dh) (setq y0 (aspirate-first-cons y0)))
      (setq ans (vconcat y0 [t] ending))
     )
     ((equal ylast 'h)
      ; Examples:
      ; 'nah' : natsyaami
      ; 'vah' : vakShyaami
      ; 'muh' : mokShyaami
      ; 'tRih' : tarkShyati
      ; The following also aspirate the first consonant: 
      ; 'dah' : dhakShyaami
      ; 'duh' : dhokShyaami
      ; 'guh' : ghokShyaami
      ; 'gaah' : ghaakShyate
      ; 'gRih' : gharkShyate
;        (fol-msg (format "check-h: %s: %s %s \n" dhaatu y ending ))
      (setq y0 (substring y 0 -1))
      (cond
       ((member yfirst '(n))
        (setq ans (vconcat y0 [t] ending))
       )
       ((member yfirst '(v m t))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
       )
       ((member yfirst '(d g))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
        (setq ans (aspirate-first-cons ans))
       )
       (t
;        (setq ans (vconcat y ending))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
       )
      )
     )
     ((equal ylast 's)
      ; Kale 480. 't' is substituted for the ending 's' of a root
      ; when followed by any non-conjugational termination
      ; beginning with 's'
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [t] ending))
     )
     ((and (< 2 ny)
	   (member (substring y -2) '([ch Ch] [sh ch])))
      ; from example of 'prachCh' (Antoine2 p. 89)
      (setq y0 (substring y 0 -2))
      (setq ans (vconcat y0 [k Sh] (substring ending 1)))
     )
     ((member ylast '(j ch Sh))
      ; from example of 'sRij' (Antoine2 p. 89),
      ; and of 'kRiSh'
      ; takSh (p. 304)
      (setq y0 (substring y 0 -1))
      (cond
       ((equal (substring y0 -1) [~n])
        (setq y0 (vconcat (substring y0 0 -1) [~N]))
       )
       ((member (elt (substring y0 -1) 0) '(k j s))
	(setq y0 (substring y0 0 -1)) ; drop the last penultimate letter
       )
      )
      (setq ans (vconcat y0 [k Sh] (substring ending 1)))
     )
    )
   ))
   ; end of case: efirst = 's
   ((and (equal dhaatu 'guh) (equal seT-code 'seT))
    ; the 'u' is lengthened, rather than gunated (Kale example p. 304)
    (setq ans (vconcat [g uu h] ending))
   )
   (t
    ; joining for other cases like 'conjugation-join'
    (sandhi-pair-skiprefs-set (list 'Antoine72-4 'Antoine72-5))
    (setq ans
     (or
      (solution (sandhi-pair y ending 'internal 'join))
      (solution (sandhi-pair y ending nil 'join))
      (vconcat y ending)
     )   
    )
    (sandhi-pair-skiprefs-set nil)
   )
  )
  (setq ans (or (sandhi-single ans) ans))
  (setq ans (solution ans))
  ans
 )
)

(defun aspirate-last-cons (tok)
 ;aspirate the last consonant found in tok
 ;tok assumed to be a sequence of Itrans tokens
 (let (n i x y ans)
  (setq ans (copy-sequence tok))
  (setq n (length tok))
  (setq i (1- n))
  (while (<= 0 i)
   (setq x (elt tok i))
   (when (consonant-P x)
    (setq y (aspirate x))
    (aset ans i y)
    (setq i -1) ; end loop
   )
   (setq i (1- i))
  )
  ans
 )
)
(defun aspirate-first-cons (tok)
 ;aspirate the first consonant found in tok
 ;tok assumed to be a sequence of Itrans tokens
 (let (n i x y ans)
  (setq ans (copy-sequence tok))
  (setq n (length tok))
  (setq i 0)
  (while (< i n)
   (setq x (elt tok i))
   (when (consonant-P x)
    (setq y (aspirate x))
    (aset ans i y)
    (setq i n) ; end loop
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun first-cons (tok)
 ;return the first consonant found in tok
 ;tok assumed to be a sequence of Itrans tokens
 (let (n i x y ans)
  (setq n (length tok))
  (setq i 0)
  (while (< i n)
   (setq x (elt tok i))
   (when (consonant-P x)
    (setq ans x)
    (setq i n) ; end loop
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun augment-a (b0)
 ; b0 is a tok-array.
 ; add the augment 'a' 
 ; applicable to conditional (lRi~N),
 ; imperfect (la~N)
 (let (bfirst b1)
  (setq bfirst (elt b0 0))
  (cond
   ((vowel-P bfirst)
    ; 08-20-03. Use vrddhi1 so 'e'->'ai'
    (setq b1 (vconcat (vrddhi1 bfirst) (substring b0 1)))
   )
   ((equal bfirst 'Ch)
    ; based upon example of 'ChRid' (lRi~N)
    (setq b1 (vconcat [a] [ch]  b0))
   )
   (t   
    (setq b1 (vconcat [a] b0))
   )
  )
 b1
 )
)
(defun samprasaaraNa-P (dhaatu class)
 ; Kale 502, p. 308. 
 (cond
  ((member dhaatu '(
   vach yaj vap vah
   vad svap  vash vyach
   prachCh vrashch 
   grah vyadh
   ve hve shvi
   vye jyaa
   bhrasj
   ))
   t
  )
  ((and (equal dhaatu 'vas) (equal class 1)) ; vas (to dwell)
   t
  )
  (t
   nil
  )
 )
)
(defun samprasaaraNa (tok)
 ; Kale 502 p. 308
 (let (ans nparts wparts parts types c1 v c2 c1a c1b sv sv-vowel)
  (setq wparts (word-parts tok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  (setq nparts (length parts))
  ; for [g r a h], parts=[[g r] [a] [h]]
  (when (< 1 nparts)
   (setq c1 (elt parts 0))
   (setq v (elt parts 1))
  )
  (when (< 2 nparts)
   (setq c2 (elt parts 2))
  )
  (when c1
   (setq c1a (substring c1 0 -1))
   (setq c1b (substring c1 -1))
   (setq sv (elt c1b 0))
  )
  (cond 
   ((equal sv 'y) (setq sv-vowel 'i))
   ((equal sv 'r) (setq sv-vowel 'Ri))
   ((equal sv 'v) (setq sv-vowel 'u))
  )
  (cond
   ((equal tok [bh r a s j])
    (setq ans [bh Ri j j])
   )
   ((member tok '([v e] [h v e] [sh v i]))
    (setq ans (vconcat c1a [uu]))
   )
   ((member tok '([v y e] [j y a a]))
    (setq ans (vconcat c1a [ii]))
   )
   (sv-vowel
;    (setq ans (vconcat c1a (vector sv-vowel) c2))
    (let (ans1 c2a)
     (setq c2a (vconcat c2 [a]))
     (setq ans1 (conjugation-join (vector sv-vowel) c2a))
     (setq ans1 (substring ans1 0 -1)) ; remove [a]
     (setq ans (vconcat c1a ans1))
;      (fol-msg (format "samprasaaraNa: %s %s %s %s %s\n"
; 		      c1a sv-vowel c2 ans1 ans))
    )
   )
   (t
    ; samprasaaraNa not applicable.
    ; just return tok
    (setq ans tok)
   )
  )
  ans
 )
)

(defun benedictive-base (dhaatu class pada upa-syms &optional seT-gen)
 (let (atok types parts wparts tense b)
  (setq tense 'aashiirli~N)
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))

  (let (nb lc pc)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (cond
      ((member types '("CVC" "VC"))
       ;e.g. prachCh -> ([[p r] [a] [ch Ch]] "CVC"), pc = 'a'
       (setq pc (elt (elt (substring parts -2 -1) 0) 0))
      )
      (t (setq pc (elt b (- nb 2))) ; penultimate char
      )
     )
    )
    ;-- step1: modify 'b' as appropriate for this general tense
    (cond
     ((equal pada 'P)
      ; benedictive P
      (cond
       ((and (equal dhaatu 'ii) upa-syms)
	; Kale 588. 'ii' shortens its 'ii' when joined with prepositions
	(setq b (vconcat (substring b 0 -1) [i]))
       )
       ((and (equal dhaatu 'uuh) upa-syms)
	; Kale 588. 'uuh' shortens its 'uu' when joined with prepositions
	; before weak terminations beginning with 'y', as is the
	; case in benedictive parasmaipada.
	(setq b (vconcat [u] (substring b 1) ))
       )
       ((equal lc 'i)
	; Kale 581. final 'i' lengthened
	(setq b (vconcat (substring b 0 -1) [ii]))
       )
       ((equal lc 'u)
	; Kale 581. final 'u' lengthened
	(setq b (vconcat (substring b 0 -1) [uu]))
       )
       ((equal dhaatu 'Ri)
	; Kale 582. The root 'Ri' is gunated
	(setq b (gunate-final-vowel b))
       )
       ((and (equal lc 'Ri) (< 2 nb))
	; Kale 582. Final 'Ri' preceded by conjunct consonant is gunated
	(setq b (gunate-final-vowel b))
       )
       ((equal lc 'Ri)
	; Kale 581. Final 'Ri' changed to 'ri'
	(setq b (vconcat (substring b 0 -1) [r i]))
       )
       ((equal lc 'RI)
	; Kale 581. Final 'RI' becomes 'iir' after non-labial
	; and becomes 'uur' after labial or 'v'
	(if (or (labial-P pc) (equal pc 'v))
	 (setq b (vconcat (substring b 0 -1) [uu r]))
	 (setq b (vconcat (substring b 0 -1) [ii r]))
	)
       )
       ((kale-584-P dhaatu)
        ; Kale 584. A penultimate nasal is dropped for these
        (setq b (vconcat (substring b 0 -2) (substring b -1)))
       )
       ((and (samprasaaraNa-P dhaatu class) (not (equal dhaatu 'jyaa)))
	 ; Kale 583. Roots capable of taking samprasaaraNa take it
	(setq b (samprasaaraNa b))
       )
       ((equal dhaatu 'shaas)
	; 583. 'shaas' substitutes 'i' for its vowel
	(setq b [sh i Sh])
       )
       ((or (member dhaatu '(daa dhaa maa sthaa gai))
	    (and (equal dhaatu 'paa) (equal class 1))
	    (and (equal dhaatu 'haa) (equal class 3))
	)
        ;Kale 585. in these cases, the final 'aa' changes to 'e'
        (setq b (vconcat (substring b 0 -1) [e]))
       )
       ((and (equal types "CV")
	    (member lc '(aa e ai o))
	    (< 1 (length (elt parts 0))) ; initial conjunct consonant
        )
        ;Kale 585. final 'aa' (original or substituted), if it be
        ; preceded by a conjunct consonant, is changed to 'e' optionally:
	(let (b1 b2)
	 (setq b1 (vconcat (substring b 0 -1) [e]))
	 (setq b2 (vconcat (substring b 0 -1) [aa]))
	 (setq b (list b1 b2))
	)
       )
       ((equal dhaatu 'han)
	; Kale p. 359 -example
	(setq b [v a gh])
       )
       (t ; otherwise, no guna or vrddhi for benedictive P
       )
      )
     )
     ((equal pada 'A)
      ; benedictive A
      (if (not seT-gen)
       (setq seT-gen (ForC-seTCode dhaatu class pada upa-syms tense))
      )
      (cond
       ((and (equal dhaatu 'ii) upa-syms)
	; Kale 588. 'ii' shortens its 'ii' when joined with prepositions
	(setq b (vconcat (substring b 0 -1) [i]))
       )
       ((equal dhaatu 'dRish)
        ; By Kale p. 364 example
	; as 'dRish is 1 'P in dhaatukosha, this case only
	; occurs during the 'regular' passive formation
	; There is no gunation, so nothing to do
       )
       ((and (equal lc 'Ri) (equal seT-gen 'aniT))
	; Kale 587 Final 'Ri' unchanged when 'i' not prefixed
       )
       ((and (equal lc 'RI) (equal seT-gen 'aniT))
	; Kale 587 When 'i' is not prefixed,
	; final 'RI' preceded by a labial or 'v' is changed to 'uur';
	; otherwise, it is changed to 'iir'
	(if (or (labial-P pc) (equal pc 'v))
	 (setq b (vconcat (substring b 0 -1) [uu r]))
	 (setq b (vconcat (substring b 0 -1) [ii r]))
	)
       )
       ((equal lc 'aa)
	; don't gunate final 'aa' - leave it unchanged
       )
       ((and (equal types "CV")
	    (member lc '(e ai o))
        )
        ;Kale 585. final 'e', 'ai', 'o' become 'aa'
	(setq b (vconcat (substring b 0 -1) [aa]))
       )
       ((equal dhaatu 'bhrasj)
        ; Kale 464. The root 'bhrasj' (6 P 'to fry') assumes the
        ; forms 'bhrasj' and 'bharj' in the non-conjugational tenses
        (let (b1)
         (setq b (list [bh r a j j] [bh a r j]))
        )
       )
       ; otherwise, the radical vowel takes its guNa substitute
       (t
	 (setq b (gunate-final-vowel b))
       )
      ) 
     )              
    )
   )
  b
 )
)
(defun future-base (dhaatu class pada upa-syms tense &optional seT-gen)
 (let (atok types parts wparts b)
  (if (not seT-gen)
   (setq seT-gen (ForC-seTCode dhaatu class pada upa-syms tense))
  )
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))

  (let (nb lc pc strong)
    (if (and (equal tense 'aashiirli~N) (equal pada 'P))
     (setq strong nil)
     (setq strong t)
    )
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (cond
      ((member types '("CVC" "VC"))
       ;e.g. prachCh -> ([[p r] [a] [ch Ch]] "CVC"), pc = 'a'
       (setq pc (elt (elt (substring parts -2 -1) 0) 0))
      )
      (t (setq pc (elt b (- nb 2))) ; penultimate char
      )
     )
    )
    ;-- step1: modify 'b' as appropriate for this general tense
    (cond
     ((equal class 11) ; causal.
      (setq b atok) ; assume causal base comes in as dhaatu
;       (let (toks Eng-def)
;        (setq toks (causal-base dhaatu class pada upa-syms Eng-def))
;        (setq b (solution toks))
;        (setq class 11)
;       )
     )
     ((equal dhaatu 'daridraa)
      ; Kale 467. 'daridraa' drops its 'aa' before a non-conjugation
      ; termination except in the Desiderative and the Aorist where
      ; it retains it optionally
      (setq b (substring b 0 -1))
     )
     ((and (equal dhaatu 'gaa) (equal upa-syms '(adhi)))
      ; Kale 486. See note in 'conjugation-tab-ForC'
      ; No guna but final vowel changed to 'ii'
      (setq b (vconcat (substring b 0 -1) [i]))
     )
     ((member lc '(aa e ai o))
      ; Kale 459. Roots ending in 'e', 'ai', and 'o' are treated as
      ; roots ending in 'aa'
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
     ((and (member dhaatu '(mii mi dii)) strong)
      ; Kale 459. The roots 'mi' (5 U 'to throw'),
      ; 'mii' (9 U 'to kill'), and 'dii' (4 A 'to perish')
      ; are treated as roots ending in 'aa' before a termination
      ; causing guna or vrddhi.
      ; In particular, this is the case for luT, lRiT, lRi~N;
      ; but not the case for benedictive-A
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
     ((and (equal dhaatu 'lii) strong)
      ; Kale 459. The root 'lii' (9 P, 4 A 'to adhere or cling to') changes
      ; its vowel optionally to 'aa' before a termination causing
      ; guna or vrddhi.  
      ; In particular, this is the case for luT, lRiT, lRi~N
      (let (b1)
       (setq b (gunate-final-vowel b))
       (setq b1 [l aa])
       (setq b (list b b1))
      )
     )
     ((equal class 10)
      ; Kale 460 In the general tenses,
      ; roots of the tenth class preserve their 'ay' (i.e.,
      ; 'aya' with the final 'a' dropped), with all the changes that the
      ; root undergoes before it.
      ; The function 'dhaatu-a~Nga' performs this task.
      ; (dhaatu-a~Nga 'gaN 10 'P) -> (([g a N a y] ((count)) nil))
      (let (toks)
       (setq toks (class-a-base dhaatu 10 pada))
       (setq b (solution toks))
      )
     )
     ((and (equal dhaatu 'as) (equal class 2))
      ; Kale 462. 'as' substitutes 'bhuu' for itself
      ; NOTE: Although Kale does not say that the class 2 form of 'as'
      ; is intended, I have assumed this is meant. i.e., the
      ; class 4 form of 'as' (meaning 'to throw') is not altered
      (setq b [bh u u])
      (if strong (setq b (gunate-final-vowel b)))
     )
     ((equal dhaatu 'bruu)
      ; Kale 462. 'bruu' substitutes 'vach' for itself
      (setq b [v a ch])
      (if strong (setq b (gunate-final-vowel b)))
     )
     ((kuTaadi-P dhaatu)
      ; Kale 463.
      ; Neither guNa nor vRiddhi is substituted for the vowel of a few
      ; roots of the 6th class even before a strong termination except
      ;  (a) the 'a' of 1S and 3S of the Perfect
      ;  (b) the 'ay' of the causal
      ;  (c) the 'i' of the 3S of the Passive Aorist
      ; no change to b. It is already 'atok'
     )
     ((equal dhaatu 'bhrasj)
      ; Kale 464. The root 'bhrasj' (6 P 'to fry') assumes the
      ; forms 'bhrajj' and 'bharj' in the non-conjugational tenses
      (let (b1 b2)
;       (setq b (gunate-final-vowel b))
;       (setq b1 (gunate-final-vowel [bh a r j]))
;       (setq b1 [bh a r j])
;       (setq b (list b b1))
       (setq b (list [bh r a j j] [bh a r j]))
      )
     )
     ((and (equal dhaatu 'vij) (member class '(6 7)))
      ; Kale 466. The intermediate 'i' is weak in the case of the
      ; root 'vij' (6 A 7 P 'to tremble')
      ; In the case of luT, lRiT, lRi~N, I think this means
      ; no gunation. Thus, no change to 'b' is required here
     )
     ((and (equal dhaatu 'dhuu) (equal class 6))
      ; Kale example p. 305.
      ; end result is 'dhuvitaasmi', etc.
      ; thus, no gunation - no change to 'b' is required here
     )
     ((and (equal dhaatu 'uurNu) strong)
      ; Kale 466. The intermediate 'i' is optionally weak in the case of the
      ; root 'uuRNu' (to cover)
      ; In the case of luT, lRiT, lRi~N, I think this means
      ; no gunation as an option.
      (let (b1)
       (setq b1 (gunate-final-vowel b))
       (setq b (list b b1))
      )
     )
     ((member dhaatu '(diidhii vevii))
      ; Kale 467. The root 'diidhii' (2 A 'to shine') does not take
      ; guNa or vRiddhi before any termination. It also drops its
      ; final vowel before the intermediate 'i' and before 'y'.
      ; The root 'vevii' (2 A 'to go') takes the same changes.
      ; Note: As these are roots of more than one syllable, they are 'seT'.
      ; For the luT, lRiT, lRi~N, this means that the final 'ii'
      ; is always dropped.
      ; Similarly, for ashiirli~N, whether P or A
      (setq b (substring b 0 -1))
     )
     (t
      ; the default situation. gunate the vowel
      (setq b (gunate-final-vowel b))
     )
    )
   )
  
  b
 )
)
(defun ForC-seTCode (dhaatu class pada upa-syms &optional tense)
 (let (temp)
  (if (not tense) (setq tense ForC-sym))
  (cond
   ((and (equal tense 'aashiirli~N) (equal pada 'P))
    (setq temp '(aniT))
   )
   ; based upon example of 'budh' (p. 366) (passive -> A)
   ((and (equal dhaatu 'budh) (equal pada 'A))
    (setq temp '(seT))
   )
   (t
    (cond
     ((equal pada 'PASSIVE)
      (setq temp (construct-seT-code1a dhaatu class 'A upa-syms))
     )
     (t
      (setq temp (construct-seT-code1a dhaatu class pada upa-syms))
     )
    )
   )
  )
  (solution temp) ; returned
 )
)
(defun kale-584-P (dhaatu)
 (if (member dhaatu '(a~nch a~nj bha~nj ra~nj sa~nj sva~nj
			granth manth  und skand syand
			indh bandh dambh stambh
			daMsh bhraMsh sraMs tRiMh))
  t
 )
)

(defun construct-futpart1a (dhaatu class pada upasargas)
; future participle (active) parasmaipada and atmanepada
; The form returned is a list of symbols, where
;  When pada = 'P', each symbol ends in 'y'; 
;  to get the declension table in a particular gender for
;  the future participle from one of these symbols, say x,  ending in 'y':
;     (say, x = 'gamiShy)
;   (a) (setq p (pres-part-praatipadikas (sym-concat x 'ante) 6 'P x))
;     (p = '(gamiShyat gamiShyant SW))
;   (b) (setq dtab (declension-pres-part-P p gender)), e.g.
;      [gamiShyan gamiShyantau gamiShyantaH 
;       gamiShyantam gamiShyantau gamiShyataH 
;      ...
;      ]
;  When pada = 'A', each symbol ends in 'aan' (or 'aaN'), the form
;   needed for the declension ending in 'a'.
 (let (ans ctab conjelt c tok ans1)
  (setq ctab (conjugation-tab-lRiT upasargas class pada dhaatu 'ACTIVE))
  (setq conjelt (elt ctab 0)) ; 3S of simple (second) future
  (if (not (listp conjelt)) (setq conjelt (list conjelt)))
  (while conjelt
   (setq c (car conjelt))
   (setq conjelt (cdr conjelt))
   (cond
    ((equal pada 'P)
     ; c ends in 'ati', e.g. 'gamiShyati'.
     ; return 'gamiShy', by dropping last 3 chars ('ati')
     (setq tok (car (ITRANS-parse-words-1 (symbol-name c))))
     (setq ans1 (substring tok 0 -3))
    )
    ((equal pada 'A)
     ; c ends in 'ate', e.g. 'gamiShyate'.
     ; return 'gamiShyamaaN', by
     ;  (a) dropping last 2 chars ('te')
     ;  (b) joining 'maana'
     ;  (c) dropping final 'a'
     (setq tok (car (ITRANS-parse-words-1 (symbol-name c))))
     (setq ans1 (substring tok 0 -2))
     (setq ans1 (conjugation-join ans1 [m aa n a]))
     (setq ans1 (substring ans1 0 -1))
    )
    (t (setq ans1 nil))
   )
   (when ans1
    (setq ans1 (sym-without-space ans1))
    (setq ans (append-if-new ans ans1))
   )
  )
  ans
 )
)
(defun construct-futppart1a (dhaatu class pada upasargas)
; future participle (active) parasmaipada and atmanepada
; The form returned is a list of symbols, 
; where each symbol ends in 'aan' (or 'aaN'), the form
;   needed for the declension ending in 'a'.
 (let (ans ctab conjelt c tok ans1)
  (setq ctab (conjugation-tab-lRiT upasargas class pada dhaatu 'PASSIVE))
  (setq conjelt (elt ctab 0)) ; 3S of simple (second) future
  (if (not (listp conjelt)) (setq conjelt (list conjelt)))
  (while conjelt
   (setq c (car conjelt))
   (setq conjelt (cdr conjelt))
   ; c ends in 'ate', e.g. 'gamiShyate'.
   ; return 'gamiShyamaaN', by
   ;  (a) dropping last 2 chars ('te')
   ;  (b) joining 'maana'
   ;  (c) dropping final 'a'
   (setq tok (car (ITRANS-parse-words-1 (symbol-name c))))
   (setq ans1 (substring tok 0 -2))
   (setq ans1 (conjugation-join ans1 [m aa n a]))
   (setq ans1 (substring ans1 0 -1))
   (when ans1
    (setq ans1 (sym-without-space ans1))
    (setq ans (append-if-new ans ans1))
   )
  )
  ans
 )
)
