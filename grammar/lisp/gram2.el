; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; gram2.el  
; begun 07-20-02 by ejf
; Code in the following may be used:
;   itrans.el, gram1.el, sandhi.el
; The particular spelling of the symbols representing
; the phonetic elements (the 'tokens') must be consistent with 
; that in itrans.el

(defvar all-special-tenses 
 '(laT la~N loT vidhili~N)
)

(defvar passive-irregs)
(defvar passive-data)
(defvar participle-list)
(defvar person-number-set
 [[3 S] [3 D] [3 P]
  [2 S] [2 D] [2 P]
  [1 S] [1 D] [1 P]]
)
(defvar 1-P-properties-set
 [1-P-3S 1-P-3D 1-P-3P
  1-P-2S 1-P-2D 1-P-2P
  1-P-1S 1-P-1D 1-P-1P])
(defvar 1-A-properties-set
 [1-A-3S 1-A-3D 1-A-3P
  1-A-2S 1-A-2D 1-A-2P
  1-A-1S 1-A-1D 1-A-1P])
(defvar 2-P-properties-set
 [2-P-3S 2-P-3D 2-P-3P
  2-P-2S 2-P-2D 2-P-2P
  2-P-1S 2-P-1D 2-P-1P])
(defvar 2-A-properties-set
 [2-A-3S 2-A-3D 2-A-3P
  2-A-2S 2-A-2D 2-A-2P
  2-A-1S 2-A-1D 2-A-1P])

; laT endings (method of Antoine)
(defun word-parts (tokar)
 (let (types parts type part i x n more conP)
  (setq n (length tokar))
  (setq parts (vector))
  (setq types (vector))
  (setq i 0)
;  get c1
  (while (< i n)
   (setq x (elt tokar i))
   (setq conP (consonant-P x))
   (setq type (if conP "C" "V"))
   (setq more 't)
   (setq part (vector))
   (while (and (< i n) more)
    (setq x (elt tokar i))
    (setq more (if conP (consonant-P x) (vowel-P x)))
    (when more
     (setq part (vconcat part (vector x)))
     (setq i (1+ i))
    )
   )
   (setq parts (vconcat parts (vector part)))
   (setq types (concat types type))
  )
  (list parts types)
 )
)
(defun dhaatu-parts (tokar)
 (let (parts types x p1 t1 e)
  (setq x (word-parts tokar))
  (setq parts (elt x 0))
  (setq types (elt x 1))
  (setq e (vector (vector)))
  (cond
   ((< 3 (length parts))
    ; 03-19-02 dealing with [s a M m aa n] where
    ; parts = [[s] [a] [M m] [aa] [n]]
    ; types = "CVCVC"
    ; we want to return ([s a M m] [aa] [n] "CVC")
    ; so want p1 = [[s a M m] [aa] [n]] t1 = "CVC"
    (setq t1 (substring types -3))
    (let (u u1 v v1 w)
     (setq w (substring parts -2)) ; [[aa] [n]]
     (setq u (substring parts 0 -2)) ; [[s] [a] [M m]]
     (setq u1 (append u nil)) ; ([s] [a] [M m])
     (setq v (apply 'vconcat u1)) ; [s a M m]
     (setq v1 (vector v)) ; [[s a M m]]
     (setq p1 (vconcat v1 w)) ; [[s a M m] [aa] [n]]
    )
   )
   ((= 3 (length parts))
   (setq p1 parts)
   (setq t1 types)
   )
   ((= 2  (length parts))
    (setq t1 types)
    (cond
     ((equal types "CV")
      (setq p1 (vconcat parts e))
     )
     (t ; types = "VC")
      (setq p1 (vconcat e parts))
     )
    )
   )
   ((= 1  (length parts))
    (setq t1 types)
    (cond
     ((equal types "V")
      (setq p1 (vconcat e parts e))
     )
     (t ; types = "C", should not happen!
      (setq p1 (vconcat parts e e))
     )
    )
   )
  )
  (append p1 (list t1))
 )
)

(defun dhaatu-a~Nga-a-REGULAR (dhaatu class pada)
 ;dhaatu is a symbol
 ;class is a number (conjugation class 1, 4, 6, or 10)
 ;pada is a symbol ('P or 'A)
 ;returns a~Nga as a symbol array
 (let (a~Nga dhaatu-tokar cvsym dhaatu-string procname)
  (setq procname "dhaatu-a~Nga-a-REGULAR")
  (setq cvsym (intern (format "%s-%s" class pada)))
;   (when (not (member class (get dhaatu cvsym)))
;    (fol-msg
;     (format "(Warning) dhaatu-a~Nga-a: conjugation-pada not listed: %s %s\n"
;      (symbol-name dhaatu) cvsym))
;   )
  (setq dhaatu-string (symbol-name dhaatu))
  (setq dhaatu-tokar (car (ITRANS-parse-words-1 dhaatu-string)))
  (setq a~Nga
   (cond
    ((= class 1) (dhaatu-a~Nga-1 dhaatu-tokar))
    ((= class 4) (dhaatu-a~Nga-4 dhaatu-tokar))
    ((= class 6) (dhaatu-a~Nga-6 dhaatu-tokar))
    ((= class 10) (dhaatu-a~Nga-10 dhaatu-tokar))
    ((member class '(2 3 5 7 8 9))
     dhaatu-tokar
    )
    (t (fol-msg (format
	 "(Error) %s: conjugation should be 1-10: %s %s\n" procname
        (symbol-name dhaatu) class))
     nil
    )
   )
  )
 )
)
(defun dhaatu-a~Nga-1 (tokar)
 ; following Antoine I.7 and Kale 388
 ; tokar is an array of alphabetical tokens
 ; the function returns a token array
 (let (ans x c1 v c2 type  )
  (setq x (dhaatu-parts tokar))
  (setq c1 (elt x 0)) (setq v (elt x 1)) (setq c2 (elt x 2))
  (setq type (elt x 3))
  (cond
   ; 1.  A final vowel takes guna
   ((or (string= type "CV") (string= type "V"))
    (setq v (guna (elt v 0))))
   ; 2. penultimate i u Ri Li may be modified 
   ;    preceding a compound consonant beginning with r or v
   ((and (member (elt v 0) '(i u Ri Li)) (< 1 (length c2)))
    (setq v (kale-395 c1 v c2 type)))
   ; 3. A short medial vowel takes guna
   ;  a medial vowel is a vowel which stands between consonants
   ;  Note: Kale refers to 'the penultimate' short vowel - i.e.,
   ;  a vowel followed by a consonant, but not necessarily also
   ;  preceded by a consonant - i.e. type = "VC".  The 
   ;  following uses the Kale interpretation
   ;  a short medial vowel is a medial vowel provided
   ;    1) it is a short simple vowel
   ;    2) the final consonant (c2) is not compound.
   ; Note that when the length of c2 is non-zero, then the
   ; type is either CVC or VC (it cannot be CV or V)
   ((and (= (length c2) 1)
      (shortsimplevowel-P (elt v 0))
     )
    (setq v (guna (elt v 0))))
   ; 4. Vowel (long) RI is modified :
   ;  For conjugation 1 verbs, this could not happen in types CV or V,
   ;  as guna is already applied by case 1. However, it could happen
   ;  with types CVC or VC as RI is not short
   ((equal v [RI]) 
    (setq v (kale-394 c1 v c2 type)))
  )
 ; in get-conj-elt-1, the next letter will be "a" or "aa"
 ; when the original root ends in a vowel whose
 ; guna is "e" or "o", this may require a sandhi change
 ; to v
 (if (or (string= type "CV") (string= type "V"))
  (progn
   (setq v (sandhi-internal-diphthong-A v ))
   (vconcat c1 v)
  )
  (vconcat c1 v c2)
  )
 )
)
(defun dhaatu-a~Nga-4 (tokar)
 ; following Antoine I.16 and Kale 389
 ; tokar is assumed to be an array of alphabetical tokens
 ; the function returns a token array
 (let (ans x c1 v c2 type tokar1)
  ; "y" is added to root
  (setq tokar1 (vconcat tokar [y]))
  (setq x (dhaatu-parts tokar1))
  (setq c1 (elt x 0)) (setq v (elt x 1)) (setq c2 (elt x 2))
  (setq type (elt x 3))
  (cond
   ; 2. penultimate i u Ri Li may be modified 
   ;   preceding a compound consonant beginning with "r" or "v"
   ((and (member (elt v 0) '(i u Ri Li)) (< 1 (length c2)))
    (setq v (kale-395 c1 v c2 type)))
   ; 4. Vowel RI may be modified
   ((equal v [RI]) 
    (setq v (kale-394 c1 v c2 type)))
  )
 (vconcat c1 v c2)
 )
)
(defun dhaatu-a~Nga-6 (tokar)
 ; following Antoine I.23, Kale 390
 ; tokar is assumed to be an array of alphabetical tokens
 ; the function returns a token array
 ; 05-13-04: bruu has base bru (Whitney) when in class 6.
 (let (ans x c1 v c2 type tokar1 v0)
  (setq tokar1 tokar)
  (setq x (dhaatu-parts tokar1))
  (setq c1 (elt x 0)) (setq v (elt x 1)) (setq c2 (elt x 2))
  (setq type (elt x 3))
  (cond
   ; 1. penultimate i u R^i L^i may be modified 
   ;   preceding a compound consonant beginning with "r" or "v"
   ((and (member (elt v 0) '(i u Ri Li)) (< 1 (length c2)))
    (setq v (kale-395 c1 v c2 type)))
   ; 2. final vowel may be changed
   ((= 0 (length c2))
    (setq v0 (elt v 0))
    (cond
     ((member v0 '(i ii))
      (setq v (vconcat v [y])))
     ((equal tokar [b r uu])
      (setq v [u v])  ; shorten the 'uu'
     )
     ((member v0 '(u uu))
      (setq v (vconcat v [v])))
     ((equal v0 'Ri)
      (setq v [r i y]))
     ((equal v0 'RI)
      (setq v [i r]))    
     )
    )
   )
  (vconcat c1 v c2)
 )
)
(defun dhaatu-a~Nga-10 (tokar)
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
 (let (ans x c1 v c2 type tokar1 v0)
  (setq tokar1 tokar)
  (setq x (dhaatu-parts tokar1))
  (setq c1 (elt x 0)) (setq v (elt x 1)) (setq c2 (elt x 2))
  (setq type (elt x 3))
  (cond
   ((or (string= type "CV") (string= type "V"))
    ; 1.  A final vowel takes vrddhi and may be subject to sandhi
    ;     change before the affixation of [a y]
    (setq v (vrddhi (elt v 0)))
    (setq v (sandhi-internal-diphthong-A v )) ; [ai] -> [aa y]
   )
    ; in following rules, type is either CVC or VC
   ((and (= (length c2) 1) (equal v [a])) ; short 'a' not prosodially long
    ; 2. A penultimate 'a' takes vrddhi
    ; Note: Kale also has additional requirement 'not prosodially long'.
    ; Later Note: (Kale 14 p. 11). Short vowels when followed by a
    ; conjunct consonant are said to be 'prosodially long' ; e.g. the
    ; 'a' in 'daND
    (setq v (vrddhi (elt v 0)))
   )
   ((and (member (elt v 0) '(i u Ri Li)) (< 1 (length c2)))
    (setq v (kale-395 c1 v c2 type))
   ; 3a. penultimate i u R^i L^i may be modified 
   ;   preceding a compound consonant beginning with "r" or "v"
   )
   ((and (= (length c2) 1)
      (shortsimplevowel-P (elt v 0))
    )
    ; 4. penultimate short vowel (other than A) takes guna
    ;  a short medial vowel is a medial vowel provided
    ;    1) it is a short simple vowel
    ;    2) the final consonant (c2) is not compound.
    ; Note that when the length of c2 is non-zero, then the
    ; type is either CVC or VC (it cannot be CV or V)
    (setq v (guna (elt v 0)))
   )
   ((equal v [RI]) 
    ; 4. Vowel (long) RI is modified (when it does not take guna or vrddhi)
    ;  
    (setq v (kale-394 c1 v c2 type))
   )
  )
  ; [a y] is added
  (vconcat c1 v c2 [a y])
 )
)
(defun class10-base (tok &optional Eng-def)
 ; Given the token representation 'tok' of a root,
 ; construct a list of token arrays, representing 
 ; the base(s) (ending in '[a y]') of the root as
 ; formed for the 10th conjugational class.
 ; This incorporates the information of Kale  400 p. 253,
 ; where the root vowel is unchanged.
 ; Eng-def is assumed to be a list of definitions,
 ; each of which is itself a list of words
 (let (b1 b2 ans dhaatu def)
  (setq dhaatu (sym-without-space tok))
  (setq def (modify-Eng-def Eng-def))
  (cond 
   ((member dhaatu '(
     ; Kale 400 : these preserve the root unchanged
     agh ; to sin
     kath ; to tell
     kShap ; to send , to pass
     gaN ; to count
     ; NOTE : 'gal' ; U to filter , A to throw appears in
     ; Kale 'dhaautkosha' as 10A, and lengthens 'a': gaalayate
     ; Thus, I exclude it here
    ; gal 
     var ; choose , seek
     dhvan ; sound
     mah ; to honor
     rach ; to compose
     ras ; taste
     rah ; forsake
     ; 'raT' (shout) appears only as class 1 in dhaatukosha of Kale
     pat ; go
     stan ; thunder
     svar ; blame
     pad ; go
     vaT ; separate
     karN ; bore , pierce : This is prosodially long. so 'a' normally short
     ; Chad ; conceal (This shows as lengthening 'a' - Why here?
     chap ; grind , cheat
     shrath ; be-weak
     ; shlath ; be-weak (not in dhaatukosha or Apte dictionary)
     vyay ; spend , give
     spRih ; desire
     mRig ; hunt
     ; mRiSh ; bear ; 'marShayati - e' in dhaatukosha and Apte dictionary
     guN ; invade , advise , multiply
     ; kuuN  ; speak , converse ('kuuNay' is REGULAR)
     ; grah ; take (not in dhaatukosha or Apte dictionary)
     kuh ; astonish
     sphuT ; break-open (Kale dhaatukosha shows 'sphoTayati')?
     sukh ; make-happy
    ))
    (setq ans (vconcat tok [a y]))
   )
   ; those (few) roots whose vowel is unchanged when
   ; a certain definition is used.
   ; 
   ((member dhaatu '(
    ; these have two forms, one with the root vowel unchanged,
    ; and one with the root vowel lengthened
     shaTh ; 'shaThay' speak ill - 'shaaThay' leave unfinished - ashiishaThata
     paT ; 'paTay' weave - 'paaTay' tear - apapaTas apapaTata
     kal ; 'kalay' count - 'kaalay' throw - achakalat achakalata
     laj ; 'lajay' shine - 'laajay' conceal - alalajat aliilajat
     vas ; 'vasay' dwell - 'vaasay' love , cut - aviivasat 
     puT ; 'puTay' bind - 'poTay' speak , shine - apupuTat apuupuTat
    ))
    (setq b1 (vconcat tok [a y]))
    (setq b2 (dhaatu-a~Nga-10 tok))
    (cond
     ((and (equal dhaatu 'shaTh) (member 'speak def)) (setq ans b1))
     ((and (equal dhaatu 'shaTh) (member 'leave def)) (setq ans b2))

     ((and (equal dhaatu 'paT) (member 'weave def)) (setq ans b1))
     ((and (equal dhaatu 'paT) (member 'tear def)) (setq ans b2))

     ((and (equal dhaatu 'kal) (member 'count def)) (setq ans b1))
     ((and (equal dhaatu 'kal) (member 'throw def)) (setq ans b2))

     ((and (equal dhaatu 'laj) (member 'shine def)) (setq ans b1))
     ((and (equal dhaatu 'laj) (member 'conceal def)) (setq ans b2))

     ((and (equal dhaatu 'vas) (member 'dwell def)) (setq ans b1))
     ((and (equal dhaatu 'vas) (member 'love def)) (setq ans b2))

     ((and (equal dhaatu 'puT) (member 'bind def)) (setq ans b1))
     ((and (equal dhaatu 'puT) (member 'speak def)) (setq ans b2))
     (t
      ; if 'def' is not given as above, return both forms
      (setq ans (list b1 b2))
     )
    )
   )
   ((equal dhaatu 'aMs)
    (setq b1 (dhaatu-a~Nga-10 tok))
    (setq b2 [a M s aa p a y])
    (setq ans (list b1 b2))
   )
   ((equal dhaatu 'kRip)
    (setq b1 (dhaatu-a~Nga-10 tok))
    (setq b2 [k Ri p aa y])
    (setq ans (list b1 b2))
   )
   (t
    (setq ans (dhaatu-a~Nga-10 tok)) ; the regular formation
   )
  )
  (if (not (listp ans)) (setq ans (list ans)))
  ans
 )
)

(defun class-a-base (dhaatu class pada)
 ;dhaatu is a symbol
 ;class is a number (conjugation class 1, 4, 6, or 10)
 ;pada is a symbol ('P or 'A)
 ;returns base as a (list of) symbol arrays
 (let (base dhaatu-tokar dhaatu-string)
  (setq dhaatu-string (symbol-name dhaatu))
  (setq dhaatu-tokar (car (ITRANS-parse-words-1 dhaatu-string)))
  (setq base
   (cond
    ((class-a-base-irreg dhaatu class pada))
    ((= class 1) (dhaatu-a~Nga-1 dhaatu-tokar))
    ((= class 4) (dhaatu-a~Nga-4 dhaatu-tokar))
    ((= class 6) (dhaatu-a~Nga-6 dhaatu-tokar))
    ((= class 10) (class10-base dhaatu-tokar))
    
    (t 
     nil
    )
   )
  )
  (if (not (listp base)) (setq base (list base)))
  base
 )
)
(defun class-a-base-irreg (dhaatu class pada)
 (let (ans)
  (cond
   ((= class 1)
    (cond 
     ((equal dhaatu 'ag) (setq ans 'a~Ng)) ; 1 P ag
     ((equal dhaatu 'Ri) (setq ans 'RichCh)) ; 1 P ar
     ((equal dhaatu 'Rit) (setq ans 'Ritiiy)) ; 1 A art
     ((equal dhaatu 'kam) (setq ans 'kaamay)) ; 1 A kam
     ((equal dhaatu 'kasj) (setq ans 'kajj)) ; 1 A kasj
     ((equal dhaatu 'kit) (setq ans 'chikits)) ; 1 P ket
     ((equal dhaatu 'kram) (setq ans 'kraam)) ; 1 P kram
     ((equal dhaatu 'klam) (setq ans 'klaam)) ; 1 P klam
     ; 05-11-04. 'gam' can have a normal base, as well as 'gachCh'
     ((equal dhaatu 'gam) (setq ans '(gachCh gam)))
;     ((equal dhaatu 'gam) (setq ans 'gachCh))
     ((and (equal dhaatu 'gup) (equal pada 'P)) (setq ans 'gopaay)) ; 1 P gop
     ((and (equal dhaatu 'gup) (equal pada 'A)) (setq ans 'jugups)) ; 1 A gop
     ((equal dhaatu 'guh) (setq ans 'guuh)) ; 1 U goh
     ((equal dhaatu 'ghraa) (setq ans 'jighr)) ; 1 P ghra
     ((equal dhaatu 'cham) (setq ans 'chaam)) ; 1 P cham
     ((equal dhaatu 'jabh) (setq ans 'jambh)) ; 1 A jabh
     ; 01-09-05. 'titikSh' is desiderative form.
;     ((equal dhaatu 'tij) (setq ans 'titikSh)) ; 1 A tej
     ((equal dhaatu 'daMsh) (setq ans 'dash)) ; 1 P daMsh
     ((equal dhaatu 'daa) (setq ans 'yachCh)) ; 1 P da
     ((equal dhaatu 'dRish) (setq ans 'pashy)) ; 1 P darsh
     ((equal dhaatu 'dhuup) (setq ans 'dhuupaay)) ; 1 P dhuup
     ((equal dhaatu 'dhmaa) (setq ans 'dham)) ; 1 P dhma
     ((equal dhaatu 'paN) (setq ans 'paNaay)) ; 1 P paN
     ((equal dhaatu 'paa) (setq ans 'pib)) ; 1 P pa
     ((equal dhaatu 'much) (setq ans 'mu~nch)) ; 1 A moch
     ((equal dhaatu 'murCh) (setq ans 'muurchCh)) ; 1 P muurCh
     ((equal dhaatu 'mRij) (setq ans 'maarj)) ; 1 P marj
     ((equal dhaatu 'mnaa) (setq ans 'man)) ; 1 P mna
     ((equal dhaatu 'yam) (setq ans 'yachCh)) ; 1 P yam
     ((equal dhaatu 'ra~nj) (setq ans 'raj)) ; 1 U ra~nj
     ((equal dhaatu 'lasj) (setq ans 'lajj)) ; 1 A lasj
     ((equal dhaatu 'shad) (setq ans 'shiiy)) ; 1 A shad
     ((equal dhaatu 'ShThiv) (setq ans 'ShThiiv)) ; 1 P ShThev
     ((equal dhaatu 'sad) (setq ans 'siid)) ; 1 P sad
     ((equal dhaatu 'sa~nj) (setq ans 'saj)) ; 1 P sa~nj
     ((equal dhaatu 'sad) (setq ans 'siid)) ; 1 P sad
     ((equal dhaatu 'sasj) (setq ans 'sajj)) ; 1 P sasj
     ((equal dhaatu 'sRi) (setq ans '(sar dhaav))) ; 1 P sar
     ((equal dhaatu 'sthaa) (setq ans 'tiShTh))
     ((equal dhaatu 'sva~nj) (setq ans 'svaj)) ; 1 A sva~nj
    )
   )
   ((= class 4) 
    (cond
     ((equal dhaatu 'kram) (setq ans 'kraamy)) ; 4 P kramy
     ((equal dhaatu 'klam) (setq ans 'klaamy)) ; 4 P klamy
     ((equal dhaatu 'kSham) (setq ans 'kShaamy)) ; 4 P kShamy
     ((equal dhaatu 'jan) (setq ans 'jaay)) ; 4 A jany
     ((equal dhaatu 'tam) (setq ans 'taamy)) ; 4 P tamy
     ((equal dhaatu 'dam) (setq ans 'daamy)) ; 4 P damy
     ((equal dhaatu 'do) (setq ans 'dy)) ; 4 P doy
     ((equal dhaatu 'bhraMsh) (setq ans 'bhrashy)) ; 4 P bhraMshy
     ((equal dhaatu 'bhram) (setq ans 'bhraamy)) ; 4 P bhramy
     ((equal dhaatu 'mad) (setq ans 'maady)) ; 4 P mady
     ((equal dhaatu 'mid) (setq ans 'medy)) ; 4 P midy
     ((equal dhaatu 'ra~nj) (setq ans 'rajy)) ; 4 U ra~njy
     ((equal dhaatu 'vyadh) (setq ans 'vidhy)) ; 4 P vyadhy
     ((equal dhaatu 'sham) (setq ans 'shaamy)) ; 4 P shamy
     ((equal dhaatu 'sho) (setq ans 'shy)) ; 4 P shoy
     ((equal dhaatu 'shram) (setq ans 'shraamy)) ; 4 P shramy
     ((equal dhaatu 'so) (setq ans 'sy)) ; 4 P soy
    )
   )
   ((= class 6) 
    (cond
     ((equal dhaatu 'iSh) (setq ans 'ichCh)) ; 6 P iSh
     ((equal dhaatu 'kRit) (setq ans 'kRint)) ; 6 P kRit
     ((equal dhaatu 'khid) (setq ans 'khind)) ; 6 P khid
     ((equal dhaatu 'gRI) (setq ans '(gir gil))) ; 6 P gir
     ((equal dhaatu 'chRit) (setq ans 'chRint)) ; 6 P chRit
     ((equal dhaatu 'dhuu) (setq ans 'dhu)) ; 6 P dhuuv
     ((equal dhaatu 'pish) (setq ans 'piMsh)) ; 6 P pish
     ((equal dhaatu 'prachCh) (setq ans 'pRichCh)) ; 6 P prachCh
     ((equal dhaatu 'bhrasj) (setq ans 'bhRijj)) ; 6 U bhrasj
     ((equal dhaatu 'masj) (setq ans 'majj)) ; 6 P masj
     ((equal dhaatu 'much) (setq ans 'mu~nch)) ; 6 U much
     ((equal dhaatu 'lip) (setq ans 'limp)) ; 6 U lip
     ((equal dhaatu 'lup) (setq ans 'lump)) ; 6 U lup
     ((equal dhaatu 'vichCh) (setq ans '(vichCh vichChaay))) ; 6 P vichCh
     ((equal dhaatu 'vyach) (setq ans 'vich)) ; 6 P vyach
     ((equal dhaatu 'vrashch) (setq ans 'vRishch)) ; 6 P vrashch
     ((equal dhaatu 'sad) (setq ans 'siid)) ; 6 P sad
     ((equal dhaatu 'sasj) (setq ans 'sajj)) ; 6 P sasj
     ((equal dhaatu 'sich) (setq ans 'si~nch)) ; 6 U sich
     ((equal dhaatu 'vid) (setq ans 'vind)) ; 6 U vid
     ((equal dhaatu 'suu) (setq ans 'su)) ; 6 P suuv
    )
   )
   ((= class 10)
    ; According to Kale Dhaatukosha, these class 10 verbs have
    ; two forms. Sometimes, the given forms are associated with
    ; different meanings (e.g. 'vas') and sometimes not (e.g. 'aMs')
    ;
    (cond
     ((equal dhaatu 'aMsh) (setq ans '(aMshaapay aMshay))) ; 10 U
     ((equal dhaatu 'aMs) (setq ans '(aMsay aMsaapay))) ; 10 U
     ((equal dhaatu 'kal) (setq ans '(kalay kaalay))) ; 10 U
     ((equal dhaatu 'kRip) (setq ans '(kRipay kRipaay))) ;10 P 
     ((equal dhaatu 'kuN) (setq ans '(kuNay koNay))) ; 10 P 
     ((equal dhaatu 'gad) (setq ans '(gaday gaaday))) ; 10 U 
     ((equal dhaatu 'dhuu) (setq ans '(dhuunay dhaavay))) ; 10 U 
     ((equal dhaatu 'paT) (setq ans '(paTay paaTay))) ; 10 U 
     ((equal dhaatu 'paN) (setq ans '(paNaay paaNay))) ; 10 U 
     ((equal dhaatu 'pan) (setq ans '(panaay paanay))) ; 10 U 
     ((equal dhaatu 'puT) (setq ans '(puTay poTay))) ; 10 U 
     ((equal dhaatu 'prii) (setq ans '(priiNay praayay))) ; 10 U 
     ((equal dhaatu 'laj) (setq ans '(lajay laajay))) ; 10 U 
     ((equal dhaatu 'vas) (setq ans '(vasay vaasay))) ; 10 U 
     ((equal dhaatu 'shaTh) (setq ans '(shaThay shaaThay))) ; 10 U 
     ((equal dhaatu 'dabh) (setq ans '(dambhay))) ; nasal insertion

;      ((equal dhaatu 'aMsh) (setq ans 'aMshaapay)) ; 10 U aMshay
;      ((equal dhaatu 'aMs) (setq ans 'aMsaapay)) ; 10 U (aMsay aMsaapay)
;      ((equal dhaatu 'kal) (setq ans 'kalay)) ; 10 U (kalay kaalay)
;      ((equal dhaatu 'kRip) (setq ans '(kRipay kRipaay))) ;10 P (karpay kRipaay)
;      ((equal dhaatu 'kuN) (setq ans 'kuNay)) ; 10 P koNay
;      ((equal dhaatu 'gad) (setq ans 'gaday)) ; 10 U gaaday
;      ((equal dhaatu 'dhuu) (setq ans 'dhuunay)) ; 10 U dhaavay
;      ((equal dhaatu 'paT) (setq ans 'paTay)) ; 10 U (paTay paaTay)
;      ((equal dhaatu 'paN) (setq ans 'paNaay)) ; 10 U paaNay
;      ((equal dhaatu 'pan) (setq ans 'panaay)) ; 10 U paanay
;      ((equal dhaatu 'puT) (setq ans 'puTay)) ; 10 U (puTay poTay)
;      ((equal dhaatu 'prii) (setq ans 'priiNay)) ; 10 U praayay
;      ((equal dhaatu 'laj) (setq ans 'lajay)) ; 10 U (lajay laajay)
;      ((equal dhaatu 'vas) (setq ans 'vasay)) ; 10 U (vasay vaasay)
;      ((equal dhaatu 'shaTh) (setq ans 'shaThay)) ; 10 U (shaThay shaaThay)
;      ((equal dhaatu 'dabh) (setq ans 'dambhay))
    )
   )
   (t 
     nil
   )
  )
  (when ans
   (if (not (listp ans)) (setq ans (list ans)))
   (setq ans 
    (mapcar
     (lambda (x) (car (ITRANS-parse-words-1 (symbol-name x))))
     ans
    )
   )
  )
  ans
 )
)
(defun class-b-base (dhaatu class pada upasargas)
 ; return a list of token arrays
 (let (ans tok pc lc)
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0))
  (if (< 1 (length tok))
   (setq pc (elt (substring tok -2 -1) 0))
  )
  (cond
   ((equal class 5)
    (cond
     ((equal dhaatu 'shru) (setq tok [sh Ri])) ; Antoine2#16 p.11
    )
   )
   ((equal class 9)
    (cond
     ((and (nasal-P pc) (not (vowel-P lc)))
      ; Antoine2#27. Roots having a penultimate nasal drop it
      ; Note: this includes 'j~naa' of #29 
      (setq tok (vconcat (substring tok 0 -2) (substring tok -1)))
     )
     ((equal dhaatu 'jyaa)
      ; Antoine2#29
      (setq tok [j i]) 
     )
     ((equal dhaatu 'grah)
      ; Antoine2#29
      (setq tok [g Ri h]) 
     )
     ((equal dhaatu 'j~naa)
      ; Antoine#29
      (setq tok [j aa])
     )
     ((kale-414-P dhaatu)
       (setq lc (shorten-vowel lc))
       (setq tok (vconcat (substring tok 0 -1) (vector lc)))
     )
     ((kale-414-opt-P dhaatu)
       (let (tok1)
        (setq lc (shorten-vowel lc))
        (setq tok1 (vconcat (substring tok 0 -1) (vector lc)))
	(list tok tok1)
       )
     )
     ((and (longvowel-P lc)
	   (not (member dhaatu '(krii prii))) ; these do not short long vowel
      )
      (let ( ok)
       (setq ok '(lii dhuu puu luu dRI shRI stRI))
       (setq lc (shorten-vowel lc))
       (setq tok (vconcat (substring tok 0 -1) (vector lc)))
       (when (not (member dhaatu ok))
	(fol-msg (format "class-b-base (%s): %s should be checked\n"
			 class dhaatu))
       )
      )
     )
    )
   )
   ((equal class 8)
   )
  )
  (setq ans (list tok))
  ans
 )
)
(defun kale-414-P (dhaatu)
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
 (if (member dhaatu '(
       rii lii vlii plii dhuu puu luu RI kRI gRI jRI nRI dRI
       pRI bhRI mRI vRI shRI stRI)
     )
  t
 )
)
(defun kale-414-opt-P (dhaatu)
; Kale 414
; The following roots have their finals shortened optionally:
;  kShii bhrii vrii
 (if (member dhaatu '(kShii bhrii vrii))
  t
 )
)
(defun kale-394 (c1 v c2 type)
   ;  returns a possibly modified vowel
   ;   the following rule is due to Kale (sec. 394) and applies
   ;  to roots of conjugations 1,4,6,10. It applies when guna
   ;  (or vrddhi) has not applied
   ;  assume already tested that v = [RI] (long)
  ; Note: the condition Kale phrases as 
  ;   'a labial or ##va## precedes'
  ; is interpreted by the predicate labial-P, since I 
  ; include ##va## among the labials
  (cond
   ((string= type "CVC")
    (if (labial-P (elt c1 0)) 
     [uu r]
     [ii r]
    )
   )
   ((string= type "CV")
    (if (labial-P (elt c1 0)) 
     [u r]
     [i r]
    )
   )
   ((string= type "VC")
    [ii r]
   )
   ((string= type "V")
    [i r]
   )
   (t v)  ; this condition should never be reached!
  )
)
(defun kale-395 (c1 v c2 type)
 ; returns a possibly modified vowel
   ;   the following rule is due to Kale (sec. 395) and applies
   ;  to roots of conjugations 1,4,6,10. It applies when guna
   ;  (or vrddhi) has not applied
   ;  assume already tested that 
   ;   a. v = [v0] and v0 is i u R^i or L^i
; we first check that c2
;  a. begins with either "r" or "v", and
;  b. is a compound consonant

  (cond 
    ; when c2 is not present, v is unchanged
   ((= (length c2) 0) v)
    ;  in subsequent cases, 0 < (length c2)
    ;  when c2 does not start with "r" or "v", v is unchanged
   ((not (member (elt c2 0) '(r v))) v)
    ;  in subsequent cases, c2 begins with "r" or "v"
    ;  when c2 is a compound consonant, v is lengthened
   ((< 1 (length c2))
    (vector (lengthen-vowel (elt v 0))))
    ;  now, 1 = (length c2).
    ; otherwise, v is unchanged
   (t v)
  )
)

(defun conjugation-tab-pr (a~Nga class pada dhaatu)
 (mapcar
  (lambda (tense )
   (fol-msg (format "--- %s %s %s %s ---\n" a~Nga class pada tense))
   (citation-pr (conjugation-tab a~Nga tense class pada dhaatu))
  )
  all-special-tenses
 )
 t
)
(defun conjugation-tab-upa-pr (a~Nga class pada dhaatu upa)
 (mapcar
  (lambda (tense)
   (fol-msg (format "--- %s %s %s %s ---\n" a~Nga class pada tense))
   (citation-pr 
    (vconcat 
     (mapcar 
      (lambda (x) (conjugation-join-sym upa x))
      (conjugation-tab a~Nga tense class pada dhaatu)
     )
    )
   )
  )
  all-special-tenses
 )
 t
)
(defun conjugation-tab (a~Nga-sym tense-sym class pada dhaatu &optional voice)
 (if (member tense-sym all-special-tenses)
 ;1. present-system conjugations : depend on class of root
 (cond
  ((equal voice 'PASSIVE)
   ; a~Nga-sym assumed to have (a) passive base
   (conjugation-tab-1 a~Nga-sym tense-sym 4 'A)
  )
  ((member class '(1 4 6 10))
   (conjugation-tab-1 a~Nga-sym tense-sym class pada))
  ((equal class 5)
   (conjugation-tab-5 a~Nga-sym tense-sym class pada))
  ((equal class 8)
   (cond 
    ((equal dhaatu 'kRi)
     (conjugation-citation-irreg dhaatu tense-sym class pada)
    )
    (t
     (conjugation-tab-8 a~Nga-sym tense-sym class pada)
   ))
  )
  ((equal class 9)
   (conjugation-tab-9 a~Nga-sym tense-sym class pada))
  ((equal class 2)
   (cond 
    ((equal dhaatu 'as)
     (conjugation-citation-irreg dhaatu tense-sym class pada)
    )
    ((equal dhaatu 'vid) ; vid 2P (to know)
     (conjugation-citation-irreg dhaatu tense-sym class pada)
    )
    ((and (equal dhaatu 'bruu) (equal class 2) (equal pada 'P)
	  (equal tense-sym 'laT))
     (conjugation-citation-irreg dhaatu tense-sym class pada)
    )
    (t
     (conjugation-tab-2 a~Nga-sym tense-sym class pada dhaatu)
   ))
  )
  ((equal class 3)
   (conjugation-tab-3 a~Nga-sym tense-sym class pada dhaatu))
  ((equal class 7)
   (conjugation-tab-7 a~Nga-sym tense-sym class pada dhaatu))
  (t nil)
 )
 ;2. other tenses 
 (cond
  ((equal tense-sym 'liT) ; perfect tense
   (conjugation-tab-liT a~Nga-sym class pada dhaatu)
  )
  ((equal tense-sym 'luT) ; periphrastic future
   (conjugation-tab-luT a~Nga-sym class pada dhaatu)
  )
  ((equal tense-sym 'lRiT) ; simple future
   (conjugation-tab-lRiT a~Nga-sym class pada dhaatu)
  )
  (t nil)
 )
 )
)
(defun conjugation-tab-1 (a~Nga-sym tense-sym class pada)
 ; a~Nga is a token array
 (let (ending endings u a~Nga0 a~Nga1 w ans a~Nga i1 i2 n)
  (setq a~Nga (car (ITRANS-parse-words-1 (symbol-name a~Nga-sym))))
  ;--- construct array of endings
  
   (setq endings (conj-endings tense-sym class pada))
   (setq n (length endings))
   (setq i1 0)
   (setq i2 n)
   (when t
    ; perform certain adjustments (these could be stored)
    (let (i ending u y)
     (setq i i1)
     (while (< i i2)
      (setq ending (elt endings i))
      (if (equal ending [])
       (setq u [])
       (setq u (vector (elt ending 0)))
      )
      (setq y (cond
       ((equal u [m]) [aa])
       ((equal u [v]) [aa])
       ((equal u [a]) []) ; "a" dropped before "a"
       ((equal u [e]) []) ; "a" dropped before "e" (Antoine I.40)
       ((equal u []) [a]) ; e.g. imperative P 2 S
       (t [a]) ; the usual, default case for the A-conjugations
      ))
      (setq ending (conjugation-join y ending))
      (aset endings i ending)
      (setq i (1+ i))
     )
    )
   )
  
;  (fol-msg (format "endings(2) = %s\n" endings))
  ;--- for imperfect, the augment 'a' must be prefixed to a~Nga
  ; a~Nga0 is the result
  (if (not (equal tense-sym 'la~N))
   (setq a~Nga0 a~Nga)
   ; else prefix augment, which forms vrddhi with an initial vowel
   (setq u (elt a~Nga 0))
   (if (vowel-P u)
    (setq a~Nga0 (vconcat (vrddhi u) (substring a~Nga 1)))
    (setq a~Nga0 (vconcat [a] a~Nga))
   )
  )
  ; join the parts for each ending
  (let (i y thisans)
   (setq ans (make-vector n nil))
   (setq i i1)
   (while (< i i2)
    ; a~Nga1 is what joins to the ending
    (setq ending (elt endings i))
    (setq a~Nga1 a~Nga0)
    ; the joining may require sandhi joining "a" to initial vowel
    (setq thisans (conjugation-join a~Nga1 ending))
;   (setq thisans (solution thisans))
;   (setq thisans
;    (or (sandhi-single thisans) thisans))
    (setq thisans (sym-without-space thisans))
;    (fol-msg (format "%s: %s + %s = %s\n" i a~Nga1 ending thisans))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-5 (a~Nga-sym tense-sym class pada)
 (let (endings strengths ans n atok)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name a~Nga-sym))))
  ;--- 4. for imperfect, the augment 'a' must be prefixed to atok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt atok 0))
    (if (vowel-P u)
     (setq atok (vconcat (vrddhi u) (substring atok 1)))
     (setq atok (vconcat [a] atok))
    )
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a)
   (setq a (elt (substring atok -1) 0))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    (setq strength (elt strengths i)) ; S or W
    (setq base nil)
    (if (vowel-P a)
     ; (a) root ends with vowel
     (if (equal strength 'S)
      (setq base (vconcat atok [n o]))
      ; weak ending
      (progn 
       (cond
        ((vowel-P e) (setq base (vconcat atok [n v])))
        ((member e '(v m))
	 (setq base (list (vconcat atok [n u]) (vconcat atok [n]))))
        (t (setq base (vconcat atok [n u])))
       )
       (if (equal ending [h i]) ; loT P 2S
	(setq ending [])
       )
      )
     )
     ; (b) root ends with consonant
     (if (equal strength 'S)
      (setq base (vconcat atok [n o]))
      ; weak ending
      (progn 
       (cond
        ((vowel-P e) (setq base (vconcat atok [n u v])))
        ((member e '(v m))
	 (setq base (vconcat atok [n u])))
        (t (setq base (vconcat atok [n u])))
       )
      )
     )
    )
    (setq thisans (conjugation-join base ending))
    (setq thisans (sym-without-space thisans))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-8 (a~Nga-sym tense-sym class pada)
 (let (endings strengths ans n atok)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name a~Nga-sym))))
  ;--- 4. for imperfect, the augment 'a' must be prefixed to atok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt atok 0))
    (if (vowel-P u)
     (setq atok (vconcat (vrddhi u) (substring atok 1)))
     (setq atok (vconcat [a] atok))
    )
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a)
   (setq a (elt (substring atok -1) 0))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    (setq strength (elt strengths i)) ; S or W
    (setq base nil)
     (if (equal strength 'S)
      (setq base (vconcat atok [o]))
      ; weak ending
      (progn 
       (cond
        ((vowel-P e) (setq base (vconcat atok [v])))
        ((member e '(v m))
	 (setq base (list (vconcat atok [u]) (vconcat atok []))))
        (t (setq base (vconcat atok [u])))
       )
       (if (equal ending [h i]) ; loT P 2S
	(setq ending [])
       )
      )
     )
    (setq thisans (conjugation-join base ending))
;    (setq thisans (solution thisans))
    (setq thisans (sym-without-space thisans))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-9 (a~Nga-sym tense-sym class pada)
 (let (endings strengths ans n atok)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name a~Nga-sym))))
  ;--- 4. for imperfect, the augment 'a' must be prefixed to atok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt atok 0))
    (if (vowel-P u)
     (setq atok (vconcat (vrddhi u) (substring atok 1)))
     (setq atok (vconcat [a] atok))
    )
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a)
   (setq a (elt (substring atok -1) 0))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    (setq strength (elt strengths i)) ; S or W
    (setq base nil)
    (if (equal strength 'S)
     (setq base (vconcat atok [n aa]))
     ; weak ending
     (progn
      (if (vowel-P e)
       (setq base (vconcat atok [n]))
       (setq base (vconcat atok [n ii]))
      )
      ; imperative 2s P for roots ending in cons
      (when (and (equal ending [h i]) (consonant-P a))
       (setq base atok)
       (setq ending [aa n a])
      )
     )
    )
    (if (equal a~Nga-sym 'kShubh)
     (setq thisans (conjugation-join base ending 'not-n-N))
     (setq thisans (conjugation-join base ending))
    )
;    (setq thisans (solution thisans))
    (setq thisans (sym-without-space thisans))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-2 (a~Nga-sym tense-sym class pada dhaatu)
 ; a~Nga-sym is used for 'i' (as upasargas)
 (let (endings strengths ans n atok ibeg dhaatu-ending upasargas)
  (setq upasargas a~Nga-sym)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq dhaatu-ending (elt (substring atok -1) 0))
  (setq ibeg 0) ; position of 1st char
  ;--- 4. for imperfect, the augment 'a' must be prefixed to atok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt atok 0))
    (if (vowel-P u)
     (setq atok (vconcat (vrddhi u) (substring atok 1)))
     (progn
      (setq atok (vconcat [a] atok))
      (setq ibeg 1) ; change position of 1st char
     )
    )
;    (fol-msg (format "la~N chk: atok=%s\n" atok))
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a abeg atok0 a-save parts
	    s-base w-base  s-atok w-atok ending0 base0
	    thisans-tok da-save asp-abeg)
   ;#45.2 Final and short medial vowels take guna in the strong forms
   ; #48: Final 'aa' does not take guna (it remains 'aa' in all forms)
   (setq parts (word-parts atok)) ; used by rule#44
   (cond
    ((equal (elt (substring atok -1) 0) 'aa)
     (setq s-atok atok)
    )
    ((equal (substring (elt parts 1) -1) "V")
     (let (part1 p1 p2 v v0)
      (setq part1 (elt parts 0)) ; 
      (setq p1 (substring part1 0 -1)) ; all but last
      (setq p2 (substring part1 -1)) ; last
      (setq p2 (elt p2 0)) ; a 1-element array containing a vowel
      (setq v0 (elt p2 0)) ; the vowel
      (setq v (guna v0))
      (setq p2 (if (vectorp v) v (vector v)))
      (setq part1 (vconcat p1 p2))
      (setq s-atok (vconcat (flatten part1)))
     )
    )
    ((equal (substring (elt parts 1) -2) "VC")
     (let (part1 p1 p2 p3 v v0)
      (setq part1 (elt parts 0)) ; 
      (setq p1 (substring part1 0 -2)) ; all but last two
      (setq p2 (substring part1 -2 -1))
      (setq p2 (elt p2 0)) ; a 1-element array containing a vowel
      (setq v0 (elt p2 0)) ; the vowel
      (setq p3 (substring part1 -1)) ; 
      ; only gunate when v0 is a short simple vowel AND
      ; the final consonant is not compound
      (when (and (= (length (elt p3 0)) 1) 
	    (shortsimplevowel-P v0)) 
       (setq v (guna v0))
       (setq p2 (if (vectorp v) v (vector v)))
      )
      ;#59. mRij takes vrddhi (not guna) in strong form
      (when (equal dhaatu 'mRij)
       (setq v (vrddhi v0))
       (setq p2 (if (vectorp v) v (vector v)))
      )
      (setq part1 (vconcat p1 p2 p3))
      (setq s-atok (vconcat (flatten part1)))
      )
     )
    (t 
     (setq s-atok atok)
    )
   )
   (setq w-atok atok)
   (setq i 0)
   (while (< i n)
    (setq strength (elt strengths i)) ; S or W
    (if (equal strength 'S)
     (setq atok s-atok)
     (setq atok w-atok)
    )
    (setq atok0 (substring atok 0 -1)) ; all but last character
    (setq a-save (elt (substring atok -1) 0)) ; last character
    (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
    (setq abeg (elt atok ibeg)) ; 1st character
    (setq asp-abeg (aspirate abeg)) ; aspirated version of 1st char
    (setq a a-save)
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    (setq base (copy-sequence atok))
    (setq ending0 ending) (setq base0 atok)
;    (fol-msg (format "CHK: %s %s %s %s %s \n"
;		     i ending0 strength base atok))
    (cond
     ; #36. Before weak terminations beginning with a vowel, the
     ; final 'i' and 'u', short or long,
     ; are changed respectively to 'iy' and 'uv'
     
     ((and (equal strength 'W) (vowel-P e))
      (cond
       ((equal dhaatu 'i)
	; Kale 426, p. 270.
	; The 'i' of the root 'i' P is changed to 'y' before a
	; weak vowel termination. But, 'i' with 'adhi' ('A) is
	; regularly conjugated
	(if (and (not upasargas) (equal pada 'P))
         (setq base (vconcat (substring atok 0 -1) [aa y]))
	 (setq base (vconcat atok [y]))
	)
;	
       )
       ((member a '(i ii)) ; 
        (setq base (vconcat atok0 [i y])))
       ((member a '(u uu uv))
        (setq base (vconcat atok0 [u v])))
      )
     )
     ; no other considerations if termination is a vowel
     ((vowel-P e))
     ; #51. For verbs ending in 'u' (like 'nu'), the 'u' takes
     ; vrddhi before strong terminations beginning with a consonant
     ; (if it were regular, then the 'u' would be gunated.)
     ; Further processing is permitted, since this cond step always fails
     ((progn
       (when (and (equal dhaatu-ending 'u) (equal strength 'S))
	(setq a 'au)
	(setq base (vconcat atok0 [au]))
       )
       nil
      )
     )
     ; #37. Before terminations beginning with a nasal or a semi-vowel,
     ; (other than [h i])
     ; the consonant sandhi offers no special difficulty
     ((or (nasal-P e) (and (semivowel-P e) (not (equal ending [h i])))))
     ;
     ;--- Note: from here on, 'e' is a consonant but not a nasal or semivowel
     ;
     ;#40. Before terminations beginning with a consonant except a
     ;  nasal or a semi-vowel,
     ;  the final 'h' of a verbal base is changed to 'Dh'.
     ;  When, however, the verbal root begins with 'd', the final
     ;  'h' is changed to 'gh'
     ; we implement this in a cond step which fails
     ; so subsequent steps see this change
     ((progn
       (when (equal a 'h) 
        ; Rule 39 will now consider root to end with an aspirate
        (if (equal abeg 'd)
	 (setq a 'gh)
	 (setq a 'Dh)
        )
        (setq a-save a) ; last character
        (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
        (setq base (vconcat atok0 (vector a)))
	(setq atok base)
;	(fol-msg (format "#40. a-save, da-save changed: %s %s\n"
;			 a-save da-save))
       )
       nil
      )
     )
     ; #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
     ; the 2nd pers. singular imperative parasmaipada is changed
     ; to 'dhi' when the verbal base ends with a consonant
     ; except a nasal or a semi-vowel
     ; Note: This change is sometimes modified by #42. Thus,
     ; I implement in such a way as to change the ending, but
     ; have the condition fail, so further change possibilities
     ; will be examined
     ((progn
       (when (and (equal ending [h i]) (consonant-P a))
	(setq ending [dh i])
	(setq e 'dh)
       )
       nil
      )
     )
     ; #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
     ; imperfect parasmaipada are dropped after a verbal base ending
     ; with a consonant. The final consonant of the base is then treated
     ; according to the rule given in Part I-72(3).
     ; Note: 
     ((and (equal tense-sym 'la~N) (equal pada 'P) (consonant-P a)
	   (member ending '([t] [s]))) ;3S or 2S
      ;1. If 'a' is aspirate, deaspirate it
      ;  and if 'a' is a soft aspirate, after losing the aspiration,
      ;  throw it back if possible on previous syllable. This is like
      ;  39.1 and 2 below.
      (when (not (equal da-save a-save))
       (setq a da-save)
       (aset base ibeg asp-abeg)
      )
      ;2. case of 3S
      (when (equal ending [t])
       (setq ending [])
       (if (equal a 's)
        (setq base (vconcat atok0 [t]))
        (setq base (sandhi-legalise-final-cons base))
       )
      )
      ;3. case of 2S
      (when (equal ending [s])
       (setq ending [])
       (setq base (sandhi-legalise-final-cons base)) ; a list
       (cond
        ((equal a 'd)
	 (setq base (append-if-new base (vconcat atok [H]))))
        ((equal a 's)
	 (setq base (append-if-new base (vconcat atok0 [t]))))
       )
      )
      (setq base (solution base)) ; don't want a list unless necessary
     )
     ;#42. Before terminations beginning with 't' 'th' and 'dh',
     ; the final 'Dh' of a verbal root is dropped, while the following
     ; 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
     ; vowel is lengthened.
     ; NOTE: 'beginning' with does not mean 'equal'; i.e. this not applicable
     ; to 3S of imperfect
     ((and (equal a 'Dh) (member e '(t th dh)) (/= (length ending) 1))
      (setq e 'Dh)
      (setq ending (vconcat (vector e) (substring ending 1)))
      (let (v atok1)
       (setq v (elt (substring atok0 -1) 0)) ; preceding vowel
       (setq v (lengthen-vowel v))
       (if (< 1 (length atok0))
        (setq atok1 (substring atok0 0 -1)) ; all but last
	(setq atok1 [])
       )
       (setq base (vconcat atok1 (vector v)))
      )
     )
     ;#41. Before terminations beginning with 's' (i.e., before
     ; 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
     ((and (member a '(Dh Sh)) (equal e 's))
      (setq a 'k)
      (setq base (vconcat atok0 (vector a)))
     )
     ;#39.1 The final aspirate of a verbal base loses its aspiration
     ;#39.2 A soft aspirate, after losing its aspiration, throws it back,
     ;   if possible, on the previous syllable
     ;#39.3 The previous rule does not apply before the termination
     ;   'dhi' of the 2nd pers. sing. imperative parasmaipada
     ;   (Note: implemented indirectly by #34 above)?????
     ;#39.4 It does not apply either before terminations beginning with
     ;   't' or 'th', in which case the lost aspiration is thrown forward
     ;   on the following 't' or 'th' which are softened (to 'dh')
     ; NOTE: this IS applicable to 3S of imperfect whose ending = 't'
     ((not (equal da-save a-save)) ; so 'a' is an aspirate
      (setq a da-save) ; 'a' loses aspiration
;      (fol-msg (format "chk: ending0=%s\n" ending0))
      (cond
       ((and (equal i 5) (equal e 'dh)) ; 2P
	(aset base ibeg asp-abeg)
        (setq base (vconcat (substring base 0 -1) (vector a)))
       )
       ((member e '(t th dh))
        (setq e 'dh)
        (setq ending (vconcat (vector e) (substring ending 1)))
        (setq base (vconcat atok0 (vector a)))
       )
       (t
        (aset base ibeg asp-abeg)
        (setq base (vconcat (substring base 0 -1) (vector a)))
       )
      )
     )
     ;#43. The final 's' of a verbal base is dropped before
     ; soft dentals
     ((and (equal a 's) (soft-P e) (dental-P e))
      (setq base (substring base 0 -1))
     )
     ;#44. When a verbal root ends with a conjunct consonant having
     ; 'k' or 's' for its first member, it drops that 'k' and 's'
     ; before a termination beginning with a consonant except a
     ; nasal or a semi-vowel.
     ((let (part1 p2 p2a p2b)
      (setq part1 (elt parts 0)) ; an array of token arrays
      (setq p2 (elt (substring part1 -1) 0)) ; last element, a tok array
      (when (and (< 1 (length p2)) (member (elt p2 0) '(k s)))
       (setq p2b (substring p2 1)) ; all but initial letter
       (setq p2a (substring part1 0 -1)) ; all but last
       (setq p2 (vconcat p2a p2b))
       (setq base (vconcat (flatten p2)))
      )
     ))
    )
    (setq thisans-tok (conjugation-join base ending))
    (setq thisans (sym-without-space thisans-tok))
;    (fol-msg (format "%s + %s -> %s = %s\n" base ending thisans-tok thisans))
    ;
    ;----- irregularity overrides
    ;
    (cond
     ;#48. verbs ending in 'aa': 3P imperfect Parasmaipada ending
     ; optionally 'uH'
     ((and (equal i 2) (equal tense-sym 'la~N) (equal pada 'P)
	   (equal a 'aa) (not (equal dhaatu 'daridraa)))
      (let (thisans1)
       (setq thisans1 (vconcat atok0 [u H]))
       (setq thisans1 (sym-without-space thisans1))
       (setq thisans (list thisans thisans1))
      )
     )
     ;#49 'i' (to go) is regular except for the 3P present and imperfect
     ; parasmaipada which are 'yanti' and 'yantu' (rether than
     ; 'iyanti' and 'iyantu'
     ((and (equal dhaatu 'i) (equal i 2) (equal pada 'P)
	   (member tense-sym '(laT loT)))
      (setq thisans-tok (substring thisans-tok 1))
      (setq thisans (sym-without-space thisans-tok))
     )
     ;#51. The three verbs 'stu' (to praise), 'tu' (to grow), and
     ; 'ru' (to sound) are conjugated like 'nu', but they optionally
     ; insert 'ii' before all terminations beginning with a consonant
     ((and (member dhaatu '(stu tu ru)) (consonant-P e))
      (let (ending1 thisans1)
       (setq ending1 (conjugation-join [ii] ending))
       (if (equal strength 'S)
        (setq base (vconcat atok0 [o])) ; undo #51
	(setq base (vconcat atok0 [u v])) ; do #36
       )
       (setq thisans-tok (conjugation-join base ending1))
       (setq thisans1 (sym-without-space thisans-tok))
       (setq thisans (list thisans thisans1))
      )
     )
     ;#52. 'shii' (2A) (to lie down) takes guna all through. The 3P of
     ; present, imperfect, and imperative insert 'r' before ending.
     ((equal dhaatu 'shii)
      (setq base s-atok) ; gunated form
      (if (and (equal i 2) ;3P
		 (member tense-sym '(laT la~N loT)))
	(setq thisans-tok (conjugation-join base (vconcat [r] ending)))
	(setq thisans-tok (conjugation-join base ending))
      )
      (setq thisans (sym-without-space thisans-tok))
     )
     ;#53. 'bruu' (2P/2A) (to speak) inserts 'ii' before strong terminations
     ; beginning with a consonant
     ((and (equal dhaatu 'bruu) (equal strength 'S) (consonant-P e))
      (setq thisans-tok (conjugation-join base
			(conjugation-join [ii] ending)))
      (setq thisans (sym-without-space thisans-tok))
     )
     ;#55. 'ad' (2P) (to eat) is regular except in the 2nd and 3rd pers. sing.
     ; imperfect where it inserts 'a' before the terminations 's' and 't'.
     ; 3S = aadat, 2s = aadaH
     ((and (equal dhaatu 'ad) (equal tense-sym 'la~N)
	   (member i '(0 3))) ; 3S=0, 2S=1
      (setq ending (elt endings i))
      (setq base atok)
      (setq thisans-tok (conjugation-join base
			(conjugation-join [a] ending)))
      (setq thisans (sym-without-space thisans-tok))
     )
     ;#58. 'dviSh' (2P/A) (to hate) optionally takes the termination 'us' in
     ; 3P imperfect parasmaipada.
     ((and (equal dhaatu 'dviSh) (equal tense-sym 'la~N) (equal pada 'P)
	   (equal i 2))
      (let (thisans1)
       (setq thisans1 (vconcat base [u H]))
       (setq thisans1 (sym-without-space thisans1))
       (setq thisans (list thisans thisans1))
      )
     )
     ;#58a. In both Antoine and Kale (#431), the combination
     ; 'Sh' + 'dh' -> 'D' + 'Dh'.  Neither author mentions this as
     ; an irregularity, but I could find no sandhi rule to justify
     ; this transition.
     ; I code it to apply just to  'dviSh'
     ((and (equal dhaatu 'dviSh) (equal a 'Sh) (equal e 'dh))
      (setq thisans-tok (vconcat atok0 [D Dh] (substring ending 1)))
      (setq thisans (sym-without-space thisans-tok))
     )
     ;#59. 'mRij' (2P) takes vrddhi in its strong forms.
     ; It optionally takes vrddhi before weak terminations beginning with
     ; a vowel.
     ; Note 1: part of this logic appears at the top of
     ; the routine, where s-atok is determined.
     ; Note 2: Special rules of sandhi seem to apply, although
     ; neither Kale nor Antoine seem to consider them special
     ((equal dhaatu 'mRij)
      (setq base base0)
      (setq ending ending0)
      (cond
       ((or (equal ending [t]) (equal ending [s]))
       ; change last letter to 'T, drop ending
	(setq base (vconcat (substring base 0 -1) [T]))
	(setq thisans-tok base)
	(setq thisans (sym-without-space thisans-tok))
       )
       ((member e '(t th)) ; but ending /= [t]
	; change the last letter ('j') to 'Sh'
        (setq base (vconcat (substring base 0 -1) [Sh]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((equal e 's) ; but ending /= [s]
	; change last letter ('j') to 'k'
        (setq base (vconcat (substring base 0 -1) [k]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((equal ending [h i])
	; change last letter ('j') to 'D')
        (setq base (vconcat (substring base 0 -1) [D]))
        (setq thisans-tok (conjugation-join base [dh i]))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((and (equal strength 'W) (vowel-P e))
	(let (thisans1)
         (setq thisans1 (conjugation-join s-atok ending))
         (setq thisans1 (sym-without-space thisans1))
         (setq thisans (list (solution thisans) (solution thisans1)))
        )
       )
      )
     )
     ;#60. 'han' (to kill) (2P/A)
     ; (1) drops is 'n' before weak terminations beginning with
     ; any consonant except a nasal or semivowel
     ; (2) drops its 'a' and changes the 'h' to 'gh' before vowel endings.
     ; (3) has 'jahi' for imperative 2S
     ((equal dhaatu 'han)
      (cond
       ((equal ending0 [h i]) (setq thisans 'jahi))
       ((member ending0 '([t i] [t u]))
	; otherwise, get 'haMti, rather than 'hanti, etc.
	(setq thisans-tok (vconcat base0 ending0))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((and (equal strength 'W) (consonant-P e)
	     (not (semivowel-P e)) (not (nasal-P e)))
	(setq base (substring base 0 -1)) ; drop the 'n
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ; don't do the usual vowel change when
       ; (a) ending starts with 'aa'
       ; (b) ending = [a m]
       ((and (vowel-P e) (equal strength 'W))
	; drop [h a n], put back [gh n]
	(setq base (substring base 0 -3))
	(setq base (vconcat base [gh n]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
      )
     )
     ;#61. 'an' (to breathe), 'shvas' (to sigh), 'svap' (to sleep), and
     ; 'rud' (to weep)
     ; (a) insert 'i' before all terminations beginning with a consonant
     ;     except 'y'
     ; (b) They insert 'ii' or 'a' before the 's' and 't' of the 2nd
     ;     and 3rd pers. sing. imperfect
     ; Note: Kale includes 'jakSh' (to eat) in this category.
     ;'jakSh' (to eat) also has this change; in addition, 
     ;  (a) 3rd pl. laT and loT drop the 'n' (ending = 'ati' or 'atu')
     ;  (b) 3rd pl. la~N has ending 'uH'
     ((member dhaatu '(an shvas svap rud jakSh))
      (setq base base0)
      (setq ending ending0)
;      (fol-msg (format "%s %s\n" base ending))
      (setq e (elt ending 0))
      (cond
       ((equal e 'y))
       ((member ending '([s] [t]))
	(let (thisans1 thisans2)
	 (setq thisans-tok (conjugation-join base (vconcat [ii] ending)))
	 (setq thisans1 (sym-without-space thisans-tok))
	 (setq thisans-tok (conjugation-join base (vconcat [a] ending)))
	 (setq thisans2 (sym-without-space thisans-tok))
	 (setq thisans (list thisans1 thisans2))
	)
       )
       ((consonant-P e)
	(setq thisans-tok (conjugation-join base
					    (conjugation-join [i] ending)))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((and (equal dhaatu 'jakSh) (= i 2))
	(cond
	 ((equal tense-sym 'laT)
	  (setq ending [a t i])
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 ((equal tense-sym 'loT)
	  (setq ending [a t u])
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 ((equal tense-sym 'la~N)
	  (setq ending [u H])
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	)
       )
      )
     )
     ;#62. Five verbs belong to the second conjugation have some
     ; characteristics of the 3rd conjugation: All drop the
     ; 'n' of the 3rd pers pl. laT and loT, and use 'uH'
     ; for the 3rd pler pl. of la~N.
     ; Each has some other peculiarities
     ; (a) chakaas (to shine)
     ((member dhaatu '(chakaas jaagRi daridraa shaas))
      (when (= i 2) ; 3rd pl.
       (cond
	((equal tense-sym 'laT)
	 (setq ending [a t i])
	 (setq thisans-tok (conjugation-join base ending))
	 (setq thisans (sym-without-space thisans-tok))
	)
	((equal tense-sym 'loT)
	 (setq ending [a t u])
	 (setq thisans-tok (conjugation-join base ending))
	 (setq thisans (sym-without-space thisans-tok))
	)
	((equal tense-sym 'la~N)
	 (setq ending [u H])
	 (setq thisans-tok (conjugation-join base ending))
	 (setq thisans (sym-without-space thisans-tok))
	)
       )
      )
      (cond
       ((equal dhaatu 'chakaas)
	(cond
	 ((equal ending [dh i])
	  ; Kale has an optional form where ending 's' becomes 'd'
	  (let (thisans1)
           (setq base (vconcat atok0 [d]))
	   (setq thisans-tok (conjugation-join base ending))
           (setq thisans1 (sym-without-space thisans-tok))
           (setq thisans (list thisans thisans1))
          )
	 )
	)
       )
       ((equal dhaatu 'jaagRi)
	; in la~N 3P, gunate before adding ending [u H]
	(cond
	 ((and (= i 2) (equal tense-sym 'la~N))
	  (setq ending [u H])
	  (setq base (vconcat atok0 (guna a)))
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 ((member ending '([t i] [t u]))
	  ; when conjugation-join is used, the results are (AntoineI-54)
	  ; jaagar + ti = jaagasti (Kale has jaagarti) and
	  ; jaagar + tu = jaagastu (Kale has jaagartu)
	  (setq thisans-tok (vconcat base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	)
       )
       ((equal dhaatu 'daridraa)
	; drops final 'aa' before weak terminations beginning with a vowel
	; changes final 'aa' to 'i' before weak terminates beginning with cons.
;        (when (= i 2) (fol-msg (format "%s %s\n" atok0 ending0)))
	(cond
	 ((equal strength 'S)) ; no change
	 ((consonant-P e)
	  (setq base (vconcat atok0 [i]))
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 ((equal ending0 [a n])
	  ; use ending [u H], drop [aa]
	  (setq base atok0)
	  (setq ending [u H])
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 ((vowel-P e)
	  (setq base atok0)
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	)
       )
       ((equal dhaatu 'shaas)
	; (a) changes 'aa' to 'i' before weak terminations beginning with a
	;  consonant. Thus sh i s, which becomes sh i Sh
	; (b) 2nd pers. sing. imperative is 'shaadhi' (already taken care
	;   of by #43)
	(cond
	 ((equal ending0 [h i])); 2nd pers. sing. imperative
	 ((and (equal strength 'W) (consonant-P e))
	  (setq base (vconcat (substring atok0 0 -1) [i Sh]))
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	)
       )
      )
     )
     ; Kale #434. 'vach' (to speak) is deficient in 3rd pers pl.
     ; I fail this step (thus to pass word on) to further
     ; processing.
     ((progn
       (when (equal dhaatu 'vach)
        (when (and (equal tense-sym 'laT) (= i 2))
         (setq thisans nil)
        )
        nil ; so cond fails
       )
      )
     )
     ; Note:  Although not mentioned by Kale or Antoine, some special
     ; sandhi rules are required for 'vach'. I presume these
     ; are also applicable to other roots ending in 'ch'.
     ; Namely, before consonants (other than semivowels) the 'ch' of
     ; 'vach' is changed to 'k', then other (usual) sandhi changes are
     ; applied.
     ; Note: I apply these to 'pRich' (the only other conj-2 verb in
     ; Antoine that ends in
     ((equal a 'ch)
      (cond
       ((equal e 'm)
	; note (conjugation-join [v a ch] [m i]) = [v a g m i].
	; acc. to Kale, the correct answer is [v a ch m i]
        (setq thisans-tok (vconcat base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((and (consonant-P e) (not (semivowel-P e)))
	(setq base (vconcat atok0 [k]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
      )
     )          
    )
    (if (listp thisans) (setq thisans (flatten thisans)))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-3 (a~Nga-sym tense-sym class pada dhaatu)
 ; a~Nga-sym is unused. 
 ; Antoine#69.
 ; The verbal base is formed by reduplication
 (let (endings strengths ans n atok ibeg dhaatu-ending rtok dtok)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq dtok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq rtok (reduplicative-pfx dtok))
  ;#76. 'nij' (to cleanse) and 'vij' (to separate)
  ; take guna in reduplication
  (when (member dhaatu '(nij vij))
   (setq rtok (gunate-final-vowel rtok))
  )
  (setq dhaatu-ending (elt (substring dtok -1) 0))
  (setq ibeg 0) ; position of 1st char
  ;--- 4. for imperfect, the augment 'a' must be prefixed to rtok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt rtok 0))
    (if (vowel-P u)
     (setq rtok (vconcat (vrddhi u) (substring rtok 1)))
     (progn
      (setq rtok (vconcat [a] rtok))
      (setq ibeg 1) ; change position of 1st char
     )
    )
;    (fol-msg (format "la~N chk: rtok=%s\n" rtok))
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a abeg atok0 a-save parts
	    s-base w-base  s-atok w-atok ending0 base0
	    thisans-tok da-save asp-abeg)
   ;#69.3 In parasmaipada, in 3P of laT and loT, the 'n' is dropped.
   (when (equal pada 'P)
    (if (equal tense-sym 'laT)
     (aset endings 2 [a t i]))
    (if (equal tense-sym 'loT)
     (aset endings 2 [a t u]))
   )
   ;#69.2 Final and short medial vowels take guna in the strong forms   
   (setq s-atok (gunate-final-vowel dtok))
   (setq s-atok (vconcat rtok s-atok))
   (setq w-atok (vconcat rtok dtok))
   ;#72. 'daa' (to give) and 'dhaa' (to put) form their weak bases
   ; in 'dad' and 'dadh' (i.e., the 'aa' is dropped)
   ; the strong bases keep 'aa' (rather than gunate to 'a')
   (when (member dhaatu '(daa dhaa))
    (setq w-atok (substring w-atok 0 -1))
    (setq s-atok (vconcat (substring s-atok 0 -1) [aa]))
   )
   ;#74. 'haa' (3A) (to depart) has (strong and weak) base
   ; 'jihii', but the 'ii' is dropped before terminations beginning
   ; with a vowel.
   (when (and (equal dhaatu 'haa) (equal pada 'A))
    (if (equal tense-sym 'la~N)
     (setq w-atok [a j i h ii])
     (setq w-atok [j i h ii])
    )
    (setq s-atok w-atok)
   )
   ;#74. 'maa' (3A) (to measure) has (strong and weak) base
   ; 'mimii', but the 'ii' is dropped before terminations beginning
   ; with a vowel.
   (when (and (equal dhaatu 'maa) (equal pada 'A))
    (if (equal tense-sym 'la~N)
     (setq w-atok [a m i m ii])
     (setq w-atok [m i m ii])
    )
    (setq s-atok w-atok)
   )
   ;#75. 'haa' (3P) (abandon) has strong base 'jahaa' and weak base
   ; either 'jahii' or 'jahi'.  Before weak terminations beginning
   ; with a vowel or with 'y', the base is 'jah'
   ; The weak stem situation is straightened out in the irregulities section
   (when (and (equal dhaatu 'haa) (equal pada 'P))
    (if (equal tense-sym 'la~N)
     (setq s-atok [a j a h aa])
     (setq s-atok [j a h aa])
    )
    (setq w-atok (vconcat (substring s-atok 0 -1) [ii]))
   )
   (setq atok w-atok)
;  (fol-msg (format "w-atok = %s, s-atok = %s\n" w-atok s-atok))
;   (setq s-parts (word-parts s-atok))
;   (setq w-parts (word-parts w-atok))
   (setq parts (word-parts
     (car (ITRANS-parse-words-1 (symbol-name dhaatu)))))
   (setq i 0)
   (while (< i n)
    (setq strength (elt strengths i)) ; S or W
    (if (equal strength 'S)
     (setq atok s-atok)
     (setq atok w-atok)
    )
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    ;#77. Final 'RI after labial (or 'v) becomes 'uur (in weak stem)
    ;Kale#394. This change to 'uur occurs when the ending starts
    ; with a consonant.  When the ending starts with a vowel,
    ; 'RI after labial (or 'v) becomes 'ur
    ; However, the imperfect 3P follows the special rule of 69.4
    (when (and (equal strength 'W) (< 1 (length dtok))
	       (equal (elt dtok 1) 'RI)
	       (not (and (= i 2) (equal pada 'P) (equal tense-sym 'la~N))))
     (let (a0)
      (setq a0 (elt dtok 0)) ; 1st letter of root
      (when (or (labial-P a0) (equal a0 'v))
       (if (consonant-P e)
        (setq atok (vconcat (substring w-atok 0 -1) [uu r]))
        (setq atok (vconcat (substring w-atok 0 -1) [u r]))
       )
      )
     )
    )
    (setq atok0 (substring atok 0 -1)) ; all but last character
    (setq a-save (elt (substring atok -1) 0)) ; last character
    (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
    (setq abeg (elt atok ibeg)) ; 1st character
    (setq asp-abeg (aspirate abeg)) ; aspirated version of 1st char
    (setq a a-save)
    (setq base (copy-sequence atok))
    (setq ending0 ending) (setq base0 atok)
    (cond
     ; #69.4.  The 3rd pers plur. imperf. parasmaipada takes the
     ; termination 'us' before which
     ; (a) a final 'aa' is dropped
     ; (b) a final 'i' 'u' or 'Ri' (short or long) takes guna
     ((and (= i 2) (equal pada 'P) (equal tense-sym 'la~N))
      (setq ending [u s])
      (cond
       ((equal a 'aa) (setq base atok0))
       ((member a '(i u Ri ii uu RI))
	(setq a (guna a)) ; now a vector
	(setq base (vconcat atok0 a))
       )
      )
     )
     ; no other considerations if termination is a vowel
     ((vowel-P e))
     ; #37. Before terminations beginning with a nasal or a semi-vowel,
     ; (other than [h i])
     ; the consonant sandhi offers no special difficulty
     ((or (nasal-P e) (and (semivowel-P e) (not (equal ending [h i])))))
     ;
     ;--- Note: from here on, 'e' is a consonant but not a nasal or semivowel
     ;
     ;#40. Before terminations beginning with a consonant except a
     ;  nasal or a semi-vowel,
     ;  the final 'h' of a verbal base is changed to 'Dh'.
     ;  When, however, the verbal root begins with 'd', the final
     ;  'h' is changed to 'gh'
     ; we implement this in a cond step which fails
     ; so subsequent steps see this change
     ((progn
       (when (equal a 'h) 
        ; Rule 39 will now consider root to end with an aspirate
        (if (equal abeg 'd)
	 (setq a 'gh)
	 (setq a 'Dh)
        )
        (setq a-save a) ; last character
        (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
        (setq base (vconcat atok0 (vector a)))
	(setq atok base)
;	(fol-msg (format "#40. a-save, da-save changed: %s %s\n"
;			 a-save da-save))
       )
       nil
      )
     )
     ; #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
     ; the 2nd pers. singular imperative parasmaipada is changed
     ; to 'dhi' when the verbal base ends with a consonant
     ; except a nasal or a semi-vowel
     ; Note: This change is sometimes modified by #42. Thus,
     ; I implement in such a way as to change the ending, but
     ; have the condition fail, so further change possibilities
     ; will be examined
     ((progn
       (when (and (equal ending [h i]) (consonant-P a)
	      (not (nasal-P a)) (not (semivowel-P a)))
	(setq ending [dh i])
	(setq e 'dh)
       )
       nil
      )
     )
     ; #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
     ; imperfect parasmaipada are dropped after a verbal base ending
     ; with a consonant. The final consonant of the base is then treated
     ; according to the rule given in Part I-72(3).
     ; Note: 
     ((and (equal tense-sym 'la~N) (equal pada 'P) (consonant-P a)
	   (member ending '([t] [s]))) ;3S or 2S
      ;1. If 'a' is aspirate, deaspirate it
      ;  and if 'a' is a soft aspirate, after losing the aspiration,
      ;  throw it back if possible on previous syllable. This is like
      ;  39.1 and 2 below.
      (when (not (equal da-save a-save))
       (setq a da-save)
       (aset base ibeg asp-abeg)
      )
      ;2. case of 3S
      (when (equal ending [t])
       (setq ending [])
       (if (equal a 's)
        (setq base (vconcat atok0 [t]))
        (setq base (sandhi-legalise-final-cons base))
       )
      )
      ;3. case of 2S
      (when (equal ending [s])
       (setq ending [])
       (setq base (sandhi-legalise-final-cons base)) ; a list
       (cond
        ((equal a 'd)
	 (setq base (append-if-new base (vconcat atok [H]))))
        ((equal a 's)
	 (setq base (append-if-new base (vconcat atok0 [t]))))
       )
      )
      (setq base (solution base)) ; don't want a list unless necessary
     )
     ;#42. Before terminations beginning with 't' 'th' and 'dh',
     ; the final 'Dh' of a verbal root is dropped, while the following
     ; 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
     ; vowel is lengthened.
     ; NOTE: 'beginning' with does not mean 'equal'; 
     ; i.e. this not applicable to 3S of imperfect
     ((and (equal a 'Dh) (member e '(t th dh)) (/= (length ending) 1))
      (setq e 'Dh)
      (setq ending (vconcat (vector e) (substring ending 1)))
      (let (v atok1)
       (setq v (elt (substring atok0 -1) 0)) ; preceding vowel
       (setq v (lengthen-vowel v))
       (if (< 1 (length atok0))
        (setq atok1 (substring atok0 0 -1)) ; all but last
	(setq atok1 [])
       )
       (setq base (vconcat atok1 (vector v)))
      )
     )
     ;#41. Before terminations beginning with 's' (i.e., before
     ; 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
     ((and (member a '(Dh Sh)) (equal e 's))
      (setq a 'k)
      (setq base (vconcat atok0 (vector a)))
     )
     ;#39.1 The final aspirate of a verbal base loses its aspiration
     ;#39.2 A soft aspirate, after losing its aspiration, throws it back,
     ;   if possible, on the previous syllable
     ;#39.3 The previous rule does not apply before the termination
     ;   'dhi' of the 2nd pers. sing. imperative parasmaipada
     ;   (Note: implemented indirectly by #34 above)
     ;#39.4 It does not apply either before terminations beginning with
     ;   't' or 'th', in which case the lost aspiration is thrown forward
     ;   on the following 't' or 'th' which are softened (to 'dh')
     ; NOTE: this IS applicable to 3S of imperfect whose ending = 't'
     ; NOTE: for 'dhaa', #39.2 is used rather than #39.4
     ((not (equal da-save a-save)) ; so 'a' is an aspirate
      (setq a da-save) ; 'a' loses aspiration
      (if (and (member e '(t th)) (not (equal dhaatu 'dhaa)))
       (progn
        (setq e 'dh)
        (setq ending (vconcat (vector e) (substring ending 1)))
        (setq base (vconcat atok0 (vector a)))
       )
       (progn
        (aset base ibeg asp-abeg)
        (setq base (vconcat (substring base 0 -1) (vector a)))
       )
      )
     )
     ;#43. The final 's' of a verbal base is dropped before
     ; soft dentals
     ((and (equal a 's) (soft-P e) (dental-P e))
      (setq base (substring base 0 -1))
     )
     ;#44. When a verbal root ends with a conjunct consonant having
     ; 'k' or 's' for its first member, it drops that 'k' and 's'
     ; before a termination beginning with a consonant except a
     ; nasal or a semi-vowel.
     ((let (part1 p2 p2a p2b)
      (setq part1 (elt parts 0)) ; an array of token arrays
      (setq p2 (elt (substring part1 -1) 0)) ; last element, a tok array
      (when (and (< 1 (length p2)) (member (elt p2 0) '(k s)))
       (setq p2b (substring p2 1)) ; all but initial letter
       (setq p2a (substring part1 0 -1)) ; all but last
       (setq p2 (vconcat p2a p2b))
       (setq base (vconcat (flatten p2)))
      )
     ))
    )
    (setq thisans-tok (conjugation-join base ending))
    (setq thisans (sym-without-space thisans-tok))
;    (fol-msg (format "%s + %s -> %s = %s\n" base ending thisans-tok thisans))
    ;
    ;----- irregularity overrides
    ;
    (cond
     ;#71 'hu' (sacrifice) has 2S loT of 'juhudhi (regular would be 'juhuhi)
     ((equal dhaatu 'hu)
      (if (and (equal i 3) (equal tense-sym 'loT) (equal pada 'P))
       (setq thisans 'juhudhi)
      )
     )
     ; #72. 'daa' (to give) has 2S loT of 'dehi
     ((equal dhaatu 'daa)
      (if (and (equal i 3) (equal tense-sym 'loT) (equal pada 'P))
       (setq thisans 'dehi)
      )
     )
     ; #72. 'dhaa' (to put) has 2S loT of 'dhehi
     ((equal dhaatu 'dhaa)
      (if (and (equal i 3) (equal tense-sym 'loT) (equal pada 'P))
       (setq thisans 'dhehi)
      )
     )
     ; #73. 'bhii' (to fear) optionally changes its final 'ii' to 'i'
     ; before weak terminations beginning with a consonant
     ((equal dhaatu 'bhii)
      (when (and (equal strength 'W) (consonant-P e))
       (setq base (vconcat atok0 [i]))
       (setq thisans-tok (conjugation-join base ending))
       (setq thisans (list thisans (sym-without-space thisans-tok)))
      )
     )
     ;#74. 'haa' (3A) (to depart) has (strong and weak) base
     ; 'jihii', but the 'ii' is dropped before terminations beginning
     ; with a vowel. The Base was handled above.
     ((and (equal dhaatu 'haa) (equal pada 'A))
      (when (vowel-P e)
       (setq base atok0)
       (setq thisans-tok (conjugation-join base ending))
       (setq thisans (sym-without-space thisans-tok))
      )
     )
     ;#74. 'maa' (3A) (to depart) has (strong and weak) base
     ; 'mimii', but the 'ii' is dropped before terminations beginning
     ; with a vowel. The Base was handled above.
     ((and (equal dhaatu 'maa) (equal pada 'A))
      (when (vowel-P e)
       (setq base atok0)
       (setq thisans-tok (conjugation-join base ending))
       (setq thisans (sym-without-space thisans-tok))
      )
     )
     ;#75. 'haa' (3P) (abandon) has strong base 'jahaa' and weak base
     ; either 'jahii' or 'jahi'.  Before weak terminations beginning
     ; with a vowel or with 'y', the base is 'jah'
     ; imperative 2S has 3 forms: jahaahi jahiihi jahihi
     ((and (equal dhaatu 'haa) (equal pada 'P))
      (cond
       ((and (equal tense-sym 'loT) (equal i 3))
	(setq thisans '(jahaahi jahiihi jahihi))
       )
       ((equal strength 'W)
	(cond
	 ((or (vowel-P e) (equal e 'y))
	  (setq base atok0)
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (sym-without-space thisans-tok))
	 )
	 (t ; other weak endings have an option
	  (setq base (vconcat atok0 [i])) ; use 'i' in place of 'ii'
	  (setq thisans-tok (conjugation-join base ending))
	  (setq thisans (list thisans (sym-without-space thisans-tok)))
	 )
	)
       )
      )
     )
     ;#76. 'nij' (to cleanse) and 'vij' (to separate) 
     ; (a) take guna in reduplication (done earlier in routine)
     ; (b) the radical vowel does not take guna before strong
     ;     terminations beginning with a vowel 
     ;Note 2: Special rules of sandhi seem to apply, although
     ; neither Kale nor Antoine seem to consider them special.
     ; These are similar to (but not identical to) those for the
     ; 2nd conjugation verb 'mRij'
     ((member dhaatu '(nij vij))
      (setq base base0)
      (setq ending ending0)
      (if (equal ending [h i]) (setq ending [dh i]))
      (setq e (elt ending 0))
      (cond
       ((and (equal strength 'S) (vowel-P e))
	(setq base w-atok)
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((or (equal ending [t]) (equal ending [s]))
       ; change last letter to ', drop ending
	(setq base (vconcat (substring base 0 -1) [k]))
	(setq thisans-tok base)
	(setq thisans (sym-without-space thisans-tok))
       )
       ((member e '(t th)) ; but ending /= [t]
	; change the last letter ('j') to 'k'
        (setq base (vconcat (substring base 0 -1) [k]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((equal e 's) ; but ending /= [s]
	; change last letter ('j') to 'k'
        (setq base (vconcat (substring base 0 -1) [k]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
       ((equal e 'dh)
	; change last letter ('j') to 'g'
        (setq base (vconcat (substring base 0 -1) [g]))
        (setq thisans-tok (conjugation-join base ending))
	(setq thisans (sym-without-space thisans-tok))
       )
      )
     )
     ;#77. root ends in 'Ri or 'RI
     ((member (elt (substring dtok -1) 0) '(Ri RI)) 
      (when (and (equal a 'r) (member e '(t th)) (< 1 (length ending)))
       (setq thisans-tok (vconcat base ending))
       (setq thisans (sym-without-space thisans-tok))
;       (fol-msg (format "chk Ri:(%s) %s %s -> %s\n"
; 			strength base ending thisans))       
      )
     )
    )
    (if (listp thisans) (setq thisans (flatten thisans)))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-tab-7 (a~Nga-sym tense-sym class pada dhaatu)
 ; a~Nga-sym is unused. 
 ; Antoine#84.
 ; (1) All the verbs of the 7th conjugation end with a consonant
 ; (2) Before the formation of the verbal base, a penultimate nasal
 ;     is dropped
 ; (3) In the strong forms, 'na' is inserted between the radical vowel
 ;     and the final consonant.
 ; (4) In the weak forms, 'n' is inserted between the radical vowel 
 ;      and the final consonant.
 (let (endings strengths ans n atok ibeg dhaatu-ending rtok dtok)
  ;--- 1. construct endings and strengths
  (setq endings (conj-endings tense-sym class pada))
  (setq strengths (conj-strengths tense-sym class pada))
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. atok
  (setq dtok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ; #84.2 the verbal base (rtok) removes a penultimate nasal
  ; In the case of 'hiMs', this nasal is spelled with 'M'
  (setq rtok dtok)
  (let (tmp c)
   (setq tmp (substring rtok -2))
   (setq c (elt tmp 0))
   (when (or (nasal-P c) (equal c 'M))
    (setq rtok (vconcat (substring rtok 0 -2) (substring rtok -1)))
   )
  )
  (setq dhaatu-ending (elt (substring dtok -1) 0))
  (setq ibeg 0) ; position of 1st char
  ;--- 4. for imperfect, the augment 'a' must be prefixed to rtok
  (when (equal tense-sym 'la~N)
   (let (u)
    (setq u (elt rtok 0))
    (if (vowel-P u)
     (setq rtok (vconcat (vrddhi u) (substring rtok 1)))
     (progn
      (setq rtok (vconcat [a] rtok))
      (setq ibeg 1) ; change position of 1st char
     )
    )
;    (fol-msg (format "la~N chk: rtok=%s\n" rtok))
   )
  )
  ;--- 5. combine base and endings to get ans
  (let (i y thisans base ending strength e a abeg atok0 a-save parts
	    s-base w-base  s-atok w-atok ending0 base0
	    thisans-tok da-save asp-abeg old-thisans-tok)
   ;#84.3 In strong forms, insert 'na' between radical vowel and final cons
   ; Note, after a preceding 'r', 'Na' is inserted.
   ;#84.4 In weak forms, insert 'n' between radical vowel and final cons
   ;# NOTE: The weak nasal is consistent with the varga of the final
   ;  consonant
   (setq s-atok (vconcat (substring rtok 0 -1) [n a] (substring rtok -1)))
   (setq s-atok (or (sandhi-n-N s-atok) s-atok))
   (let (weak-nasal)
     ; Sometimes, the anusvara, M, is used; but the following
     ; logic does not show this
    (cond
     ((guttural-P dhaatu-ending) (setq weak-nasal [~N]))
     ((palatal-P dhaatu-ending) (setq weak-nasal [~n]))
     ((cerebral-P dhaatu-ending) (setq weak-nasal [N]))
     (t (setq weak-nasal [n]))
    )
    (setq w-atok (vconcat (substring rtok 0 -1) weak-nasal
			  (substring rtok -1)))
   )
   (setq atok w-atok)
   (setq parts (word-parts
     (car (ITRANS-parse-words-1 (symbol-name dhaatu)))))
   (setq i 0)
   (while (< i n)
    (setq strength (elt strengths i)) ; S or W
    (if (equal strength 'S)
     (setq atok s-atok)
     (setq atok w-atok)
    )
    (setq ending (elt endings i)) ; a tok arr
    (setq e (elt ending 0))
    ;Antoine2-#87. 'tRih' (7P to kill) inserts 'ne instead of 'na'
    ; before strong terminations beginning with a consonant
    (when (equal dhaatu 'tRih)
      (when (and (equal strength 'S) (consonant-P e))
       (setq atok (vconcat (substring rtok 0 -1) [n e] (substring rtok -1)))
      )
    )
    (setq atok0 (substring atok 0 -1)) ; all but last character
    (setq a-save (elt (substring atok -1) 0)) ; last character
    (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
    (setq abeg (elt atok ibeg)) ; 1st character
    (setq asp-abeg (aspirate abeg)) ; aspirated version of 1st char
    (setq a a-save)
    (setq base (copy-sequence atok))
    (setq ending0 ending) (setq base0 atok)
    (cond
     ; no other considerations if termination is a vowel
     ((vowel-P e))
     ; #37. Before terminations beginning with a nasal or a semi-vowel,
     ; (other than [h i])
     ; the consonant sandhi offers no special difficulty
     ((or (nasal-P e) (and (semivowel-P e) (not (equal ending [h i])))))
     ;
     ;--- Note: from here on, 'e' is a consonant but not a nasal/semivowel
     ;
     ; In the case of weak endings (beginning with consonant), ???
     ;#40. Before terminations beginning with a consonant except a
     ;  nasal or a semi-vowel,
     ;  the final 'h' of a verbal base is changed to 'Dh'.
     ;  When, however, the verbal root begins with 'd', the final
     ;  'h' is changed to 'gh'
     ; we implement this in a cond step which fails
     ; so subsequent steps see this change
     ((progn
       (when (equal a 'h) 
        ; Rule 39 will now consider root to end with an aspirate
        (if (equal abeg 'd)
	 (setq a 'gh)
	 (progn
	  (setq a 'Dh)
	  ; for weak endings, also change the nasal to 'N
	  (when (equal strength 'W)
	   (let (weak-nasal)
	    (setq weak-nasal [N])
	    (setq atok0 (vconcat (substring rtok 0 -1) weak-nasal))
	    (setq atok  (vconcat atok0 [Dh]))
	   )
	  )
	 )
        )
        (setq a-save a) ; last character
        (setq da-save (de-aspirate a-save)) ; deaspirated version of last char
        (setq base (vconcat atok0 (vector a)))
	(setq atok base)
;	(fol-msg (format "#40. a-save, da-save changed: %s %s\n"
;			 a-save da-save))
       )
       nil
      )
     )
     ; #34. In the 2nd, 3rd, and 7th conjugations, the 'hi' of
     ; the 2nd pers. singular imperative parasmaipada is changed
     ; to 'dhi' when the verbal base ends with a consonant
     ; except a nasal or a semi-vowel
     ; Note: This change is sometimes modified by #42. Thus,
     ; I implement in such a way as to change the ending, but
     ; have the condition fail, so further change possibilities
     ; will be examined
     ((progn
       (when (and (equal ending [h i]) (consonant-P a)
	      (not (nasal-P a)) (not (semivowel-P a)))
	(setq ending [dh i])
	(setq e 'dh)
       )
       nil
      )
     )
     ; #38 The terminations 's' and 't' of the 2nd and 3rd pers sing.
     ; imperfect parasmaipada are dropped after a verbal base ending
     ; with a consonant. The final consonant of the base is then treated
     ; according to the rule given in Part I-72(3).
     ; Note: 
     ((and (equal tense-sym 'la~N) (equal pada 'P) (consonant-P a)
	   (member ending '([t] [s]))) ;3S or 2S
      ;1. If 'a' is aspirate, deaspirate it
      ;  and if 'a' is a soft aspirate, after losing the aspiration,
      ;  throw it back if possible on previous syllable. This is like
      ;  39.1 and 2 below.
      (when (not (equal da-save a-save))
       (setq a da-save)
       (aset base ibeg asp-abeg)
      )
      ;2. case of imperf. 3S
      (when (equal ending [t])
       (setq ending [])
       (if (equal a 's)
        (setq base (vconcat atok0 [t]))
        (setq base (sandhi-legalise-final-cons base))
       )
      )
      ;3. case of imperf. 2S
      (when (equal ending [s])
       (setq ending [])
       (setq base (sandhi-legalise-final-cons base)) ; a list
       (cond
        ((equal a 'd) 
	 (setq base (append-if-new base (vconcat atok0 [H]))))
        ((equal a 's)
	 (setq base (append-if-new base (vconcat atok0 [t]))))
       )
      )
      (setq base (solution base)) ; don't want a list unless necessary
     )
     ;#42. Before terminations beginning with 't' 'th' and 'dh',
     ; the final 'Dh' of a verbal root is dropped, while the following
     ; 't', 'th' and 'dh' are changed to 'Dh' and a preceding short
     ; vowel is lengthened.
     ; NOTE: 'beginning' with does not mean 'equal'; 
     ; i.e. this not applicable to 3S of imperfect
     ((and (equal a 'Dh) (member e '(t th dh)) (/= (length ending) 1))
      (setq e 'Dh)
      (setq ending (vconcat (vector e) (substring ending 1)))
      (let (v atok1)
       (setq v (elt (substring atok0 -1) 0)) ; preceding vowel
       (setq v (lengthen-vowel v))
       (if (< 1 (length atok0))
        (setq atok1 (substring atok0 0 -1)) ; all but last
	(setq atok1 [])
       )
       (setq base (vconcat atok1 (vector v)))
      )
     )
     ;#41. Before terminations beginning with 's' (i.e., before
     ; 'si' , 'se' and 'sva'), 'Dh' and 'Sh' are changed to 'k'
     ((and (member a '(Dh Sh)) (equal e 's))
      (setq a 'k)
      (setq base (vconcat atok0 (vector a)))
     )
     ;#39.1 The final aspirate of a verbal base loses its aspiration
     ;#39.2 A soft aspirate, after losing its aspiration, throws it back,
     ;   if possible, on the previous syllable
     ;#39.3 The previous rule does not apply before the termination
     ;   'dhi' of the 2nd pers. sing. imperative parasmaipada
     ;   (Note: implemented indirectly by #34 above)
     ;#39.4 It does not apply either before terminations beginning with
     ;   't' or 'th', in which case the lost aspiration is thrown forward
     ;   on the following 't' or 'th' which are softened (to 'dh')
     ; NOTE: this IS applicable to 3S of imperfect whose ending = 't'
     ; NOTE: for 'dhaa', #39.2 is used rather than #39.4
     ((not (equal da-save a-save)) ; so 'a' is an aspirate
      (setq a da-save) ; 'a' loses aspiration
      (if (and (member e '(t th)) (not (equal dhaatu 'dhaa)))
       (progn
        (setq e 'dh)
        (setq ending (vconcat (vector e) (substring ending 1)))
        (setq base (vconcat atok0 (vector a)))
       )
       (progn
        (aset base ibeg asp-abeg)
        (setq base (vconcat (substring base 0 -1) (vector a)))
       )
      )
     )
     ;#43. The final 's' of a verbal base is dropped before
     ; soft dentals
     ((and (equal a 's) (soft-P e) (dental-P e))
      (setq base (substring base 0 -1))
     )
     ;#44. When a verbal root ends with a conjunct consonant having
     ; 'k' or 's' for its first member, it drops that 'k' and 's'
     ; before a termination beginning with a consonant except a
     ; nasal or a semi-vowel.
     ((let (part1 p2 p2a p2b)
      (setq part1 (elt parts 0)) ; an array of token arrays
      (setq p2 (elt (substring part1 -1) 0)) ; last element, a tok array
      (when (and (< 1 (length p2)) (member (elt p2 0) '(k s)))
       (setq p2b (substring p2 1)) ; all but initial letter
       (setq p2a (substring part1 0 -1)) ; all but last
       (setq p2 (vconcat p2a p2b))
       (setq base (vconcat (flatten p2)))
      )
     ))
    )
    (setq thisans-tok (conjugation-join base ending))
    (setq thisans (sym-without-space thisans-tok))
;    (fol-msg (format "%s + %s -> %s = %s\n" base ending thisans-tok thisans))
    ;
    ;----- irregularity overrides
    ;
    (cond
     ; Without the next override, the answer from above is
     ; [p i N Sh] + [dh i] -> [p i N Sh Dh i] = piNShDhi
     ; This disagrees with both Antoine/Kale
     ((equal dhaatu 'piSh)
      (when (and (equal tense-sym 'loT) (= i 3))
       (setq base [p i N D])
       (setq thisans-tok (conjugation-join base ending))
       (setq thisans (sym-without-space thisans-tok))
      )
     )
     ;Note 2: Special rules of sandhi seem to apply, although
     ; neither Kale nor Antoine seem to consider them special.
     ; These are similar to  those for the
     ; 2nd conjugation verb 'mRij' and 3rd conj verb 'nij'.
     ; In addition, the type of the inserted nasal for weak
     ; terminations requires change, depending on other changes.
     ; The changes are coded to work similarly for roots ending
     ; in 'ch'. 
     ((member dhaatu-ending '(j ch))
      (setq base base0)
      (setq ending ending0)
      (setq old-thisans-tok thisans-tok)
      (when (equal strength 'W)
       ; default nasal is '~n' , since it occurs before palatal
       (setq base (vconcat (substring base 0 -2) [~n] (substring base -1)))
       (setq thisans-tok (conjugation-join base ending))
       (when (not (equal base base0))
        (fol-msg (format "weak base=%s  old=%s  ending=%s\n"
			 base base0 ending ))
       )
      )
      (if (equal ending [h i]) (setq ending [dh i]))
      (setq e (elt ending 0))
      (cond
       ((and (equal dhaatu-ending 'ch) (or (semivowel-P e) (equal e 'm)))
	; conjugation-join changes 'ari~nch' + 'mahi' to 'ari~njmahi'
	; Kale/Antoine show answer as 'ari~nchmahi'
	(setq thisans-tok (vconcat base ending))
       )
       ((or (equal ending [t]) (equal ending [s]))
       ; change last letter to ', drop ending
	(setq base (vconcat (substring base 0 -1) [k]))
	(setq thisans-tok base)
       )
       ((member e '(t th)) ; but ending /= [t]
	; change the last letter ('j') to 'k'
	; change nasal for weak terminations
	(if (equal strength 'S)
         (setq base (vconcat (substring base 0 -1) [k]))
	 (setq base (vconcat (substring base 0 -2) [~N k]))
	)
        (setq thisans-tok (conjugation-join base ending))
       )
       ((equal e 's) ; but ending /= [s]
	; change last letter ('j') to 'k'
	; change nasal for weak terminations
	(if (equal strength 'S)
         (setq base (vconcat (substring base 0 -1) [k]))
	 (setq base (vconcat (substring base 0 -2) [~N k]))
	)
        (setq thisans-tok (conjugation-join base ending))
       )
       ((equal e 'dh)
	; change last letter ('j' or 'ch') to 'g'
	; change nasal for weak terminations
	(if (equal strength 'S)
         (setq base (vconcat (substring base 0 -1) [g]))
	 (setq base (vconcat (substring base 0 -2) [~N g]))
	)
        (setq thisans-tok (conjugation-join base ending))
       )
      )
      ; construct symbol, thisans variable
      (setq thisans (sym-without-space thisans-tok))
     )
    )
    (if (listp thisans) (setq thisans (flatten thisans)))
    (aset ans i thisans)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun conjugation-join-sym (base-sym sup-sym &optional option)
 (sym-without-space 
  (conjugation-join
   (car (ITRANS-parse-words-1 (symbol-name base-sym)))
   (car (ITRANS-parse-words-1 (symbol-name sup-sym)))
   option
  )
 )
)
(defun conjugation-join (base-tok sup &optional option)
 ;base-tok is either an array (of tokens) or a list of arrays
 ;otherwise, there will be an error
 (cond 
  ((listp sup)
   (mapcar (lambda (x) (conjugation-join1 base-tok x option)) sup))
  ((listp base-tok)
   (mapcar (lambda (x) (conjugation-join1 x sup option)) base-tok))
  (t (conjugation-join1 base-tok sup option))
 )
)
(defun conjugation-join1 (y ending option)
 (let (ans skiprefs)
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ans
   (or
    (solution (sandhi-pair y ending 'internal 'join))
    (solution (sandhi-pair y ending nil 'join))
    (vconcat y ending)
   )   
  )
  (if (not (equal option 'not-n-N))
   (setq ans (or (sandhi-single ans) ans))
  )
  (setq ans (solution ans))
  (sandhi-pair-skiprefs-set nil)
  ans
 )
)

(defun sym-with-space (symseq)
 (intern (mapconcat 'symbol-name symseq " "))
)
(defun sym-without-space (x)
; (fol-msg (format "sym-without-space: %s\n" x))
 (let (y)
  (cond
   ((listp x) (mapcar 'sym-without-space x)) ; recursive
   ((vectorp x)
    (setq y (mapconcat 'san-symbol-name x ""))
    (if (stringp y)
	(intern y)
        (fol-msg (format "expected y to be a string: %s\n" y))
	nil
   ))
   (t
    (fol-msg (format "sym-without-space err: %s\n" x))
    nil
   )
  )
 )
)
(defun san-symbol-name (x)
 (let (y z w)
  (cond
   ((symbolp x)
     (cond 
       ((equal x 'AVAGRAHA) ".a")
       ((equal x 'VISARGA) "H")
       ((equal x 'DANDA) ".")
       (t
	(symbol-name x)
       )
      )
   )
   (t (symnum-name x)
   )
;    (t
;     (fol-msg (format "san-symbol-name: not a symbol: %s\n" x))
;     nil
;    )
  )
 )
)
(defun symnum-name (x)
 (if (symbolp x) (symbol-name x) (format "%s" x))
)
(defun sym-begins-with (x)
 (let (s tokar)
  (setq s (symbol-name x))
  (setq tokar (car (ITRANS-parse-words-1 s)))
  (if (and tokar (< 0 (length tokar)))
   (elt tokar 0)
  )
 )
)
(defun sym-ends-with (x)
 (let (s tokar)
  (setq s (symbol-name x))
  (setq tokar (car (ITRANS-parse-words-1 s)))
  (if (and tokar (< 0 (length tokar)))
   (elt (substring tokar -1) 0)
  )
 )
)

(defun sym-concat2 (s1 s2)
 (intern (concat (symbol-name s1) (symbol-name s2)))
)
(defun sym-concat (s1 &rest all)
 (let (ans)
  (setq ans s1)
  (while all
   (setq ans (sym-concat2 ans (car all)))
   (setq all (cdr all))
  )
  ans
 )
)
(defun apply-seT (tok code)
 (let (toki ans)
  (setq toki (conjugation-join tok [i]))
  (cond
   ((equal code 'seT) (setq ans toki))
   ((equal code 'aniT) (setq ans tok))
   ((equal code 'veT) (setq ans (list tok toki)))
   (t ;error condition
    (setq ans tok)
   )
  )
  ans
 )
)

(defun upasarga-join (upasargas sym)
 (let (ans x x1 y)
  (setq x1 (append upasargas (list sym)))
  (setq x
   (mapcar
    (lambda (u) (car (ITRANS-parse-words-1 (symbol-name u))))
    x1
   )
  )
  (setq ans (car x ))
  (setq x (cdr x))
  (while x
   (setq y (car x))
   (setq x (cdr x))
   (setq ans
    (or
     (solution (sandhi-pair ans y  'internal 'join))
     (solution (sandhi-pair ans y  nil 'join))
     (vconcat ans y)
    )
   )
  )
  (sym-without-space ans)
 )
)
(defun conjugation-citation-irreg (dhaatu tense-sym class pada)
 (let (form form-sym)
  (setq form (format "%s-%s-%s" tense-sym class pada))
  (setq form-sym (intern form))
  (sanget2 'Dhaatu-irreg (list dhaatu form-sym))
 )
)

(defun doc-ending-gram2 ()
 "The following routines pertain to Sangram and are
  probably obsolete.  11-08-04"
)
(defun dhaatu-class-padas (dhaatu)
 (let (ans Sangram gramtype xgram)
  (setq Sangram (get dhaatu 'Sangram))
  (setq gramtype 'dhaatu)
  (setq xgram (plist-get Sangram gramtype))
  (setq ans nil)
  (when xgram
   (let (Eng-def other-info saarvadhaatuka info-class info-class-padas
		 info-class-pada class pada)
    (setq other-info (plist-get xgram 'other-info))
    (setq saarvadhaatuka (plist-get other-info 'saarvadhaatuka))
;   saarvadhaatuka is a property list, 
;   whose properties are classes (numbers 1-10)
;   The logic uses the implementation fact that property lists are 
;   lists with form (name1 val1 name2 val2 ...)
    (while saarvadhaatuka
     (setq class (car saarvadhaatuka))
     (setq saarvadhaatuka (cdr saarvadhaatuka))
     (setq info-class (car saarvadhaatuka))
     (setq saarvadhaatuka (cdr saarvadhaatuka))
     ; info-class is a property list with properties = pada (P, A)
     (while info-class
      (setq pada (car info-class))
      (setq info-class (cdr info-class))
      (setq info-class-padas (car info-class))
      (setq info-class (cdr info-class))
      ; append element (list class pada) to answer
      (setq ans (append ans (list (list class pada))))
     )
    )
   )
  )
  ans
 )
)
(defvar Sangram-types)
(defun dhaatu-a~Nga (dhaatu class pada)
 ;dhaatu is a symbol
 ;class is a number
 ;pada is a symbol ('P or 'A)
 ;returns a~Nga(s) as a list, each element of which
 ;is a triple = (a~Nga Eng-def upasarga)
 (let (Sangram gramtype xgram)
  ; this code could be modernized with 'sanget and 'sanget2 (03-26-03)
  (setq Sangram (get dhaatu 'Sangram))
  (setq gramtype 'dhaatu)
  (setq xgram (plist-get Sangram gramtype))
  (when xgram
   (let (Eng-def other-info saarvadhaatuka info-class info-class-padas
		 info-class-pada)
    (setq Eng-def (plist-get xgram 'Eng-def))
    (setq other-info (plist-get xgram 'other-info))
    (setq saarvadhaatuka (plist-get other-info 'saarvadhaatuka))
    (setq info-class (plist-get saarvadhaatuka class))
    (setq info-class-padas (plist-get info-class pada))
;   (fol-msg (format "info-class-padas=%s\n" info-class-padas))
    (let (a~Ngas ans)
     (setq a~Ngas (mapcar
      (lambda (info-class-pada)
	(list
	   (plist-get info-class-pada 'a~Nga)
	   (plist-get info-class-pada 'Eng-def)
	   (plist-get info-class-pada 'upasarga)
	)
      )
      info-class-padas
     ))
;     (fol-msg (format "check: a~Ngas = %s\n" a~Ngas))
     ; in case a~Ngas are not avail the usual way
     ; the next gives a default notion of what they WOULD be
     ; if 'dhaatu represented a REGULAR root (not sure if this is useful)
;      (when (not a~Ngas)
;       (setq a~Ngas '(((REGULAR) NODEF nil)))
;      )
     (setq ans (mapcar
      (lambda (aeu)
       (let (xes x a~Nga-tok therest thisans)
	(setq xes (elt aeu 0)) ; the given list of a~ngas
	(setq therest (cdr aeu))
	(setq thisans nil)
	(while xes
	 (setq x (car xes))
	 (setq xes (cdr xes))
	 (cond
          ((and (eq x 'REGULAR) (member class '(1 4 6 10)))
	   (setq a~Nga-tok (dhaatu-a~Nga-a-REGULAR dhaatu class pada))
	  )
	  ((member class '(1 4 6 10))
	   (let (x-string) ; when x != REGULAR
            (setq x-string (symbol-name x))
            (setq a~Nga-tok (car (ITRANS-parse-words-1 x-string)))
	   )
	  )
	  ((eq x 'IRREGULAR)
	   (setq a~Nga-tok [I R R E G U L A R])
	  )
	  ((and (member class '(2 3 5 7 8 9)) (member pada '(P A)) x)
	   (let (x-string) ; 1-24-03 provisional for non-a conjugations
            (setq x-string (symbol-name x))
            (setq a~Nga-tok (car (ITRANS-parse-words-1 x-string)))
	   )
          )
	  (t
	   (setq a~Nga-tok [E R R])
	  )
	 )
	 (setq thisans (append-if-new thisans a~Nga-tok))
	); end of while
	(setq thisans (solution thisans)) ; in case just 1
	(cons thisans therest)
	)
       )
       a~Ngas)
      )
     ;
     ans ; the answer
    )
   )
  )
 )
)

(defun class-pada-a~Ngas (dhaatu)
 ; return lists of the form
 ; (class pada a~Nga Eng-def upasarga)
  (apply 'append (mapcar ; the answer
   (lambda (class-pada)
    (let (class pada class-type a~Ngas)
     (setq class (elt class-pada 0))
     (setq pada (elt class-pada 1))
     (setq a~Ngas (dhaatu-a~Nga dhaatu class pada))
     (mapcar
      (lambda (aeu)
       (let (a~Nga upasarga Eng-def a~Nga-sym therest)
	(setq a~Nga (elt aeu 0))
	(setq a~Nga-sym (sym-without-space a~Nga))
	(setq therest (cdr aeu))
	(append (list class pada a~Nga-sym) therest)
       )
      )
      a~Ngas
     )
    )
   )
   ; class-pada pairs 
   (dhaatu-class-padas dhaatu) 
  ))
)

(defun conjugation-citation-cp 
  (dhaatu tense-sym class pada &optional upa-list)
 (let (citations citation ans)
  (setq citations (conjugation-citation dhaatu tense-sym))
  (while citations
   (setq citation (car citations))
   (setq citations (cdr citations))
   ; citation has form (c p tab def)
   (let (c p tab def u)
    (setq c (elt citation 0))
    (setq p (elt citation 1))
    (setq u (elt citation 4))
    (when (and (equal class c) (equal pada p) (equal u upa-list))
     (setq ans (cdr (cdr citation)))
     (setq citations nil) ; to exit outer loop
    )
   )
  )
  ans
 )
)
(defun conjugation-citation (dhaatu tense-sym)
 (let (cpas ans)
  (setq ans nil)
  (setq cpas (class-pada-a~Ngas dhaatu))
  ; (02-01-03) Logic added to repeat the irregularities of
  ; the nil upasarga for non-nil upasargas without listed irregularities.
  ; This was for 'alam kRi' , where 'alam' functions like
  ; an upasarga, although it is not considered such.
  (while cpas
   (let (cpa a~Nga thisans a~Nga-tok class pada  form-tab
	     Eng-def upasarga)
    (setq cpa (car cpas))
    (setq class (elt cpa 0))
    (setq pada (elt cpa 1))
    (setq a~Nga (elt cpa 2))
    (setq Eng-def (elt cpa 3))
    (setq upasarga (elt cpa 4))
    (setq thisans nil)
    (cond
     ((equal a~Nga 'IRREGULAR)
       (setq thisans (conjugation-citation-irreg dhaatu tense-sym class pada))
      (when thisans
       (setq thisans (vconcat thisans))) ; turn into array
     )
     ((member class '(1 2 3 4 5 6 7 8 9 10))
      (setq thisans (conjugation-tab a~Nga tense-sym class pada dhaatu))
     )
    )
    (when thisans
     (if upasarga
      (setq thisans (list class pada thisans Eng-def upasarga)) ; add info
      (setq thisans (list class pada thisans Eng-def)) ; add info
     )
     (setq ans (append ans (list thisans))) ; add thisans to ans
    )
   )
   (setq cpas (cdr cpas))
  )
  ans
 )
)
(defun modify-conj-endings-1 (tense-sym pada)
 (let (class ending endings ans i1 i2 n)
  ;--- construct array of endings
  (setq class 1) 
   (setq endings (conj-endings tense-sym class pada))
   (setq n (length endings))
   (setq ans (copy-sequence endings))
   (setq i1 0)
   (setq i2 n)
    ; perform certain adjustments (these could be stored)
    (let (i ending u y)
     (setq i i1)
     (while (< i i2)
      (setq ending (elt endings i))
      (if (equal ending [])
       (setq u [])
       (setq u (vector (elt ending 0)))
      )
      (setq y (cond
       ((equal u [m]) [aa])
       ((equal u [v]) [aa])
       ((equal u [a]) []) ; "a" dropped before "a"
       ((equal u [e]) []) ; "a" dropped before "e" (Antoine I.40)
       ((equal u []) [a]) ; e.g. imperative P 2 S
       (t [a]) ; the usual, default case for the A-conjugations
      ))
      (setq ending (conjugation-join y ending))
      (aset endings i ending)
      (setq i (1+ i))
     )
    )
   
;  (fol-msg (format "endings(1) = %s\n" ans))
;  (fol-msg (format "endings(2) = %s\n" endings))
  endings
 )
)
(defun get-form-tab (class pada)
 (let (class-type form-tab)
  (setq class-type
   (if (member class '(1 4 6 10)) 1 2))
  (setq form-tab 
   (eval (intern-soft (format "%s-%s-properties-set" class-type pada))))
 )
)
