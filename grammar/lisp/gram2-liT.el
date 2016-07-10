; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; gram2-liT.el  
; begun 07-20-02 by ejf
; Code in the following may be used:
;   itrans.el, gram1.el, sandhi.el
; The particular spelling of the symbols representing
; the phonetic elements (the 'tokens') must be consistent with 
; that in itrans.el
(defvar liT-1-P-endings
 "a atuH uH  tha athuH a  a va ma"
)
(defvar liT-1-A-endings
 "e aate ire  se aathe dhve  e vahe mahe"
)
(defvar liT-1-P-strengths [S W W  S W W  S W W])
(defvar liT-doc-Passive ()
 "Kale 595.
  (a) The Reduplicative perfect of a root in the passive is formed
    in the ordinary way, every root being considered Atmanepadi.
  (b) The Periphrastic perfect of the Passive does not differ from
    that of the Active, except in that the Auxiliary verbs take
    Atmanepadi terminations necessarily.
 "
)
(defvar liT-r-bitab nil)
(defvar liT-r-endings nil)
(defun conjugation-tab-liT (upa-syms class pada dhaatu &optional voice dbg)
 ; Perfect tense.
 ; both reduplicative and periphrastic
 ; voice should be 'ACTIVE or 'PASSIVE
; (if (equal voice 'PASSIVE) (setq pada 'A))
 (setq liT-r-bitab nil)
 (let (ans1 ans2 ans)
  (when dbg
   (fol-msg (format "conjugation-tab-liT %s\n" (list upa-syms class pada dhaatu voice)))
  )
  (when (reduplicative-liT-P dhaatu class)
   (setq ans1 (conjugation-tab-liT-r upa-syms class pada dhaatu voice dbg)))
  (when (periphrastic-liT-P dhaatu class)
   (setq ans2 (conjugation-tab-liT-p upa-syms class pada dhaatu voice dbg)))
  (cond
   ((and ans1 (not ans2)) ; reduplicative only
    (setq ans ans1)
   )
   ((and ans2 (not ans1)) ;  periphrastic only
    (setq ans ans2)
   )
  ((and ans1 ans2) ; both forms
   (setq ans (join-arrays ans1 ans2))
   )
  )
 )
) 
(defun conjugation-tab-liT-r (upa-syms class pada dhaatu &optional voice dbg)
 ; reduplicative perfect tense.
 ; A few roots have two optional forms. In this routine,
 ; the main perfect routine is called for two roots,
 ; and the two results combined.
 ; For the majority of roots, the main routine is called
 ; for the given dhaatu.
 ; Some other irregular forms, algorithmically problematic,
 ; are also included here.
 ; voice should be 'ACTIVE or 'PASSIVE or nil
  (when dbg
   (fol-msg (format "conjugation-liT-r %s\n" (list upa-syms class pada dhaatu  voice)))
  )

 (if (equal voice 'PASSIVE) (setq pada 'A))
 (setq liT-r-bitab nil)
 (let (ans)
  (cond
   ((member dhaatu '(shvi ve shranth granth dambh sva~nj
		     ad chi aj chakSh vid))
    ;--- verbs with optional forms
;    (fol-msg (format "dhaatu=%s\n" dhaatu))
    (let (ans1 ans2 ans3)
     (cond
      ((equal dhaatu 'shvi)
       ;Kale p. 318
       ; 'shvi' is to be optionally considered as 'shu' in the Perfect
       (setq ans1 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (setq ans2 (conjugation-tab-liT-r-main upa-syms class pada 'shu nil dbg))
      )
      ((equal dhaatu 've)
       ;Kale p. 318
       ; ve (to weave) has a regular form and an irregular form.
       ; In the irregular form,
       ;  it is considered 'uvay' before strong forms,
       ;  and it is considered either 'uuy' or 'uuv' before weak forms.
       (setq ans1 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (if (equal pada 'P)
        (setq ans2 
         [uvaaya (uuyatuH uuvatuH) (uuyuH uuvuH)
	  uvayitha (uuyathuH uuvathuH) (uuya uuva)
	  (uvaaya uvaya) (uuyiva uuviva) (uuyima uuvima)
         ]
        )
        (setq ans2
	 [(uuye uuve) (uuyaate uuvaate) (uuyire uuvire)
	  (uuyiShe uuviShe) (uuyaathe uuvaathe)
	      (uuyidhve uuvidhve uuyiDhve uuviDhve)
	  (uuye uuve) (uuyivahe uuvivahe) (uuyimahe uuvimahe)
	 ]
        )
       )
      )
      ;Kale p.323
      ; The roots 'shranth', 'granth', 'dambh', and 'sva~nj' drop their
      ; nasal optionally before the terminations of the Perfect.
      ; When the nasal is dropped,
      ; 'shranth', 'granth', 'dambh' obey #500, even before the strong
      ; terminations
      ((member dhaatu '(shranth granth dambh sva~nj))
       (setq ans1 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (let (tok seTPerfCodes tok1 dhaatu1)
	(setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
	(setq tok1 (vconcat (substring tok 0 -2) (substring tok -1)))
	(setq dhaatu1 (sym-without-space tok1))
	(setq seTPerfCodes (construct-seTPERF-code1a dhaatu class pada upa-syms))
;	(fol-msg (format "tok1=%s, dhaatu1=%s, seTPerfCodes=%s\n"
;			tok1 dhaatu1 seTPerfCodes))
	(setq ans2
	 (conjugation-tab-liT-r-main upa-syms
			           class pada dhaatu1 seTPerfCodes dbg))
       )
      )
      ((equal dhaatu 'ad)
       ;Kale #511, p. 324
       ; 'ghas' (1P) is to be optionally substituted for 'ad' in the Perfect
       (setq ans1 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (setq ans2 (conjugation-tab-liT-r-main upa-syms 1 pada 'ghas nil dbg))
      )
      ((equal dhaatu 'chi)
       ;Kale #514, p. 326
       ; 'chi' optionally changed to 'ki' after the reduplicative
       ; syllable in the Perfect and the Desiderative.
       ; The change to 'ki' appears in the basic algorithm in ans1.
       ; We simply assert the value in 'ans2'
       (setq ans1 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (setq ans2
	[chichaaya chichyatuH chichyuH
	 (chichetha chichayitha) chichyathuH chichya
	 (chichaya chichaaya) chichyiva chichyima]
       )
      )
      ((and (equal dhaatu 'aj) (equal pada 'P))
       ;Kale #515 p. 327, #477
       ; 'vii' (2 P) is substituted for 'aj' (1 P 'to go')
       ;   necessarily before any
       ; non-conjugational termination, and optionally before such as
       ; begin with any consonant except 'y':
       ;   'vetaa  ajitaa'
       ;   'veShyati ajiShyati'
       (setq ans1 (conjugation-tab-liT-r-main upa-syms 2 pada 'vii nil dbg))
       ;(fol-msg (format "ans1=%s\n" ans1))
       (let (i n)
        ; only the 'i' forms of 'aj' provide optional answers
        (setq ans2 (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))     
        ;(fol-msg (format "ans2(a)=%s\n" ans2))
        (setq n (length ans2))
        (setq i 0)
        (while (< i n)
	 (when (not (member i '(3 7 8)))
	  (aset ans2 i nil)
	 )
	 (setq i (1+ i))
        )
        ;(fol-msg (format "ans2(b)=%s\n" ans2))
       )
      )
      ; July 2, 2016. corrected 'kshaa' to 'kShaa' 
      ((equal dhaatu 'chakSh)
       (let (C)
	(setq C (construct-seTPERF-code1a dhaatu class pada upa-syms))
	(cond
         ((equal pada 'P)
	  (setq ans1
	    (conjugation-tab-liT-r-main upa-syms class pada 'khyaa C dbg))
	  (setq ans2
	    (conjugation-tab-liT-r-main upa-syms class pada 'kShaa C dbg))
	 )
         ((equal pada 'A)
	  (setq ans1
	    (conjugation-tab-liT-r-main upa-syms class pada dhaatu C dbg))
	  (setq ans2
	    (conjugation-tab-liT-r-main upa-syms class pada 'khyaa C dbg))
	  (setq ans3
	    (conjugation-tab-liT-r-main upa-syms class pada 'kShaa C dbg))
	 )
	)
       )
      )
      ((and (equal dhaatu 'vid) (equal pada 'P))
       ; ans1 is the form described by Antoine2#121. (no reduplication)
       ; However, Kale(p. 330) shows 'viveda', etc.
       ; Whitney shows both forms, identifying the no-redup form as part
       ; of vedic Sanskrit, and the Kale form as part of classical Sanskrit.
       ; ans1 gives the Vedic form.
       ; ans2 gives the classical form
       (setq ans1
	    (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (setq ans2
	 [viveda vividatuH vividuH
	  viveditha vividathuH vivida
	  viveda vividiva vividima]
       )
      )
      (t
       ; otherwise, the pada of the suspect root means it is
       ; treated without optional form. We mimic this by setting
       ; This could arise, for instance, for the passive of a normally
       ; 'P' root, since the passive is just the 'A' form conjugation
       ; ans2 to ans1
       (setq ans1
	    (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
       (setq ans2 ans1)
      )
     )
;    (fol-msg (format "ans1=%s\n, ans2=%s\n" ans1 ans2))
     (setq ans (join-arrays ans1 ans2))
     (when ans3 (setq ans (join-arrays ans ans3))) ; just for 'chakSh
    )
   )
   ; some other irregularites
   ((equal dhaatu 'vye)
    ; Kale 506 p. 319 'vye'.
    ; 'vye' becomes 'vivyay' before strong terminations and 'vivii'
    ; before the weak ones in the Perfect
    (if (equal pada 'P)
     (setq ans
      [vivyaaya vivyatuH vivyuH
       vivyayitha vivyathuH vivya
       (vivyaaya vivyaya) vivyiva vivyima
      ]
     )
     (setq ans
      [vivye vivyaate vivyire
       vivyiShe vivyaathe (vivyidhve vivyiDhve)
       vivye vivyivahe vivyimahe 
      ]
     )
    )
   )
   ((equal dhaatu 'hve)
    ; Kale 506, p. 319.
    ; 'hve' (to call) is to be considered 'hu' (to sacrifice) in the Perfect
    ; class is changed to that of 'hu' (namely, 3)
    (setq ans (conjugation-tab-liT-r-main upa-syms 3 pada 'hu nil dbg))
   )
   ((and (equal pada 'P) (equal dhaatu 'ta~nch))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    (aset ans 4 (list (elt ans 4) 'tata~NkthuH))
   )
   ((and (equal pada 'P) (equal dhaatu 'mRij))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; otherwise 1D and 2D are
    ; (mamaarjva mamaarjiva mamRijva mamRijiva) and
    ; (mamaarjma mamaarjima mamRijma mamRijima).
    ; the 1st form of each is not used
    (aset ans 7 (cdr (elt ans 7)))
    (aset ans 8 (cdr (elt ans 8)))
   )
   ; Kale p. 320
   ((and (equal pada 'P) (equal dhaatu 'tRip))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    (let (tmp)
     (setq tmp (elt ans 3))
     (if (not (listp tmp)) (setq tmp (list tmp)))
     (aset ans 3 (append tmp '(tatraptha)))
    )
   )
   ; Kale p. 320
   ((and (equal pada 'P) (equal dhaatu 'dRip))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    (let (tmp)
     (setq tmp (elt ans 3))
     (if (not (listp tmp)) (setq tmp (list tmp)))
     (aset ans 3 (append tmp '(dadraptha)))
    )
   )
   ; Kale p. 321
   ((and (equal pada 'A) (equal dhaatu 'trap))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    (aset ans 5 '(trebdhve trepidhve)) ; was (trepdhve trepidhve)
   )
   ; Kale p. 321
   ((and (equal pada 'A) (equal dhaatu 'ash))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    (aset ans 5 '(aana~NDhve aanashidhve)) ; was (aanashdhve aanashidhve)
   )
   ; Kale p. 322
   ((and (equal pada 'A) (equal dhaatu 'gaah))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (jagaakShe jagaahiShe)
    (aset ans 3 '(jaghaakShe jagaahiShe))
    ; was (jagaahdhve jagaahidhve)
    (aset ans 5 '(jaghaaDhve jagaahidhve jagaahiDhve))
   )
   ; Kale p. 322
   ((and (equal pada 'A) (equal dhaatu 'gRih))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (jagRikShe jagRihiShe)
    (aset ans 3 '(jaghRikShe jagRihiShe))
    ; was (jagRihdhve jagRihidhve)
    (aset ans 5 '(jaghRiDhve jagRihidhve jagRihiDhve))
   )
   ; Kale p. 322
   ((and (equal pada 'A) (equal dhaatu 'guh))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (juguhse juguhiShe)
    (aset ans 3 '(jughukShe juguhiShe))
    ; was (juguhdhve juguhidhve)
    ; When 'Dh' or 'r' is dropped,
    ; the preceeding 'a', 'i' or 'u' is lengthened
    (aset ans 5 '(jughuuDhve juguhidhve juguhiDhve))
   )
   ; Kale p. 322
   ((and (equal pada 'P) (equal dhaatu 'guh))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was jugoha
    (aset ans 0 'juguuha)
    (aset ans 6 'juguuha)
    ; was (jugoDha jugohitha)
    ; Kale actually shows 'jagoDha', but I considered the 'ja' an error
    (aset ans 3 '(jugoDha juguuhitha))
   )
   ; Kale p. 322
   ((and (equal pada 'P) (equal dhaatu 'druh))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (dudroDha dudrohitha)
    ; The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
    ; changed to 'gh' or to 'Dh' when followed by
    ;  (a) any consonant except a nasal or a semivowel, or
    ;  (b) by nothing
    (aset ans 3 '(dudroDha dudrogdha dudrohitha))
   )
   ; Kale p. 322
   ((and (equal pada 'P) (equal dhaatu 'muh))
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (mumoDha mumohitha)
    ; The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
    ; changed to 'gh' or to 'Dh' when followed by
    ;  (a) any consonant except a nasal or a semivowel, or
    ;  (b) by nothing
    (aset ans 3 '(mumoDha mumogdha mumohitha))
   )
   ; Kale p. 323
   ((and (equal dhaatu 'snih) (equal pada 'P) )
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (siShNeDha siShNehitha)
    ; The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
    ; changed to 'gh' or to 'Dh' when followed by
    ;  (a) any consonant except a nasal or a semivowel, or
    ;  (b) by nothing
    (aset ans 3 '(siShNeDha siShNegdha siShNehitha))
   )
   ; Kale p. 323
   ((and (equal dhaatu 'snuh) (equal pada 'P) )
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was (suShNoDha suShNohitha)
    ; The final 'h' of roots 'druh', 'muh', 'snih', and 'snuh' is
    ; changed to 'gh' or to 'Dh' when followed by
    ;  (a) any consonant except a nasal or a semivowel, or
    ;  (b) by nothing
    (aset ans 3 '(suShNoDha suShNogdha suShNohitha))
   )
   ; Kale #517, p. 327
   ; The base of 'i' with 'adhi' (to study) is 'ajijagaa'
   ((and (equal dhaatu 'i) (equal pada 'A) (equal upa-syms '(adhi)))
    (let (seTPerfCodes)
     (setq seTPerfCodes (construct-seTPERF-code1a dhaatu class pada upa-syms))
     (setq ans (conjugation-tab-liT-r-main upa-syms class pada 'jag
					 seTPerfCodes dbg))
    )
   )
   ; Kale 518, p. 327
   ((and (equal dhaatu 'uurNu) (equal pada 'P) )
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was uurNunavitha
    (aset ans 3 '(uurNunavitha  uurNunuvitha))
   )
   ((and (equal dhaatu 'pyai) (equal pada 'A))
    ;Kale #523 p. 329
    ; 'pii' is substituted for 'pyai' (1 A 'to grow fat')
    ; in the Perfect and in the Frequentative
    (let (seTPerfCodes)
     (setq seTPerfCodes (construct-seTPERF-code1a dhaatu class pada upa-syms))
     (setq ans (conjugation-tab-liT-r-main upa-syms class pada 'pii
					 seTPerfCodes dbg))
    )
   )
   ((and (equal dhaatu 'vij) (equal pada 'P))
    ; Kale 525, p. 329. For the forms of vij see #466:
    ; 'viveja' 1S, 'vivijitha' 2S, 'vivijathuH' 2D, 'vivija' 2P
    ; Kale #466: The intermediate 'i' is weak in the case of
    ; the root 'vij' (6 A 7 P); and optionally so in the case of 'uurNu'
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ; was vivejitha
    (aset ans 3 'vivijitha)
   )
   (t
    ; the usual case. liT-main provides the answer
    (setq ans (conjugation-tab-liT-r-main upa-syms class pada dhaatu nil dbg))
    ;(fol-msg (format "check1: ans=%s\n" ans))
   )
  )
  ans
 )
)
(defun join-arrays (ans1 ans2)
 ; if ans1,2 are arrays of the same length,
 ; return the array which joins the elements
 ; otherwise return nil
 (let (ans n i x1 x2 x y)
  (when (and (arrayp ans1)
	     (arrayp ans2)
	     (equal (length ans1) (length ans2))
	)
   (setq n (length ans1))
   (setq ans (make-vector n nil))
   (setq i 0)
   (while (< i n)
    (setq x1 (elt ans1 i))
    (setq x2 (elt ans2 i))
    (if (not (listp x1)) (setq x1 (list x1)))
    (if (not (listp x2)) (setq x2 (list x2)))
    (setq x x1)
    (while x2
     (setq y (car x2))
     (setq x2 (cdr x2))
     (setq x (append-if-new x y))
    )
;    (setq x (append x1 x2))
    (setq x (solution x))
    (aset ans i x)
    (setq i (1+ i))
   )
  )
  (when (and (not ans1) (arrayp ans2))
   (setq ans ans2)
  )
  ans
 )
)
(defun conjugation-tab-liT-r-main (upa-syms class pada dhaatu
			    &optional seTPerfCodes dbg)
 ; perfect conjugation, Main routine
 ; note 'upa-syms is actually 'upa-syms'.  This is needed 
 ; for the case of 'sam-kRi'
 (let (endings strengths ans n atok seT-gen seT-th seT-upa btab itab
       bitab wparts parts types redups redup redup2 lc)
 (when dbg
  (fol-msg (format "conjugation-tab-liT-r-main %s\n" (list upa-syms class pada dhaatu seTPerfCodes)))
 )
  ;--- 1. construct endings and strengths; init ans
  (let (conj-class sym name tense-sym )
   (setq conj-class 1) (setq tense-sym 'liT)
   (setq name (format "%s-%s-%s" tense-sym conj-class  pada))
   (setq sym (intern-soft name))
   (setq endings (sanget 'Sup sym))
   (setq endings (copy-sequence endings))
   (setq liT-r-endings endings)
;   (fol-msg (format "(endings)%s : %s\n" name endings))
   (setq name (format "%s-%s-%s-strengths" tense-sym conj-class  pada))
   (setq sym (intern-soft name))
   (setq strengths (sanget 'Sup sym))
   (setq strengths (copy-sequence strengths))
;   (fol-msg (format "(strengths)%s : %s\n" name strengths))
  )
  ;--- 2. init ans
  (setq n (length endings))
  (setq ans (make-vector n nil))
  ;--- 3. Get bitab, which will be joined to endings
  (setq bitab (liT-main-get-bitab upa-syms class pada dhaatu seTPerfCodes dbg))
  ;(fol-msg (format "check: bitab=%s\n" bitab))
  ; one modification to 'endings'
  (when (equal pada 'P)
   (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   (setq lc (elt (substring atok -1) 0))
   (when (member lc '(aa e ai o))
    ; Antoine2##112
    ; When the root ends in 'aa', the perfect
    ; a. takes ending 'au' in 1S and 3S parasmaipada 
    (aset endings 0 [au])
    (aset endings 6 [au])
   )
  )
  (when nil ; 't' for debug
   (fol-msg (format "bitab=%s\n" bitab))
   (fol-msg (format "endings=%s\n" endings))
  )
  ;--- 6. combine base and endings to get ans
  ; Note: ans, a vector, has its values reset in perfect-bitab-join
  (perfect-bitab-join bitab endings ans dbg) 
  ;--- 7. Irregularities not yet covered
  (cond
   ((equal dhaatu 'ah)
    ;Antoine2#120. The root 'ah' (to say) has not all the forms of the
    ;perfect. The first person is completely lacking, and so is 2P.
    ;NOTE: 2S is irregular (the regular form would be 'aahitha'
    (aset ans 3 'aattha)
    (aset ans 5 nil)
    (aset ans 6 nil)
    (aset ans 7 nil)
    (aset ans 8 nil)
   )
   ((and (equal pada 'P) (member dhaatu '(mi mii)))
    ; Kale 505 (p. 312). confirmed by Antoine appendix
    ; 'mi 5 P/A'
    ; 'mii 9 P/A'
    (aset ans 0 'mamau) ; was 'mimaaya'
    (aset ans 3 '(mamaatha mamitha)) ; was (mimetha mimayitha)
    (aset ans 6 'mamau) ; was (mimaya mimaaya)
   )
   ((and (equal pada 'P) (equal dhaatu 'lii))
    ; Kale 505 (p. 312) confirmed by Antoine appendix
    ; lii 9 P , 4 A : to adhere
    ; lii 1 P : to melt
    (aset ans 0 '(lilaaya lalau)) ; was lilaaya
    (aset ans 3 '(liletha lilayitha lalaatha lalitha)) ;was (liletha lilayitha)
    (aset ans 6 '(lilaya lilaaya lalau)) ;was (lilaya lilaaya)
   )
   ((and (equal pada 'P) (equal dhaatu 'prachCh))
    ; Kale 505, p.313
;    (aset ans 3 '(paprachChitha papraShTa)) ; was paprachChitha
   )
   ((and (equal pada 'P) (equal dhaatu 'bhrasj))
    (aset ans 3 '(babhraShTha babhrajjitha  babharShTha babharjitha))
    ; was (babhrajktha babhrajjitha babharktha babharjitha)
   )
   ((and (equal pada 'P) (equal dhaatu 'sRij))
    (aset ans 3 '(sasraShTha sasarjitha)) 
    ; was (sasarktha sasarjitha) (Kale p. 314)
   )
   ((and (equal pada 'P) (equal dhaatu 'dRish))
    (aset ans 3 '(dadraShTha dadarshitha))
    ; was dadarshitha (Kale p. 314)
   )
   ((and (equal pada 'P) (equal dhaatu 'dah))
    (aset ans 3 '(dadagdha dehitha))
    ; was  (dadahtha dehitha) (Kale p. 315)
   )
   ((and (equal pada 'P) (equal dhaatu 'nah))
    (aset ans 3 '(nanaddha nehitha))
    ; was  (nanahtha nehitha) (Kale p. 315)
   )
   
  )
  ans
 )
)
(defun liT-main-get-bitab (upa-syms class pada dhaatu
			    &optional seTPerfCodes dbg)
 (let (atok seT-gen seT-th seT-upa btab itab
       bitab wparts parts types redups redup redup2 n)
  (when dbg
   (fol-msg (format "liT-main-get-bitab %s\n" (list upa-syms class pada dhaatu seTPerfCodes)))
  )
  ;--- 3a. atok
  (cond
   ((and (equal dhaatu 'kRi) (member upa-syms '((sam) (saMs))))
    ;Kale #504
    (setq atok (car (ITRANS-parse-words-1 (symbol-name 'skRi))))
   )
   (t
    (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   )
  )
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 3c. redups
  ; redups is a list. 
  (setq redups (reduplicate-perfect atok wparts))
  ;(fol-msg (format "check: redups=%s\n" redups))
  (setq redup (elt redups 0))
  (setq redup2 (elt redups 1))
  (if (not redup2) (setq redup2 redup))
  ;--- 4. seT codes
  (let (temp)
   (if seTPerfCodes
    (setq temp seTPerfCodes)
    (setq temp (construct-seTPERF-code1a dhaatu class pada upa-syms dbg))
   )
   (setq temp (solution temp))
   ;(fol-msg (format "check: temp=%s\n" temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (when (equal dhaatu 'vid)
    (setq temp '(aniT aniT)) ; Antoine2#120
   )
   ;Kale 515
   (when (and (equal dhaatu 'kRi) (equal upa-syms '(sam)))
    (setq temp '(seT seT)) 
   )
   ;Kale p. 321
   (when (and (equal dhaatu 'kuSh) (equal upa-syms '(nir)))
    (setq temp '(veT veT))
   )
   (setq seT-gen (elt temp 0))
   (setq seT-th (elt temp 1))
   (setq seT-upa (elt temp 2))
   ;(fol-msg (format "check: sew_gen=%s,sew_th=%s,sew_upa=%s\n" seT-gen seT-th seT-upa))
  )
  ;--- 5a. get default table of i-inserts
  (if (equal pada 'P)
    ; parasmaipada
    (setq itab (vector
     'NA 'NA 'NA
      seT-th 'NA 'NA
      'NA seT-gen seT-gen
    ))
    ; atmanepada
    (setq itab (vector
     'NA 'NA 'NA
     seT-gen 'NA seT-gen
     'NA seT-gen seT-gen
    ))
  )
  ;(fol-msg (format "check: itab=%s\n" itab))
  (setq n (length itab))
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
  (let (b nb lc pc bw)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1)
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
    (setq b redup)
    (setq bw redup2)
    (when (member lc '(aa e ai o))
     (setq bw (substring b 0 -1)) ; Antoine2#112
    )
   ;(fol-msg (format "check: b=%s, bw=%s, lc=%s,pc=%s\n" b bw lc pc))
   (if (equal pada 'P)
   ;---- parasmaipada
    (cond
     ((member lc '(aa e ai o))
      ; Antoine2##112
      ; When the root ends in 'aa', the perfect
      ; a. takes ending 'au' in 1S and 3S parasmaipada 
      ; b. optionally drops 'aa' in 2S paramaipada
      ; c. drop 'aa' before other terminations
      ; Similarly, for roots ending in 'e ai o'; Note, for 2S-Parasmaipada,
      ; the ending vowel is changed to 'aa'
      (let (bth b1s b3s bcon b0)
       (setq b (vconcat bw [aa])) ; change last char to 'aa'
       (setq b3s bw)
       (setq b1s bw)
;       (aset endings 0 [au])
;       (aset endings 6 [au])
        ; guna or vrddhi
       (setq bth (list b bw))
       (setq btab (vector
         b3s bw bw
         bth bw bw
         b1s bw bw)
        )
        (setq bitab (perfect-bitab btab itab dbg))
        ; override 2S
        (aset bitab 3
	     (list (list b 'NA) (list bw 'seT)))
       )
     )
     ((or
       (and (equal pc 'a) ; medial 'a'
	   (equal types "CVC")
	   (equal (length (elt parts 0)) 1) ; simple 1st consonant
	   (equal (length (elt parts 2)) 1) ; simple last consonant
           (equal (elt atok 0) (elt redup 0)) ; initial consonant unchanged
	   ; exceptions
           (not (member dhaatu '(jan))) ;Antoine
	   (not (equal (elt atok 0) 'v)) ;Kale #500
	   (not (member dhaatu '(shas dad))) ;Kale #500
	   (not (member dhaatu '(radh jabh))) ;Kale #508, p. 320
       )
       ; roots which follow the rule, though not fitting usual mold
       (member dhaatu '(bhaj tRI trap phal))
       (and (equal dhaatu 'raadh) (equal upa-syms '(apa)))
       ; Kale#509. Optional forms of 'shranth', 'granth'
       (member dhaatu '(shrath grath))
       ; Kale #512. The logic provides optional forms for
       ; these roots
       (member dhaatu '(jRI bhram tras phaN raaj bhraaj bhraash
			bhlaash syam svan))
      )
      ; Antoine2#113. Kale#500
      ; Roots which have a medial 'a' preceded and followed by a single
      ; consonant, and which keep their initial consonant unchanged in
      ; reduplication - such roots form their perfect as follows:
      ; 1. before weak terminations, there is no reduplication and the
      ; medial 'a' of the root is changed to 'e'.
      ; 2. In the parasmaipada 2S, there are two forms:
      ;   a. one form using the base as above and inserting 'i'
      ;   b. another using 'redup' and not inserting 'i'
      ;N.B. The root 'bhaj', although beginning with an asplirate, is
      ;conjugated like 'pat', e.g. 'babhaaja', 'bhejatuH', 'bhejuH'.
      ; The same applies to
      ;  'tRi' ('tataara', 'teratuH', 'teruH'),
      ;  'trap' ('tatraapa' ,  'trepatuH' , 'trepuH'
      ; 'apa' 'raadh' ('raraadha', 'redhatuH', 'redhuH')
      (let (bs bth b1s b3s)
       (setq bw (copy-sequence atok))
       (if (member (length atok) '(1 2 3))
	 (aset bw 1 'e)
	 (aset bw 2 'e) ; exception: initial consonant is compound for 'trap'
       )
       (setq bs b)
       (setq b3s (gunate-final-vowel bs t)) ; vrddhi
       (setq b1s (list
	(gunate-final-vowel bs) (gunate-final-vowel bs t))) ; guna or vrddhi
       (setq bth (list
	 (gunate-final-vowel bs) ; guna
	 bw))
       (cond
	((equal dhaatu 'tRI)
	 (setq bw [t e r])
	 (setq bth bw)
        )
	((equal dhaatu 'jRI)
	 (setq bw [j e r])
        )
	((equal dhaatu 'raadh)
	 (setq bth bw)
	)
	;Kale 509
	((member dhaatu '(shrath grath dabh))
	 (setq b1s bw)
	 (setq b3s bw)
	 (setq bth bw)
	)
	;Kale 512 p. 325
	((equal dhaatu 'phal)
	 (setq bth (list bw))
	)
       )
       (setq btab (vector
         b3s bw bw
         bth bw bw
         b1s bw bw)
       )
       (when (member dhaatu '(jRI bhram tras phaN raaj bhraaj bhraash
			bhlaash syam svan))
        ; the form is optional
        (setq btab (vector
          b3s (list b bw) (list b bw)
          (list b bw) (list b bw) (list b bw)
          b1s (list b bw) (list b bw))
        )
       )
;       (fol-msg (format "check: btab=%s\n" btab))
       (setq bitab (perfect-bitab btab itab dbg))
       (when (not (member dhaatu '(tRI raadh shrath grath dabh phal
             jRI bhram tras phaN raaj bhraaj bhraash
			bhlaash syam svan)))
        ; override for 2s
        (aset bitab 3
	 (list (list (elt bth 0) 'aniT)
	       (list (elt bth 1) 'seT)))
       )
      )
     )
     ((equal dhaatu 'bhrasj)
      ; Kale 505 p.314
      (setq b (list redup redup2))
      (setq btab (vector
	 b b b
	 b b b
	 b b b
       ))
       (setq bitab (perfect-bitab btab itab dbg))
     )
     ((kuTaadi-P dhaatu)
      ; Kale 463; Kale 505 p. 316
      ; By the general rule of how kuTaadi roots behave in the
      ; non-conjugational tenses, the 2S is not strengthed,
      ; while the 1S and 3S are strengthed
      ; Roots of the kuTaadi class retain their vowel unchanged
      ; optionally in the 1S of the perfect:
      ;  nu -> (nunaava nunuva)  (otherwise, (nunava nunaava  ))
      ; kuT -> (chukoTa chukuTa) (otherwise, (chukoTa chukauTa))
      ; Notice in the case of 'nu', the unstrengthed vowel and the
      ;   vrddhi-strenghthened one are used
      ; However, in the case of 'kuT', the unstrengthed vowel and
      ; the guna-strengthened one are used.
      ; How to decide which subcase is not discussed by Kale.
      (let (bth b1s b3s bv bg)
       (setq bv (gunate-final-vowel b t)) ; vrddhi
       (setq bg (gunate-final-vowel b)) ; guna
       (setq bth b) ;  don't gunate in 2S
       (cond
	((equal dhaatu 'sphur)
	 ; per Kale, p. 316. This violates the general rule
	 (setq b3s bg)
	 (setq b1s b3s)
	)
        ((equal types "CV") ; treat like 'nu'
	 (setq b3s bv)
	 (setq b1s (list b b3s))
	)
	(t ; treat like 'kuT'
	 (setq b3s bg)
	 (setq b1s (list b b3s))
	)
       )
       (setq btab (vector
         b3s bw bw
         bth bw bw
         b1s bw bw)
        )
        (setq bitab (perfect-bitab btab itab dbg))
       )
     )
     ((or (equal pc 'a) ; medial 'a'
	  (vowel-P lc)) ; final vowel
;      (fol-msg (format "chk. b=%s\n" b))
      (let (bth b1s b3s vrddhi guna)
       ;(fol-msg (format "check: pc = a\n"))
       (when (member dhaatu '(i Ri)) ;07-25-03
	 (setq b redup2)
	 (setq bw redup)
       )
;       (fol-msg (format "chk. b=%s, bw=%s\n" b bw))
       (if (equal dhaatu 'i)
	(setq vrddhi [i y ai])
	(setq vrddhi (gunate-final-vowel b t))
       )
       (setq guna (gunate-final-vowel b))
       (setq b3s vrddhi) ; vrddhi
       (setq b1s (list guna vrddhi)) ; guna or vrddhi
       (setq bth guna) ; guna
;       (fol-msg (format "chk: b3s bth b1s = %s %s %s\n" b3s bth b1s))
       (when (member dhaatu '(shRI dRI pRI))
	;Kale #499. Guna is optionally used before weak terminations for
	; 'shRI' , 'dRI', and 'pRI'
	;NOTE: Without this code, the 'guna' substitute is the only one
	; used - this occurs in 'perfect-join1'. To allow the other
	; option, I replace 'RI' with 'r' and have this, along with
        ; the original weak form, as the new weak form
	(let (bwalt)
	 (setq bwalt (vconcat (substring bw 0 -1) [r]))
	 (setq bw (list bw bwalt))
	)
       )
       (setq btab (vector
         b3s bw bw
         bth bw bw
         b1s bw bw)
        )
        (setq bitab (perfect-bitab btab itab dbg))
        ;(fol-msg (format "check2. bitab=%s\n" bitab))
       )
      )
;      ((and (shortsimplevowel-P pc) ; medial short vowel (other than a)
;	    (not (equal types "VC")))
       ((shortsimplevowel-P pc)
       
       (let (bs bth)
        (setq bs (gunate-final-vowel b))
	(setq bth bs)
        (setq btab (vector
          bs bw bw
          bth bw bw
          bs bw bw)
         )
        )
;        (fol-msg (format "short-simple-vowel case: %s\n" dhaatu b bs bw))
        (setq bitab (perfect-bitab btab itab dbg))
       )
       (t
	; default situation, rarely encountered!  (e.g. 'raadh', 'uSh')
;	(fol-msg (format "default: dhaatu=%s\n" dhaatu))
	(let (bs bth)
         (setq bs b)
	 (setq bth b)
         (setq btab (vector
           bs bw bw
           bth bw bw
           bs bw bw)
          )
	 )
	 (setq bitab (perfect-bitab btab itab dbg))
        )
       )
   ;---- atmanepada
    (cond
     ((member lc '(aa e ai o)) ;
      (setq b redup)
      (setq b (substring b 0 -1)) ; all but last char
       (setq btab (vector
         b b b
         b b b
         b b b
        )
       )
      (setq bitab (perfect-bitab btab itab dbg))
     )
     ((or
       (and (equal pc 'a) ; medial 'a'
	   (equal types "CVC")
	   (equal (length (elt parts 0)) 1) ; simple 1st consonant
	   (equal (length (elt parts 2)) 1) ; simple last consonant
           (equal (elt atok 0) (elt redup 0)) ; initial consonant unchanged
	   ; exceptions
           (not (member dhaatu '(jan))) ;Antoine
	   (not (equal (elt atok 0) 'v)) ;Kale #500
	   (not (member dhaatu '(shas dad))) ;Kale #500
	   (not (member dhaatu '(radh jabh))) ;Kale #508, p. 320
	   (not (member dhaatu '(jag))) ; Kale #516. 'jag' used for 'adhi-i'
       )
       (member dhaatu '(bhaj tRI trap phal))
       (and (equal dhaatu 'raadh) (equal upa-syms '(apa)))
       ; Kale#509. Optional forms of 'shranth', 'granth'
       (member dhaatu '(shrath grath)) 
       ; Kale #512. The logic provides optional forms for
       ; these roots
       (member dhaatu '(jRI bhram tras phaN raaj bhraaj bhraash
			bhlaash syam svan))
      )
      ; Antoine2#113.
      ; Roots which have a medial 'a' preceded and followed by a single
      ; consonant, and which keep their initial consonant unchanged in
      ; reduplication - such roots form their perfect as follows:
      ; 1. before weak terminations, there is no reduplication and the
      ; medial 'a' of the root is changed to 'e'.
      (let ()
       (setq bw (copy-sequence atok))
       (if (member (length atok) '(1 2 3))
	 (aset bw 1 'e)
	 (aset bw 2 'e) ; exception: initial consonant is compound for 'trap'
       )
;       (if (equal dhaatu 'tRI) (setq bw [t e r]))
       (cond
	((equal dhaatu 'tRI)
	 (setq bw [t e r])
        )
	((equal dhaatu 'jRI)
	 (setq bw [j e r])
        )
       )
       (setq btab (vector
         bw bw bw
         bw bw bw
         bw bw bw)
       )
       (when (member dhaatu '(jRI bhram tras phaN raaj bhraaj bhraash
			bhlaash syam svan))
        ; the form is optional
        (setq btab (vector
          (list b bw)  (list b bw) (list b bw)
          (list b bw) (list b bw) (list b bw)
          (list b bw) (list b bw) (list b bw))
        )
       )
       (setq bitab (perfect-bitab btab itab dbg))
      )
     )
     ((equal dhaatu 'bhrasj)
      ; Kale 505 p.314
      (setq b (list redup redup2))
      (setq btab (vector
	 b b b
	 b b b
	 b b b
       ))
       (setq bitab (perfect-bitab btab itab dbg))
     )
     (t ; default
      (let ()
       (setq btab (vector
         bw bw bw
         bw bw bw
         bw bw bw)
       )
       (setq bitab (perfect-bitab btab itab dbg))
      )
     )
    )
   )
  )
  (when bitab
   (setq liT-r-bitab (append-if-new liT-r-bitab bitab))
  )
  bitab
 )
)
(defun perfect-bitab-join (bitab endings &optional ans dbg)
 (let (i n y thisans base ending strength e a seT-code bi thisbi thisans1)
  (when dbg
   (fol-msg (format "perfect-bitab-join %s\n" (list bitab endings ans)))
  )
  (setq n (length bitab))
  (if (not ans) (setq ans (make-vector n nil)))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq bi (elt bitab i)) ; a list of base-seTcode pairs
    (setq thisans nil)
    (while bi
     (setq thisbi (car bi))
     (setq bi (cdr bi))
     (setq base (elt thisbi 0))
     (setq seT-code (elt thisbi 1))
     (setq thisans1 (perfect-join base seT-code ending dbg))
     (setq thisans1 (sym-without-space thisans1))
     (setq thisans (append-if-new thisans thisans1))
    )
;    (fol-msg (format "%s: %s (%s %s)\n" i thisans (elt bitab i) ending ))
    (setq thisans (solution thisans)) ; so singletons appear w/o parens
    (aset ans i thisans)
    (setq i (1+ i))
   )
   ans
  )
)
(defun perfect-bitab (btab itab &optional dbg)
 (let (n i ans b c thisans b1 c1 c0)
  (when dbg
   (fol-msg (format "perfect-bitab %s\n" (list btab itab)))
  )
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
  (when nil
   (fol-msg (format "perfect-bitab:\nbtab = %s\nitab = %s\nresult=%s\n" btab itab ans))
  )
  ans
 )
)
(defun perfect-join (base-tok seT-code sup &optional dbg)
 (cond 
  ((listp sup)
   (mapcar (lambda (x) (perfect-join base-tok seT-code x)) sup))
  ((listp base-tok)
   (mapcar (lambda (x) (perfect-join x seT-code sup)) base-tok))
  ((equal seT-code 'veT)
   (mapcar (lambda (x) (perfect-join base-tok x sup)) '(seT aniT))
  )
  (t (perfect-join1 base-tok seT-code sup dbg))
 )
)
(defun perfect-join1 (y seT-code ending0 &optional dbg)
 ; based on 'conjugation-join'
 ; seT-code is either nil, 'seT' or 'aniT'
 (let (ans skiprefs ylast efirst ending y0 ny)
  ; insert 'i' if needed
  (setq ending
   (if (equal seT-code 'seT)
    (conjugation-join [i] ending0)
    ending0
   )
  )
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ny (length y))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq efirst (elt ending 0))
  (when dbg; nil  ;dbg
    (fol-msg (format "y = %s, ending=%s ylast=%s efirst=%s\n" y ending ylast efirst))
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
   ((and (vowel-P efirst) (member ylast '(i ii Ri)))
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
        ((equal ylast 'ii) (fol-msg (format "check\n")) (vconcat y0 [i y] ending))
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
   ((and (vowel-P efirst) (member ylast '(u uu RI)))
    ; 2nd special sandhi rule for perfect (Antoine2#110)
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
    ; 3rd special sandhi rule for perfect (Antoine2#110)
    (setq ans (vconcat y [Dh] (substring ending 1)))
   )
   ((and (equal efirst 'th) (< 2 ny)
	 (equal (substring y -2) [a r]))
    ; this rule so [ch a k a r] + [th a]
    ; becomes [ch a k a r th a] rather than [ch a k a s th a], which
    ; is what 'sandhi-pair' does
    (setq ans (vconcat y ending))
   )
   ((and (equal efirst 'th)
	 (< 2 ny)
	 (member (substring y -2) '([ch Ch] [sh ch] [r j] [k Sh]))
    )
    ; prachCh , vrashch, mRij, akSh
    (setq y0 (substring y 0 -2))
    (if (equal (substring y -2) [r j])
     (setq y0 (substring y 0 -1)))
    (setq ans (vconcat y0 [Sh Th a]))
   )
   ;Kale p. 321. Example of klish
   ((and (equal efirst 'th) (equal ylast 'sh))
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [Sh Th a]))
   )
    ; Kale #506. p. 317
   ((and (equal efirst 'th)
	 (equal y [u v a h]))
    (setq ans [u v o Dh a])
   )
   ;Kale p. 322. Example of 'muh', 'druh', 'snih', 'snuh'
   ((and (equal efirst 'th) (equal ylast 'h))
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [Dh a]))
   )
   ((and (equal efirst 'th) 
	 (member ylast '(j ch)))
    ; this rule [bh a j] + [th a] -> [bh a k th a]
    ; rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
    ; but [bh a ~n j] + [th a] -> [bh a ~N k th a]
;    (fol-msg (format "y=%s ending=%s\n" y ending))
    (cond
     ((equal y [i y a j])
      (setq ans (vconcat [i y a] [Sh] [Th a]))
     )
     ((equal y '[b a bh a ~n j])
      (setq ans (vconcat [b a bh a ~N k] ending))
     )
     ((equal y '[m a m a j j])
      (setq ans (vconcat [m a m a ~N k] ending))
     )
     (t
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [k] ending))
     )
    )
   )
   ((and (equal efirst 'th)
	 (equal ylast 'dh))
    ; so [v i v y a dh] + [th a] -> [v i v y a d dh a]
    ; sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [d] [dh] (substring ending 1)))
   )
   ((and (equal efirst 'th)
	 (equal y [s a s a h]))
    ; Kale #506
    (setq ans [s o Dh a])
   )
   ((and (equal efirst 'th)
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
   ;Kale p. 321 (footnote)
   ;Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
   ;note 'n' may be changed to 'N' by sandhi-single (see below)
   ((and (equal ylast 'm) (member efirst '(m v)))
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [n] ending)) 
   )
   ; Kale p. 321. based on example of 'ash'
   ; 'gaah', 'gRih' also have this change, but they have an
   ; additional change (aspiration of 'g')
   ((and (member ylast '(sh h)) (equal efirst 's))
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [k Sh] (substring ending 1))) 
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
    (when nil
     (fol-msg (format "from sandhi-pair, ans=%s\n" ans))
    )
   )
  )
  (setq ans (or (sandhi-single ans) ans))
;  (if (not (equal option 'not-n-N))
;   (setq ans (or (sandhi-single ans) ans))
;  )
  (setq ans (solution ans))
  (when dbg
   (fol-msg (format "perfect-join1 %s => %s\n" (list y seT-code ending0) ans))
  )
  ans
 )
)
(defun reduplicate-perfect (tok &optional wparts)
 ; unlike 'reduplicate', this returns a list (possibly with
 ; just one element).
 (let (pfxes pfx ans thisans)
  (cond
   ;Antoine2#114 The roots 'vach' (to speak), 'vad' (to speak),
   ;  'vap' (to sow), 'vas' (to dwell), and 'vah' (to carry) reduplicate in
   ;  'u' (e.g. 'uvach') Furthermore, before weak terminations, the radical
   ;  'v' is also changed to 'u', which, together with the 'u' of the 
   ;  reduplication, contracts into 'uu' (e.g., 'uuch'). This routine 
   ;  returns both forms (e.g., '([u v a ch] [uu ch]))
   ; Kale #506, p. 317 : 'vash' 2P to desire
   ((and (equal (elt tok 0) 'v)
	 (member tok '([v a ch] [v a d] [v a p] [v a s] [v a h]
		       [v a sh])))
    (let (ans1 ans2)
     (setq ans1 (vconcat [u] tok))
     (setq ans2 (conjugation-join [uu] (substring tok -1)))
     (if (equal tok [v a s])
      ; otherwise we are joining [uu] [s]. Since 's' is a final 's' (at
      ; this point)
      ; it gets turned into visarga by conjugation-join
      (setq ans2 [uu Sh])
     )
     (setq ans (list ans1 ans2))
    )
   )
   ;Antoine2#115
      ;Antoine2#115. [y a j] (to sacrifice) reduplicates in 'i'.
      ;Before weak terminations, the radical 'y' is also changed to 'i',
      ;which, together with the 'i' of the reduplication, contracts to 'ii'
   ((equal tok [y a j])
    (let (ans1 ans2)
     (setq ans1 (vconcat [i] tok))
     (setq ans2 (conjugation-join [ii] (substring tok -1)))
     (setq ans (list ans1 ans2))
    )
   )
   ;Antoine2#116. The roots 'vyadh' (to pierce), 'svap' (to sleep),
      ; and 'grah' (to seize) reduplicate as follows:
      ; 'vivyadh', 'suShvap', 'jagrah'.  Before weak terminations, the
      ;radical 'y', 'v' and 'r' are changed to 'i', 'u', and 'Ri'
      ;respectively.  The changed of 'y' to 'i', 'v' to 'u', and 'r' to 'Ri'
      ;is called 'samprasaaraNa'
   ((equal tok [v y a dh])
    ;Antoine2#116
    (setq ans (list [v i v y a dh] [v i v i dh]))
   )
   ((equal tok [v y a ch])
    ;Kale 506, p. 317
    (setq ans (list [v i v y a ch] [v i v i ch]))
   )
   ((equal tok [s v a p])
    ;Antoine2#116
    (setq ans (list [s u Sh v a p] [s u Sh u p]))
   )
   ((equal tok [g r a h])
    ;Antoine2#116
    (setq ans (list [j a g r a h] [j a g Ri h]))
   )
   ((equal tok [j y aa])
    ;Kale 306., p. 317.
    (setq ans '([j i j y aa])) ; otherwise, ([j a j y aa])
   )
   ;Antoine2#118. The roots 'chi' (to collect), 'ji' (to conquer), and
   ; 'hi' (to impel) change their radical consonants to a guttural;
   ; namely 'ch'->'k', 'j'->'g', and 'h'->'gh'
   ((equal tok [ch i])
    ; Note: Kale 514 says this is an optional form for 'chi';
    ; the other form is [ch i ch i].  Only the form 'chiki' is implemented
    ; here. It would be awkward to get both forms.
    ; The other form is implemented in an ad-hoc way in conjugation-tab-liT
    (setq ans '([ch i k i]))
   )
   ((equal tok [j i])
    (setq ans '([j i g i]))
   )
   ((equal tok [h i])
    (setq ans '([j i gh i]))
   )
   ;Antoine2#119. The root 'bhuu' takes the irregular base 'babhuuv' and
   ;keeps its long 'uu' throughout the perfect conjugation.
   ; (otherwise, its base would be [b u bh uu])
   ((equal tok [bh uu])
    (setq ans '([b a bh uu v]))
   )
   ((equal tok [v i d])
   ;Antoine2#121. The root 'vid' (to know) forms a perfect without
   ;reduplication, which has present meaning.
    (setq ans '([v i d]))
   )
   ((equal tok [Ri])
    (setq ans '([aa r])) ; Kale 515
   )
   ; Kale 505 p. 314. bhrasj ; previously [b a bh r a s j]
   ((equal tok [bh r a s j])
    (setq ans (list [b a bh r a j j] [b a bh a r j]))
   )
   ; Kale 476. masj.
   ; When followed by any consonant except a nasal or a semi-vowel,
   ;  'n' is inserted before the ending consonant and the 's' is dropped.
   ; Otherwise, the 's' is changed to 'j'.
   ; The parasmaipada 2S without 'i' will have to be changed elsewhere
   ((equal tok [m a s j])
    (setq ans (list [m a m a j j])) 
   )
   ; Kale 505 p. 314. Chid ; previously [ch i Ch i d]
   ((equal tok [Ch i d])
    (setq ans (list [ch i ch Ch i d]))
   )
   ; Kale 505 p. 315. RichCh 6 P (to go).
   ((equal tok [Ri ch Ch])
    (setq ans (list [aa n a r ch Ch])) ; previously ([aa n Ri ch Ch])
   )
   ; Kale 506 p. 319 'vye'.
   ; 'vye' becomes 'vivyay' before strong terminations and 'vivii'
   ; before the weak ones in the Perfect
   ((equal tok [v y e])
    (setq ans (list [v i v y a y] [v i v ii]   ))
   )
   ; Kale 508, p. 320 'kLip'
   ((equal tok [k Li p])
    (setq ans (list [ch a k Li p]))
   )
   ; Kale 508, p. 320 'mRij'
   ; otherwise, ([m a m Ri j])
   ; note two variations for weak endings
   ((equal tok [m Ri j])
    (setq ans '([m a m aa r j] ([m a m aa r j] [m a m Ri j])))
   )
   ; Kale 508, p. 321.
   ; 'radh' and 'jabh' insert a nasal in non-conjugational tenses
   ; when their final is followed by a vowel.
   ; 'radh', however, does not insert the nasal in the Aorist, or
   ; when it takes 'i'; howver, it does insert the nasal in the Perfect
   ((equal tok [r a dh])
    (setq ans '([r a r a n dh]))
   )
   ((equal tok [j a bh])
    (setq ans '([j a j a m bh]))
   )
   ((equal tok [j a g])
    ; Kale 517, p. 327
    ; 'jag' is used as base for Perfect of 'adhi-i'
    (setq ans '([j a g]))
   )
   ((equal tok [uu r N u])
    ; Kale 518, p. 327
    ; 'uurNu' forms its base as 'uurNunu'
    ; its vowel is optionally not gunated before a strong termination
    (setq ans (list (vconcat tok [n u])))
   )
   ((equal tok [d ii])
    ; Kale 520, p. 329.
    ; 'y' is prefixed to vowel weak terminations in the case of
    ; 'dii' (4 A 'to obey).
    (setq ans '([d i d ii y])) ; was ([d i d ii])
   )
   ((equal tok [d e])
    ; Kale 521, p. 329
    ; 'de' (1 A 'to protect') assums as its base the form
    ; 'digi' in the Perfect
    ; NOTE: based on the sample 1S, 1P, and 2P forms, I use 'digyi'
    (setq ans '([d i g y i])) ; was ([d a d e])
   )
   ((equal tok [d y u t])
    ; Kale 522, p. 329
    ; 'dyut' (1 A 'to shine') assumes as its base the form 'didyut'
    (setq ans '([d i d y u t])) ; was ([d u d y u t])
   )
   ((equal tok [v y a th])
    ; Kale 524, p. 329
    ; 'vyath' (1 A 'to suffer') takes samprasaaraNa in the reduplicative
    ; syllable in the Perfect
    (setq ans '([v i v y a th])) ; was ([v a v y a th])
   )
   (t 
    (setq pfxes (reduplicative-pfx-perfect tok wparts))
    (if (not (listp pfxes)) (setq pfxes (list pfxes)))
    (while pfxes
     (setq pfx (car pfxes))
     (setq pfxes (cdr pfxes))
     (if ans
       ; i.e., this is second element of list
       ; only occurs in examples like 'iSh' and 'uSh'
      (progn
      (setq thisans (reduplicate-join pfx tok))
      )
      (progn
       (setq thisans (reduplicate-join pfx tok))
      )
     )
     (setq ans (append-if-new ans thisans))
    )
    ;Antoine2#117. The roots 'jan' (to be born), 'khan' (to dig),
    ;'gam' (to go), 'ghas' (to eat) and 'han' (to kill) drop their
    ;medial 'a' before weak terminations.
    ;The 'h' of 'han' is changed to 'gh' (in weak and strong forms).
    ;The 'j' of 'jan' is changed to 'j~n' (in weak form)
    (when (member tok '([j a n] [g a m] [h a n] [kh a n] [gh a s]))
     (let (ans1 ans2)
      (setq ans1 (car ans))
      (if (equal tok [h a n]) ; [j a gh a n]
	(setq ans1 (vconcat (substring ans1 0 -3) [gh] (substring ans1 -2)))
      )
      (cond
       ((equal tok [j a n])
	(setq ans2 (vconcat (substring ans1 0 -2) [~n]))
       )
       ((equal tok [gh a s])
	(setq ans2 (vconcat (substring ans1 0 -3) [k Sh]))
       )
       (t
	(setq ans2 (vconcat (substring ans1 0 -2) (substring ans1 -1)))
       )
      )
      (setq ans (list ans1 ans2))
     )
    )
   )
  ) 
  ans
 )
)
(defun reduplicative-pfx-perfect (tok &optional wparts)
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
 
 (let (ctok vtok c v ans parts types)
  (when (not wparts)
   (setq wparts (word-parts tok))
  )
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  (cond
   ((member types '("CV" "CVC"))
    (setq ctok (elt parts 0)) ; initial consonant
    (setq vtok (elt parts 1)) ; final or intermedicate vowel
    ; 3. Only 1st letter of initial conjunct consonant is used
    (setq c (elt ctok 0))
    ; 4. But if initial consonant is sibilant + hard cons, then
    ; use the hard cons
    ; 5. A long vowel becomes short in reduplication
    ; 6. Medial 'e' becomes 'i' in reduplication;
    ;    medial 'o' and 'au' become 'u' in reduplcation
    ; 7. Final 'e', 'o', and 'au' become 'a' in reduplication
    ;*** 8. 'Ri' and 'RI' become 'i' in reduplication
    ;8. A radical 'Ri' becomes 'a' in reduplication
    ; NOTE: Not sure whether 'RI' is handled like 'Ri'.
    ; The code assumes so
    ; NOTE2: in 'kLip' (Kale p. 320), 'Li' becomes 'a' in reduplication
    (when (and (= 2 (length ctok))
	       (sibilant-P c)
	       (hard-P (elt ctok 1)))
     (setq c (elt ctok 1))
    )
    ; 1. initial aspirate loses aspiration
;    (setq c (de-aspirate c))
    (when (not (member c '(sh)))
     (setq c (de-aspirate c))
    )
    ; 2. initial guttural changed to palatal. initial 'h' -> 'j'
    (cond
     ((equal c 'h) (setq c 'j))
     ((guttural-P c)
      (setq c (corresponding-letter c guttural-set palatal-set))
     )
    )
    (setq v (elt vtok 0))
    ; 5. shorten the vowel
    (setq v (shorten-vowel v))
    (cond
     ((and (equal types "CVC") (equal v 'e)) ; medial 'e' -> 'i'
      (setq v 'i)
     )
     ((and (equal types "CVC") (member v '(o au))) ; medial 'o', 'au' -> 'u'
      (setq v 'u)
     )
     ((and (equal types "CV") (member v '(e ai o au))) ; final e,o,au->a
      (setq v 'a)
     )
     ((equal v 'Ri) ; Ri , RI -> a
      (setq v 'a)
     )
    )
    (setq ans (vector c v))
   )
   ((member types '("V" "VC"))
    ; 8. Initial 'Ri' becomes 'aan' in reduplication
    ; (Note: The word 'Ri' is an exception, handled in 'reduplicate-perfect')
    ; For the conj-7 verb 'Ri', we return [i y] (???)
    ; Initial 'a' followed by a simple consonant becomes 'aa'
    ; Initial 'a' followed by a conjunct consonant becomes 'aan'
    ; Initial 'i' and 'u' may reduplicate in 'ii' and 'uu' or
    ; in 'iy' and 'uv'. Both are returned
    (setq vtok (elt parts 0))
    (setq v (elt vtok 0)) ; initial  vowel
    (cond
     ((member v '(Ri RI)) ; Ri, RI ->a
      (setq ans [aa n])
     )
     ((and (equal v 'a) (equal types "VC"))
      (setq ctok (elt parts 1)) ; final consonant
      (cond
       ((equal ctok [sh])
	(setq ans [aa n])
       )
       ((equal (length ctok) 1)
	(setq ans [aa])
       )
       (t
	(setq ans [aa n])
       )
      )
     )
     ((equal tok [i])
      (setq ans (list [ii] [i y a]))
     )
     ((equal v 'i)
      (setq ans (list [i y a] [ii] ))
     )
     ((equal v 'u)
      (setq ans (list [u v a] [uu] ))
     )
     (t  (setq ans (vector v)))
    )
   )
   (t ; multisyllabic roots: use same as perfect
;   (fol-msg (format "reduplicate: unexpected form: %s %s %s\n"
;		     tok parts types))
    (setq ans (reduplicative-pfx tok wparts))
   )
  )
  ans
 )
)
(defun kale-463-P (dhaatu)
 (kuTaadi-P dhaatu)
)
(defun kuTaadi-P (dhaatu)
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
 (if (member dhaatu
   '(kuT puT kuch kuj dhur sphuT truT luT sphur
     gur nu du ku))
  t
 )
)

(defun conjugation-tab-liT-p (upa-syms class pada dhaatu &optional voice dbg)
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

 (let (ans base sfxtab n i xar yar j m sfxpada)
  (when dbg
   (fol-msg (format "periphrastic-liT-p %s\n" (list upa-syms class pada dhaatu  voice)))
  )
  (setq sfxpada (if (equal voice 'PASSIVE) 'PASSIVE pada))
  (setq sfxtab (periphrastic-suffix sfxpada))
  (setq base (periphrastic-base dhaatu class pada))
  (when nil ; dbg
   (fol-msg (format "%s %s %s: sfxtab=%s\nbase=%s\n"
		   dhaatu class pada sfxtab base))
  )
  (when (and base  sfxtab)
   (setq n (length sfxtab))
   (setq ans (make-vector n nil))
   (setq i 0)
   (while (< i n)
    (setq xar (elt sfxtab i)) ; a list
    (setq yar (mapcar
     (lambda (x)
      (mapcar
       (lambda (b) (conjugation-join-sym b x))
       base
      )
     )
     xar
    ))
    (setq yar (flatten yar))
    (aset ans i yar)
    (setq i (1+ i))
   )
  )
  ans
 )
)
(defun periphrastic-base (dhaatu class pada &optional dtype)
 ; returns a symbol list
 (let (ans)
  (cond
   ((member (format "%s" dtype) '("c"))
    (setq ans (causal-bases-gentense
    dhaatu class pada nil 'liT-p 'ACTIVE))
   )
   ((member class '(10 1 4 6))
    (setq ans (construct-conjbase1a dhaatu class pada nil)) ; symbol list
   )
   ((member dhaatu '(bhii hrii bhRi hu))
    (setq ans (reduplicate dhaatu)) ; a symbol
   )
   (t ; a non-a conjugation verb, other than one of the 4 specials above
    (setq ans dhaatu)
   )
  )
  
  (when (and ans (not (equal ans 'vid)) (symbolp ans))
   (let (tok)
    (setq tok (car (ITRANS-parse-words-1 (symbol-name ans))))
    (setq ans (sym-without-space (gunate-final-vowel tok)))
   )
  )
  (when (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defvar periphrastic-suffix-P nil)
(defvar periphrastic-suffix-A nil)
(defvar periphrastic-suffix-PASSIVE nil)
(defun periphrastic-suffix (pada)
 (let (ans)
  (cond
   ((equal pada 'P)
    (if (not periphrastic-suffix-P)
	(setq periphrastic-suffix-P (periphrastic-init pada)))
    (setq ans periphrastic-suffix-P)
   )
   ((equal pada 'A)
    (if (not periphrastic-suffix-A)
	(setq periphrastic-suffix-A (periphrastic-init pada)))
    (setq ans periphrastic-suffix-A)
   )
   ((equal pada 'PASSIVE)
    (if (not periphrastic-suffix-PASSIVE)
	(setq periphrastic-suffix-PASSIVE (periphrastic-init pada)))
    (setq ans periphrastic-suffix-PASSIVE)
   )
  )
  ans
 )
)
(defun periphrastic-init (pada)
 (let (ans ans1 ans2 ans3)
  (cond
   ((equal pada 'P)
    (setq ans1 (periphrastic-init1 2 pada 'as))
    (setq ans2 (periphrastic-init1 8 pada 'kRi))
    (setq ans3 (periphrastic-init1 1 pada 'bhuu))
   )
   ((equal pada 'A)
    (setq ans1 (periphrastic-init1 2 'P 'as))
    (setq ans2 (periphrastic-init1 8 pada 'kRi))
    (setq ans3 (periphrastic-init1 1 'P 'bhuu))
   )
   ((equal pada 'PASSIVE)
    (setq ans1 (periphrastic-init1 2 'A 'as))
    (setq ans2 (periphrastic-init1 8 'A 'kRi))
    (setq ans3 (periphrastic-init1 1 'A 'bhuu))
   )
  )
  (setq ans (join-arrays ans1 ans2))
  (setq ans (join-arrays ans ans3))
  ans
 )
)
(defun periphrastic-init1 (class pada dhaatu)
 (let (ans n i x y)
  (setq ans (conjugation-tab-liT-r nil class pada dhaatu))
  (setq n (length ans))
  (setq i 0)
  (while (< i n)
   (setq x (elt ans i))
;   (setq y (conjugation-join-sym 'aam x))
   (if (not (listp x)) (setq x (list x)))
   (setq y (mapcar (lambda (x1) (conjugation-join-sym 'aam x1)) x))
   (aset ans i y)
   (setq i (1+ i))
  )
  ans
 )
)
(defun reduplicative-liT-P (dhaatu class)
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
 (let (ans tok wparts types)
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq wparts (word-parts tok))
  (setq types (elt wparts 1))
  (cond
   ((member dhaatu '(uurNu RichCh)) (setq ans t))
   ((member dhaatu '(uSh vid bhii bhRi hRi hrii jaagRi daridraa))
    (setq ans t))
   ((not (member class '(1 2 3 4 5 6 7 8 9))) (setq ans nil))
   ((member (elt tok 0) '(ii uu RI LI)) (setq ans nil))
   ((not (member types '("CV" "CVC" "V" "VC"))) (setq ans nil))
   ((member dhaatu '(day kaas aas)) (setq ans nil))
   (t (setq ans t))
  )
  ans
 )
)
(defun periphrastic-liT-P (dhaatu class &optional dtype dbg)
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
 (let (ans tok wparts types)
  (when dbg
   (fol-msg (format "periphrastic-liT-P %s\n" (list dhaatu class)))
  )
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq wparts (word-parts tok))
  (setq types (elt wparts 1))
  (cond
   (dtype (setq ans t)) ; derived types take the periphrasitc perfect
   ((member dhaatu '(uurNu RichCh)) (setq ans nil))
   ((member dhaatu '(uSh vid bhii bhRi hRi hrii jaagRi daridraa))
    (setq ans t))
   ((equal class 10) (setq ans t))
   ((member (elt tok 0) '(ii uu RI LI)) (setq ans t))
   ((not (member types '("CV" "CVC" "V" "VC"))) (setq ans t))
   ((member dhaatu '(day kaas aas)) (setq ans t))
  )
  ans
 )
)

(defun unused-perfect-gunate-final-vowel (tok &optional vriddhi-flag)
 ; same as aorist-gunate-final-vowel
 (let (ans m  v v1 j)
  (setq ans tok)
  (setq m (length tok))
  (setq j m)
  (while (< 0 j)
   (setq j (1- j))
   (setq v (elt tok j))
   (when (vowel-P v)
    (if (and vriddhi-flag (equal v 'e))
;     (setq v1 (vrddhi1 v)) ; e->ai
     (setq v1 (vrddhi v))
     (setq v1 (if vriddhi-flag (vrddhi v) (guna v)))
    )
    (setq ans (vconcat (substring tok 0 j) v1 (substring tok (1+ j) m)))
    (setq j 0) ; end loop
   )
  )
  (when nil
   (fol-msg (format "%s %s -> %s (%s)\n" tok vriddhi-flag ans
		(gunate-final-vowel tok vriddhi-flag)   ))
  )
;  (setq ans (gunate-final-vowel tok vriddhi-flag))
  ans
 )
)

(defun perf-part-active-r (dhaatu class pada upa-syms)
 (let (ans ctab  ctab1 ctab2 tok stok wtok s w x  n i lc bitab1 bit1
       base seT-code)
  ; get ctab, so list liT-r-bitab is computed
  (setq ctab (conjugation-tab-liT-r upa-syms  class pada dhaatu))
  (cond 
   ((equal pada 'P)
    ; ctab1 is for the strong form (which ends in 'vas')
    ; ctab2 is for the weak form (used before vowel endings)
    (setq ctab1 (elt ctab 7)) ; 1st dual. Ends in 'va'
    (if (not (listp ctab1)) (setq ctab1 (list ctab1)))
    (setq bitab1 (elt (car liT-r-bitab) 7)) ;1D
    (setq ctab2 (elt ctab 2)) ; 3P. Ends in 'uH'
    (if (not (listp ctab2)) (setq ctab2 (list ctab2)))
    (setq n (length ctab1)) ; assume length of ctab2 also = n
    (setq i 0)
    (while (< i n)
     ; get 's'
     (setq x (elt ctab1 i))
     (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
     (setq bit1 (elt bitab1 i))
     (setq base (elt bit1 0)) ; tok array
     (setq seT-code (elt bit1 1))
     (cond       
      ((and (equal seT-code 'seT)
	    (member (elt (substring base -1) 0) '(i ii Ri))
       )
       (if (equal (length base) 1) ; like root='i'
	(setq stok (vconcat tok [s]))
        (setq stok (vconcat base [v a s])) ; like root='nii'
       )
      )
      (t
       (setq stok (vconcat tok [s])) ; append 's'
      )
     )
     (setq s (sym-without-space stok))
     ; get 'w'
     (setq x (elt ctab2 i))
     (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
     (setq tok (substring tok 0 -1))
     ; normally append 'Sh', but if last letter is 'a' or 'aa', use 's'
     (setq lc (elt (substring tok -1) 0))
     (cond
      ((member lc '(a aa))
       (setq wtok (vconcat tok [s])) ;H->s
      )
      (t
       (setq wtok (vconcat tok [Sh])) ;H->Sh
      )
     )
     (setq w (sym-without-space wtok))
     (setq ans (append-if-new ans (list s w)))
     ; next iterate
     (setq i (1+ i))
    )
    ;--- excpetions
    (cond
     ((equal dhaatu 'bha~nj)
      (setq ans '((babha~njvas babha~njuSh)))
     )
     ((equal dhaatu 'bhind)
      (setq ans '((bibhidvas bibhiduSh)))
     )
     ((equal dhaatu 'a~nj)
      (setq ans '((aajivas aajuSh)))
     )
    )
   )
   ((equal pada 'A)
    ;Antoine#125(4) The perfect participle active atmanepada ('kaanach')
    ; is formed by added 'aana' to the weak base, exactly as the
    ; 3D termination  'aate' is added.
    (setq ctab1 (elt ctab 1)) ; 3D
    (if (not (listp ctab1)) (setq ctab1 (list ctab1)))
    (setq n (length ctab1))
    (setq i 0)
    (while (< i n)
     ; get 's'
     (setq x (elt ctab1 i))
     (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
     (setq stok (declension-join (substring tok 0 -2) [n a]))
     (setq s (sym-without-space stok))
     (setq ans (append-if-new ans s))
     (setq i (1+ i))
    )
   )
  )
  ans
 )
)

(defun old-periphrastic-base (dhaatu class pada)
 ; 
 (let (ans)
  (cond
   ((member class '(10 1 4 6))
    (let (tok-list tok)
     (setq tok-list (class-a-base dhaatu class pada))
     ; just use 1st one. Shld this be changed?
     ; ((equal dhaatu 'kRip) (setq ans '(kRipay kRipaay)))
     (setq tok (car tok-list)) 
     (setq ans (sym-without-space tok)) ; a symbol
    )
   )
   ((member dhaatu '(bhii hrii bhRi hu))
    (setq ans (reduplicate dhaatu)) ; a symbol
   )
   (t ; a non-a conjugation verb, other than one of the 4 specials above
    (setq ans dhaatu)
   )
  )
  (when (and ans (not (equal ans 'vid)) (symbolp ans))
   (let (tok)
    (setq tok (car (ITRANS-parse-words-1 (symbol-name ans))))
    (setq ans (sym-without-space (gunate-final-vowel tok)))
   )
  )
  ans
 )
)
