; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; irreg.el  
; begun 02-05-03 by ejf

(defun subanta-irreg (subanta form Eng-def val)
; (fol-msg (format "subanta-irreg: %s %s %s\n" subanta form Eng-def))
 (sanput2 'Subanta-irreg (list subanta form) val)
 t
)

(defun dhaatu-irreg (dhaatu form Eng-def val)
; (fol-msg (format "dhaatu-irreg: %s %s %s\n" dhaatu form Eng-def))
 (sanput2 'Dhaatu-irreg (list dhaatu form) val)
 t
)
(defvar nil-declension (make-vector 24 nil))
(defvar nil-conjugation (make-vector 9 nil))
(defun declension-M-a-IRR (vstem cstem1 &optional cstem2)
 "AntoineII.1 Some masculine nouns in 'a', besides their regular
  declension, have, from the acc. pl. onward (but not vocative),
  optional forms borrowed from a stem ending with a consonant."
 (let (dtab-v dtab-c dtab i n i1 i2)
  ;(fol-msg (format "declension-M-a-IRR: %s %s %s\n" vstem cstem1 cstem2))
  (setq dtab-v (declension-general vstem 'M 'a nil))
  (if (not cstem2)
   (setq dtab-c (declension-general-1cons cstem1 cstem1 'M  '1cons nil))
   (setq dtab-c
    (declension-general-1cons cstem1 (list cstem1 cstem2) 'M  '1cons nil))
  )
;  (fol-msg (format "dtab-c=%s\n" dtab-c))
  (setq n (length dtab-v))
  (setq dtab (copy-sequence dtab-v))
  (setq i1 5) ; acc pl
  (setq i2 20) ; loc pl
  (setq i i1)
  (while (<= i i2)
   (aset dtab i (list (elt dtab-v i) (elt dtab-c i)))
   (setq i (1+ i))
  )
  dtab
 )
)
(defun declension-F-aa-IRR (vstem cstem1 &optional cstem2)
 "AntoineII.4 Some feminine nouns in 'aa', besides their regular
  declension, have, from the acc. pl. onward (but not vocative),
  optional forms borrowed from a stem ending with a consonant."
 (let (dtab-v dtab-c dtab i n i1 i2)
  (setq dtab-v (declension-general vstem 'F 'aa nil))
  (if (not cstem2)
   (setq dtab-c (declension-general-1cons cstem1 cstem1 'F  '1cons nil))
   (setq dtab-c
    (declension-general-1cons cstem1 (list cstem1 cstem2) 'F  '1cons nil))
  )
  (setq n (length dtab-v))
  (setq dtab (copy-sequence dtab-v))
  (setq i1 5) ; acc pl
  (setq i2 20) ; loc pl
  (setq i i1)
  (while (<= i i2)
   (aset dtab i (list (elt dtab-v i) (elt dtab-c i)))
   (setq i (1+ i))
  )
  dtab
 )
)
(defun declension-F-aa1-IRR (vstem cstem1 &optional cstem2)
 "AntoineII.5 'jaraa' has, in addition to its forms as a feminine
 noun ending in 'aa', optional forms borrowed from a stem ending
 with a consonant. These forms (a) are present from the nominative dual
 through the locative plural and (b) occur only before terminations
 beginning with a vowel. Based on the normal masculine (and fem) 
 endings, (see 'sup-M-normal' in gram3.el), the optional forms occur
 in cases 1D 1P 2S 2D 2P 3S 4S 5S 6S 6D 6P 7S 7D.
 11-22-03: Deshpande (p. 148) shows 8D and 8P with alternate forms, like 1D/1P
 Since Antoine does not explicitly show these forms, I follow Deshpande
 and include the alternates. (Kale p. 82 agrees with Deshpande)
"
 (let (dtab-v dtab-c dtab i n i1 i2 sups sup)
  (setq dtab-v (declension-general vstem 'F 'aa nil))
  (if (not cstem2)
   (setq dtab-c (declension-general-1cons cstem1 cstem1 'F  '1cons nil))
   (setq dtab-c
    (declension-general-1cons cstem1 (list cstem1 cstem2) 'F  '1cons nil))
  )
  (setq n (length dtab-v))
  (setq dtab (copy-sequence dtab-v))
  (setq sups (sup-get 'F 'normal))
  (setq i1 1) ; nom dual
  (setq i2 23) ; voc pl
  (setq i i1)
  (while (<= i i2)
   (when (not (equal i 21)) ; exclude vocative sing
    (setq sup (elt sups i))
    (if (vowel-P (elt sup 0))
     (aset dtab i (list (elt dtab-v i) (elt dtab-c i)))
    )
   )
   (setq i (1+ i))
  )
  dtab
 )
)
(defun declension-M-aa1-IRR (vstem cstem1 &optional cstem2)
 "11-22-03: Deshpande (p. 148) shows 'nirjara' (m.) declined 
 with differences similar to that of 'jaraa'
"
 (let (dtab-v dtab-c dtab i n i1 i2 sups sup)
  (setq dtab-v (declension-general vstem 'M 'a nil))
  (if (not cstem2)
   (setq dtab-c (declension-general-1cons cstem1 cstem1 'M  '1cons nil))
   (setq dtab-c
    (declension-general-1cons cstem1 (list cstem1 cstem2) 'M  '1cons nil))
  )
  (setq n (length dtab-v))
  (setq dtab (copy-sequence dtab-v))
  (setq sups (sup-get 'M 'normal))
  (setq i1 1) ; nom dual
  (setq i2 23) ; voc pl
  (setq i i1)
  (while (<= i i2)
   (when (not (equal i 21)) ; exclude vocative sing
    (setq sup (elt sups i))
    (if (vowel-P (elt sup 0))
     (aset dtab i (list (elt dtab-v i) (elt dtab-c i)))
    )
   )
   (setq i (1+ i))
  )
  dtab
 )
)
(defun declension-N-i1-IRR (citation-sym )
; note: citation-sym Is used (e.g. 'asthi')
; and praatipadikas is computed from citation-sym:
;  e.g. 'asthi' -> 'asth'
; Two declensions are computed:
; (1) (declension-general-cons 'asthan 'asth 'N 'an nil)
; (2) (declension-general 'asth 'N 'i nil)
; The first declension is used for positions 3S through 7P
; whose normal endings begin with vowels 
 (let (procname ans sups n d1 d2 p i praatipadikas gender form )
  (setq procname (format "declension-N-i1-IRR"))
  (setq gender 'N)
  (setq form 'i1)
  (let (citation-tok sym1 tok1)
   (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
   (setq tok1 (substring citation-tok 0 -1)) ; drop 'i'
   (setq sym1 (sym-without-space tok1))
   (setq praatipadikas (list sym1))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   praatipadikas ; should not be nil
   (or (not (listp praatipadikas)) (= (length praatipadikas) 1))
   (setq p (solution praatipadikas)) ; asth
   (let (csym)
    (setq csym (sym-without-space (vector p 'an))) ; asthan
    (setq d1 (declension-general-cons csym p 'N 'an nil))
   )
   (setq d2 (declension-general p 'N 'i nil))
   
   (let (i sup i1 i2)
    (setq i1 6) ; case 3S
    (setq i2 20) ; case 7S
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (aset ans i
      (if (and (<= i1 i) (<= i i2) (vowel-P (elt sup 0)))
       (elt d1 i)
       (elt d2 i)
      )
     )
     (setq i (1+ i))
    )
    t ; so and succeeds
   )
  )
  ans
 )
)
(defun irregs-init ()
 "Initialize all irregular declensions, etc"

(subanta-irreg 'asmad 'M-IRR-PRON 'I
; asmad M (there is actually no gender distinction)
; The optional forms (maa, me, nau, naH) should not be used
; 1. at the beginning of a sentence
; 2. in connection with cha (and), vaa (or), and eva (just, indeed)
  [aham  (aavaam aavam) vayam  ; 'aavam' is Vedic (P.Scharf) 12-29-03
  (maam maa)  (aavaam nau)  (asmaan naH)
   mayaa  aavaabhyaam  asmaabhiH
  (mahyam me)  (aavaabhyaam nau)  (asmabhyam naH)
  mat  aavaabhyaam  asmat
  (mama me)  (aavayoH nau)  (asmaakam naH)
  mayi  aavayoH  asmaasu
  nil nil nil] 
)
(subanta-irreg 'yuShmad 'M-IRR-PRON 'you
; yuShmad M (there is actually no gender distinction)
; The optional forms (tvaa, tee, vaam, vaH) should not be used
; 1. at the beginning of a sentence
; 2. in connection with cha (and), vaa (or), and eva (just, indeed)
 [tvam  (yuvaam yuvam)  yuuyam ; 'yuvam'  is Vedic (P.Scharf) 12-29-03
 (tvaam tvaa)  (yuvaam vaam)  (yuShmaan vaH)
 tvayaa  yuvaabhyaam  yuShmaabhiH
 (tubhyam te)  (yuvaabhyaam vaam)  (yuShmabhyam vaH)
 tvat  yuvaabhyaam  yuShmat
 (tava te)  (yuvayoH vaam)  (yuShmaakam vaH)
 tvayi  yuvayoH  yuShmaasu
 nil nil nil]
)
(subanta-irreg 'idam 'M-IRR-PRON 'this
 [ayam  imau  ime
 (imam enam)  (imau enau)  (imaan enaan)
 (anena enena)  aabhyaam  ebhiH
 asmai  aabhyaam  ebhyaH
 asmaat  aabhyaam  ebhyaH
 asya  (anayoH enayoH)  eShaam
 asmin  (anayoH enayoH)  eShu
  nil nil nil]
)
(subanta-irreg 'idam 'F-IRR-PRON 'this
 [ iyam  ime  imaaH
 (imaam enaam)  (ime ene)  (imaaH enaaH)
 (anayaa enayaa)  aabhyaam  aabhiH
 asyai  aabhyaam  aabhyaH
 asyaaH  aabhyaam  aabhyaH
 asyaaH  (anayoH enayoH)  aasaam
 asyaam  (anayoH enayoH)  aasu
 nil nil nil
 ]
)
(subanta-irreg 'idam 'N-IRR-PRON 'this
 [idam  ime  imaani
 (idam enat)  (ime ene)  (imaani enaani)
 (anena enena)  aabhyaam  ebhiH
 asmai  aabhyaam  ebhyaH
 asmaat  aabhyaam  ebhyaH
 asya  (anayoH enayoH)  eShaam
 asmin  (anayoH enayoH)  eShu
 nil nil nil
]
)
(subanta-irreg 'adas 'M-IRR-PRON 'that
; amii ( M 1 P of adas) follows a special sandhi rule:
; The final 'ii' of 'amii' never combines with following vowels.
 [asau  amuu  amii
  amum  amuu  amuun
  amunaa  amuubhyaam  amiibhiH
  amuShmai  amuubhyaam  amiibhyaH
  amuShmaat  amuubhyaam  amiibhyaH
  amuShya  amuyoH  amiiShaam
  amuShmin  amuyoH  amiiShu
  nil nil nil
 ]
)
(subanta-irreg 'adas 'N-IRR-PRON 'that
 [adaH  amuu  amuuni
  adaH  amuu  amuuni
  amunaa  amuubhyaam  amiibhiH
  amuShmai  amuubhyaam  amiibhyaH
  amuShmaat  amuubhyaam  amiibhyaH
  amuShya  amuyoH  amiiShaam
  amuShmin  amuyoH  amiiShu
  nil nil nil
 ]
)
(subanta-irreg 'adas 'F-IRR-PRON 'that
 [asau  amuu  amuuH
  amuum  amuu  amuuH
  amuyaa  amuubhyaam  amuubhiH
  amuShyai  amuubhyaam  amuubhyaH
  amuShyaaH  amuubhyaam  amuubhyaH
  amuShyaaH  amuyoH  amuuShaam
  amuShyaam  amuyoH  amuuShu
  nil nil nil
 ]
)
(subanta-irreg 'tad 'M-b-PRON 'that
; saH (and eShaH - see etad M below) follow a special sandhi rule:
; saH and eShaH drop their visarga before ANY consonant and
; before ANY vowel except 'a'.
; Before 'a', they become 'so' and 'eSho' while the following 'a' is
; elided
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'saH)
  ans
 )
)
(subanta-irreg 'lakShmii 'F-ii 'Lakshmi
  ;Kale p.42
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'lakShmiiH)
  ans
 )
)
(subanta-irreg 'tantrii 'F-ii 'lute
 ;Kale p.42
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'tantriiH)
  ans
 )
)
(subanta-irreg 'tarii 'F-ii 'boat
 ;Kale p.42
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'tariiH)
  ans
 )
)
(subanta-irreg 'avii 'F-ii 'woman
 ;Kale p.42
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'aviiH)
  ans
 )
)
(subanta-irreg 'tad 'F-b-PRON 'that
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'saa)
  ans
 )
)
(subanta-irreg 'tyad 'M-b-PRON 'that
 ; Kale 135, p. 91
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'syaH)
  ans
 )
)
(subanta-irreg 'tyad 'F-b-PRON 'that
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'syaa)
  ans
 )
)
(subanta-irreg 'etad 'M-b-PRON 'this
; saH (and eShaH - see etad M below) follow a special sandhi rule:
; saH and eShaH drop their visarga before ANY consonant and
; before ANY vowel except 'a'.
; Before 'a', they become 'so' and 'eSho' while the following 'a' is
; elided
; Kale 135, p. 91. etad has several optional forms 
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'eShaH)
  (aset ans 3 '(etam enam))
  (aset ans 4 '(etau enau))
  (aset ans 5 '(etaan enaan))
  (aset ans 6 '(etena enena))
  (aset ans 16 '(etayoH enayoH))
  (aset ans 19 '(etayoH enayoH))
  ans
 )
)
(subanta-irreg 'etad 'F-b-PRON 'this
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'eShaa)
  (aset ans 3 '(etaam enaam))
  (aset ans 4 '(ete ene))
  (aset ans 5 '(etaaH enaaH))
  (aset ans 6 '(etayaa enayaa))
  (aset ans 16 '(etayoH enayoH))
  (aset ans 19 '(etayoH enayoH))
  ans
 )
)
(subanta-irreg 'etad 'N-b-PRON 'this
; Kale 135, p. 91. etad has several optional forms 
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 3 '(etat enat))
  (aset ans 4 '(ete ene))
  (aset ans 5 '(etaani enaani))
  (aset ans 6 '(etena enena))
  (aset ans 16 '(etayoH enayoH))
  (aset ans 19 '(etayoH enayoH))
  ans
 )
)
(subanta-irreg 'kim 'N-b-PRON 'who
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'kim)
  (aset ans 3 'kim)
  ans
 )
)

(subanta-irreg 'svasRi 'F-Ri-A 'sister
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 5 'svasRIH)
  ans
 )
)
(subanta-irreg 'nRi 'M-Ri-R 'man
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 17 '(nRINaam nRiNaam))
  ans
 )
)
; (subanta-irreg 'eka 'M-IRR-PRON 'one
;  [ekaH nil nil
;   ekam nil nil
;   ekena nil nil
;   ekasmai nil nil
;   ekasmaat nil nil
;   ekasya nil nil
;   ekasmin nil nil
;   nil nil nil
;  ]
; )
; (subanta-irreg 'eka 'F-IRR-PRON 'one
;  [ekaa nil nil
;   ekaam nil nil
;   ekayaa nil nil
;   ekasyai nil nil
;   ekasyaaH nil nil
;   ekasyaaH nil nil
;   ekasyaam nil nil
;   nil nil nil
;  ]
; )
; (subanta-irreg 'eka 'N-IRR-PRON 'one
;  [ekam nil nil
;   ekam nil nil
;   ekena nil nil
;   ekasmai nil nil
;   ekasmaat nil nil
;   ekasya nil nil
;   ekasmin nil nil
;   nil nil nil
;  ]
; )

(subanta-irreg 'avayaaj 'M-1cons 'priest
    ; Kale #103, p. 62. The word 'avayaaj' (a kind of priest) changes
    ; final to 's' before consonantal terminations, the preceding
    ; 'aa' becoming 'a'.
 [avayaaH avayaajau avayaajaH
 avayaajam avayaajau avayaajaH
 avayaajaa avayobhyaam avayobhiH
 avayaaje avayobhyaam avayobhyaH
 avayaajaH avayobhyaam avayobhyaH
 avayaajaH avayaajoH avayaajaam
 avayaaji avayaajoH avayassu
 avayaaH avayaajau avayaajaH]
)
(subanta-irreg 'puroDaash 'M-1cons 'offering
    ; Kale #103, p. 62. The word 'puroDaash' (sacrificial food) changes
    ; final to 's' before consonantal terminations, the preceding
    ; 'aa' becoming 'a'.
 [puroDaaH puroDaashau puroDaashaH
 puroDaasham puroDaashau puroDaashaH
 puroDaashaa puroDobhyaam puroDobhiH
 puroDaashe puroDobhyaam puroDobhyaH
 puroDaashaH puroDobhyaam puroDobhyaH
 puroDaashaH puroDaashoH puroDaashaam
 puroDaashi puroDaashoH puroDassu
 puroDaaH puroDaashau puroDaashaH]
)
(subanta-irreg 'dvi 'M-IRR-PRON 'two
 [nil dvau nil
  nil dvau nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvayoH nil
  nil dvayoH nil
  nil nil nil
 ]
)
(subanta-irreg 'dvi 'F-IRR-PRON 'two
 [nil dve nil
  nil dve nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvayoH nil
  nil dvayoH nil
  nil nil nil
 ]
)
(subanta-irreg 'dvi 'N-IRR-PRON 'two
 [nil dve nil
  nil dve nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvaabhyaam nil
  nil dvayoH nil
  nil dvayoH nil
  nil nil nil
 ]
)
(subanta-irreg 'tri 'M-IRR-PRON 'three
 [nil nil trayaH
  nil nil triin
  nil nil tribhiH
  nil nil tribhyaH
  nil nil tribhyaH
  nil nil trayaaNaam
  nil nil triShu
  nil nil nil
 ]
)
(subanta-irreg 'tri 'F-IRR-PRON 'three
 [nil nil tisraH
  nil nil tisraH
  nil nil tisRibhiH
  nil nil tisRibhyaH
  nil nil tisRibhyaH
  nil nil tisRiNaam
  nil nil tisRiShu
  nil nil nil
 ]
)
(subanta-irreg 'tri 'N-IRR-PRON 'three
 [nil nil triiNi
  nil nil triiNi
  nil nil tribhiH
  nil nil tribhyaH
  nil nil tribhyaH
  nil nil trayaaNaam
  nil nil triShu
  nil nil nil
 ]
)

(subanta-irreg 'chatur 'M-IRR-PRON 'four
 [nil nil chatvaaraH
  nil nil chaturaH
  nil nil chaturbhiH
  nil nil chaturbhyaH
  nil nil chaturbhyaH
  nil nil chaturNaam
  nil nil chaturShu
  nil nil nil
 ]
)
(subanta-irreg 'chatur 'F-IRR-PRON 'four
 [nil nil chatasraH
  nil nil chatasraH
  nil nil chatasRibhiH
  nil nil chatasRibhyaH
  nil nil chatasRibhyaH
  nil nil chatasRiNaam
  nil nil chatasRiShu
  nil nil nil
 ]
)
(subanta-irreg 'chatur 'N-IRR-PRON 'four
 [nil nil chatvaari
  nil nil chatvaari
  nil nil chaturbhiH
  nil nil chaturbhyaH
  nil nil chaturbhyaH
  nil nil chaturNaam
  nil nil chaturShu
  nil nil nil
 ]
)

(subanta-irreg 'ShaSh 'M-IRR-PRON 'six
 [nil nil ShaT
  nil nil ShaT
  nil nil ShaDbhiH
  nil nil ShaDbhyaH
  nil nil ShaDbhyaH
  nil nil ShaNNaam
  nil nil ShaTsu
  nil nil nil
 ]
)
(subanta-irreg 'ShaSh 'F-IRR-PRON 'six
 [nil nil ShaT
  nil nil ShaT
  nil nil ShaDbhiH
  nil nil ShaDbhyaH
  nil nil ShaDbhyaH
  nil nil ShaNNaam
  nil nil ShaTsu
  nil nil nil
 ]
)
(subanta-irreg 'ShaSh 'N-IRR-PRON 'six
 [nil nil ShaT
  nil nil ShaT
  nil nil ShaDbhiH
  nil nil ShaDbhyaH
  nil nil ShaDbhyaH
  nil nil ShaNNaam
  nil nil ShaTsu
  nil nil nil
 ]
)
(subanta-irreg 'aShTan 'M-IRR-PRON 'eight
 [nil nil (aShTa aShTau)
  nil nil (aShTa aShTau)
  nil nil (aShTabhiH aShTaabhiH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil aShTaanaam
  nil nil (aShTasu aShTaasu)
  nil nil nil
 ]
)
(subanta-irreg 'aShTan 'F-IRR-PRON 'eight
 [nil nil (aShTa aShTau)
  nil nil (aShTa aShTau)
  nil nil (aShTabhiH aShTaabhiH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil aShTaanaam
  nil nil (aShTasu aShTaasu)
  nil nil nil
 ]
)
(subanta-irreg 'aShTan 'N-IRR-PRON 'eight
 [nil nil (aShTa aShTau)
  nil nil (aShTa aShTau)
  nil nil (aShTabhiH aShTaabhiH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil (aShTabhyaH aShTaabhyaH)
  nil nil aShTaanaam
  nil nil (aShTasu aShTaasu)
  nil nil nil
 ]
)
(subanta-irreg 'paada 'M-a 'foot
 (declension-M-a-IRR 'paad 'pad)
)
(subanta-irreg 'danta 'M-a 'tooth
 (declension-M-a-IRR 'dant 'dat)
)
(subanta-irreg 'maasa 'M-a 'month
 (declension-M-a-IRR 'maas 'maas 'maaH)
)
(subanta-irreg 'nishaa 'F-aa 'night
 (declension-F-aa-IRR 'nish 'nish 'niD)
)
(subanta-irreg 'naasikaa 'F-aa 'nose
 (declension-F-aa-IRR 'naasik 'nas 'naH)
)
(subanta-irreg 'jaraa 'F-aa 'old-age
 (declension-F-aa1-IRR 'jar 'jaras 'jaraH)
)
(subanta-irreg 'nirjara 'M-a 'god
 (declension-M-aa1-IRR 'nirjar 'nirjaras 'nirjaraH)
)
(subanta-irreg 'ambaa 'F-aa 'mother
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 21 'amba) ; vocative singular is 'amba' rather than 'ambe'
  ans
 )
)
(subanta-irreg 'strii 'F-ii 'woman
 [strii striyau striyaH
  (striyam striim) striyau (striyaH striiH)
  striyaa striibhyaam striibhiH
  striyai striibhyaam striibhyaH
  striyaaH striibhyaam striibhyaH
  striyaaH striyoH striiNaam
  striyaam striyoH striiShu
  stri striyau striyaH  
 ]
)
(subanta-irreg 'akShi 'N-i 'eye
 (declension-N-i1-IRR 'akShi)
)
(subanta-irreg 'asthi 'N-i 'bone
 (declension-N-i1-IRR 'asthi)
)
(subanta-irreg 'sakthi 'N-i 'thigh
 (declension-N-i1-IRR 'sakthi)
)
(subanta-irreg 'dadhi 'N-i 'curd
 (declension-N-i1-IRR 'dadhi)
)
(subanta-irreg 'punarbhuu 'F-uu 'remarried-widow
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 3 'punarbhvam)
  ans
 )
)
(subanta-irreg 'pati 'M-i 'husband
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 6 'patyaa)
  (aset ans 9 'patye)
  (aset ans 12 'patyuH)
  (aset ans 15 'patyuH)
  (aset ans 18 'patyau)
  ans
 )
)
(subanta-irreg 'sakhi 'M-i 'companion
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'sakhaa) (aset ans 1 'sakhaayau) (aset ans 2 'sakhaayaH)
  (aset ans 3 'sakhaayam) (aset ans 4 'sakhaayau)
  (aset ans 6 'sakhyaa)
  (aset ans 9 'sakhye)
  (aset ans 12 'sakhyuH)
  (aset ans 15 'sakhyuH)
  (aset ans 18 'sakhyau)
  (aset ans 22 'sakhaayau) (aset ans 23 'sakhaayaH)
  ans
 )
)
(subanta-irreg 'kroShTu 'M-u 'jackal
 [kroShTaa kroShTaarau kroShTaaraH
  kroShTaaram kroShTaarau kroShTuun
  (kroShTunaa kroShTraa) kroShTubhyaam kroShTubhiH
  (kroShTave kroShTre) kroShTubhyaam kroShTubhyaH
  (kroShToH kroShTuH) kroShTubhyaam kroShTubhyaH
  (kroShToH kroShTuH) (kroShTvoH kroShTroH) kroShTuunaam
  (kroShTau kroShTari) (kroShTvoH kroShTroH) kroShTuShu
  kroShTo kroShTaarau kroShTaaraH]
)
(subanta-irreg 'pathin 'M-in 'path
 ; 'panthan' m. (path)  : Antoine2-#63 and Kale #123.
 ; This declined on the pattern of 'raajan'. 
 ; It has strong stem = 'panthaan' (which is regular),
 ; but irregular middle stem = 'pathi' and weak stem 'path'.
 ; Also, it irregularly add 'H' to  1S and 8S = 'panthaaH'
 ; Also, it shows no alternate in 7S
 ; NOTE: Kale p. 79 cites this as 'pathin'.
 ; Since 'Apte Practical Dictionary' also cites as 'pathin' and
 ; does not show 'panthan', I go with 'pathin'
 [panthaaH panthaanau panthaanaH
  panthaanam panthaanau pathaH
  pathaa pathibhyaam pathibhiH
  pathe pathibhyaam pathibhyaH
  pathaH pathibhyaam pathibhyaH
  pathaH pathoH pathaam
  pathi  pathoH pathiShu
  panthaaH panthaanau panthaanaH]
)
(subanta-irreg 'mathin 'M-in 'churning-handle
 ; Kale p. 79. Like 'pathin'
 [manthaaH manthaanau manthaanaH
  manthaanam manthaanau mathaH
  mathaa mathibhyaam mathibhiH
  mathe mathibhyaam mathibhyaH
  mathaH mathibhyaam mathibhyaH
  mathaH mathoH mathaam
  mathi  mathoH mathiShu
  manthaaH manthaanau manthaanaH]
)
(subanta-irreg 'RibhukShin'M-in 'name-of-Indra
 ; Kale p. 79. Like 'pathin'
 ; but does NOT insert the 'n' in the first five inflections.
 [RibhukShaaH RibhukShaaNau RibhukShaaNaH
  RibhukShaaNam RibhukShaaNau RibhukShaH
  RibhukShaa RibhukShibhyaam RibhukShibhiH
  RibhukShe RibhukShibhyaam RibhukShibhyaH
  RibhukShaH RibhukShibhyaam RibhukShibhyaH
  RibhukShaH RibhukShoH RibhukShaam
  RibhukShi  RibhukShoH RibhukShiShu
  RibhukShaaH RibhukShaaNau RibhukShaaNaH]
)
(subanta-irreg 'ahan 'N-an 'day
 ; 'ahan' n. (day) : Antoine2-#64 and Kale #120
 ; This is declined like 'naaman', except 
 ; (a) 1S, 2S, 8S append visarga: ahaH
 ; (b) middle stem is 'ahas', rather than 'aha'
 [ahaH (ahnii ahanii) ahaani
  ahaH (ahnii ahanii) ahaani
  ahnaa ahobhyaam ahobhiH
  ahne ahobhyaam ahobhyaH
  ahnaH ahobhyaam ahobhyaH
  ahnaH ahnoH ahnaam
  (ahni ahani) ahnoH ahaHsu
  ahaH (ahnii ahanii) ahaani]
)
(subanta-irreg 'shvan 'M-an 'dog
 ; 'shvan' m. (dog) : Antoine2-#65 and Kale #119
 ; This is declined like 'raajan', except
 ; (a) the weak stem is 'shun' (not 'shvn')
 ; (b) there is only one form in 7S (using weak stem) [acc. to Kale]
 [shvaa shvaanau shvaanaH
  shvaanam shvaanau shunaH
  shunaa shvabhyaam shvabhiH
  shune shvabhyaam shvabhyaH
  shunaH shvabhyaam shvabhyaH
  shunaH shunoH shunaam
  shuni shunoH shvasu
  shvan shvaanau shvaanaH]
)
(subanta-irreg 'yuvan 'M-an 'young-man
 ; 'yuvan' m. (young man) : Antoine2-#66 and Kale #119
 ; This is declined like 'raajan', except
 ; (a) the weak stem is 'yuun' (not 'yuvn')
 ; (b) there is only one form in 7S (using weak stem) [acc. to Kale]
 [yuvaa yuvaanau yuvaanaH
  yuvaanam yuvaanau yuunaH
  yuunaa yuvabhyaam yuvabhiH
  yuune yuvabhyaam yuvabhyaH
  yuunaH yuvabhyaam yuvabhyaH
  yuunaH yuunoH yuunaam
  yuuni yuunoH yuvasu
  yuvan yuvaanau yuvaanaH]
)
(subanta-irreg 'maghavan 'M-an 'Indra
 ; 'maghavan' m. (Indra) : Antoine2-#67 and Kale #119
 ; This is declined like 'raajan', except
 ; (a) the weak stem is 'maghon' (not 'maghavn')
 ; (b) there is only one form in 7S (using weak stem) [acc. to Kale]
 [maghavaa maghavaanau maghavaanaH
  maghavaanam maghavaanau maghonaH
  maghonaa maghavabhyaam maghavabhiH
  maghone maghavabhyaam maghavabhyaH
  maghonaH maghavabhyaam maghavabhyaH
  maghonaH maghonoH maghonaam
  maghoni  maghonoH maghavasu
  maghavan maghavaanau maghavaanaH]
) 
;(subanta-irreg 'vRitrahan 'M-IRR 'Indra
 ; 'vRitrahan' m. (Indra) : Antoine2-#68 and Kale-#111
 ; 'vRitraH' was a demon, whom Indra killed.
 ; Compounds ending in 'han' take 'aa' in 1S, 'an' in 8S.
 ; Their strong stem ends in 'han'.
 ; Their middle stem ends in 'ha'
 ; Their weak stem ends in 'ghn'
;  [vRitrahaa vRitrahaNau vRitrahaNaH
;   vRitrahaNam vRitrahaNau vRitraghnaH
;   vRitraghnaa vRitrahabhyaam vRitrahabhiH
;   vRitraghne vRitrahabhyaam vRitrahabhyaH
;   vRitraghnaH vRitrahabhyaam vRitrahabhyaH
;   vRitraghnaH vRitraghnoH vRitraghnaam
;   (vRitraghni vRitrahaNi) vRitraghnoH vRitrahasu
;   vRitrahan vRitrahaNau vRitrahaNaH]
;)

(subanta-irreg 'arvan 'M-an 'horse
 ; Kale#121, p. 78. Except when preceded by the
 ; negative particle 'an' (forming a negative TatpuruSha), 'arvan' is
 ; declined like a word ending in 't' in all cases except 1S, vS:
 ; N. arvaa, arvantau, arvantaH
 ; V. arvan, arvantau, arvantaH
 ; A. arvantam, arvantau, arvataH.
 (let (ans)
  (setq ans (declension-general-mat 'arvat 'arv 'M 'vat nil))
  (aset ans 0 'arvaa)
  (aset ans '21 'arvan)
  ans
 )
)
(subanta-irreg 'puuShan 'M-an 'sun
 ; Kale#111, p. 111. 
 ; 'puuShan', 'aryaman', and nouns ending in 'han' lengthen their 'a'
 ; in the Nom. sing. only.
 ; the 'n' of 'han' is changed to 'N' after 'h'.
 (let (ans i)
  (setq ans (declension-general-an 'puuShan 'puuSh 'M 'an nil))
  (aset ans 1 'puuShaNau)
  (aset ans 2 'puuShaNaH)
  (aset ans 3 'puuShaNam)
  (aset ans 4 'puuShaNau)
;  (aset ans 5 'puuShaNaH)
  (aset ans 22 'puuShaNau)
  (aset ans 23 'puuShaNaH)
  ans
 )
)
(subanta-irreg 'aryaman 'M-an 'deity-name
 ; Kale#111, p. 111. 
 ; 'puuShan', 'aryaman', and nouns ending in 'han' lengthen their 'a'
 ; in the Nom. sing. only.
 ; the 'n' of 'han' is changed to 'N' after 'h'.
 (let (ans i)
  (setq ans (declension-general-an 'aryaman 'aryam 'M 'an nil))
  (aset ans 1 'aryamaNau)
  (aset ans 2 'aryamaNaH)
  (aset ans 3 'aryamaNam)
  (aset ans 4 'aryamaNau)
  (aset ans 22 'aryamaNau)
  (aset ans 23 'aryamaNaH)
  ans
 )
)
(subanta-irreg 'dos 'M-IRR 'arm
 ; 'dos' m. (arm) : Antoine2-#78
 ; This is declined like '1cons' (noun with 1 consonant), except
 ; that from the 2P (acc. pl.) thru 7P (loc. pl.), there
 ; are optionally the forms as if it were 'doShan' and declined
 ; like 'raajan'.
 ; I had 'doSham' for 2S but Kale p.83 showed 'doH'
 [
  doH doShau doShaH
  doH doShau (doShaH doShNaH) 
  (doShaa doShNaa) (dorbhyaam doShabhyaam) (dorbhiH doShabhiH)
  (doShe doShNe) (dorbhyaam doShabhyaam) (dorbhyaH doShabhyaH)
  (doShaH doShNaH) (dorbhyaam doShabhyaam) (dorbhyaH doShabhyaH)
  (doShaH doShNaH) (doShoH doShNoH) (doShaam doShNaam)
  (doShi doShNi doShaNi) (doShoH doShNoH) (doHShu doShasu)
  doH doShau doShaH
 ]
)
(subanta-irreg 'dos 'N-IRR 'arm
 ; Same as masc. Apte Dictionary lists as both m. and n.
 [
  doH doShau doShaH
  doH doShau (doShaH doShNaH) 
  (doShaa doShNaa) (dorbhyaam doShabhyaam) (dorbhiH doShabhiH)
  (doShe doShNe) (dorbhyaam doShabhyaam) (dorbhyaH doShabhyaH)
  (doShaH doShNaH) (dorbhyaam doShabhyaam) (dorbhyaH doShabhyaH)
  (doShaH doShNaH) (doShoH doShNoH) (doShaam doShNaam)
  (doShi doShNi doShaNi) (doShoH doShNoH) (doHShu doShasu)
  doH doShau doShaH
 ]
)
(subanta-irreg 'aashis 'F-IRR 'blessing
 ; 'aashis' f. (blessing) : Antoine2-#79
 ; This is declined like '1cons' (noun with 1 consonant), except
 ; that it lengthens its 'i' (to 'ii') in 1S and all terminations
 ; beginning with a consonant.
 [aashiiH aashiShau aashiShaH
  aashiSham aashiShau aashiShaH
  aashiShaa aashiirbhyaam aashiirbhiH
  aashiShe aashiirbhyaam aashiirbhyaH
  aashiShaH aashiirbhyaam aashiirbhyaH
  aashiShaH aashiShoH aashiShaam
  aashiShi aashiShoH aashiiHShu
  aashiiH aashiShau aashiShaH
 ]
)
(subanta-irreg 'old-div 'F-1cons 'sky
 ; 'div' f. (sky) : Antoine2-#80
 ; This is declined like a noun with 1 consonant, except
 ; (a) 1S is 'dyauH'
 ; (a)' 8S is 'dyauH'
 ; (b) 2S is optionally 'dyaam'
 ; (c) before terminations beginning with a consonant it uses stem 'dyu'
 ;    (this is accomplished in algorithm, by providing two praatipadikas
 ;     ('div' and 'dyu')
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'dyauH)
  (aset ans 3 '(divam dyaam))
  (aset ans 21 'dyauH)
  ans
 )
)
(subanta-irreg 'anehas 'M-1cons 'time
 ;Kale#109, p.68. The nom. sing of the m. nouns
 ; 'anehas' (time), 'purudaMsas' (name of Indra), and 
 ; 'uSanas' (name of 'SukrAcArya') end in 'A' rather than 'AH'
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'anehaa)
  ans
 )
)
(subanta-irreg 'purudaMsas 'M-1cons 'name-of-Indra
 ;Kale#109, p.68. The nom. sing of the m. nouns
 ; 'anehas' (time), 'purudaMsas' (name of Indra), and 
 ; 'uSanas' (name of 'SukrAcArya') end in 'A' rather than 'AH'
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'purudaMsaa)
  ans
 )
)
(subanta-irreg 'ushanas 'M-1cons 'name-of-SukrAcArya
 ;Kale#109, p.68. The nom. sing of the m. nouns
 ; 'anehas' (time), 'purudaMsas' (name of Indra), and 
 ; 'uSanas' (name of 'SukrAcArya') end in 'A' rather than 'AH'
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'ushanaa)
  ans
 )
)
(subanta-irreg 'puMs 'M-IRR 'man
 ; 'puMs' m. (man) : Antoine2-#81, Kale-#113
 ; This is declined as a noun with three stems,
 ; the strong stem is 'pumaaMs', the middle stem is 'pum',
 ; and the weak stem is 'puMs'.
 [pumaan pumaaMsau pumaaMsaH
  pumaaMsam pumaaMsau puMsaH
  puMsaa pumbhyaam pumbhiH
  puMse  pumbhyaam pumbhyaH
  puMsaH pumbhyaam pumbhyaH
  puMsaH puMsoH puMsaam
  puMsi  puMsoH puMsu
  puman pumaaMsau pumaaMsaH
 ]
)
(subanta-irreg 'anaDuh 'M-IRR 'ox
 ; 'anaDvah' m. (ox) : Antoine2-#82 , Kale-#102
 ; This is declined as a noun with three stems,
 ; the strong stem is 'anaDvaah', 
 ; the middle stem is 'anaDut',
 ; the weak stem is 'anaDuh'.
 ; The nom. sing. is 'anaDvaan'
 ; the voc. sing. is 'anaDvan'
 ; Note: Kale (p. 61) and Apte both cite as 'anaDuh', so I go with that.
 [anaDvaan anaDvaahau anaDvaahaH
  anaDvaaham anaDvaahau anaDuhaH
  anaDuhaa anaDudbhyaam anaDudbhiH
  anaDuhe  anaDudbhyaam anaDudbhyaH
  anaDuhaH anaDudbhyaam anaDudbhyaH
  anaDuhaH anaDuhoH anaDuhaam
  anaDuhi  anaDuhoH anaDutsu
  anaDvan anaDvaahau anaDvaahaH
 ]
)
(subanta-irreg 'ap 'F-IRR 'water
 ; 'ap' f. (water) : Antoine2-#83
 ; This is declined only in the plural
 ; It lengthens is 'a' in 1P and 8P.
 ; It changes 'p' to 't' before 'bhiH' and 'bhyaH'
 ; NOTE: by Apte, the 'plural only' is a feature of classical language;
 ; Vedic has singular and plural forms. SL has all forms, and I go with
 ; that
;  [nil nil aapaH
;   nil nil apaH
;   nil nil adbhiH
;   nil nil adbhyaH
;   nil nil adbhyaH
;   nil nil apaam
;   nil nil apsu
;   nil nil aapaH
;  ]
 [aap aapau aapaH
 aapam aapau apaH
 apaa adbhyaam adbhiH
 ape adbhyaam adbhyaH
 apaH adbhyaam adbhyaH
 apaH apoH apaam
 api apoH apsu
 ap aapau aapaH]

)
; kRi: kar kur
(subanta-irreg 'sras 'M-1cons 'falling
     ;Kale#112.
     ;'sras' (that falls) and 'dhvas' (a destroyer) change their
     ; final 's' to 't' before consonantal terminations.
     ; 'suhiMs' (one who kills well) and 'jighaaMs' (desirous of killing)
     ; change their final 's' to 'n' and drop the preceding nasal
     ; before consonantal terminations.
     ; The voc. sing. is same as nom. sing.
     ; The neuter 1P/vP/2P inserts nasal
 [srat srasau srasaH
 srasam srasau srasaH
 srasaa sradbhyaam sradbhiH
 srase sradbhyaam sradbhyaH
 srasaH sradbhyaam sradbhyaH
 srasaH srasoH srasaam
 srasi srasoH sratsu
 srat srasau srasaH]
)
(subanta-irreg 'sras 'N-1cons 'falling
 [srat srasii sraMsi
  srat srasii sraMsi 
  srasaa sradbhyaam sradbhiH
  srase sradbhyaam sradbhyaH
  srasaH sradbhyaam sradbhyaH
  srasaH srasoH srasaam
  srasi srasoH sratsu
  srat srasii sraMsi]
)
(subanta-irreg 'dhvas 'M-1cons 'destroyer
 [dhvat dhvasau dhvasaH
 dhvasam dhvasau dhvasaH
 dhvasaa dhvadbhyaam dhvadbhiH
 dhvase dhvadbhyaam dhvadbhyaH
 dhvasaH dhvadbhyaam dhvadbhyaH
 dhvasaH dhvasoH dhvasaam
 dhvasi dhvasoH dhvatsu
 dhvat dhvasau dhvasaH]
)
(subanta-irreg 'dhvas 'N-1cons 'destroyer
 [dhvat dhvasii dhvaMsi
  dhvaH dhvasii dhvaMsi 
  dhvasaa dhvadbhyaam dhvadbhiH
  dhvase dhvadbhyaam dhvadbhyaH
  dhvasaH dhvadbhyaam dhvadbhyaH
  dhvasaH dhvasoH dhvasaam
  dhvasi dhvasoH dhvatsu
  dhvat dhvasii dhvaMsi]
)
(subanta-irreg 'suhiMs 'M-1cons 'one-who-kills-well
 [suhin suhiMsau suhiMsaH
 suhiMsam suhiMsau suhiMsaH
 suhiMsaa suhinbhyaam suhinbhiH
 suhiMse suhinbhyaam suhinbhyaH
 suhiMsaH suhinbhyaam suhinbhyaH
 suhiMsaH suhiMsoH suhiMsaam
 suhiMsi suhiMsoH (suhinsu suhintsu)
 suhin suhiMsau suhiMsaH]
)
(subanta-irreg 'suhiMs 'N-1cons 'one-who-kills-well
 [suhin suhiMsii suhiMsi 
  suhin suhiMsii suhiMsi
  suhiMsaa suhinbhyaam suhinbhiH
  suhiMse suhinbhyaam suhinbhyaH
  suhiMsaH suhinbhyaam suhinbhyaH
  suhiMsaH suhiMsoH suhiMsaam
  suhiMsi suhiMsoH (suhinsu suhintsu)
  suhin suhiMsii suhiMsi]
)
(subanta-irreg 'jighaaMs 'M-1cons 'desirous-of-killing
 [jighaan jighaaMsau jighaaMsaH
 jighaaMsam jighaaMsau jighaaMsaH
 jighaaMsaa jighaanbhyaam jighaanbhiH
 jighaaMse jighaanbhyaam jighaanbhyaH
 jighaaMsaH jighaanbhyaam jighaanbhyaH
 jighaaMsaH jighaaMsoH jighaaMsaam
 jighaaMsi jighaaMsoH (jighaansu jighaantsu)
 jighaan jighaaMsau jighaaMsaH]
)
(subanta-irreg 'jighaaMs 'N-1cons 'desirous-of-killing
 [jighaan jighaaMsii jighaaMsi 
  jighaan jighaaMsii jighaaMsi
  jighaaMsaa jighaanbhyaam jighaanbhiH
  jighaaMse jighaanbhyaam jighaanbhyaH
  jighaaMsaH jighaanbhyaam jighaanbhyaH
  jighaaMsaH jighaaMsoH jighaaMsaam
  jighaaMsi jighaaMsoH (jighaansu jighaantsu)
  jighaan jighaaMsii jighaaMsi]
)
(subanta-irreg 'yuj 'M-1cons 'yoke
 ; Kale p.59
 (let (ans)
  (setq ans (copy-sequence nil-declension))
  (aset ans 0 'yu~N)
  (aset ans 1 'yu~njau)
  (aset ans 2 'yu~njaH)
  (aset ans 3 'yu~njam)
  (aset ans 4 'yu~njau)
  (aset ans 21 (aref ans 0))
  (aset ans 22 (aref ans 1))
  (aset ans 23 (aref ans 2))
  ans
 )
)
(subanta-irreg 'kha~nj 'M-1cons 'lame-man
 ;Kale #105, p. 65
 ; 'kru~nch' (heron) 'kha~nj' (lame man) 'suvalg' (beautifully bounding)
 ; These become 'kru~N khan 'suval' before consonantal terminations.
 [khan kha~njau kha~njaH
  kha~njam kha~njau kha~njaH
  kha~njaa khanbhyaam khanbhiH
  kha~nje khanbhyaam khanbhyaH
  kha~njaH khanbhyaam khanbhyaH
  kha~njaH kha~njoH kha~njaam
  kha~nji kha~njoH khansu
  khan kha~njau kha~njaH]
)
(subanta-irreg 'kru~nch 'M-1cons 'heron
 ;Kale #105, p. 65
 ; 'kru~nch' (heron) 'kha~nj' (lame man) 'suvalg' (beautifully bounding)
 ; These become 'kru~N khan 'suval' before consonantal terminations.
 [kru~N kru~nchau kru~nchaH
  kru~ncham kru~nchau kru~nchaH
  kru~nchaa kru~Nbhyaam kru~NbhiH
  kru~nche kru~Nbhyaam kru~NbhyaH
  kru~nchaH kru~Nbhyaam kru~NbhyaH
  kru~nchaH kru~nchoH kru~nchaam
  kru~nchi kru~nchoH kru~NkShu
  kru~N kru~nchau kru~nchaH]
)
(dhaatu-irreg 'kRi 'laT-8-P 'do
 [karoti kurutaH kurvanti karoShi kuruthaH kurutha karomi kurvaH kurmaH]
)
(dhaatu-irreg 'kRi 'la~N-8-P 'do
 [akarot akurutaam akurvan akaroH akurutam akuruta akaravam akurva akurma]
)
(dhaatu-irreg 'kRi 'loT-8-P 'do
 [karotu kurutaam kurvantu kuru kurutam kuruta karavaaNi karavaava karavaama]
)
(dhaatu-irreg 'kRi 'vidhili~N-8-P 'do
 [kuryaat kuryaataam kuryuH kuryaaH kuryaatam kuryaata kuryaam kuryaava kuryaama]
)
(dhaatu-irreg 'kRi 'laT-8-A 'do
 [kurute kurvaate kurvate kuruShe kurvaathe kurudhve kurve kurvahe kurmahe]
)
(dhaatu-irreg 'kRi 'la~N-8-A 'do
 [akuruta akurvaataam akurvata akuruthaaH akurvaathaam akurudhvam akurvi akurvahi akurmahi]
)
(dhaatu-irreg 'kRi 'loT-8-A 'do
[kurutam kurvaataam kurvataam kuruShva kurvaathaam kurudhvam karavai karavaavahai karavaamahai]
)
(dhaatu-irreg 'kRi 'vidhili~N-8-A 'do
[kurviita kurviiyaataam kurviiran kurviithaaH kurviiyaathaam kurviidhvam kurviiya kurviivahi kurviimahi]
)
; as: as st san sth sv sm edh syaa syu
(dhaatu-irreg 'as 'laT-2-P 'do
 [asti staH santi asi sthaH stha asmi svaH smaH]
)
(dhaatu-irreg 'as 'la~N-2-P 'do
 [aasiit aastaam aasan aasiiH aastam aasta aasam aasva aasma]
)
(dhaatu-irreg 'as 'loT-2-P 'do
 [astu staam santu edhi stam sta asaani asaava asaama]
)
(dhaatu-irreg 'as 'vidhili~N-2-P 'do
 [syaat syaataam syuH syaaH syaatam syaata syaam syaava syaama]
)


; vid 2P (to know) Kale 436.
; laT: optionally takes the terminations of the Perfect
; la~N :  Note: Kale does not mention these as irregularities
;        3P is 'aviduH'  2S is 'aveH' or 'aved'
; loT : in addition to the normal imperative, there is
;  an optional form made by appending 'vidaam' to the imperative of 'kRi'
; vidhili~N : regular 
(dhaatu-irreg 'vid 'laT-2-P 'know
 (let (x y z i n)
  (setq x (conjugation-tab-2 nil 'laT 2 'P 'vid))
  ; x = [vetti vittaH vidanti vetsi vitthaH vittha vedmi vidvaH vidmaH]
  (setq y [veda vidatuH viduH vettha vidathuH vida veda vidva vidma])
  (setq n (length x))
  (setq i 0)
  (setq z (make-vector n nil))
  (while (< i n)
   (aset z i (list (elt x i) (elt y i)))
   (setq i (1+ i))
  )
  z 
 )
)
(dhaatu-irreg 'vid 'la~N-2-P 'know
 (let (x)
  (setq x (conjugation-tab-2 nil 'la~N 2 'P 'vid))
  ; x = [avet avittaam avidan (avet aved) avittam avitta avedam avidva avidma]
  (aset x 2 'avidan)
  (aset x 3 '(aveH aved))
  x
 )
)
(dhaatu-irreg 'vid 'loT-2-P 'know
 (let (x y z i n)
  (setq x (conjugation-tab-2 nil 'loT 2 'P 'vid))
  ; x = [vettu vittaam vidantu viddhi vittam vitta vedaani vedaava vedaama]
  (setq y
	[vidaa~Nkarotu vidaa~Nkurutaam vidaa~Nkurvantu
	 vidaa~Nkuru vidaa~Nkurutam vidaa~Nkuruta 
	 vidaa~NkaravaaNi vidaa~Nkaravaava vidaa~Nkaravaama])
  (setq n (length x))
  (setq i 0)
  (setq z (make-vector n nil))
  (while (< i n)
   (aset z i (list (elt x i) (elt y i)))
   (setq i (1+ i))
  )
  z 
 )
)
(dhaatu-irreg 'vid 'vidhili~N-2-P 'know
 (conjugation-tab-2 nil 'vidhili~N 2 'P 'vid)
 ; [vidyaat vidyaataam vidyuH 
 ;  vidyaaH vidyaatam vidyaata vidyaam vidyaava vidyaama]
)
(dhaatu-irreg 'bruu 'laT-2-P 'speak
 ; 05-18-05.
 ;  the form 'prAha' occurs in mbh 3.264.028
 ; also must change 'conjugation-tab' in irreg.el
 [(braviiti aaha) (bruutaH aahatuH) (bruvanti aahuH)
  (braviiShi aattha) (bruuthaH aahathuH) bruutha 
  braviimi bruuvaH bruumaH] 
)
(subanta-irreg 'sraMs 'M-1cons 'falling
 ; SL has these 'srans' and 'dhvans'
 ; I have put these to be the same as 'sras' and 'dhvas'
 ; Not sure if correct. 1-4-04
 [srat srasau srasaH
 srasam srasau srasaH
 srasaa sradbhyaam sradbhiH
 srase sradbhyaam sradbhyaH
 srasaH sradbhyaam sradbhyaH
 srasaH srasoH srasaam
 srasi srasoH sratsu
 srat srasau srasaH]
)
(subanta-irreg 'sraMs 'N-1cons 'falling
 [srat srasii sraMsi
  sraH srasii sraMsi 
  srasaa sradbhyaam sradbhiH
  srase sradbhyaam sradbhyaH
  srasaH sradbhyaam sradbhyaH
  srasaH srasoH srasaam
  srasi srasoH sratsu
  srat srasii sraMsi]
)
(subanta-irreg 'dhvaMs 'M-1cons 'destroyer
 [dhvat dhvasau dhvasaH
 dhvasam dhvasau dhvasaH
 dhvasaa dhvadbhyaam dhvadbhiH
 dhvase dhvadbhyaam dhvadbhyaH
 dhvasaH dhvadbhyaam dhvadbhyaH
 dhvasaH dhvasoH dhvasaam
 dhvasi dhvasoH dhvatsu
 dhvat dhvasau dhvasaH]
)
(subanta-irreg 'dhvaMs 'N-1cons 'destroyer
 [dhvat dhvasii dhvaMsi
  dhvaH dhvasii dhvaMsi 
  dhvasaa dhvadbhyaam dhvadbhiH
  dhvase dhvadbhyaam dhvadbhyaH
  dhvasaH dhvadbhyaam dhvadbhyaH
  dhvasaH dhvasoH dhvasaam
  dhvasi dhvasoH dhvatsu
  dhvat dhvasii dhvaMsi]
)

)
