; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; numeral.el  
; begun 03-05-03 by ejf
; AntoineII, Chap 3, Section 22: Cardinals from 19 to 99
; Also refer to  Kale (sections 159 and following)
; The cardinals from 19 to 99 are formed as follows:
; (1) The numbers from 'eka' to 'navan' are prefixed to the multiples
;     of 10 (20,30,..., 90) . 
;     The ones ending in 'an' drop the 'n' when forming. 
;     The combination of the prefix to the multiple of 10
;     uses external sandhi of the 'nojoin' form, but in fact joins the words!
;     e.g.  'chatuH' + 'pa~nchaasat' -> 'chatuHpa~nchaasat'
;     (note the joining form for 'chatur' is 'chatuH')
; (2) 'dvi' 'tri' and 'aShTa' are changed to 'dvaa' 'trayaH' 'aShTaa'
;    (a) necessarily before 'viMshatiH' and 'triMshat'
;    (b) optionally before 'chatvaariMshat', 'pa~nchaashat',
;        'ShaShTiH', 'saptatiH', 'navatiH'
; (3) 'dvi' 'tri' and 'aShTan' are unchanged before 'ashiitiH'
; (4) the multiples of 10 are
;    viMshatiH triMshat chatvaariMshat pa~nchaashat ShaShTiH
;    saptatiH ashiitiH navatiH
; (5) 19, 29, ... , 89 may be expressed also as '1 less than' the
;     next multiple of 10 (19 = 1 less than 20, etc). There are
;     three prefixes that may be used to express '1 less than':
;     'ekona' 'uuna' and 'ekaanna'
; (6) cardinals from 19 to 99 are feminine nouns (19 when expressed
;     as 'navadashan' is viewed as an adjective, declined alike in M F N)
;     Furthermore, they are used in the singular.  The noun whose number
;     is being given
;     (a) normally follows the cardinal
;     (b) is declined in the plural
;     (c) is in the case of the cardinal OR in case 6.
; AntoineII, Chap 3, Section 23: Ordinals from 19 to 99
;  The ordinals from 19th to 99th are formed as follows:
;  (1) from 19th to 29th use the suffix 'viMsha' or 'viMshatitama'
;  (2) from 29th to 39th use the suffix 'triMsha' or 'triMshattama'
;  (3) from 39th to 49th use the suffix 'chatvaariMsha' or 'chatvaariMshattama'
;  (4) from 49th to 59th use the suffix 'pa~nchaasha' or 'pa~nchaashattama'
;  (5) from 59th to 69th use the suffix 'ShaShTa' or 'ShaShTitama'
;      The simple ordinal 60th is 'ShaShTitama'
;  (6) from 69th to 79th use the suffix 'saptata' or 'saptatitama'
;      The simple ordinal 70th is 'saptatitama'
;  (7) from 79th to 89th use the suffix 'ashiita' or 'ashiititama'
;      The simple ordinal 80th is 'ashiititama'
;  (8) from 89th to 99th use the suffix 'navata' or 'navatitama'
;      The simple ordinal 90th is 'navatitama'
;
; The ordinals are declined 
;  (a) in the masculine like nouns in 'a': ashiitaH, ashiitau, ashiitaaH, etc.
;  (b) in the neuter like nouns in 'a' : ashiitam, ashiite, ashiitaani, etc.
;  (c) in the feminine like nouns in 'ii': ashiitii, ashiityau, ashiityaH, etc.
; Differences (due to application of sandhi) between 'numeral-join' and KALE
; 64 ==== KALE: chatuShShaShTiH
; 73 ==== KALE: trayassaptati (not trayaHsaptatiH)
; 74 ==== KALE: chatussaptatiH (not chatuHsaptatiH)
; 93 ==== KALE: (trinavatiH trayonavatiH) [not (triNavatiH trayoNavatiH)]
; 94 ==== KALE: (chaturnavatiH) [not (chaturNavatiH)]
; 96 ==== KALE: (ShaNNavatiH) [not (ShaDnavatiH)]

(defun san-numeral (num)
 "Given a positive integer 'num', form the Sanskrit for
  the cardinal and ordinals for that number. 
  The answer is returned as (list cards ords)
  where, since there may be multiple answers, both cards
  and ords are lists of symbols."
 (cond
  ((not (numberp num))
   (fol-msg (format "san-numeral (Error): %s\n" num))
   nil
  )
  ((or (< num 1) (< 99 num))
   (fol-msg (format "san-numeral (Error): %s\n" num))
   nil
  )
  ((<= num 18) (san-numeral-1-19 num))
  (t
   (let (ans cards ords units tens)
    (setq ans (san-numeral-1-19 num)) ; nil unless num=19
    (setq cards (elt ans 0))
    (setq ords (elt ans 1))
    (setq units (mod num 10)) ; 0-9
    (setq tens (/ num 10)) ; 1-9
    (let (pfx sfx theseans thisans p pfxes)
     (setq p (elt cardinal-pfx units))
     (setq pfx
      (cond
       ((not (member units '(2 3 8))) p)
       ((member tens '(2 3)) (elt p 1))
       ((equal tens 8) (elt p 0))
       (t p)
      )
     )
     (when (and (<= 2 tens) (<= tens 9))
      ;-- cards
      (setq sfx (elt card-multiples-of-10-2-10 (- tens 2)))
      (setq theseans (numeral-join-1-sym pfx sfx))
      (if (not (listp theseans)) (setq theseans (list theseans)))
      (while theseans
       (setq thisans (car theseans))
       (setq theseans (cdr theseans))
       (setq cards (append-if-new cards thisans))
      )
      ;-- ords
      (setq sfx (elt ord-multiples-of-10-2-10 (- tens 2))) ; a list
      (if (member tens '(6 7 8 9)) ; use 2nd ('tama') form
	(setq sfx (list (elt sfx 1))))
      (setq theseans (numeral-join-1-sym pfx sfx))
      (if (not (listp theseans)) (setq theseans (list theseans)))
      (setq theseans (flatten theseans))
      (while theseans
       (setq thisans (car theseans))
       (setq theseans (cdr theseans))
       (setq ords (append-if-new ords thisans))
      )
     )
     ; add other forms for 19, 29,..., 99
     (when (= units 9)
      ;-- cards
      (setq sfx (elt card-multiples-of-10-2-10 (1+ (- tens 2))))
      (setq theseans (numeral-join-1-sym cardinal-pfx-1-less sfx))
      (if (not (listp theseans)) (setq theseans (list theseans)))
      (while theseans
       (setq thisans (car theseans))
       (setq theseans (cdr theseans))
       (setq cards (append-if-new cards thisans))
      )
      ;-- ords
      (setq sfx (elt ord-multiples-of-10-2-10 (1+ (- tens 2))))
      (setq theseans (numeral-join-1-sym cardinal-pfx-1-less sfx))
      (if (not (listp theseans)) (setq theseans (list theseans)))
      (setq theseans (flatten theseans))
      (while theseans
       (setq thisans (car theseans))
       (setq theseans (cdr theseans))
       (setq ords (append-if-new ords thisans))
      )
     )
    )
    (list cards ords)
   )
  )
 )
)
(defun san-numeral-1-19 (num)
 (when (and (numberp num) (<= 1 num) (<= num 19))
  (list (list (elt cardinals-1-19 (1- num)))
	(list (elt ordinals-1-19 (1- num))))
 )
)
(defvar cardinals-1-19
 [eka dvi tri chatur pa~nchan ShaSh aShTan saptan navan dashan
  ekaadashan dvaadashan trayodashan chaturdashan pa~nchadashan
  ShoDashan saptadashan aShTaadashan navadashan])
(defvar ordinals-1-19
 [prathama dvitiiya tRitiiya (turiiya turya chaturtha) pa~nchama
  ShaShTha saptama aShTama navama dashama
  ekaadasha dvaadasha trayodasha chaturdasha pa~nchadasha
  ShoDasha saptadasha aShTaadasha navadasha]
)
(defvar card-multiples-of-10-2-10
 [viMshatiH triMshat chatvaariMshat pa~nchaashat
  ShaShTiH saptatiH ashiitiH navatiH shatam]
)
(defvar ord-multiples-of-10-2-10
 [(viMsha viMshatitama)
  (triMsha triMshattama)
  (chatvaariMsha chatvaariMshattama)
  (pa~nchaasha pa~nchaashattama)
  (ShaShTa ShaShTitama) ; 2nd for 60
  (saptata saptatitama) ; 2nd for 70
  (ashiita ashiititama) ; 2nd for 80
  (navata navatitama) ; 2nd for 90
  (shatama)
 ]
)
(defvar cardinal-pfx
 [nil eka (dvi dvaa) (tri trayaH) chatuH pa~ncha
  ShaT sapta (aShTa aShTaa) nava]
)
(defvar cardinal-pfx-1-less
 '(ekona uuna ekaanna)
)
(defun numeral-join-1x-sym (base-sym sup-sym)
 (let (ans)
  (setq ans
   (cond 
    ((equal sup-sym nil) base-sym)
    ((equal base-sym nil) sup-sym)
    ((listp sup-sym)
     (mapcar   (lambda (x) (numeral-join base-sym x)) sup-sym))
    ((listp base-sym)
     (mapcar (lambda (x) (numeral-join x sup-sym)) base-sym))
    (t
     (let (base-tok sup-tok)
      (setq base-tok (car (ITRANS-parse-words-1 (symbol-name base-sym))))
      (setq sup-tok (car (ITRANS-parse-words-1 (symbol-name sup-sym))))
      (numeral-join base-tok sup-tok)
     )
    )
   )
  )
  (if (not (listp ans)) (setq ans (list ans)))
  (mapcar 'sym-without-space ans)
 )
)
(defun numeral-join-1-sym (base-sym sup-sym)
 (cond 
  ((equal sup-sym nil) base-sym)
  ((equal base-sym nil) sup-sym)
  ((listp sup-sym)
   (mapcar   (lambda (x) (numeral-join-1-sym base-sym x)) sup-sym))
  ((listp base-sym)
   (mapcar (lambda (x) (numeral-join-1-sym x sup-sym)) base-sym))
  (t
   (let (base-tok sup-tok tok)
    (setq base-tok (car (ITRANS-parse-words-1 (symbol-name base-sym))))
    (setq sup-tok (car (ITRANS-parse-words-1 (symbol-name sup-sym))))
    (setq tok (numeral-join base-tok sup-tok))
    (sym-without-space tok)
   )
  )
 )
)
(defun numeral-join (base-tok sup)
; base-tok and sup are token arrays
 (let (ans)
;  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ans (cond
;   ((solution (sandhi-pair base-tok sup 'internal 'join)))
   ((solution (sandhi-pair base-tok sup nil 'join)))
;   ((solution (sandhi-pair base-tok sup 'optional 'join)))
   ; for nouns ending in 's', e.g., for 'sumanas',
   ; in 3P, base-tok = [s u m a n a H] and sup =  [bh i H]
   ((and (arrayp base-tok)
	 (equal (substring base-tok -1) [H])
    (let (ans1)
     (setq ans1 (sandhi-pair base-tok sup nil 'nojoin))
     (when ans1   ;(([s u m a n o] [bh i H]))
      (setq ans1 (car ans1)) ; ([s u m a n o] [bh i H])
      (setq ans1 (apply 'vconcat ans1)) ; [s u m a n o bh i H]
     )
    ))
   )
   ((symbolp base-tok) (sym-without-space (vector base-tok sup)))
   ((arrayp base-tok) (vconcat base-tok sup))
   (t
    (fol-msg (format "numeral-join error. wrong types: %s %s\n"
		     base-tok sup))
    nil
   )
  ))
;  (sandhi-pair-skiprefs-set nil)
  (setq ans (or (sandhi-single ans) ans))
 )
)
(defvar san-numeral-test-result nil)
(defun san-numeral-test (n1 &optional n2)
 (let (i x s)
  (setq san-numeral-test-result nil)
  (cond 
   ((not (numberp n1)))
   ((or (not (numberp n2)) (< n2 n1)))
   ((< n1 1))
   (t
    (setq i n1)
    (while (<= i n2)
     (setq x (san-numeral i))
     (fol-msg (format "%s = %s\n" i x))
     (setq san-numeral-test-result (append-if-new san-numeral-test-result x))
     (setq i (1+ i))
    )
   )
  )
 )
)

(defun answers-for-19-99 ()
 (san-numeral-test 19 99)
19 = '((navadashan ekonaviMshatiH uunaviMshatiH ekaannaviMshatiH) (navadasha ekonaviMsha uunaviMsha ekaannaviMsha ekonaviMshatitama uunaviMshatitama ekaannaviMshatitama))
20 = '((viMshatiH) (viMsha viMshatitama))
21 = '((ekaviMshatiH) (ekaviMsha ekaviMshatitama))
22 = '((dvaaviMshatiH) (dvaaviMsha dvaaviMshatitama))
23 = '((trayoviMshatiH) (trayoviMsha trayoviMshatitama))
24 = '((chaturviMshatiH) (chaturviMsha chaturviMshatitama))
25 = '((pa~nchaviMshatiH) (pa~nchaviMsha pa~nchaviMshatitama))
26 = '((ShaDviMshatiH) (ShaDviMsha ShaDviMshatitama))
27 = '((saptaviMshatiH) (saptaviMsha saptaviMshatitama))
28 = '((aShTaaviMshatiH) (aShTaaviMsha aShTaaviMshatitama))
29 = '((navaviMshatiH ekonatriMshat uunatriMshat ekaannatriMshat) (navaviMsha navaviMshatitama ekonatriMsha uunatriMsha ekaannatriMsha ekonatriMshattama uunatriMshattama ekaannatriMshattama))
30 = '((triMshat) (triMsha triMshattama))
31 = '((ekatriMshat) (ekatriMsha ekatriMshattama))
32 = '((dvaatriMshat) (dvaatriMsha dvaatriMshattama))
33 = '((trayastriMshat) (trayastriMsha trayastriMshattama))
34 = '((chatustriMshat) (chatustriMsha chatustriMshattama))
35 = '((pa~nchatriMshat) (pa~nchatriMsha pa~nchatriMshattama))
36 = '((ShaTTriMshat) (ShaTTriMsha ShaTTriMshattama))
37 = '((saptatriMshat) (saptatriMsha saptatriMshattama))
38 = '((aShTaatriMshat) (aShTaatriMsha aShTaatriMshattama))
39 = '((navatriMshat ekonachatvaariMshat uunachatvaariMshat ekaannachatvaariMshat) (navatriMsha navatriMshattama ekonachatvaariMsha uunachatvaariMsha ekaannachatvaariMsha ekonachatvaariMshattama uunachatvaariMshattama ekaannachatvaariMshattama))
40 = '((chatvaariMshat) (chatvaariMsha chatvaariMshattama))
41 = '((ekachatvaariMshat) (ekachatvaariMsha ekachatvaariMshattama))
42 = '((dvichatvaariMshat dvaachatvaariMshat) (dvichatvaariMsha dvaachatvaariMsha dvichatvaariMshattama dvaachatvaariMshattama))
43 = '((trichatvaariMshat trayashchatvaariMshat) (trichatvaariMsha trayashchatvaariMsha trichatvaariMshattama trayashchatvaariMshattama))
44 = '((chatushchatvaariMshat) (chatushchatvaariMsha chatushchatvaariMshattama))
45 = '((pa~nchachatvaariMshat) (pa~nchachatvaariMsha pa~nchachatvaariMshattama))
46 = '((ShaTchatvaariMshat) (ShaTchatvaariMsha ShaTchatvaariMshattama))
47 = '((saptachatvaariMshat) (saptachatvaariMsha saptachatvaariMshattama))
48 = '((aShTachatvaariMshat aShTaachatvaariMshat) (aShTachatvaariMsha aShTaachatvaariMsha aShTachatvaariMshattama aShTaachatvaariMshattama))
49 = '((navachatvaariMshat ekonapa~nchaashat uunapa~nchaashat ekaannapa~nchaashat) (navachatvaariMsha navachatvaariMshattama ekonapa~nchaasha uunapa~nchaasha ekaannapa~nchaasha ekonapa~nchaashattama uunapa~nchaashattama ekaannapa~nchaashattama))
50 = '((pa~nchaashat) (pa~nchaasha pa~nchaashattama))
51 = '((ekapa~nchaashat) (ekapa~nchaasha ekapa~nchaashattama))
52 = '((dvipa~nchaashat dvaapa~nchaashat) (dvipa~nchaasha dvaapa~nchaasha dvipa~nchaashattama dvaapa~nchaashattama))
53 = '((tripa~nchaashat trayaHpa~nchaashat) (tripa~nchaasha trayaHpa~nchaasha tripa~nchaashattama trayaHpa~nchaashattama))
54 = '((chatuHpa~nchaashat) (chatuHpa~nchaasha chatuHpa~nchaashattama))
55 = '((pa~nchapa~nchaashat) (pa~nchapa~nchaasha pa~nchapa~nchaashattama))
56 = '((ShaTpa~nchaashat) (ShaTpa~nchaasha ShaTpa~nchaashattama))
57 = '((saptapa~nchaashat) (saptapa~nchaasha saptapa~nchaashattama))
58 = '((aShTapa~nchaashat aShTaapa~nchaashat) (aShTapa~nchaasha aShTaapa~nchaasha aShTapa~nchaashattama aShTaapa~nchaashattama))
59 = '((navapa~nchaashat ekonaShaShTiH uunaShaShTiH ekaannaShaShTiH) (navapa~nchaasha navapa~nchaashattama ekonaShaShTa uunaShaShTa ekaannaShaShTa ekonaShaShTitama uunaShaShTitama ekaannaShaShTitama))
60 = '((ShaShTiH) (ShaShTitama))
61 = '((ekaShaShTiH) (ekaShaShTitama))
62 = '((dviShaShTiH dvaaShaShTiH) (dviShaShTitama dvaaShaShTitama))
63 = '((triShaShTiH trayaHShaShTiH) (triShaShTitama trayaHShaShTitama))
64 = '((chatuHShaShTiH) (chatuHShaShTitama))
65 = '((pa~nchaShaShTiH) (pa~nchaShaShTitama))
66 = '((ShaTShaShTiH) (ShaTShaShTitama))
67 = '((saptaShaShTiH) (saptaShaShTitama))
68 = '((aShTaShaShTiH aShTaaShaShTiH) (aShTaShaShTitama aShTaaShaShTitama))
69 = '((navaShaShTiH ekonasaptatiH uunasaptatiH ekaannasaptatiH) (navaShaShTitama ekonasaptata uunasaptata ekaannasaptata ekonasaptatitama uunasaptatitama ekaannasaptatitama))
70 = '((saptatiH) (saptatitama))
71 = '((ekasaptatiH) (ekasaptatitama))
72 = '((dvisaptatiH dvaasaptatiH) (dvisaptatitama dvaasaptatitama))
73 = '((trisaptatiH trayaHsaptatiH) (trisaptatitama trayaHsaptatitama))
74 = '((chatuHsaptatiH) (chatuHsaptatitama))
75 = '((pa~nchasaptatiH) (pa~nchasaptatitama))
76 = '((ShaTsaptatiH) (ShaTsaptatitama))
77 = '((saptasaptatiH) (saptasaptatitama))
78 = '((aShTasaptatiH aShTaasaptatiH) (aShTasaptatitama aShTaasaptatitama))
79 = '((navasaptatiH ekonaashiitiH uunaashiitiH ekaannaashiitiH) (navasaptatitama ekonaashiita uunaashiita ekaannaashiita ekonaashiititama uunaashiititama ekaannaashiititama))
80 = '((ashiitiH) (ashiititama))
81 = '((ekaashiitiH) (ekaashiititama))
82 = '((dvyashiitiH) (dvyashiititama))
83 = '((tryashiitiH) (tryashiititama))
84 = '((chaturashiitiH) (chaturashiititama))
85 = '((pa~nchaashiitiH) (pa~nchaashiititama))
86 = '((ShaDashiitiH) (ShaDashiititama))
87 = '((saptaashiitiH) (saptaashiititama))
88 = '((aShTaashiitiH) (aShTaashiititama))
89 = '((navaashiitiH ekonanavatiH uunanavatiH ekaannanavatiH) (navaashiititama ekonanavata uunanavata ekaannanavata ekonanavatitama uunanavatitama ekaannanavatitama))
90 = '((navatiH) (navatitama))
91 = '((ekanavatiH) (ekanavatitama))
92 = '((dvinavatiH dvaanavatiH) (dvinavatitama dvaanavatitama))
93 = '((triNavatiH trayoNavatiH) (triNavatitama trayoNavatitama))
94 = '((chaturNavatiH) (chaturNavatitama))
95 = '((pa~nchanavatiH) (pa~nchanavatitama))
96 = '((ShaDnavatiH) (ShaDnavatitama))
97 = '((saptanavatiH) (saptanavatitama))
98 = '((aShTanavatiH aShTaanavatiH) (aShTanavatitama aShTaanavatitama))
99 = '((navanavatiH ekonashatam uunashatam ekaannashatam) (navanavatitama ekonashatama uunashatama ekaannashatama))
)
