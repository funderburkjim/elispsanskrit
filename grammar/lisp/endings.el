; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; endings.el
; 08-29-03: material moved from 'gram2.el' and gram3.el
; --verbs
(defun solution (x)
 (let (y)
  (setq y x)
  (while (and y (listp y) (equal (length y) 1))
   (setq y (car y))
  )
  y
 )
)
(defun flatten (in)
 ; assume vin is a sequence
 ; return a  list
 (let (i n out x y)
  (setq n (length in))
  (setq i 0)
  (while (< i n)
   (setq x (elt in i))
   (setq i (1+ i))
   (cond
    ((stringp x)
     (setq out (append out (list x)))
    )
    ((sequencep x)
     (setq y (flatten x))
     (setq out (append out y))
    )
    (t
     (setq out (append out (list x)))
    )
   )
  )
  out
 )
)
(defun list-replace (l old new)
 (mapcar
  (lambda (x)
   (if (equal x old) new x)
  )
  l
 )
)
(defun construct-endings-all ()
 (let (bufin)
  (setq bufin
    (find-file-noselect (sangram-filename "endings.txt" "construct"))
  )
  (with-current-buffer bufin
   (erase-buffer)
  )
 )
 (construct-endings-1 ADJ-a-endings 'ADJ-a)
 (construct-endings-1 ADJ-a-endings-nN 'ADJ-a-N)
 (construct-endings-2 (elt ADJ-a-endings 1) 'NOUN-a 'M)
 (construct-endings-2 (elt ADJ-a-endings 3) 'NOUN-aa 'F)
 (construct-endings-2 (elt ADJ-a-endings 5) 'NOUN-a 'N)

 (construct-endings-2 (elt ADJ-a-endings-nN 1) 'NOUN-a-N 'M)
 (construct-endings-2 (elt ADJ-a-endings-nN 3) 'NOUN-aa-N 'F)
 (construct-endings-2 (elt ADJ-a-endings-nN 5) 'NOUN-a-N 'N)

 (construct-endings-2 (elt ADJ-a-endings 1) 'ADJ-aI 'M)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'F 'ii)))
      'ADJ-aI 'F)
 (construct-endings-2 (elt ADJ-a-endings 5) 'ADJ-aI 'N)

 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'F 'ii)))
		      'NOUN-ii 'F)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'M 'Ri-A)))
		      'NOUN-Ri-A 'M)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'M 'Ri-R)))
		      'NOUN-Ri-R 'M)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'F 'Ri-A)))
		      'NOUN-Ri-A 'F)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'F 'Ri-R)))
		      'NOUN-Ri-R 'F)
 (construct-endings-2 (vconcat (mapcar 'sym-without-space (sup-get 'N 'Ri)))
		      'NOUN-Ri 'N)
 (construct-endings-2 (vconcat
		       (mapcar 'sym-without-space (sup-get 'M 'Ri-ADJ)))
		      'ADJ-Ri 'M)
 (construct-endings-2 (vconcat
		       (mapcar 'sym-without-space (sup-get 'F 'Ri-ADJ)))
		      'ADJ-Ri 'F)
 (construct-endings-2 (vconcat
		       (mapcar 'sym-without-space (sup-get 'N 'Ri-ADJ)))
		      'ADJ-Ri 'N)
 
 (let (x y endings type)
  (setq x explain-subanta-endings)
  (while x
   (setq type (car x))
   (setq x (cdr x))
   (setq endings (car x))
   (setq x (cdr x))
   ; type is like 'i-ADJ-N' 'i-ADJ' or 'i-NOUN' or 'i-NOUN-N'
   ; these are to be changed to ADJ-i-N, ADJ-i, NOUN-i, NOUN-i-N
   (let (w w1 type1)
    (setq w (gen-word-list (symbol-name type) "-"))
    (if (equal (length w) 3)
     (progn
      (setq w1 (list (elt w 1) (elt w 0) "N"))
      (fol-msg (format "%s\n" w1))
     )
     (setq w1 (list (elt w 1) (elt w 0)))
    )
    (setq type1 (mapconcat 'concat w1 "-"))
    (construct-endings-1 endings type1)
   )
  )
 )
 ;-------
 (let (x y z w endings conj type type1 voice)
  (setq x conjtab-endings)
  (while x
   (setq conj (car x))
   (setq type1 (plist-get  all-tenses-SL-plist conj))
   (setq x (cdr x))
   (setq z (car x))
   (setq x (cdr x))
;   (construct-endings-1 endings type1)
   (while z
    (setq voice (car z))
    (setq z (cdr z))
    (setq endings (car z))
    (setq z (cdr z))
    (cond
     ((equal voice 'PA)
      (setq voice 'PV)
      (construct-endings-2 endings type1 voice)
     )
     ((equal voice 'P-N)
      ; skip this one, just for imperative 1S is there
      ; an 'n' which changes to retroflex nasal after 'r'
     )
     (t ; run of the mill
      (construct-endings-2 endings type1 voice)
     )
    )
    
   )
  )
 )
 ; present participle active parasmaipada
 (mapcar
  (lambda (strength)
   (let (endings code)
    (setq endings (plist-get2 PRESPART-endings (list 'P strength)))
    (setq code (format "ADJ-PRAP-%s" strength))
    (construct-endings-1 endings code)
   )
  )
  '(S W SW VW)
 )
 ; mat and vat type adjectives and nouns
 (construct-endings-1 at-endings 'ADJ-at)
 (construct-endings-2 (elt at-endings 1) 'NOUN-at 'M)
 (construct-endings-2 (elt at-endings 3) 'NOUN-at 'F)
 (construct-endings-2 (elt at-endings 5) 'NOUN-at 'N)
 ; nouns, adjs ending in 'in'
 (let (tmp)
  (setq tmp '(M [inii inau inaH inam inau inaH inaa ibhyaam ibhiH ine ibhyaam ibhyaH inaH ibhyaam ibhyaH inaH inoH inaam ini inoH isu i inau inaH] F [inii iyau iyaH iniim iyau iniiH iyaa iniibhyaam iniibhiH iyai iniibhyaam iniibhyaH iyaaH iniibhyaam iniibhyaH iyaaH iyoH iniinaam iyaam iyoH iniiShu ini iyau iyaH] N [i inii ini i inii ini inaa ibhyaam ibhiH ine ibhyaam ibhyaH inaH ibhyaam ibhyaH inaH inoH inaam ini inoH isu (i in) inii ini]))
 (construct-endings-1 tmp 'ADJ-in)
 (construct-endings-2 (elt tmp 1) 'NOUN-in  'M)
 (construct-endings-2 (elt tmp 3) 'NOUN-in 'F)
 (construct-endings-2 (elt tmp 5) 'NOUN-in 'N)
 )
 ; 4-14-04. Nouns ending in 'man' (preceded by a constant or not)
 (construct-endings-2
  [a aRI ARi a aRI ARi aRA aByAm aBiH aRe aByAm aByaH aRaH aByAm aByaH
     aRaH aRoH aRAm aRi aRoH asu (a an) aRI ARi]
  'NOUN-manC-N  'N  nil 't)
 (construct-endings-2
  [a anI Ani a anI Ani anA aByAm aBiH ane aByAm aByaH anaH aByAm aByaH
     anaH anoH anAm ani anoH asu (a an) anI Ani]
  'NOUN-manC 'N  nil 't)

 (construct-endings-2
  [A ARO ARaH ARam ARO aRaH aRA aByAm aBiH aRe aByAm aByaH aRaH aByAm 
      aByaH aRaH aRoH aRAm aRi aRoH asu an ARO ARaH]
  'NOUN-manC-N  'M  nil 't)
 (construct-endings-2
  [A AnO AnaH Anam AnO anaH anA aByAm aBiH ane aByAm aByaH anaH aByAm 
    aByaH anaH anoH anAm ani anoH asu an AnO AnaH]
  'NOUN-manC 'M  nil 't)


 (construct-endings-2
  [A ARO ARaH ARam ARO aRaH aRA aByAm aBiH aRe aByAm aByaH aRaH aByAm 
      aByaH aRaH aRoH aRAm aRi aRoH asu an ARO ARaH]
  'NOUN-manC-N  'F  nil 't)
 (construct-endings-2
  [A AnO AnaH Anam AnO anaH anA aByAm aBiH ane aByAm aByaH anaH aByAm 
    aByaH anaH anoH anAm ani anoH asu an AnO AnaH]
  'NOUN-manC 'F nil 't)

 (construct-endings-2
  [A AnO AnaH Anam AnO naH nA aByAm aBiH ne aByAm aByaH naH aByAm aByaH
     naH noH  (ni ani) noH asu an AnO AnaH]
  'NOUN-manV 'M nil 't)
 (construct-endings-2
  [A AnO AnaH Anam AnO naH nA aByAm aBiH ne aByAm aByaH naH aByAm aByaH
     naH noH  (ni ani) noH asu an AnO AnaH]
  'NOUN-manV 'F nil 't)
 (construct-endings-2
  [a (nI anI) Ani a (nI anI) Ani nA aByAm aBiH ne aByAm aByaH naH aByAm
     aByaH naH noH  (ni ani) noH asu (a an) (nI anI) Ani]
  'NOUN-manV 'N nil 't)

 (construct-endings-2
  [A ARO ARaH ARam ARO RaH RA aByAm aBiH Re aByAm aByaH RaH aByAm aByaH 
    RaH RoH RAm (Ri aRi) RoH asu an ARO ARaH]
  'NOUN-manV-N 'M nil 't)
 (construct-endings-2
  [A ARO ARaH ARam ARO RaH RA aByAm aBiH Re aByAm aByaH RaH aByAm aByaH 
    RaH RoH RAm (Ri aRi) RoH asu an ARO ARaH]
  'NOUN-manV-N 'F nil 't)
 (construct-endings-2
  [a (RI aRI) ARi a (RI aRI) ARi RA aByAm aBiH Re aByAm aByaH RaH 
   aByAm aByaH RaH RoH RAm (Ri aRi) RoH asu (a an) (RI aRI) ARi]
  'NOUN-manV-N 'N nil 't)

 ; done.. close the file
 (let (bufin)
  (setq bufin
    (find-file-noselect (sangram-filename "endings.txt" "construct"))
  )
  (with-current-buffer bufin
   (save-buffer)
  )
 )
)
(defun construct-endings-1 (x category-subtype-cond 
   &optional bufin no-translate)
  (let (gender endings)
   (while x
    (setq gender (car x))
    (setq x (cdr x))
    (setq endings (car x))
    (setq x (cdr x))
    (construct-endings-2 endings category-subtype-cond gender bufin
			 no-translate)
   )
   t
  )
)
(defun construct-endings-2 (endings category-subtype-cond gender 
   &optional bufin no-translate)
 (if (not bufin)
   (setq bufin
    (find-file-noselect (sangram-filename "endings.txt" "construct"))
   )
  ) 
 (with-current-buffer bufin
  (goto-char (point-max))
  (insert (format-endings endings category-subtype-cond gender no-translate))
 )
)
(defun format-endings (endings-in category-subtype-cond gender 
  &optional no-translate)
 (let (ans i e n cn endings theset)
  (cond
   ((member gender '(M F N)) (setq theset case-number-set))
   ((member gender '(P A PV)) (setq theset person-number-set))
   (t
    (fol-msg (format "format-endings: unknown '%s'\n" gender))
   )
  )
  (if no-translate
   (setq endings endings-in)
   (setq endings (translate-ITRANS-SLP1 endings-in)) ; default
  )
  (setq n (length endings))
  (setq i 0)
  (setq ans "")
  (while (< i n)
   (setq e (elt endings i))
   (setq cn (sym-without-space (elt theset i)))
   (if (not (listp e)) (setq e (list e)))
   (mapcar
    (lambda (e1)
     (setq ans (concat ans
      (format "%s %s %s %s %s\n" e1 category-subtype-cond gender i cn)
      ))
    )
    e
   )
   (setq i (1+ i))
  )
 ans
 )
)

(defvar at-endings nil)
(defun init-one-sup (endar-sym)
 (let (i p e n endar endar-string prop sym endar-vec)
  (setq sym 'Subanta)
  (setq prop endar-sym)
  (setq endar-string (eval endar-sym))
  (setq endar (ITRANS-parse-words-1 endar-string))
  (setq endar (list-replace endar [DANDA] []))
  (setq endar (sublist-gather endar [Li] [Li]))
  (setq endar-vec (vconcat endar))
  (sanput sym endar-sym endar-vec)
  t
 )
)
(defun sublist-gather (xin open-list close-list)
; xin is a sequence
; returns a list
 (let (xout i n x)
  (setq n (length xin))
  (setq xout nil)
  (setq i 0)
  (while (< i n)
   (setq x (elt xin i))
   (setq i (1+ i))
   (cond
    ((equal x open-list)
     (let (xin1 xout1 isave unfound y)
      (setq isave i)
      (setq xin1 nil)
      (setq unfound t)
      (while (and unfound (< i n))
       (setq y (elt xin i))
       (setq i (1+ i))
       (if (equal y close-list)
	(setq unfound nil)
	(setq xin1 (append xin1 (list y)))
       )
      )
      (when (not unfound) ; recursive call
       (setq xout1 (sublist-gather (vconcat xin1) open-list close-list))
       (setq xout (append xout (list xout1)))
      )
      (when unfound
       (fol-msg (format "sublist-gather error at element %s for %s\n"
			isave xin))
       (setq i n)
      )
     )
    )
    (t
     (setq xout (append xout (list x)))
    )
   )
  )
;  (fol-msg (format "gather: xin=%s\n xout=%s\n" xin xout))
  xout
 )
)
(defun init-ending-consonants ()
 (setq ending-consonants-1cons
  '((k k) (kh) (g) (gh) (~N ~N)
    (ch k) (Ch) (j k T) (jh) (~n)
    (T) (Th) (D) (Dh) (N)
    (t t) (th t) (d t) (dh t) (n n)
    (p p) (ph) (b) (bh p) (m m)
    (y) (r H) (l) (v) (h k T)
    (Sh k T) (sh T) (s H) (H) (M)
   )
 )
)
(defun init-one-vsup (sup-sym)
 (let (sup-string tense propar endar-string i p e n endar x endar-vec)
  (setq sup-string (symbol-name sup-sym))
  (let (sym)
   (setq sym (intern (format "%s-endings" sup-string)))
   (setq endar-string (eval sym))
  )
  (let (s prop-string prop-sym)
   (setq i (string-match "-" sup-string))
   (setq s (substring sup-string 0 i))
   (setq tense (intern s))
   (setq i (1+ i))
   (setq prop-string (format "%s-properties-set" (substring sup-string i)))
   (setq prop-sym (intern prop-string))
   (setq propar (eval prop-sym))
  )
  (setq endar (ITRANS-parse-words-1 endar-string))
  (setq endar-vec (vconcat endar))
  (setq n (length propar))
  (setq i 0)
  (while (< i n)
    (setq x (elt endar-vec i))
    (when (equal x [DANDA])
     (setq x [])
     (aset endar-vec i x)
    )
    (sanput tense (elt propar i) x)
   (setq i (1+ i))
  )
; (setq endar (append endar-vec nil))
  (sanput 'Sup sup-sym endar-vec)
 )
)
(defun init-one-vsup-strength (sup-sym)
 (let (sup-string sym endar-vec)
  (setq sup-string (symbol-name sup-sym))
  (setq sym (intern (format "%s-strengths" sup-string)))
  (when sym
   (condition-case nil
    (progn
     (setq endar-vec (eval sym))
     (sanput 'Sup sym endar-vec)
    )
    (error nil)
   )
  )
 )
)
(defun init-vsup-list ()
 (setq vsup-list '(
  laT-1-P laT-1-A laT-2-P laT-2-A
  la~N-1-P la~N-1-A la~N-2-P la~N-2-A
  loT-1-P loT-1-A loT-2-P loT-2-A
  vidhili~N-1-P vidhili~N-1-A vidhili~N-2-P vidhili~N-2-A
  liT-1-P liT-1-A
  luT-1-P luT-1-A
  lRiT-1-P lRiT-1-A
  lRi~N-1-P lRi~N-1-A
  aashiirli~N-1-P aashiirli~N-1-A
  lu~N1-1-P
  lu~N2-1-P lu~N2-1-A
  lu~N3-1-P lu~N3-1-A
  lu~N4-1-P lu~N4-1-A
  lu~N5-1-P lu~N5-1-A
  lu~N6-1-P 
  lu~N7-1-P lu~N7-1-A
 
 ))
)
(defun init-vsup ()
 (init-vsup-list)
 (let (all x)
  (setq all vsup-list)
  (while all
   (setq x (car all))
   (setq all (cdr all))
   (init-one-vsup x)
   (init-one-vsup-strength x)
  )
  't
 )
 (init-ADJ-a-endings) ; 10-12-03
 (init-explain-subanta-endings) ; 10-27-03
 (init-conjtab-endings) ; 10-08-03
 (init-causal-endings) ; 11-28-03
 (init-PRESPART-endings) ; 10-10-03
 (init-PASSPART-endings) ; 10-11-03
 (init-PPPART-endings) ; 10-11-03
 (init-RPPART-P-endings) ; 10-12-03
 (setq at-endings
  '(M [aan antau antaH antam antau antaH ataa adbhyaam adbhiH ate adbhyaam adbhyaH ataH adbhyaam adbhyaH ataH atoH ataam ati atoH atsu an antau antaH] F [atii atyau atyaH atiim atyau atiiH atyaa atiibhyaam atiibhiH atyai atiibhyaam atiibhyaH atyaaH atiibhyaam atiibhyaH atyaaH atyoH atiinaam atyaam atyoH atiiShu ati atyau atyaH] N [at atii anti at atii anti ataa adbhyaam adbhiH ate adbhyaam adbhyaH ataH adbhyaam adbhyaH ataH atoH ataam ati atoH atsu at atii anti])) ; 3-19-04
 't
)
;--nouns
(defun Subanta-initAll ()
 (put 'Subanta 'Sangram nil)
)
(defun Subanta-initElt (prop)
 (sanput 'Subanta prop nil)
)
(defun init-gender-form-data ()
 (setq declension-cons-forms
       '(1cons
	 vat vat-ADJ mat mat-ADJ iiyas-ADJ in in-ADJ
	 vas-ADJ an an-ADJ ach-ADJ aach-ADJ
;         IRR-PRON
	 3cons at-ADJ
        ))
 (setq gender-form-data-0
  '((M 1cons) (F 1cons) (N 1cons) 
    (M 3cons) (F 3cons) (N 3cons) 
    (PRON IRR) (M IRR-PRON) (F IRR-PRON) (N IRR-PRON) ; aham -> aham
    (M IRR) (F IRR) (N IRR)
    (F au) ; nau -> nau
    (M au) ; glau -> glau
    (ADJ at) ; sat -> sat 
 ))
 (setq gender-form-data-1
  '((F aa) ; lataa -> lat
    (F ii) ; nadii -> nad
    (M Ri-A) ; netRi -> net
    (F Ri-A) ; svasRi -> svas
    (M Ri-R) ; pitRi -> pit
    (F Ri-R) ; maatRi -> maat
    (N i) ; vaari -> vaar
    (N u) ; madhu -> madh
    (N Ri) ; dhaatRi -> dhaat
    (PRON a) (M a-PRON) (F a-PRON) (N a-PRON) ; anya -> any
    (PRON c) (M c-PRON) (F c-PRON) (N c-PRON) ; puurva -> puurv
    (PRON d) (M d-PRON) (F d-PRON) (N d-PRON) ; nema -> nem
    (PRON adj) (M adj-PRON) (F adj-PRON) (N adj-PRON) ; sarva -> sarv
    (PRON ORD) (M ORD-PRON) (F ORD-PRON) (N ORD-PRON) ; pa~nchama -> pa~ncham
    (PRON ORDa) (M ORDa-PRON) (F ORDa-PRON) (N ORDa-PRON) 
    (PRON ORDb) (M ORDb-PRON) (F ORDb-PRON) (N ORDb-PRON)
    (ADJ a) ;  andha -> andh
    (ADJ i) ; sugandhi -> sugandh
    (ADJ u) ; chaaru -> chaar
    (ADJ Ri) ; jetRi -> jet
    (ADJ aI) ; puraatana -> puraatan
     (M a-ADJ) (F a-ADJ) (N a-ADJ) ; andha -> andh
     (M i-ADJ) (F i-ADJ) (N i-ADJ) ; sugandhi -> sugandh
     (M u-ADJ) (F u-ADJ) (N u-ADJ) ; chaaru -> chaar
     (M Ri-ADJ) (F Ri-ADJ) (N Ri-ADJ) ; jetRi -> jet
     (M aI-ADJ) (F aI-ADJ) (N aI-ADJ) ; 
    (ADJ aach) (M aach-ADJ)  (N aach-ADJ) (F aach-ADJ) ; praach -> praa
    (F ii1) ; dhii -> dh
    (F ii2) ; strii -> str
    (F ii3) ; lakShmii -> lakShm
    (F uu1) ; bhruu -> bhr
    (N i1) ; akShi -> akSh
    (F o) (M o) ; go -> g
    (F ai) ; rai -> r
    ; moved from gender-form-data-2 (12-09-03)
    (M a) ; raama -> raam
    (N a) ; vana -> van
    (M i) ; muni -> mun
    (M u) ; guru -> gur
    (F i) ; mati -> mat
    (F u) ; rajju -> rajj
    (F uu) ; vadhuu -> vadh
    (M aa) ; gopaa -> gop
    (M ii) ; senaanii -> senaan; sudhiiH -> sudh
    (M ii1) ; senaanii -> senaan; sudhiiH -> sudh
    (M ii0) ; vaatapramii -> vaatapram
    (M uu) ; pratibhuu -> pratibh
    (M uu1) ; khalapuu -> khalap
    (M ai) ; rai -> r

  ))
 (setq gender-form-data-2
 '(
   (PRON b) (M b-PRON) (F b-PRON) (N b-PRON) ; yat -> y
   (M vat)  (N vat) (F vat)    ; balavat -> balav
   (M mat)  (N mat) (F mat)    ; dhiimat -> dhiim
   (ADJ vat) (M vat-ADJ)  (N vat-ADJ) (F vat-ADJ)
   (ADJ mat) (M mat-ADJ)  (N mat-ADJ) (F mat-ADJ)
   (ADJ iiyas) (M iiyas-ADJ)  (N iiyas-ADJ) (F iiyas-ADJ)
   (M in) (N in) (F in)  ; balin -> bal
   (ADJ in) (M in-ADJ) (F in-ADJ) (N in-ADJ)
   (M an) (N an) (F an)  ; raajan -> raaj
   (ADJ an) (M an-ADJ) (F an-ADJ) (N an-ADJ)
   (PRON CARD) (M CARD-PRON) (F CARD-PRON) (N CARD-PRON) ; pa~nchan -> pa~nch
  ) )
 (setq gender-form-data-3
  '((ADJ ach) (M ach-ADJ)  (N ach-ADJ) (F ach-ADJ) ; pratyach -> prat
   )
 )
 (setq gender-form-data-4 
 '((ADJ vas) (M vas-ADJ)  (N vas-ADJ) (F vas-ADJ) ; chakRivas -> chak
  )
 )
 (setq gender-form-data-all 
  (append gender-form-data-0  gender-form-data-1 gender-form-data-2
	  gender-form-data-3 gender-form-data-4
     )
 )
 t
)
(defun init-sup-list ()
 (init-gender-form-data)
 (setq sup-list '(
   sup-M-normal sup-F-normal sup-N-normal))
 (let (all x y z gender form)
  (setq all gender-form-data-all)
  (while all
   (setq x (car all))
   ; x = (gender form)
   (setq all (cdr all))
   (setq gender (elt x 0))
   (setq form (elt x 1))
   (when (member gender '(M F N))
    (setq y (format "sup-%s-%s" gender form))
    (setq z (intern y))
    (setq sup-list (append sup-list (list z)))
   )
  )
 )
)

(defun init-sup ()
 (let (all x)
  (init-sup-list)
  (init-gender-form-data)
  (setq all sup-list)
  (while all
   (setq x (car all))
   (setq all (cdr all))
   (init-one-sup x)
  )
  t
 )
)
(defun sup-get (v1 v2 &optional pfx sym)
; v1 is gender, v2 is additional speicifier. 
; v1 and v2 may be symbols or strings
 (let (prop val sups)
  (if (not pfx) (setq pfx 'sup))
  (if (not sym) (setq sym 'Subanta))
  (setq prop (intern (format "%s-%s-%s" pfx v1 v2)))
  (setq sups (sanget sym prop))
  (if (not sups)
    t ; (fol-msg (format "sup-get error. %s %s\n" v1 v2))
    (if (not (vectorp sups)) (setq sups (vconcat sups))) ; turn to vector
  )
  sups
 )
)
(defun conj-endings (tense-sym class pada)
 (let (conj-class sym name endings)
  (setq conj-class (if (member class '(1 4 6 10)) 1 2))
  (setq name (format "%s-%s-%s" tense-sym conj-class  pada))
  (setq sym (intern-soft name))
  (setq endings (sanget 'Sup sym))
  (setq endings (copy-sequence endings))
  endings
 )
)
(defun conj-strengths (tense-sym class pada)
 (let (conj-class sym name strengths)
  (setq conj-class (if (member class '(1 4 6 10)) 1 2))
   (setq name (format "%s-%s-%s-strengths" tense-sym conj-class  pada))
   (setq sym (intern-soft name))
   (setq strengths (sanget 'Sup sym))
   (setq strengths (copy-sequence strengths))
  strengths
 )
)
(defvar PRESPART-endings nil)
(defvar PASSPART-endings nil)
(defvar PPPART-endings nil)
(defvar ADJ-a-endings nil)
(defvar ADJ-a-endings-nN nil)
(defvar RPPART-P-endings nil)
(defvar conjtab-endings nil)
(defvar causal-endings nil)
(defun old-init-class-a-mod-endings ()
 (let (ans tenses padas endings-tok endings tense pada ansP ansA)
  (setq tenses '(laT la~N loT vidhili~N))
  (while tenses
   (setq tense (car tenses))
   (setq tenses (cdr tenses))
   (setq endings-tok (modify-conj-endings-1 tense 'P))
   (setq ansP (mapcar 'sym-without-space endings-tok))
   (setq endings-tok (modify-conj-endings-1 tense 'A))
   (setq ansA (mapcar 'sym-without-space endings-tok))
   (setq ans (append ans (list tense (list 'P ansP 'A ansA))))
  )
;  (setq class-a-mod-endings ans)
 )
)
(defun init-causal-endings ()
 (setq causal-endings 
  '(laT 
    (P [ati ataH anti asi athaH atha aami aavaH aamaH]
     A [ate ete ante ase ethe adhve e aavahe aamahe]
     PA [ate ete ante ase ethe adhve e aavahe aamahe]
    )
   la~N
    (P [at ataam an aH atam ata am aava aama]
     A [ata etaam anta athaaH ethaam adhvam e aavahi aamahi]
     PA [ata etaam anta athaaH ethaam adhvam e aavahi aamahi]
    )
   loT
    (P [atu ataam antu a atam ata aani aava aama]
     P-N [atu ataam antu a atam ata aaNi aava aama]
     A [ataam etaam antaam asva ethaam adhvam ai aavahai aamahai]
     PA [ataam etaam antaam asva ethaam adhvam ai aavahai aamahai]
    )
   vidhili~N
    (P [et etaam eyuH eH etam eta eyam eva ema]
     A [eta eyaataam eran ethaaH eyaathaam edhvam eya evahi emahi]
     PA [eta eyaataam eran ethaaH eyaathaam edhvam eya evahi emahi]
    )
   luT
    (P [itaa itaarau itaaraH itaasi itaasthaH itaastha itaasmi itaasvaH itaasmaH]
     A [itaa itaarau itaaraH itaase itaasaathe itaadhve itaahe itaasvahe itaasmahe]
     PA [(ayitaa itaa) (ayitaarau itaarau) (ayitaaraH itaaraH) (ayitaase itaase) (ayitaasaathe itaasaathe) (ayitaadhve itaadhve) (ayitaahe itaahe) (ayitaasvahe itaasvahe) (ayitaasmahe itaasmahe)]
    )
   lRiT
    (P [iShyati iShyataH iShyanti iShyasi iShyathaH iShyatha iShyaami iShyaavaH iShyaamaH]
     A [iShyate iShyete iShyante iShyase iShyethe iShyadhve iShye iShyaavahe iShyaamahe]
     PA [(ayiShyate iShyate) (ayiShyete iShyete) (ayiShyante iShyante) (ayiShyase iShyase) (ayiShyethe iShyethe) (ayiShyadhve iShyadhve) (ayiShye iShye) (ayiShyaavahe iShyaavahe) (ayiShyaamahe iShyaamahe)]
    )
   lRi~N
    (P [iShyat iShyataam iShyan iShyaH iShyatam iShyata iShyam iShyaava iShyaama]
     A [iShyata iShyetaam iShyanta iShyathaaH iShyethaam iShyadhvam iShye iShyaavahi iShyaamahi]
     PA [(ayiShyata iShyata) (ayiShyetaam iShyetaam) (ayiShyanta iShyanta) (ayiShyathaaH iShyathaaH) (ayiShyethaam iShyethaam) (ayiShyadhvam iShyadhvam) (ayiShye iShye) (ayiShyaavahi iShyaavahi) (ayiShyaamahi iShyaamahi)]
    )
   aashiirli~N
    (P [yaat yaastaam yaasuH yaaH yaastam yaasta yaasam yaasva yaasma]
     A [iShiiShTa iShiiyaastaam iShiiran iShiiShThaaH iShiiyaasthaam iShiidhvam iShiiya iShiivahi iShiimahi]
     PA [(ayiShiiShTa iShiiShTa) (ayiShiiyaastaam iShiiyaastaam) (ayiShiiran iShiiran) (ayiShiiShThaaH iShiiShThaaH) (ayiShiiyaasthaam iShiiyaasthaam) (ayiShiidhvam iShiidhvam) (ayiShiiya iShiiya) (ayiShiivahi iShiivahi) (ayiShiimahi iShiimahi)]
    )
   lu~N3
    (P [at ataam an aH atam ata am aava aama]
     A [ata etaam anta athaaH ethaam adhvam e aavahi aamahi]
     PA [i (iShaataam ayiShaataam) (iShata ayiShata) (iShThaaH ayiShThaaH) (iShaathaam ayiShaathaam) (idhvam ayidhvam) (iShi ayiShi) (iShvahi ayiShvahi) (iShmahi ayiShmahi)]
    )
   liT-p
    (P [(aamaasa aaMchakaara aaMbabhuuva) (aamaasatuH aaMchakratuH aaMbabhuuvatuH) (aamaasuH aaMchakruH aaMbabhuuvuH) (aamaasitha aaMchakartha aaMbabhuuvitha) (aamaasathuH aaMchakrathuH aaMbabhuuvathuH) (aamaasa aaMchakra aaMbabhuuva) (aamaasa aaMchakara aaMchakaara aaMbabhuuva) (aamaasiva aaMchakRiva aaMbabhuuviva) (aamaasima aaMchakRima aaMbabhuuvima)]
     A [(aamaasa aaMchakre aaMbabhuuva) (aamaasatuH aaMchakraate aaMbabhuuvatuH) (aamaasuH aaMchakrire aaMbabhuuvuH) (aamaasitha aaMchakRiShe aaMbabhuuvitha) (aamaasathuH aaMchakraathe aaMbabhuuvathuH) (aamaasa aaMchakRiDhve aaMbabhuuva) (aamaasa aaMchakre aaMbabhuuva) (aamaasiva aaMchakRivahe aaMbabhuuviva) (aamaasima aaMchakRimahe aaMbabhuuvima)]
     PA [(aamaase aaMchakre aaMbabhuuve) (aamaasaate aaMchakraate aaMbabhuuvaate) (aamaasire aaMchakrire aaMbabhuuvire) (aamaasiShe aaMchakRiShe aaMbabhuuviShe) (aamaasaathe aaMchakraathe aaMbabhuuvaathe) (aamaasidhve aaMchakRiDhve aaMbabhuuvidhve) (aamaase aaMchakre aaMbabhuuve) (aamaasivahe aaMchakRivahe aaMbabhuuvivahe) (aamaasimahe aaMchakRimahe aaMbabhuuvimahe)]
    )
  )
 )
)
(defun init-conjtab-endings ()
 (setq conjtab-endings
 '(laT 
  (P [ati ataH anti asi athaH atha aami aavaH aamaH] 
   A [ate ete ante ase ethe adhve e aavahe aamahe]
  ) 
  la~N 
  (P [at ataam an aH atam ata am aava aama] 
   A [ata etaam anta athaaH ethaam adhvam e aavahi aamahi]
  ) 
  loT 
  (P [atu ataam antu a atam ata aani aava aama]
   P-N [atu ataam antu a atam ata aaNi aava aama]
   A [ataam etaam antaam asva ethaam adhvam ai aavahai aamahai]
  ) 
  vidhili~N 
  (P [et etaam eyuH eH etam eta eyam eva ema] 
   A [eta eyaataam eran ethaaH eyaathaam edhvam eya evahi emahi]
  )
  luT
  (P [aa aarau aaraH aasi aasthaH aastha aasmi aasvaH aasmaH]
   A [aa aarau aaraH aase aasaathe aadhve aahe aasvahe aasmahe]
  )
  lRiT
  (P [yati yataH yanti yasi yathaH yatha yaami yaavaH yaamaH]
   A [yate yete yante yase yethe yadhve ye yaavahe yaamahe]
  )
  aashiirli~N
  (P [yaat yaastaam yaasuH yaaH yaastam yaasta yaasam yaasva yaasma]
   A [iiShTa iiyaastaam iiran iiShThaaH iiyaasthaam iidhvam iiya iivahi iimahi]
  )
  lRi~N
  (P [yat yataam yan yaH yatam yata yam yaava yaama]
   A [yata yetaam yanta yathaaH yethaam yadhvam ye yaavahi yaamahi]
  )
  liT-p
  (P [(aamaasa aaMchakaara aaMbabhuuva) (aamaasatuH aaMchakratuH aaMbabhuuvatuH) (aamaasuH aaMchakruH aaMbabhuuvuH) (aamaasitha aaMchakartha aaMbabhuuvitha) (aamaasathuH aaMchakrathuH aaMbabhuuvathuH) (aamaasa aaMchakra aaMbabhuuva) (aamaasa aaMchakara aaMchakaara aaMbabhuuva) (aamaasiva aaMchakRiva aaMbabhuuviva) (aamaasima aaMchakRima aaMbabhuuvima)]
   A [(aamaasa aaMchakre aaMbabhuuva) (aamaasatuH aaMchakraate aaMbabhuuvatuH) (aamaasuH aaMchakrire aaMbabhuuvuH) (aamaasitha aaMchakRiShe aaMbabhuuvitha) (aamaasathuH aaMchakraathe aaMbabhuuvathuH) (aamaasa aaMchakRiDhve aaMbabhuuva) (aamaasa aaMchakre aaMbabhuuva) (aamaasiva aaMchakRivahe aaMbabhuuviva) (aamaasima aaMchakRimahe aaMbabhuuvima)]
  )
  lu~N1
  (P [aat aataam uH aaH aatam aata aam aava aama] 
   A nil
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N2
  (P [at ataam an aH atam ata am aava aama]
     ;[t taam an H tam ta am va ma]
   A [ata etaam anta athaaH ethaam adhvam e aavahi aamahi] 
     ;[ta itaam anta thaaH ithaam dhvam i vahi mahi]
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N3
  (P [at ataam an aH atam ata am aava aama]
     ;[t taam an H tam ta am va ma]
   A [ata etaam anta athaaH ethaam adhvam e aavahi aamahi] 
     ;[ta itaam anta thaaH ithaam dhvam i vahi mahi]
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N4
  (P [siit staam suH siiH stam sta sam sva sma]
   A [sta saataam sata sthaaH saathaam dhvam si svahi smahi]
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N5
  (P [iit iShTaam iShuH iiH iShTam iShTa iSham iShva iShma]
   A [iShTa iShaataam iShata iShThaaH iShaathaam idhvam iShi iShvahi iShmahi]
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N6
  (P [siit siShTaam siShuH siiH siShTam siShTa siSham siShva siShma]
   A nil
   PA [i nil nil  nil nil nil  nil nil nil]
  )
  lu~N7
  (P [at ataam an aH atam ata am aava aama]
   ; [sat sataam san saH satam sata sam saava saama]
   A [ata aataam anta athaaH aathaam adhvam i aavahi aamahi]
   ; [sata saataam santa sathaaH saathaam sadhvam si saavahi saamahi]
   PA [i nil nil  nil nil nil  nil nil nil]
  )

 )
 )
 t
)
(defun init-PRESPART-endings ()
 (setq PRESPART-endings
  '(P
   (S
   (M 
    [ n ntau ntaH 
     ntam ntau taH  
     taa dbhyaam dbhiH 
     te dbhyaam dbhyaH 
     taH dbhyaam dbhyaH 
     taH toH taam 
     ti toH tsu
     n ntau ntaH
    ] 
    N 
    [ t ntii nti 
     t ntii nti 
     taa dbhyaam dbhiH 
     te dbhyaam dbhyaH 
     taH dbhyaam dbhyaH 
     taH toH taam 
     ti toH tsu 
     t ntii nti
    ] 
    F 
    [ ntii ntyau ntyaH 
     ntiim ntyau ntiiH 
     ntyaa ntiibhyaam ntiibhiH 
     ntyai ntiibhyaam ntiibhyaH 
     ntyaaH ntiibhyaam ntiibhyaH 
     ntyaaH ntyoH ntiinaam 
     ntyaam ntyoH ntiiShu 
     nti ntyau ntyaH
    ]
   )
  
  W
  (M [ n ntau ntaH ntam ntau taH taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu n ntau ntaH] N [ t tii nti t tii nti taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu t tii nti] F [ tii tyau tyaH tiim tyau tiiH tyaa tiibhyaam tiibhiH tyai tiibhyaam tiibhyaH tyaaH tiibhyaam tiibhyaH tyaaH tyoH tiinaam tyaam tyoH tiiShu ti tyau tyaH])
  
  SW
   (M [ n ntau ntaH ntam ntau taH taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu n ntau ntaH] N [ t (tii ntii) nti t (tii ntii) nti taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu t (tii ntii) nti] F [(tii ntii) (tyau ntyau) (tyaH ntyaH) (tiim ntiim) (tyau ntyau) (tiiH ntiiH) (tyaa ntyaa) (tiibhyaam ntiibhyaam) (tiibhiH ntiibhiH) (tyai ntyai) (tiibhyaam ntiibhyaam) (tiibhyaH ntiibhyaH) (tyaaH ntyaaH) (tiibhyaam ntiibhyaam) (tiibhyaH ntiibhyaH) (tyaaH ntyaaH) (tyoH ntyoH) (tiinaam ntiinaam) (tyaam ntyaam) (tyoH ntyoH) (tiiShu ntiiShu) (ti nti) (tyau ntyau) (tyaH ntyaH)])
 VW
 (M
 [t tau taH tam tau taH taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu t tau taH]
 N
 [t tii (ti nti) t tii (ti nti) taa dbhyaam dbhiH te dbhyaam dbhyaH taH dbhyaam dbhyaH taH toH taam ti toH tsu t tii (ti nti)]
 F
 [ tii tyau tyaH tiim tyau tiiH tyaa tiibhyaam tiibhiH tyai tiibhyaam tiibhyaH tyaaH tiibhyaam tiibhyaH tyaaH tyoH tiinaam tyaam tyoH tiiShu ti tyau tyaH]
 )
 
  )
  A
   (M 
    [ aH au aaH 
     am au aan 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a au aaH
    ] 
    N 
    [ am e aani 
     am e aani 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a e aani
    ] 
    F 
    [ aa e aaH 
     aam e aaH 
     ayaa aabhyaam aabhiH 
     aayai aabhyaam aabhyaH 
     aayaaH aabhyaam aabhyaH 
     aayaaH ayoH aanaam 
     aayaam ayoH aasu 
     e e aaH
    ]
   )
  )
 )
)
(defun init-PASSPART-endings ()
 (setq PASSPART-endings
  '(M 
    [ aH au aaH 
     am au aan 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a au aaH
    ] 
    N 
    [ am e aani 
     am e aani 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a e aani
    ] 
    F 
    [ aa e aaH 
     aam e aaH 
     ayaa aabhyaam aabhiH 
     aayai aabhyaam aabhyaH 
     aayaaH aabhyaam aabhyaH 
     aayaaH ayoH aanaam 
     aayaam ayoH aasu 
     e e aaH
    ]
   )
  )
)
(defun init-PPPART-endings ()
 (setq PPPART-endings
 '(PPPART ; past passive participle (kta)
   (M 
    [ aH au aaH 
     am au aan 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a au aaH
    ] 
    N 
    [ am e aani 
     am e aani 
     ena aabhyaam aiH 
     aaya aabhyaam ebhyaH 
     aat aabhyaam ebhyaH 
     asya ayoH aanaam 
     e ayoH eShu 
     a e aani
    ] 
    F 
    [ aa e aaH 
     aam e aaH 
     ayaa aabhyaam aabhiH 
     aayai aabhyaam aabhyaH 
     aayaaH aabhyaam aabhyaH 
     aayaaH ayoH aanaam 
     aayaam ayoH aasu 
     e e aaH
    ]
   )
   PAPART ; past active participle (ktavat)
   (
    M
    [ aan antau antaH antam antau ataH ataa adbhyaam adbhiH ate adbhyaam adbhyaH ataH adbhyaam adbhyaH ataH atoH ataam ati atoH atsu an antau antaH]
    F
    [ atii atyau atyaH atiim atyau atiiH atyaa atiibhyaam atiibhiH atyai atiibhyaam atiibhyaH atyaaH atiibhyaam atiibhyaH atyaaH atyoH atiinaam atyaam atyoH atiiShu ati atyau atyaH]
    N
    [ at atii anti at atii anti ataa adbhyaam adbhiH ate adbhyaam adbhyaH ataH adbhyaam adbhyaH ataH atoH ataam ati atoH atsu at atii anti]
   )
  )
 )
)
(defun init-ADJ-a-endings ()
 (setq ADJ-a-endings
 '(M  [aH au aaH am au aan ena aabhyaam aiH aaya aabhyaam ebhyaH aat aabhyaam ebhyaH asya ayoH aanaam e ayoH eShu a au aaH] F  [aa e aaH aam e aaH ayaa aabhyaam aabhiH aayai aabhyaam aabhyaH aayaaH aabhyaam aabhyaH aayaaH ayoH aanaam aayaam ayoH aasu e e aaH] N [am e aani am e aani ena aabhyaam aiH aaya aabhyaam ebhyaH aat aabhyaam ebhyaH asya ayoH aanaam e ayoH eShu a e aani])
 )
 (setq ADJ-a-endings-nN
 '(M [aH au aaH am au aan eNa aabhyaam aiH aaya aabhyaam ebhyaH aat aabhyaam ebhyaH asya ayoH aaNaam e ayoH eShu a au aaH] F [aa e aaH aam e aaH ayaa aabhyaam aabhiH aayai aabhyaam aabhyaH aayaaH aabhyaam aabhyaH aayaaH ayoH aaNaam aayaam ayoH aasu e e aaH] N [am e aaNi am e aaNi eNa aabhyaam aiH aaya aabhyaam ebhyaH aat aabhyaam ebhyaH asya ayoH aaNaam e ayoH eShu a e aaNi])
 )
)
(defun get-ADJ-a-endings (base gender)
 (if (nN-P base)
  (plist-get ADJ-a-endings-nN gender)
  (plist-get ADJ-a-endings gender)
 )
)
(defun nN-P (base)
 (if (sandhi-single (sym-concat base 'ana)) t)
)
(defun init-RPPART-P-endings ()
 (setq RPPART-P-endings
 '(
  v ; the 'uSh' endings are preceded here by 'X' so will not match
  (
   M
   [aan aaMsau aaMsaH aaMsam aaMsau XaH Xaa adbhyaam adbhiH Xe adbhyaam adbhyaH XaH adbhyaam adbhyaH XaH XoH Xaam Xi XoH atsu an aaMsau aaMsaH]
   ; F all these follow an 'uSh
   N
   [at Xii aaMsi at Xii aaMsi Xaa adbhyaam adbhiH Xe adbhyaam adbhyaH XaH adbhyaam adbhyaH XaH XoH Xaam Xi XoH atsu at Xii aaMsi]
  )
  uSh ; the 'v' endings are preceded here by 'Y' so will not match
  (
   M
   [ Yaan YaaMsau YaaMsaH YaaMsam YaaMsau aH aa Yadbhyaam YadbhiH e Yadbhyaam YadbhyaH aH Yadbhyaam YadbhyaH aH oH aam i oH Yatsu Yan YaaMsau YaaMsaH]
   F
   [ii yau yaH iim yau iiH yaa iibhyaam iibhiH yai iibhyaam iibhyaH yaaH iibhyaam iibhyaH yaaH yoH iiNaam yaam yoH iiShu i yau yaH]
   N
   [ Yat ii YaaMsi Yat ii YaaMsi aa Yadbhyaam YadbhiH e Yadbhyaam YadbhyaH aH Yadbhyaam YadbhyaH aH oH aam i oH Yatsu Yat ii YaaMsi]
  )
 )
 )
)
(defun make-explain-subanta-endings (form)
 (let (toks syms sym)
 (fol-msg (format "  (%s\n" form))
 (mapcar
  (lambda (g)
   (setq toks (sup-get g form))
   (setq syms
    (mapcar
     (lambda (tok)
      (if (not (listp tok)) (setq tok (list tok)))
      (setq sym (mapcar 'sym-without-space tok))
      (setq sym (solution sym))
      sym
     )
     toks
    )
   )
   (setq syms (vconcat syms))
   (fol-msg (format "   %s\n   %s\n" g syms))
  )
 '(M F N)
 )
 (fol-msg (format "  )\n"))
 t
 )
)
(defvar explain-subanta-endings nil)
(defun init-explain-subanta-endings ()
 (setq explain-subanta-endings
 '(
  i-ADJ
  (M
   [iH ii ayaH im ii iin inaa ibhyaam ibhiH aye ibhyaam ibhyaH eH ibhyaam ibhyaH eH yoH iinaam au yoH iShu e ii ayaH]
   F
   [iH ii ayaH im ii iiH yaa ibhyaam ibhiH (yai aye) ibhyaam ibhyaH (yaaH eH) ibhyaam ibhyaH (yaaH eH) yoH iinaam (yaam au) yoH iShu e ii ayaH]
   N
   [i inii iini i inii iini inaa ibhyaam ibhiH (ine aye) ibhyaam ibhyaH (inaH eH) ibhyaam ibhyaH (inaH eH) (inoH yoH) iinaam (ini au) (inoH yoH) iShu (i e) inii iini]
   )

  i-ADJ-N
  (M
   [iH ii ayaH im ii iin iNaa ibhyaam ibhiH aye ibhyaam ibhyaH eH ibhyaam ibhyaH eH yoH iiNaam au yoH iShu e ii ayaH]
   F
   [iH ii ayaH im ii iiH yaa ibhyaam ibhiH (yai aye) ibhyaam ibhyaH (yaaH eH) ibhyaam ibhyaH (yaaH eH) yoH iiNaam (yaam au) yoH iShu e ii ayaH]
   N
   [i iNii iiNi i iNii iiNi iNaa ibhyaam ibhiH (iNe aye) ibhyaam ibhyaH (iNaH eH) ibhyaam ibhyaH (iNaH eH) (iNoH yoH) iiNaam (iNi au) (iNoH yoH) iShu (i e) iNii iiNi]
   )

  i-NOUN
  (M
   [iH ii ayaH im ii iin inaa ibhyaam ibhiH aye ibhyaam ibhyaH eH ibhyaam ibhyaH eH yoH iinaam au yoH iShu e ii ayaH]
   F
   [iH ii ayaH im ii iiH yaa ibhyaam ibhiH (yai aye) ibhyaam ibhyaH (yaaH eH) ibhyaam ibhyaH (yaaH eH) yoH iinaam (yaam au) yoH iShu e ii ayaH]
   N
   [i inii iini i inii iini inaa ibhyaam ibhiH ine ibhyaam ibhyaH inaH ibhyaam ibhyaH inaH inoH iinaam ini inoH iShu (i e) inii iini]
  )

  i-NOUN-N
  (M
   [iH ii ayaH im ii iin iNaa ibhyaam ibhiH aye ibhyaam ibhyaH eH ibhyaam ibhyaH eH yoH iiNaam au yoH iShu e ii ayaH]
   F
   [iH ii ayaH im ii iiH yaa ibhyaam ibhiH (yai aye) ibhyaam ibhyaH (yaaH eH) ibhyaam ibhyaH (yaaH eH) yoH iiNaam (yaam au) yoH iShu e ii ayaH]
   N
   [i iNii iiNi i iNii iiNi iNaa ibhyaam ibhiH iNe ibhyaam ibhyaH iNaH ibhyaam ibhyaH iNaH iNoH iiNaam iNi iNoH iShu (i e) iNii iiNi]
  )

 u-NOUN
  (M
   [uH uu avaH um uu uun unaa ubhyaam ubhiH ave ubhyaam ubhyaH oH ubhyaam ubhyaH oH voH uunaam au voH uShu o uu avaH]
   F
   [uH uu avaH um uu uuH vaa ubhyaam ubhiH (vai ave) ubhyaam ubhyaH (vaaH oH) ubhyaam ubhyaH (vaaH oH) voH uunaam (vaam au) voH uShu o uu avaH]
   N
   [u unii uuni u unii uuni unaa ubhyaam ubhiH une ubhyaam ubhyaH unaH ubhyaam ubhyaH unaH unoH uunaam uni unoH uShu (u o) unii uuni]
  )

  u-NOUN-N
  (M
   [uH uu avaH um uu uun uNaa ubhyaam ubhiH ave ubhyaam ubhyaH oH ubhyaam ubhyaH oH voH uuNaam au voH uShu o uu avaH]
   F
   [uH uu avaH um uu uuH vaa ubhyaam ubhiH (vai ave) ubhyaam ubhyaH (vaaH oH) ubhyaam ubhyaH (vaaH oH) voH uuNaam (vaam au) voH uShu o uu avaH]
   N
   [u uNii uuNi u uNii uuNi uNaa ubhyaam ubhiH uNe ubhyaam ubhyaH uNaH ubhyaam ubhyaH uNaH uNoH uuNaam uNi uNoH uShu (u o) uNii uuNi]
  )

  u-ADJ
  (M
   [uH uu avaH um uu uun unaa ubhyaam ubhiH ave ubhyaam ubhyaH oH ubhyaam ubhyaH oH voH uunaam au voH uShu o uu avaH]
   F
   [(uH vii) (uu vyau) (avaH vyaH) (um viim) (uu vyau) (uuH viiH) (vaa vyaa) (ubhyaam viibhyaam) (ubhiH viibhiH) (vai ave vyai) (ubhyaam viibhyaam) (ubhyaH viibhyaH) (vaaH oH vyaaH) (ubhyaam viibhyaam) (ubhyaH viibhyaH) (vaaH oH vyaaH) (voH vyoH) (uunaam viinaam) (vaam au vyaam) (voH vyoH) (uShu viiShu) (o vi) (uu vyau) (avaH vyaH)]
   N
   [u unii uuni u unii uuni unaa ubhyaam ubhiH (une ave) ubhyaam ubhyaH (unaH oH) ubhyaam ubhyaH (unaH oH) (unoH voH) uunaam (uni au) (unoH voH) uShu (u o) unii uuni]
  )

   u-ADJ-N
  (M
   [uH uu avaH um uu uun uNaa ubhyaam ubhiH ave ubhyaam ubhyaH oH ubhyaam ubhyaH oH voH uuNaam au voH uShu o uu avaH]
   F
   [(uH vii) (uu vyau) (avaH vyaH) (um viim) (uu vyau) (uuH viiH) (vaa vyaa) (ubhyaam viibhyaam) (ubhiH viibhiH) (vai ave vyai) (ubhyaam viibhyaam) (ubhyaH viibhyaH) (vaaH oH vyaaH) (ubhyaam viibhyaam) (ubhyaH viibhyaH) (vaaH oH vyaaH) (voH vyoH) (uuNaam viiNaam) (vaam au vyaam) (voH vyoH) (uShu viiShu) (o vi) (uu vyau) (avaH vyaH)]
   N
   [u uNii uuNi u uNii uuNi uNaa ubhyaam ubhiH (uNe ave) ubhyaam ubhyaH (uNaH oH) ubhyaam ubhyaH (uNaH oH) (uNoH voH) uuNaam (uNi au) (uNoH voH) uShu (u o) uNii uuNi]
  )
 ))
 t
)
(defun get-explain-subanta-endings (type form base gender)
;(fol-msg (format "chk-e: %s %s %s %s\n" type form base gender))
 (let (s p ans)
  (if (nN-P base)
   (setq s (format "%s-%s-N" form type))
   (setq s (format "%s-%s" form type))
  )
  (setq p (intern s))
  (setq ans (plist-get2 explain-subanta-endings (list p gender)))
  ans
 )
)
(defvar laT-1-P-endings 
 "ti taH anti  si thaH tha  mi vaH maH"
 )
(defvar laT-1-A-endings 
 "te iite ante  se iithe  dhve  e vahe mahe"
)
; according to Kale 387:
; (defvar laT-1-A-endings
; [[T E] [I T E] [A N T E]
;  [S E] [I TH E] [DH V E]
;  [I]   [V A H E] [M A H E]]
;)
(defvar laT-2-P-endings laT-1-P-endings)
(defvar laT-2-A-endings 
 "te aate ate  se aathe dhve  e vahe mahe"
)
;-- la~N (imperfect) endings
(defvar la~N-1-P-endings
 "t taam an    s tam ta  am va ma"
)
(defvar la~N-1-A-endings
; "ta itaam anta   thaaH iithaam dhvam   i vahi mahi"
; changed based on comment in AntoineII.9
 "ta iitaam anta   thaaH iithaam dhvam   i vahi mahi"
)
(defvar la~N-2-P-endings la~N-1-P-endings)
(defvar la~N-2-A-endings
 "ta aataam ata   thaaH aathaam dhvam   i vahi mahi"
)
; loT  (imperative)
(defvar loT-1-P-endings
 "tu taam antu   . tam ta   aani aava aama"
)
(defvar loT-1-A-endings
 "taam iitaam antaam   sva iithaam dhvam   ai aavahai aamahai"
)
(defvar loT-2-P-endings 
 "tu taam antu   hi tam ta   aani aava aama"
)
(defvar loT-2-A-endings
 "taam aataam ataam   sva aathaam dhvam   ai aavahai aamahai"
)
; vidhili~N  (potential, or optative)
(defvar vidhili~N-1-P-endings
 "iit iitaam iiyuH   iiH iitam iita  iiyam iiva iima"
)
(defvar vidhili~N-1-A-endings
 "iita iiyaataam iiran   iithaaH iiyaathaam iidhvam   iiya iivahi iimahi"
)
(defvar vidhili~N-2-P-endings 
 "yaat yaataam yuH   yaaH yaatam yaata  yaam yaava yaama"
)
(defvar vidhili~N-2-A-endings vidhili~N-1-A-endings
)
(defvar laT-2-P-strengths [S W W S W W S W W])
(defvar la~N-2-P-strengths [S W W S W W S W W])
(defvar loT-2-P-strengths [S W W W W W S S S])
(defvar vidhili~N-2-P-strengths [W W W W W W W W W])
(defvar laT-2-A-strengths [W W W W W W W W W])
(defvar la~N-2-A-strengths [W W W W W W W W W])
(defvar loT-2-A-strengths [W W W W W W S S S])
(defvar vidhili~N-2-A-strengths [W W W W W W W W W])

;--- Goldman variations (incomplete)
; laT endings (method of Goldman - unused)
(defvar laT-1-P-endings-Goldman laT-1-P-endings)
(defvar laT-1-A-endings-Goldman 
 "te aate ante  se aathe dhve   e vahe mahe"
)
(defvar laT-2-P-endings-Goldman laT-1-P-endings-Goldman)
(defvar laT-2-A-endings-Goldman 
 "te aate ate  se aathe dhve  e vahe mahe"
)
(defvar vsup-list) ; list of verbal endings
;(defvar da-save nil); 

(defvar sup-list)
(defvar gender-form-data-0)
(defvar gender-form-data-1)
(defvar gender-form-data-2)
(defvar gender-form-data-3)
(defvar gender-form-data-4)
(defvar gender-form-data-all)
(defvar ending-consonants-1cons nil)
(defvar declension-cons-forms nil)

(defvar li~Nga-set
 ; li~Nga = gender
 [puMli~Nga striili~Nga napuMsakali~Nga])
(defvar gender-set
 [M F N])
; vachana = number
(defvar vachana-set
 [eka dvi bahu]) ; Singular Dual Plural (S D P)
; vibhakti = case  (this word is a feminine noun, so the
; feminine adjectival form appears in the set
; saMbodhana (vocative) is not a true case. 
(defvar vibhakti-set
 [prathamaa dvitiiyaa tRitiiyaa chaturthii
  pa~nchamii ShaShThii saptamii saMbodhana]
)
(defvar case-set
 [1 2 3 4 5 6 7 8]
)
(defvar case-number-set
 [[1 S] [1 D] [1 P]
  [2 S] [2 D] [2 P]
  [3 S] [3 D] [3 P]
  [4 S] [4 D] [4 P]
  [5 S] [5 D] [5 P]
  [6 S] [6 D] [6 P]
  [7 S] [7 D] [7 P]
  [8 S] [8 D] [8 P]]
)
; subanta = declension (or inflection) of nouns, substantive and adjective
; praatipadika = crude form or base form of noun not yet inflected
; sup = a case ending
; subanta is the process of adding the sup's to the praatipadika

; the normal masculine case endings are from Antoine (section 74, vol I)
; endings of the form 'vowel + s' will turn into 'vowel + H' (i.e visarga)
(defvar sup-M-normal
 " s au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  . au as"
)
(defvar sup-F-normal sup-M-normal)
(defvar sup-N-normal
 " . ii i
  .  ii i
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  . ii i"
)
; sup-M-a = endings of masculine nouns ending in 'a' raamaH
; the next are not explicitly given by Antoine, Kale, or Goldman. Instead,
; the declension of a 'paradigm' is given. This is too fuzzy to be a 
; program, so I provide endings which, with the appropriate algorithm,
; will provide the result.  There is some choice of endings.
; The general procedure is suggested by the formulaic expression:
; 'base' + 'endings' = answer, 
; where the combination '+' is done by algorithm.
(defvar sup-M-a
 " as au aas
  am au aan
  ena aabhyaam ais
  aaya  aabhyaam ebhyas
  aat aabhyaam ebhyas
  asya ayos aanaam
  e  ayos eShu
  a au aas"
)
(defvar sup-N-a
 "am e aani
  am e aani
  ena aabhyaam ais
  aaya  aabhyaam ebhyas
  aat aabhyaam ebhyas
  asya ayos aanaam
  e  ayos eShu
  a  e aani"
)
(defvar sup-F-aa
 "aa e aaH
  aam e aaH
  ayaa aabhyaam aabhiH
  aayai aabhyaam aabhyaH
  aayaaH aabhyaam aabhyaH
  aayaaH ayoH aanaam
  aayaam ayoH aasu
  e e aaH"
)
(defvar sup-M-i
 "iH ii ayaH
  im ii iin
  inaa ibhyaam ibhiH
  aye  ibhyaam ibhyaH
  eH   ibhyaam ibhyaH
  eH   yoH  iinaam
  au   yoH  iShu
  e    ii  ayaH"
)
(defvar sup-M-u
 "uH uu avaH
  um uu uun
  unaa ubhyaam ubhiH
  ave  ubhyaam ubhyaH
  oH   ubhyaam ubhyaH
  oH   voH  uunaam
  au   voH  uShu
  o    uu  avaH"
)
(defvar sup-F-ii
 "ii yau yaH
  iim yau iiH
  yaa iibhyaam iibhiH
  yai iibhyaam iibhyaH
  yaaH iibhyaam iibhyaH
  yaaH yoH iinaam
  yaam yoH iiShu
  i yau yaH"
)
(defvar sup-F-i
 "iH ii ayaH
  im ii iiH
  yaa ibhyaam ibhiH
  Li yai aye Li  ibhyaam ibhyaH
  Li yaaH eH Li  ibhyaam ibhyaH
  Li yaaH eH Li   yoH  iinaam
  Li yaam au Li  yoH  iShu
  e    ii  ayaH"
)
(defvar sup-F-u
 "uH uu avaH
  um uu uuH
  vaa ubhyaam ubhiH
  Li vai ave Li  ubhyaam ubhyaH
  Li vaaH oH Li  ubhyaam ubhyaH
  Li vaaH oH Li  voH  uunaam
  Li vaam au Li  voH  uShu
  o    uu  avaH"
)
(defvar sup-M-Ri-A ; masculine agent nouns ending in Ri
 "aa aarau aaraH
  aaram aarau RIn
  raa Ribhyaam RibhiH
  re  Ribhyaam RibhyaH
  uH  Ribhyaam RibhyaH
  uH  roH RINaam
  ari roH RiShu
  ar  aarau aaraH"
)
(defvar sup-M-Ri-R ; masculine relation nouns ending in Ri
 "aa arau araH
  aram arau RIn
  raa Ribhyaam RibhiH
  re  Ribhyaam RibhyaH
  uH  Ribhyaam RibhyaH
  uH  roH RINaam
  ari roH RiShu
  ar  arau araH"
)
(defvar sup-F-Ri-R ; feminine relation nouns ending in Ri
 ; like sup-M-Ri-R except for 2p which is RIH rather than RIn
 "aa arau araH
  aram arau RIH
  raa Ribhyaam RibhiH
  re  Ribhyaam RibhyaH
  uH  Ribhyaam RibhyaH
  uH  roH RINaam
  ari roH RiShu
  ar  arau araH"
)
(defvar sup-F-Ri-A sup-M-Ri-A) ; just for 'svasRi'
(defvar sup-F-uu
 "uuH vau vaH
  uum vau uuH
  vaa uubhyaam uubhiH
  vai uubhyaam uubhyaH
  vaaH uubhyaam uubhyaH
  vaaH voH uunaam
  vaam voH uuShu
  u vau vaH"
)
(defvar sup-N-i
 "i inii iini
  i inii iini
  inaa ibhyaam ibhiH
  ine  ibhyaam ibhyaH
  inaH ibhyaam ibhyaH
  inaH inoH iinaam
  ini  inoH iShu
  Li i e Li inii iini"
)
(defvar sup-N-u
 "u unii uuni
  u unii uuni
  unaa ubhyaam ubhiH
  une  ubhyaam ubhyaH
  unaH ubhyaam ubhyaH
  unaH unoH uunaam
  uni  unoH uShu
  Li u o Li unii uuni"
)
(defvar sup-N-Ri
 "Ri RiNii RINi
  Ri RiNii RINi
  RiNaa Ribhyaam RibhiH
  RiNe  Ribhyaam RibhyaH
  RiNaH Ribhyaam RibhyaH
  RiNaH RiNoH RINaam
  RiNi  RiNoH RiShu
  Li Ri ar Li RiNii RINi"
)

(defvar sup-M-a-ADJ sup-M-a)
(defvar sup-F-a-ADJ sup-F-aa)
(defvar sup-N-a-ADJ sup-N-a)

(defvar sup-M-i-ADJ sup-M-i)
(defvar sup-F-i-ADJ sup-F-i)
(defvar sup-N-i-ADJ
; like sup-N-i, or
; (except in cases 1,2, 8) like sup-M-i
 "i inii iini
  i inii iini
  inaa ibhyaam ibhiH
  Li ine aye Li  ibhyaam ibhyaH
  Li inaH eH Li ibhyaam ibhyaH
  Li inaH eH Li  Li inoH yoH Li  iinaam
  Li ini au Li  Li inoH yoH Li iShu
  Li i e Li inii iini"
)

(defvar sup-M-u-ADJ sup-M-u)
(defvar sup-F-u-ADJ
 ;**NOTE: this is unique in having 'Li' at end of 
 ; lines.  Due to a likely bug in ITRANS-parse-words-1 in
 ; init-one-sup, this caused 'nil' to appear WHEN THE 'Li'
 ; was followed by a space at end of lines. 

 ; sup-F-u-ADJ is like sup-F-u; however,
 ; and adjuective in 'u' denoting a quality
 ; has an optional form in 'ii' declined like 'nadii'
 ; e.g. mRidu - mRidvii, laghu laghvii
 "Li uH vii Li  Li uu vyau Li  Li avaH vyaH Li
  Li um viim Li  Li uu vyau Li  Li uuH viiH Li
  Li vaa vyaa Li  Li ubhyaam viibhyaam Li  Li ubhiH viibhiH Li
  Li vai ave vyai Li  Li ubhyaam viibhyaam Li  Li ubhyaH viibhyaH Li
  Li vaaH oH vyaaH Li  Li ubhyaam viibhyaam Li  Li ubhyaH viibhyaH Li
  Li vaaH oH vyaaH Li  Li voH vyoH Li  Li uunaam viinaam Li
  Li vaam au vyaam Li  Li voH vyoH Li  Li uShu viiShu Li
  Li o vi Li  Li uu vyau Li  Li avaH vyaH Li"
)
(defvar sup-N-u-ADJ
; like sup-N-u, or
; (except in cases 1,2, 8) like sup-M-u
 "u unii uuni
  u unii uuni
  unaa ubhyaam ubhiH
  Li une ave Li  ubhyaam ubhyaH
  Li unaH oH Li  ubhyaam ubhyaH
  Li unaH oH Li  Li unoH voH Li   uunaam
  Li uni au Li   Li unoH voH Li   uShu
  Li u o Li unii uuni"
)

(defvar sup-M-Ri-ADJ sup-M-Ri-A)
(defvar sup-F-Ri-ADJ
 ; a form in 'ii' declined like 'nadii'.
 ; e.g., daatrii
 " rii ryau ryaH
 riim ryau riiH
 ryaa riibhyaam riibhiH
 ryai riibhyaam riibhyaH
 ryaaH riibhyaam riibhyaH
 ryaaH ryoH riinaam
 ryaam ryoH riiShu
 ri ryau ryaH"
)
(defvar sup-N-Ri-ADJ
 ; like sup-N-Ri; however, 
 ; except cases 1,2 and 8, there is 
 ; a second form like that of sup-M-Ri-A
 "Ri RiNii RINi
  Ri RiNii RINi
  Li RiNaa raa Li Ribhyaam RibhiH
  Li RiNe re Li   Ribhyaam RibhyaH
  Li RiNaH ruH Li Ribhyaam RibhyaH
  Li RiNaH ruH Li  Li RiNoH roH Li  RINaam
  Li RiNi ari Li   Li RiNoH roH Li  RiShu
  Li Ri ar Li RiNii RINi"
)

(defvar sup-M-aI-ADJ sup-M-a)
(defvar sup-F-aI-ADJ sup-F-ii)
(defvar sup-N-aI-ADJ sup-N-a)

(defvar sup-M-adj-PRON ; sarva
 " as au e
  am au aan
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  a au e"
)
(defvar sup-N-adj-PRON
 " am e aani
  am e aani
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  a e aani"
)
(defvar sup-F-adj-PRON
 "aa e aaH
  aam e aaH
  ayaa aabhyaam aabhiH
  asyai aabhyaam aabhyaH
  asyaaH aabhyaam aabhyaH
  asyaaH ayoH aasaam
  asyaam ayoH aasu
  e e aaH"
)

(defvar sup-M-a-PRON sup-M-adj-PRON)
(defvar sup-N-a-PRON
 " at e aani
  at e aani
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  at e aani"
)
(defvar sup-F-a-PRON sup-F-adj-PRON)

(defvar sup-M-c-PRON ; puurva Antoine2#174, p114
 " as au Li e aaH Li
  am au aan
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  Li asmaat aat Li aabhyaam ebhyas
  asya ayos eShaam
  Li asmin e Li  ayos eShu
  a au Li e aaH Li"
)
(defvar sup-N-c-PRON
 " am e aani
  am e aani
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  Li asmaat aat Li aabhyaam ebhyas
  asya ayos eShaam
  Li asmin e Li ayos eShu
  a e aani"
)
(defvar sup-F-c-PRON sup-F-adj-PRON)


(defvar sup-M-d-PRON ; puurva Antoine2#174, p114
 " as au Li e aaH Li
  am au aan
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  a au Li e aaH Li"
)
(defvar sup-N-d-PRON sup-N-adj-PRON)
(defvar sup-F-d-PRON sup-F-adj-PRON)

(defvar old-sup-M-a-PRON
 " as au e
  am au aan
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  Li Li Li Li Li Li"
)
(defvar old-sup-N-a-PRON
 " at e aani
  at e aani
  ena aabhyaam ais
  asmai  aabhyaam ebhyas
  asmaat aabhyaam ebhyas
  asya ayos eShaam
  asmin  ayos eShu
  Li Li Li Li Li Li"
)
(defvar old-sup-F-a-PRON
 "aa e aaH
  aam e aaH
  ayaa aabhyaam aabhiH
  asyai aabhyaam aabhyaH
  asyaaH aabhyaam aabhyaH
  asyaaH ayoH aasaam
  asyaam ayoH aasu
  Li Li Li Li Li Li"
)

(defvar sup-M-b-PRON sup-M-a-PRON)
(defvar sup-F-b-PRON sup-F-a-PRON)
(defvar sup-N-b-PRON sup-N-a-PRON)
(defvar sup-empty
" Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li
   Li Li   Li Li   Li Li"
)

(defvar sup-M-IRR-PRON sup-empty)
(defvar sup-F-IRR-PRON sup-empty)
(defvar sup-N-IRR-PRON sup-empty)

(defvar sup-M-CARD-PRON
 "Li Li Li Li a
  Li Li Li Li a
  Li Li Li Li abhiH
  Li Li Li Li abhyaH
  Li Li Li Li abhyaH
  Li Li Li Li aanaam
  Li Li Li Li asu
  Li Li Li Li Li Li"
)
;(defvar sup-F-CARD-PRON sup-empty)
;(defvar sup-N-CARD-PRON sup-empty)
(defvar sup-F-CARD-PRON sup-M-CARD-PRON) ; 12-31-03
(defvar sup-N-CARD-PRON sup-M-CARD-PRON)
(defvar sup-M-ORDa-PRON
; ORDa-PRON is used for 'prathama, dvitiiya, and tRitiiya'. It is
; like adjectives in 'a' except for optional pronoun forms in singular of
; cases 4,5 and 7 
 " as au aas
  am au aan
  ena aabhyaam ais
  Li aaya asmai Li  aabhyaam ebhyas
  Li aat asmaat Li aabhyaam ebhyas
  asya ayos aanaam
  Li e asmin Li  ayos eShu
  a au aas"
)
(defvar sup-F-ORDa-PRON
"aa e aaH
  aam e aaH
  ayaa aabhyaam aabhiH
  Li aayai asyai Li aabhyaam aabhyaH
  Li aayaaH asyaaH Li aabhyaam aabhyaH
  aayaaH ayoH aanaam
  Li aayaam asyaam Li ayoH aasu
  e e aaH"
)
(defvar sup-N-ORDa-PRON
"am e aani
  am e aani
  ena aabhyaam ais
  Li aaya asmai Li  aabhyaam ebhyas
  Li aat asmaat Li aabhyaam ebhyas
  asya ayos aanaam
  Li e  asmin Li  ayos eShu
  a  e aani"
)

; ORDb-PRON is used for 'turiiya' and 'turya', which are exactly like
; adjectives in 'a'
(defvar sup-M-ORDb-PRON sup-M-a-ADJ)
(defvar sup-F-ORDb-PRON sup-F-a-ADJ)
(defvar sup-N-ORDb-PRON sup-N-a-ADJ)

; ORD-PRON is used for most ordinal adjectives. 
(defvar sup-M-ORD-PRON sup-M-a)
(defvar sup-F-ORD-PRON sup-F-ii)
(defvar sup-N-ORD-PRON sup-N-a)

(defvar sup-M-1cons sup-M-normal)
(defvar sup-F-1cons sup-F-normal)
(defvar sup-N-1cons sup-N-normal)
(defvar sup-M-2cons sup-M-normal)
(defvar sup-F-2cons sup-F-normal)
(defvar sup-N-2cons sup-N-normal)

(defvar sup-M-vat sup-M-normal)
(defvar sup-F-vat sup-F-ii)
(defvar sup-N-vat sup-N-normal)
(defvar sup-M-vat-ADJ sup-M-normal)
(defvar sup-F-vat-ADJ sup-F-ii)
(defvar sup-N-vat-ADJ sup-N-normal)

(defvar sup-M-mat sup-M-normal)
(defvar sup-F-mat sup-F-ii)
(defvar sup-N-mat sup-N-normal)
(defvar sup-M-mat-ADJ sup-M-normal)
(defvar sup-F-mat-ADJ sup-F-ii)
(defvar sup-N-mat-ADJ sup-N-normal)

; at-ADJ  created for 'sat' and 'asat'.
; 'sat' is  the present active participle of 'as', for which 
; there are additional meanings.
(defvar sup-M-at-ADJ sup-M-normal)
(defvar sup-F-at-ADJ sup-F-ii)
(defvar sup-N-at-ADJ sup-N-normal)

(defvar sup-M-iiyas-ADJ sup-M-normal)
(defvar sup-F-iiyas-ADJ sup-F-ii)
(defvar sup-N-iiyas-ADJ sup-N-normal)

(defvar sup-M-in  
; same as sup-M-normal, except for 1s (balii)
 " ii au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  . au as"
)
(defvar sup-F-in sup-F-ii)
(defvar sup-N-in 
 ;same as sup-N-normal except for 8s
 " . ii i
  .  ii i
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  Li . n Li ii i"
)
(defvar sup-M-in-ADJ sup-M-in)
(defvar sup-F-in-ADJ sup-F-in)
(defvar sup-N-in-ADJ sup-N-in)


(defvar sup-M-an
; same as sup-M-normal, except for 1s (raajaa)
 " aa au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  . au as"
)
(defvar sup-F-an sup-F-ii)
(defvar sup-N-an 
 ;same as sup-N-normal except for 8s
 " . ii i
  .  ii i
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  Li . n Li ii i"
)
(defvar sup-M-an-ADJ sup-M-an)
(defvar sup-F-an-ADJ sup-F-an)
(defvar sup-N-an-ADJ sup-N-an)

(defvar sup-M-vas-ADJ 
 ;sup-M-normal, except for 1S
 " . au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  . au as"
)
(defvar sup-F-vas-ADJ sup-F-ii)
(defvar sup-N-vas-ADJ sup-N-normal)

(defvar sup-M-ach-ADJ 
 ;same as sup-N-normal except for 1S 8S
 " ~N au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  ~N au as"
)
(defvar sup-F-ach-ADJ sup-F-ii)
(defvar sup-N-ach-ADJ 
 ;same as sup-N-normal except for 8s
 " k ii i
  k  ii i
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  k ii i"
)

(defvar sup-M-aach-ADJ sup-M-ach-ADJ)
(defvar sup-F-aach-ADJ sup-F-ii)
(defvar sup-N-aach-ADJ sup-N-ach-ADJ)

(defvar sup-M-3cons sup-M-normal)
(defvar sup-F-3cons sup-F-normal)
(defvar sup-N-3cons sup-N-normal)

(defvar sup-M-IRR sup-empty)
(defvar sup-F-IRR sup-empty)
(defvar sup-N-IRR sup-empty)

(defvar sup-M-aa
 ; Note: Antoine (vol2, section3) has 'aa' in 1S/8S
 ; Deshpande (p. 146) and Kale (p. 36) have 'aaH'. I
 ; have changed to 'aaH' (11-22-03)
 " aaH au aaH
  aam au aH
  aa aabhyaam aabhis
  e  aabhyaam aabhyas
  as aabhyaam aabhyas
  as os aam
  i  os aasu
  aaH au aas"
)
(defvar sup-M-ii0
 ;vaatapramii (antelope), yaayii (way, horse) , papii (sun)
 ; Kale76, p. 42
 "iiH yau yaH
  iim yau iin
  yaa iibhyaam iibhiH
  ye iibhyaam iibhyaH
  yaH iibhyaam iibhyaH
  yaH yoH yaam
  ii yoH iiShu
  iiH yau yaH"
)
(defvar sup-M-uu 
 " uus uvau uvas
  uvam uvau uvas
  uvaa uubhyaam uubhis
  uve  uubhyaam uubhyas
  uvas uubhyaam uubhyas
  uvas uvos uvaam
  uvi  uvos uuShu
  uus uvau uvas"
)
(defvar sup-M-ii 
 " iis iyau iyas
  iyam iyau iyas
  iyaa iibhyaam iibhis
  iye  iibhyaam iibhyas
  iyas iibhyaam iibhyas
  iyas iyos iyaam
  iyi  iyos iiShu
  iis iyau iyas"
)
(defvar sup-M-ii1
 " iis yau yas
  yam yau yas
  yaa iibhyaam iibhis
  ye  iibhyaam iibhyas
  yas iibhyaam iibhyas
  yas yos yaam
  yi  yos iiShu
  iis yau yas"
)
(defvar sup-M-uu1
 ; khalapuuH
 "uuH vau vaH
  vam vau vaH
  vaa uubhyaam uubhiH
  ve uubhyaam uubhyaH
  vaH uubhyaam uubhyaH
  vaH voH vaam
  vi voH uuShu
  uuH vau vaH"
)
(defvar sup-F-ii1
 " iiH yau yaH
  yam yau yaH
  yaa iibhyaam iibhis
  Li ye yai Li  iibhyaam iibhyas
  Li yaH yaaH Li iibhyaam iibhyas
  Li yaH yaaH Li yos Li yaam iinaam Li
  Li yi yaam Li  yos iiShu
  iis yau yaH"
)
; (defvar sup-F-ii1
;  " s au as
;   am au as
;   aa bhyaam bhis
;   Li e ai Li  bhyaam bhyas
;   Li as aas Li bhyaam bhyas
;   Li as aas Li os Li aam naam Li
;   Li i aam Li  os su
;   s au as"
; )
(defvar sup-F-ii2  ; not used
 " . au as
  Li am m Li au Li as s Li
  aa bhyaam bhis
  ai  bhyaam bhyas
  aaH bhyaam bhyas
  aas os naam
  aam  os su
  . au as"
)
(defvar sup-F-ii3 ; not used
 ;like nadii except for 1S
 "iiH yau yaH
  iim yau iiH
  yaa iibhyaam iibhiH
  yai iibhyaam iibhyaH
  yaaH iibhyaam iibhyaH
  yaaH yoH iinaam
  yaam yoH iiShu
  i yau yaH"
)
(defvar sup-F-uu1 
 "uuH vau vaH
  vam vau vaH
  vaa uubhyaam uubhiH
  Li ve vai Li uubhyaam uubhyaH
  Li vaH vaaH Li uubhyaam uubhyaH
  Li vaH vaaH Li voH Li vaam uunaam Li
  Li vi vaam Li voH uuShu
  uuH vau vaH"
)
(defvar sup-N-i1 sup-N-normal) 

(defvar sup-M-o ; from a stem that drops o (e.g.,for 'go', from 'g')
 " aus aavau aavas
  aam aavau aas
 avaa obhyaam obhis
  ave  obhyaam obhyas
  os obhyaam obhyas
  os avos avaam
  avi  avos oShu
  aus aavau aavas"
)
(defvar sup-F-o sup-M-o)

(defvar sup-F-au ; same as sup-F-normal except for 8S
" s au as
  am au as
  aa bhyaam bhis
  e  bhyaam bhyas
  as bhyaam bhyas
  as os aam
  i  os su
  s au as"
)
(defvar sup-M-au sup-F-au)
(defvar sup-M-ai
 "aaH aayau aayaH
  aayam aayau aayaH
  aayaa aabhyaam aabhiH
  aaye aabhyaam aabhyaH
  aayaH aabhyaam aabhyaH
  aayaH aayoH aayaam
  aayi aayoH aasu
  aaH aayau aayaH"
)
(defvar sup-F-ai sup-M-ai
)
(defvar luT-1-P-endings
 "taa taarau taaraH taasi taasthaH taastha  taasmi taasvaH taasmaH"
)
(defvar luT-1-A-endings
 "taa taarau taaraH taase taasaathe taadhve taahe taasvahe taasmahe"
)
(defvar luT-1-P-strengths [S S S  S S S  S S S])
(defvar luT-1-A-strengths [S S S  S S S  S S S])

(defvar lRiT-1-P-endings
 "syati syataH syanti  syasi syathaH syatha  syaami syaavaH syaamaH"
)
(defvar lRiT-1-A-endings
 "syate syete syante syase syethe syadhve sye syaavahe syaamahe"
)
(defvar lRiT-1-P-strengths [S S S  S S S  S S S])
(defvar lRiT-1-A-strengths [S S S  S S S  S S S])

(defvar lRi~N-1-P-endings
 "syat syataam syan  syaH syatam syata  syam syaava syaama"
)
(defvar lRi~N-1-A-endings
 "syata syetaam syanta syathaaH syethaam syadhvam  sye syaavahi syaamahi"
)
(defvar lRi~N-1-P-strengths [S S S  S S S  S S S])
(defvar lRi~N-1-A-strengths [S S S  S S S  S S S])

(defvar aashiirli~N-1-P-endings
 "yaat yaastaam yaasuH  yaaH yaastam yaasta  yaasam yaasva yaasma"
)
(defvar aashiirli~N-1-A-endings
 "siiShTa siiyaastaam siiran  siiShThaaH siiyaasthaam siidhvam  siiya siivahi siimahi"
)
(defvar aashiirli~N-1-P-strengths [W W W  W W W  W W W])
(defvar aashiirli~N-1-A-strengths [S S S  S S S  S S S])


(defvar lu~N1-1-P-endings
 "t taam us    s tam ta  am va ma"
)
(defvar lu~N1-1-P-strengths [S S S  S S S  S S S])

(defvar lu~N2-1-P-endings
 "t taam an    s tam ta  am va ma" ; same as la~N-1-P
)
(defvar lu~N2-1-A-endings
 ; same as la~N-1-A, except for 'i' replacing 'ii' in 3D and 2D
 "ta itaam anta   thaaH ithaam dhvam   i vahi mahi"
)
(defvar lu~N2-1-P-strengths [S S S  S S S  S S S])
(defvar lu~N2-1-A-strengths [S S S  S S S  S S S])

(defvar lu~N3-1-P-endings lu~N2-1-P-endings)
(defvar lu~N3-1-A-endings lu~N2-1-A-endings)
(defvar lu~N3-1-P-strengths lu~N2-1-P-strengths)
(defvar lu~N3-1-A-strengths lu~N2-1-P-strengths)

(defvar lu~N4-1-P-endings
 "siit staam suH  siiH stam sta  sam sva sma"
)
(defvar lu~N4-1-A-endings
 "sta saataam sata  sthaaH saathaam dhvam  si svahi smahi"
)
(defvar lu~N4-1-P-strengths [S S S  S S S  S S S])
(defvar lu~N4-1-A-strengths [S S S  S S S  S S S])

(defvar lu~N5-1-P-endings
 "iit iShTaam iShuH  iiH iShTam iShTa  iSham iShva iShma"
)
(defvar lu~N5-1-A-endings
 "iShTa iShaataam iShata  iShThaaH iShaathaam idhvam  iShi iShvahi iShmahi"
)
(defvar lu~N5-1-P-strengths [S S S  S S S  S S S])
(defvar lu~N5-1-A-strengths [S S S  S S S  S S S])

(defvar lu~N6-1-P-endings
 "siit siShTaam siShuH  siiH siShTam siShTa  siSham siShva siShma"
)
(defvar lu~N6-1-P-strengths [S S S  S S S  S S S])

(defvar lu~N7-1-P-endings
 "sat sataam san  saH satam sata  sam saava saama"
)
(defvar lu~N7-1-A-endings
 "sata saataam santa  sathaaH saathaam sadhvam  si saavahi saamahi"
)
(defvar lu~N7-1-P-strengths [S S S  S S S  S S S])
(defvar lu~N7-1-A-strengths [S S S  S S S  S S S])
