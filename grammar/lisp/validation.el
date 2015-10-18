; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 

; validation.el
; begun 08-28-02 by ejf
; Code from other lisp (.el) files in this directory may be referenced.

(defun validation (vfile)
 (let (ans data n record p-field q-field a-field a-field-1 nerr proc err)
  (setq data (read-colon-file-validation vfile 0))
;  (fol-msg (format "%s %s\n" (san-validation-filename vfile) (length data)))
  (setq n 0)
  (setq nerr 0)
  (setq ans t)
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
;   (fol-msg (format "record=%s\n" record))
   (setq p-field (elt record 0))
   (setq proc (elt p-field 0))
   (setq q-field (elt record 1))
;   (setq a-field (elt record 2))
   (setq a-field (substring record 2)) ; a vector
   (setq a-field (append a-field nil)) ; a list
;   (setq a-field (apply 'append a-field))
   (condition-case err
    (setq a-field-1 (apply proc q-field))
    (error
     (fol-msg (format "validation error (%s): %s %s\n" err proc q-field))
     (setq a-field-1 nil)
    )
   )
   (setq a-field (solution a-field))
   (setq a-field-1 (solution a-field-1))
   (when (not (equal a-field a-field-1))
    ; some problem here
    (setq ans nil) ; overall test fails
    (fol-msg (format "line %s error: %s %s %s %s\n"
		     n proc q-field a-field a-field-1))
    (setq nerr (1+ nerr))
    (when (< 10 nerr)
     (fol-msg (format "more than %s errors. quitting.\n" 10))
     (setq data nil)
    )
   )
  )
  (fol-msg (format "%s errors in %s records in validation of %s\n"
		    nerr n vfile))
  (setq ans (if (equal nerr 0) t nil))
 )
)
(defun 1cons-chk (sym &optional case-numbers)
 (let (declension vals c n i)
  (if (not case-numbers) (setq case-numbers '((1 1) (3 1) (3 3) (7 3))))
  (setq declension (elt (elt (declension-citation sym) 0) 2))
  (setq vals (mapcar
   (lambda (case-number)
    (setq c (elt case-number 0))
    (setq n (elt case-number 1))
    (setq i (+ (* (1- c) 3) (1- n)))
    (elt declension i)
   )
    case-numbers
  ))
  vals
 )
)
(defun 1cons-chk-pr (sym &optional case-numbers)
 (let (vals)
  (setq vals (1cons-chk sym case-numbers))
  (fol-msg (format "%s : %s\n" sym vals))
  t
 )
)
(defun 1cons-chk-pr-all ()
 (mapcar '1cons-chk-pr '(vaach vaNij samraaj dish vish))
 (1cons-chk-pr 'praavRiSh)
)
(defun validation-all (&optional files)
 (let (file ans)
  (if (not files)
  (setq files '(
   "liT.txt"
   "liTpass.txt"
   "luT.txt"
   "lRiT.txt"
   "lRi~N.txt"
   "ffc.txt"
   "aashiirli~N.txt"
   "futpass.txt"
   "aorist.txt"
   "aorist2a.txt"
   "aorist3.txt"
   "aorist6.txt"
   "aorist7.txt"
   "aorist4.txt"
   "aorist5.txt"
   "aoristpassive.txt"
   "perfpartbase.txt"
   "conjpassive.txt"
   "kta.txt"
   "tvaa.txt"
   "inf.txt"
   "potpart.txt"
   "rpp.txt"
   "perpp.txt"
   "futpart.txt"
   "specialtense.txt"
   "prespart.txt"
   "causal.txt" ; causal base
   "causal1.txt" ; conjugations of causal of 'budh' from Kale p. 372
  )))
  (setq ans t)
  (while (and files ans)
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (setq ans (validation file))
  )
 ans
 )
)

(defun validation-deshpande-all (&optional files)
 (let (file ans)
  (if (not files)
  (setq files '(
   "test101a.txt" ; 'tvaa' gerund
   "test101b.txt" ; 'ya' gerund
   "test101c.txt" ; infinitive
   "test132.txt" ; present tense passive
   "test140a.txt" ; simple future (3S)
   "test140b.txt" ; passive of simple future (3S)
   "test145.txt" ; irregular nouns
   "test317a.txt" ;  aorist passive
   "test317b.txt" ;  aorist active
  )))
  (setq ans t)
  (while (and files ans)
   (setq file (car files))
   (setq files (cdr files))
   (setq file (concat "deshpande/" file))
   (message file)
   (setq ans (validation file))
  )
 ans
 )
)
(defun liT-r (dhaatu pada &optional upa-syms voice)
 (let (class classes)
  (setq classes (get-classes dhaatu pada upa-syms))
  (setq class (car classes))
  (conjugation-tab-liT-r upa-syms class pada dhaatu voice)
 )
)
(defun get-classes (dhaatu pada upa-syms)
 (let (forms form ans thisans dcpu)
  (setq forms (sanget2 dhaatu '(dhaatu forms)))
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq dcpu (sym-dcpu form))
   (when (and ;(equal pada (elt dcpu 2))
	      (equal upa-syms (elt dcpu 3)))
    (setq thisans (elt dcpu 1))
    (setq ans (append-if-new ans thisans))
   )
  )
  ans
 )
)
(defun liT-px (dhaatu class pada sfx-dhaatu &optional upa-syms voice)
 (conjugation-tab-liT-px upa-syms class pada dhaatu sfx-dhaatu voice)
)
(defun conjugation-tab-liT-px (upa-syms class pada dhaatu dhaatu-sfx
					  &optional voice)
 ; periphrastic perfect test routine.
 ; dhaatu-sfx should be one of the symbols 'as, 'kRi, or 'bhuu
 ; voice should be 'ACTIVE or 'PASSIVE or nil
; (if (equal voice 'PASSIVE) (setq pada 'A))
 (let (ans ans1 n i j)
  (setq ans1 (conjugation-tab-liT-p upa-syms class pada dhaatu voice))
  (setq j (cond
   ((equal dhaatu-sfx 'as) 0)
   ((equal dhaatu-sfx 'kRi) 1)
   ((equal dhaatu-sfx 'bhuu) 2)
  ))
  (when (and ans1 j)
   (setq n (length ans1))
   (setq ans (make-vector n nil))
   (setq i 0)
   (while (< i n)
    (aset ans i (elt (elt ans1 i) j))
    (setq i (1+ i))
   )
   (when (and (equal j 1) (equal pada 'P) (not (equal voice 'PASSIVE)))
     ; only case where there are 2 forms of suffix. (in 1P)
     ; This is needed because of the way we joined the three suffixes
     ; together.
     (setq i 6)
     (aset ans i
       (list (elt (elt ans1 i) j) (elt (elt ans1 i) (1+ j))))
   )
  )
  ans
 )
)
(defun luT (dhaatu class pada &optional upa-syms)
 (conjugation-tab-luT upa-syms class pada dhaatu)
)
(defun lRiT (dhaatu class pada &optional upa-syms isub)
 (let (ans)
  (setq ans (conjugation-tab-lRiT upa-syms class pada dhaatu))
  (if (and ans (numberp isub) (<= 0 isub) (< isub (length ans)))
   (setq ans (elt ans isub))
  )
  ans
 )
)
(defun lRi~N (dhaatu class pada &optional upa-syms)
 (conjugation-tab-lRi~N upa-syms class pada dhaatu)
)
(defun ffc (dhaatu class pada &optional upa-syms)
 (let (ans1 ans2 ans3)
  (setq ans1 (luT dhaatu class pada upa-syms))
  (setq ans2 (lRiT dhaatu class pada upa-syms))
  (setq ans3 (lRi~N dhaatu class pada upa-syms))
  ; extract 1S, the 6th item, in each
  (vector (elt ans1 6) (elt ans2 6) (elt ans3 6))
 )
)
(defun aashiirli~N (dhaatu class pada &optional upa-syms)
 (conjugation-tab-aashiirli~N upa-syms class pada dhaatu)
)
(defun aashiirli~N1 (dhaatu class pada &optional upa-syms)
; the 1S of aashiirli~N
 (elt (conjugation-tab-aashiirli~N upa-syms class pada dhaatu) 6)
)
(defun futpass (dhaatu class pada &optional upa-syms)
 (let (ans1 ans2 ans3 ans4 passive)
  (setq passive 'PASSIVE)
  (setq ans1 (luT dhaatu class passive upa-syms))
  (setq ans2 (lRiT dhaatu class passive upa-syms))
  (setq ans3 (lRi~N dhaatu class passive upa-syms))
  (setq ans4 (aashiirli~N dhaatu class passive upa-syms))
  ; extract 1S, the 6th item, in each
  (vector (elt ans1 6) (elt ans2 6) (elt ans3 6) (elt ans4 6))
 )
)

(defun aorist1 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 1 voice)
)
(defun aorist2 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 2 voice)
)
(defun aorist2a (dhaatu class pada &optional upa-syms voice)
 (let (ctab)
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 2 voice))
  (elt ctab 0) ; 3S
 )
)
(defun aorist3 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 3 voice)
)
(defun aorist3a (dhaatu class pada &optional upa-syms voice)
 (let (ctab)
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 3 voice))
  (elt ctab 0) ; 3S
 )
)
(defun causal (dhaatu class &optional pada upa-syms def)
 (let (ans1 ans)
  (setq ans1 (causal-base
	      dhaatu class pada  upa-syms def))
  (when ans1 
   (setq ans (mapcar 'sym-without-space ans1))
   (setq ans (solution ans))
  )
  ans
 )
)
(defun aorist6 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 6 voice)
)
(defun aorist6a (dhaatu class pada &optional upa-syms voice)
 (let (ctab)
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 6 voice))
  (elt ctab 0) ; 3S
 )
)
(defun aorist7 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 7 voice)
)
(defun aorist7a (dhaatu class pada &optional upa-syms voice)
 (let (ctab)
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 7 voice))
  (elt ctab 0) ; 3S
 )
)
(defun aorist4 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 4 voice)
)
(defun aorist4a (dhaatu class pada &optional upa-syms isub voice)
 (let (ctab i)
;  (fol-msg (format "before: %s %s %s %s %s (%s)\n"
;		   dhaatu class pada upa-syms isub (numberp upa-syms)))
  (cond
   ((numberp upa-syms)
    (setq i upa-syms)
    (setq upa-syms nil)
   )
   ((numberp isub)
    (setq i isub)
   )
   (t (setq i 0)
   )
  )
; (fol-msg (format "after  : %s %s %s %s %s \n" dhaatu class pada upa-syms i))
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 4 voice))
  (elt ctab i) ; 3S
 )
)

(defun aorist5 (dhaatu class pada &optional upa-syms voice)
  (conjugation-tab-aorist upa-syms class pada dhaatu 5 voice)
)

(defun aorist5a (dhaatu class pada &optional upa-syms isub voice)
 (let (ctab i)
;  (fol-msg (format "before: %s %s %s %s %s (%s)\n"
;		   dhaatu class pada upa-syms isub (numberp upa-syms)))
  (cond
   ((numberp upa-syms)
    (setq i upa-syms)
    (setq upa-syms nil)
   )
   ((numberp isub)
    (setq i isub)
   )
   (t (setq i 0)
   )
  )
; (fol-msg (format "after  : %s %s %s %s %s \n" dhaatu class pada upa-syms i))
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu 5 voice))
  (elt ctab i) 
 )
)
(defun aorista (iaorist dhaatu class pada &optional upa-syms isub voice)
 (let (ctab i)
;  (fol-msg (format "before: %s %s %s %s %s (%s)\n"
;		   dhaatu class pada upa-syms isub (numberp upa-syms)))
  (cond
   ((numberp upa-syms)
    (setq i upa-syms)
    (setq upa-syms nil)
   )
   ((numberp isub)
    (setq i isub)
   )
   (t (setq i 0)
   )
  )
; (fol-msg (format "after  : %s %s %s %s %s \n" dhaatu class pada upa-syms i))
  (setq ctab (conjugation-tab-aorist upa-syms class pada dhaatu iaorist voice))
  (elt ctab i) 
 )
)
(defun aoristP (iaorist dhaatu class pada &optional upa-syms isub)
 (aorista iaorist dhaatu class pada  upa-syms isub 'PASSIVE)
)
(defun aoristP0 (dhaatu class pada &optional upa-syms)
 (let (varieties ans ans1 iaorist isub)
  (setq varieties (aorist-varieties dhaatu class pada upa-syms))
  (setq isub 0)
  (while varieties
   (setq iaorist (car varieties))
   (setq varieties (cdr varieties))
   (setq ans1 (aoristP iaorist dhaatu class pada upa-syms isub))
   (setq ans (append-if-new ans ans1))
  )
  (solution ans)
 )
)
(defun aoristP0-make ()
 (let (tabname dirname filename buf key  more err)
  (setq tabname "deshpande-aorist.txt")
  (setq dirname "validation")
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (while more
    (condition-case err
     (progn
      (setq key (read buf)) 
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
    (let (dhaatu class pada upasargas tmp ans)
     (setq tmp (sym-dcpu key))
     (setq dhaatu (elt tmp 0))
     (setq class (elt tmp 1))
     (setq pada (elt tmp 2))
     (setq upasargas (elt tmp 3))
     (setq ans (aoristP0 dhaatu class pada upasargas))
     (if upasargas
      (fol-msg (format "aoristP0 : %s %s %s %s : %s\n"
		      dhaatu class pada upasargas ans))
      (fol-msg (format "aoristP0 : %s %s %s : %s\n"
		      dhaatu class pada  ans))
     )
    )
   )
   (kill-buffer nil)
  )
 )
)
(defun aoristP1 (dhaatu class pada &optional upa-syms)
 (let (varieties ans ans1 iaorist isub ctab voice)
  (setq voice 'ACTIVE)
  (setq varieties (aorist-varieties dhaatu class pada upa-syms))
  (setq isub 0)
  (while varieties
   (setq iaorist (car varieties))
   (setq varieties (cdr varieties))
;   (setq ans1 (aoristP iaorist dhaatu class pada upa-syms isub))
   (setq ctab
	 (conjugation-tab-aorist upa-syms class pada dhaatu iaorist voice))
   (setq ans1 (list iaorist (elt ctab 0) (elt ctab 1) (elt ctab 2)))
   (setq ans (append-if-new ans ans1))
  )
  (solution ans)
 )
)
(defun aoristP1-make ()
 (let (tabname dirname filename buf key  more err)
  (setq tabname "deshpande-aorist.txt")
  (setq dirname "validation")
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (while more
    (condition-case err
     (progn
      (setq key (read buf)) 
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
    (let (dhaatu class pada upasargas tmp ans)
     (setq tmp (sym-dcpu key))
     (setq dhaatu (elt tmp 0))
     (setq class (elt tmp 1))
     (setq pada (elt tmp 2))
     (setq upasargas (elt tmp 3))
     (setq ans (aoristP1 dhaatu class pada upasargas))
     (if upasargas
      (fol-msg (format "aoristP1 : %s %s %s %s : %s\n"
		      dhaatu class pada upasargas ans))
      (fol-msg (format "aoristP1 : %s %s %s : %s\n"
		      dhaatu class pada  ans))
     )
    )
   )
   (kill-buffer nil)
  )
 )
)
(defun conjpass (dhaatu &optional class pada upa-syms)
 (construct-conjpassbase1a dhaatu class pada upa-syms)
)
(defun kta (dhaatu &optional class pada upa-syms)
 (let ()
  (construct-pppart1a dhaatu class pada upa-syms)
 )
)
(defun tvaa (dhaatu &optional class pada upa-syms)
 (let ()
  (construct-ippart1a-tvaa dhaatu class pada upa-syms)
 )
)
(defun lyap (dhaatu &optional class pada upa-syms)
 (let ()
  (construct-ippart1a-ya dhaatu class pada upa-syms)
 )
)
(defun seT (dhaatu &optional class pada upasargas)
 (construct-seT-code1a dhaatu class pada upasargas)
)
(defun inf (dhaatu class pada &optional upa-syms)
 (let ()
  (construct-inf1a dhaatu class pada upa-syms)
 )
)
(defun tavya (dhaatu class &optional pada upa-syms)
 (let ()
  (construct-potpart1a-tavya dhaatu class pada upa-syms)
 )
)
(defun aniiya (dhaatu class &optional pada upa-syms)
 (let ()
  (construct-potpart1a-aniiya dhaatu class pada upa-syms)
 )
)
(defun yat (dhaatu class &optional pada upa-syms)
 (let ()
  (construct-potpart1a-yat dhaatu class pada upa-syms)
 )
)
(defun kyap (dhaatu &optional class pada upa-syms)
 (let ()
  (construct-potpart1a-kyap dhaatu class pada upa-syms)
 )
)
(defun Nyat (dhaatu &optional class pada upa-syms)
 (let ()
  (construct-potpart1a-Nyat dhaatu class pada upa-syms)
 )
)
(defun rpp (dhaatu class pada &optional upa-syms)
 (let (ans1)
  (setq ans1 (construct-rppart1b dhaatu class pada upa-syms))
  (solution ans1)
 )
)
(defun perpp (dhaatu class pada &optional upa-syms)
 (let (ans1 ans2)
  (setq ans1 (construct-perppart1a dhaatu class pada upa-syms))
  (setq ans1 (solution ans1))
  (setq ans2
   (mapcar
    (lambda (x)
     (if (listp x) (car x) x)
    )
    ans1
   )
  )
  ans2
 )
)
(defun futpart (dhaatu class &optional upa-syms)
 (let (ans1 ans2 ans3)
  (setq ans1 (construct-futpart1a dhaatu class 'P upa-syms))
  (setq ans2 (construct-futpart1a dhaatu class 'A upa-syms))
  (setq ans3 (construct-futppart1a dhaatu class 'P upa-syms))
  (setq ans1 
   (mapcar (lambda (x) (sym-concat x 'at)) ans1))
  (setq ans2 
   (mapcar (lambda (x) (sym-concat x 'a)) ans2))
  (setq ans3
   (mapcar (lambda (x) (sym-concat x 'a)) ans3))
  (list
   (solution ans1) (solution ans2) (solution ans3)
  )
 )
)
(defun laT (dhaatu class pada  &optional upa-syms isub voice)
 (let (ctab)
  (setq ctab (construct-conjtab1a dhaatu class pada upa-syms 'laT voice))
  (setq ctab (solution ctab))
  (if isub
   (elt ctab isub)
   ctab
  )
 )
)
(defun la~N (dhaatu class pada  &optional upa-syms isub voice)
 (let (ctab)
  (setq ctab (construct-conjtab1a dhaatu class pada upa-syms 'la~N voice))
  (setq ctab (solution ctab))
  (if isub
   (elt ctab isub)
   ctab
  )
 )
)
(defun loT (dhaatu class pada  &optional upa-syms isub voice)
 (let (ctab)
  (setq ctab (construct-conjtab1a dhaatu class pada upa-syms 'loT voice))
  (setq ctab (solution ctab))
  (if isub
   (elt ctab isub)
   ctab
  )
 )
)
(defun vidhili~N (dhaatu class pada  &optional upa-syms isub voice)
 (let (ctab)
  (setq ctab (construct-conjtab1a dhaatu class pada upa-syms 'vidhili~N voice))
  (setq ctab (solution ctab))
  (if isub
   (elt ctab isub)
   ctab
  )
 )
)
(defun prespart (dhaatu class pada  gender &optional upa-syms isub)
 (let (dtab p)
  (setq p (construct-prespart1a-alt dhaatu class pada upa-syms))
  (setq dtab (plist-get p gender))
  (if isub
   (elt dtab isub)
   dtab
  )
 )
)
(defun Vdecl (citation gen-or-mtype form &optional gender)
 (let (ans ans1)
  (setq ans1 (construct-subanta1 citation gen-or-mtype form))
  ; ans1 has form ((subanta-type form) (M [M-decl] ...))
  (when (member gen-or-mtype '(M F N))
   (setq gender gen-or-mtype)
  )
  (setq ans (plist-get (elt ans1 1) gender))
  ans
 )
)

(defvar SL-dtab-directory "SL/dtabs")
(defun SL-dtab-extract (subanta form gender)
 "The inputs may be either symbols or strings. A file name 'tabname'
  is constructed as subanta-form-gender.txt.
  Return a declension table as read from the file named 'tabname' in
  directory 'SL'.  The result will be an array with 8 elements.
  Each element is an array with 4 elements, each being a string.
  The first element is the case (1,v,2,...,7) (v = vocative case).
  The second element is the singular, 3rd = dual, 4th = plural.
  The file is one generated from the 'declension' section of the
  'morphology' section of the Sanskrit Library Website.
  NOTES on encoding:
  subanta:  use SLP1 transliteration.
  form: use one of the following as abbreviation for the SL 'lexid':
    noun  -> noun/adjective
    compadj -> comparative adjective
    card -> cardinal number
    pron -> pronoun/pronominal adj
    prap1 -> present active participle, class 1, 4, or 10
    prap2 -> present active participle, class 2
    prap3 -> present active participle, class 3 or other reduplicating
    prap5 -> present active participle, class 5, 7, 8, or 9
    prap6 -> present active participle, class 6
    fap -> future active participle
    pfap -> perfect active participle
  gender: use one of the lower case letters 'm f n'
 "
 (SL-dtab-extract1 (format "%s-%s-%s.txt" subanta form gender))
)
(defun SL-dtab-extract1 (tabname)
 (let (filename buf dirname ans more nrow nrec err)
;  (if (not dirname) (setq dirname "SL/dtabs"))
  (setq dirname SL-dtab-directory)
  (setq filename (sangram-filename tabname dirname))
  (if (file-exists-p filename)
   (SL-dtab-extract2 filename)
   nil
  )
 )
)
(defun SL-dtab-extract2 (filename)
 (let (buf dirname ans more nrow nrec err
       REGEXP BOUND NOERROR ans ncol)
  (setq nrow 8) ; 8 rows for a declension table
  (setq ans (make-vector nrow nil)) ; ans has 1 elt for each row.
  (setq ncol 4) ; # of data items read for each row 
;  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
;  (with-current-buffer buf
  (with-temp-buffer
   (insert-file-contents filename)
   (goto-char (point-min))
   (replace-string "-" "") ; this is reason for temp buffer usage
   
   (goto-char (point-min))
   (setq more t)
   (setq nrec 0)
   (setq BOUND nil)
   (setq NOERROR t)
   (let (anytxt case-pat sing-pat dual-pat plur-pat end-pat)
;    (setq anytxt "\\([a-zA-Z0-9/]+\\)")
    (setq anytxt "\\([a-zA-Z0-9/]*\\)")
;    (setq anytxt "[a-zA-Z0-9]+")
    (setq case-pat (concat "<tr><th><b>" anytxt "</b></th>"))
    (setq sing-pat (concat "<td>" anytxt "</td>"))
    (setq dual-pat (concat "<td>" anytxt "</td>"))
    (setq plur-pat (concat "<td>" anytxt "</td>"))
    (setq end-pat "</tr>")
;   (setq REGEXP "<td>devaH</td><td>devO</td><td>devAH</td></tr>")
    (setq REGEXP (concat case-pat sing-pat dual-pat plur-pat end-pat))
;    (setq REGEXP sing-pat)
;    (fol-msg (format "REGEXP=%s\n" REGEXP))
    
   )

   (while (and (<= nrec nrow) (search-forward-regexp REGEXP BOUND NOERROR))
;    (fol-msg (format "match -> %s\n"
;		     (buffer-substring (match-beginning 0) (point))))
    (let (i n i1 i2 s exprs)
     (setq n ncol)
     (setq exprs (make-vector n nil))
     (setq i 1)
     (while (<= i n)
      (setq i1 (match-beginning i))
      (setq i2 (match-end i))
;      (fol-msg (format "beg=%s, end=%s\n" i1 i2))
      (setq s (buffer-substring i1 i2))
;      (fol-msg (format "expr#%s -> %s\n" i s))
      (if (equal s "") (setq s nil))
      (aset exprs (1- i) s)
      (setq i (1+ i))
     )
;     (fol-msg (format "rec#%s: exprs = %s\n" nrow exprs))
     (aset ans nrec exprs)
     (setq nrec (1+ nrec))
    )
   )
   
;   (kill-buffer nil) ; don't need the buffer
  )
  ans
 )
)
(defvar SL-dtab-tabnames nil)
(defun init-SL-dtab-tabnames ()
 (setq SL-dtab-tabnames '(
   "deva-noun-m.txt"
   "vana-noun-n.txt"
   "rAma-noun-m.txt"
   "ambA-noun-f.txt"
   "latA-noun-f.txt"
   "muni-noun-m.txt"
  ))
)
(defvar SL-ITRANS-genders '(m M f F n N))
(defvar SL-ITRANS-forms
 '(noun NOUN  ; noun/adjective
   compadj ADJ ; comparative adjective
   card CARD ; cardinal number
   pron PRON ; pronoun/pronominal adj
   prap1 prap1 ; present active participle, class 1, 4, or 10
   prap2 prap2 ; present active participle, class 2
   prap3 prap3 ; present active participle, class 3 or other reduplicating
   prap5 prap5 ; present active participle, class 5, 7, 8, or 9
   prap6 prap6 ; present active participle, class 6
   fap fap ; future active participle
   pfap ADJ ; perfect active participle
   adj ADJ ; THIS IS NOT PART OF SL as of 12-23-03
 )
)
(defun SL-dtab-construct (SL-subanta SL-form SL-gender)
 "Construct a declension table that will agree with
  that of SL-dtab-extract. In that are explained the coding conventions.
 "

 (let (ans  SL-dtab)
  (setq SL-dtab (SL-construct-subanta1a SL-subanta SL-form SL-gender))
  
  ; convert to form made by 'SL-dtab-extract'

  (if SL-dtab
  (let (tmp i1 i2 jrow jrowname)
   (setq tmp (mapcar-LE 'symbol-name SL-dtab))
   (setq ans
    (mapcar
     (lambda (irow)
      (setq jrow
       (cond
	((equal irow 0) 0)
	((equal irow 1) 7)
	(t (1- irow))
       )
      )
      (setq jrowname
       (if (equal jrow 7) "v" (format "%s" (1+ jrow)))
      )
      (setq i1 (* jrow 3))
      (setq i2 (+ i1 3))
      (vconcat (vector jrowname) (substring tmp i1 i2))
     )
     '(0 1 2 3 4 5 6 7)
    )
   )
   (setq ans (vconcat ans))
  )
  )
  ans
 )
)
(defun SL-construct-subanta1a (SL-subanta SL-form SL-gender)
 "Use construct-subanta1a to construct a declension table.
  However, 
  (a) use the SLP encoding for subanta (SL-subanta)
  (b) use 'noun adj' etc, instead of 'NOUN' 'ADJ' for SL-form
  (c) use 'm f n' instead of 'M F N' for SL-gender.
  (d) In the answer, express in SL form
 "
 (let (ans IT-subanta IT-form IT-gender IT-dtab SL-dtab)
  ; inputs may be strings or symbols. Convert to symbols
  (if (stringp SL-subanta) (setq SL-subanta (intern SL-subanta)))
  (if (stringp SL-form) (setq SL-form (intern SL-form)))
  (if (stringp SL-gender) (setq SL-gender (intern SL-gender)))
  ; convert inputs to IT form
  ; NOTE 1: SL-subanta may have a '-' character. This is to be 
  ;    omitted here.
  ; NOTE 2: SL-subanta ending in 'aYc' drop the 'Y' in IT-subanta,
  ;   since algorithms based on Antoine assume this. Note that
  ;   Apte has 'pratya~nch' with alternate spelling 'pratyach'
  (let (s sym)
   (with-temp-buffer
    (insert (symbol-name SL-subanta))
    (goto-char 1)
    (replace-string "-" "")
    (goto-char 1)
    (if (re-search-forward "AYc$" nil t) ; AYc at end of line
     (replace-match "Ac" nil nil)
    )
    (goto-char 1)
    (if (re-search-forward "aYc$" nil t) ; aYc at end of line
     (replace-match "ac" nil nil)
    )
    (setq s (buffer-substring (point-min) (point-max)))
    (setq sym (intern s))
    (setq IT-subanta (translate-SLP1-ITRANS sym))
   )
  )

  (setq IT-gender (plist-get SL-ITRANS-genders SL-gender))
  (setq IT-form (plist-get SL-ITRANS-forms SL-form))
  (cond
   ((member IT-form '(prap1 prap2 prap3 prap5 prap6 fap))
    (setq IT-dtab (construct-prap IT-subanta IT-form IT-gender))
   )
   (t
    (setq IT-dtab (construct-subanta1a IT-subanta IT-form IT-gender))
   )
  )
  (setq SL-dtab (translate-ITRANS-SLP1 IT-dtab))
  (when nil ; dbg
   (fol-msg (format "IT: %s %s %s\n" IT-subanta IT-form IT-gender))
   (fol-msg (format "IT-dtab=%s\n" IT-dtab))
   (fol-msg (format "SL-dtab=%s\n" SL-dtab))
  )
  SL-dtab
 )
)
(defun construct-prap-base (subanta form gender)
 ; construct present participle base, compare 'construct-prespart-base'
 ; assume 'subanta' ends in 'at'
 ; assume form is one of:
 ;   prap1 -> present active participle, class 1, 4, or 10
 ;   prap2 -> present active participle, class 2
 ;   prap3 -> present active participle, class 3 or other reduplicating
 ;   prap5 -> present active participle, class 5, 7, 8, or 9
 ;   prap6 -> present active participle, class 6
 ;   fap -> future active participle : treat like 'prap6' (Kale#116, p.71)
 (let (strong weak SW)
  (setq weak subanta)
  (cond
   ((and (equal form 'prap3) (member gender '(M F)))
    (setq strong weak)
   )
   (t
    (let (tok stok)
     (setq tok (car (ITRANS-parse-words-1 (symbol-name subanta))))
     (setq stok (vconcat (substring tok 0 -1) [n] (substring tok -1)))
     (setq strong (sym-without-space stok))
    )
   )
  )
  (cond
   ((equal form 'prap1)
    (setq SW 'S)
   )
   ((equal form 'prap6)
    (setq SW 'SW)
   )
   ((equal form 'fap)
    (setq SW 'SW)
   )
   ((and (equal form 'prap3) (equal gender 'N))
    (setq SW 'W1)
   )
   (t
    (setq SW 'W)
   )
  )
  (list weak strong SW)
 )
)
(defun construct-prap (subanta form gender)
 ; construct present participle base, compare 'construct-prespart-base'
 ; assume 'subanta' ends in 'at'
 ; assume form is one of:
 ;   prap1 -> present active participle, class 1, 4, or 10
 ;   prap2 -> present active participle, class 2
 ;   prap3 -> present active participle, class 3 or other reduplicating
 ;   prap5 -> present active participle, class 5, 7, 8, or 9
 ;   prap6 -> present active participle, class 6
 ;   fap -> future active participle
 (let (praatipadikas dtab)
  (setq praatipadikas (construct-prap-base subanta form gender))
  (setq dtab (declension-pres-part-P praatipadikas gender))
  dtab
 )
)
(defun SL-dtab-validate-one (subanta form gender)
 (let (ans dtab-extract dtab-construct title)
  (setq dtab-extract (SL-dtab-extract subanta form gender))
  (setq dtab-construct (SL-dtab-construct subanta form gender))
  (setq title (format "%s %s %s: " subanta form gender))
  (setq ans (SL-dtab-validate-one-compare
	     title
	     "SL" dtab-extract
	     "OTHER" dtab-construct))
  (when ans
    (fol-msg (format "%s ok\n" title))
  )
  ans
 )
)
(defun SL-dtab-validate-one-compare (title id1 dtab1 id2 dtab2)
 (let (ans)
  (setq ans (equal dtab1 dtab2))
  (when (and (not ans) dtab1 dtab2)
   (setq ans t)
   ; they may in fact be the same, despite not being 'equal'
;    (fol-msg (format "dtab-validate disrepancy for : %s %s %s\n"
; 		    subanta form gender))
   (let (n i e c  m j erow crow case person)
    (setq n (length dtab1))
    (setq i 0)
    (while (< i n)
     (setq erow (elt dtab1 i))
     (setq crow (elt dtab2 i))
     (setq i (1+ i))
     (setq m (length erow)) ; assume crow also has this length
     (setq j 0)
     (setq case (elt erow 0))
     (while (< j m)
      (setq e (elt erow j))
      (setq c (elt crow j))
      (setq person (elt [ID S D P] j))
      (setq j (1+ j))
      (when (not (SL-dtab-validate-one-elt e c))
       (setq ans nil)
       (fol-msg (format "%s (%s%s) %s=%s, %s=%s\n"
			title case person id1 e id2 c))
      )
     )
;      (if (not (equal erow crow))
;       (fol-msg (format "extr. = %s\ncons. = %s\n\n" erow crow))
;      )
    )
   )
  )
  (when (and (not ans) dtab1 (not dtab2))
   (fol-msg (format "%s %s=available, %s=nil\n" title id1 id2))
  )
  (when (and (not ans) (not dtab1) dtab2)
   (fol-msg (format "%s %s=nil, %s=available\n" title id1 id2))
  )
  (when (and (not dtab1) (not dtab2))
   (setq ans nil)
   (fol-msg (format "%s %s=nil, %s=nil\n" title id1 id2))
  )
  (when nil; dbg
   (fol-msg (format "chk: dtab1=%s\ndtab2=%s\n" dtab1 dtab2))
  )
  ans
 )
)
(defun SL-dtab-validate-one-elt (e c)
 ; e is a string. A '/' is used to separate list elements.
 ; c is either a string, or a list of strings.
 ; we want to be able to compare not only elements, but lists
 ; of elements.  We don't want the order of the elements in
 ; a list to matter. The elements are strings.
 (let (e1 c1 e2 c2)
  (if (not (listp c))
   (equal e c)
   (progn
    (if (listp e)
     (setq e1 (copy-sequence e))
     (setq e1 (SL-list e))
    )
    (setq c1 (copy-sequence c))
    (setq e2 (sort e1 'string<))
    (setq c2 (sort c1 'string<))
    (equal e2 c2)
   )
  )
 )
)
(defun SL-list (s)
 ; converts a string "xxx/yyy" into a list ("xxx" "yyy")
 ; If there is no '/' in 's', returns the list containing s.
 ; probably not meaningful if string begins or ends with '/'
 (let (ans i1 i2 s1)
  (with-temp-buffer
   (insert s)
   (goto-char 1)
   (setq i1 1)
   (while (search-forward "/" nil t)
    (setq i2 (match-beginning 0))
    (setq s1 (buffer-substring i1 i2))
    (setq ans (append ans (list s1)))
    (setq i1 (match-end 0)) ; after the /
   )
   ; at this stage, there are no more '/'. Strings ending in '/' are
   ; possibly not handled right
   (setq i2 (point-max))
   (setq s1 (buffer-substring i1 i2))
   (setq ans (append ans (list s1)))
  )
  ans
 )
)
(defun SL-dtab-validation (vfile)
 (let (ans data n record p-field q-field a-field a-field-1 nerr proc err ok)
  (setq data (read-colon-file-validation vfile 0))
  (fol-msg (format "%s %s\n" (san-validation-filename vfile) (length data)))
  (setq n 0)
  (setq nerr 0)
  (setq ans t)
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
;   (fol-msg (format "record=%s\n" record))
   (setq proc 'SL-dtab-validate-one)
   (setq q-field (elt record 0)) ;  subanta form gender
   (setq a-field t)
   (condition-case err
    (setq a-field-1 (apply proc q-field))
    (error
     (fol-msg (format "validation error (%s): %s %s\n" err proc q-field))
     (setq a-field-1 nil)
    )
   )
   (setq a-field (solution a-field))
   (setq a-field-1 (solution a-field-1))
   (setq ok (equal a-field a-field-1))
   (when (not ok)
    ; some problem here
    (setq ans nil) ; overall test fails
;     (fol-msg (format "line %s error: %s %s %s %s\n"
; 		     n proc q-field a-field a-field-1))
    (setq nerr (1+ nerr))
    (when (< 200 nerr)
     (fol-msg (format "more than %s errors. quitting.\n" 50))
     (setq data nil)
    )
   )
;    (when ok
;     (fol-msg (format "SL-dtabs agree: %s\n" q-field))
;    )
  )
  (fol-msg (format "%s errors in %s records in SL-dtab-validation of %s\n"
		    nerr n vfile))
  (setq ans (if (equal nerr 0) t nil))
 )
)
(defun SL-dtab-validation-all (&optional files)
 (let (file ans)
  (if (not files)
  (setq files '(
   "SL-dtab1.txt"
  )))
  (setq ans t)
  (while (and files ans)
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (setq ans (SL-dtab-validation file))
  )
 ans
 )
)
(defun SL-dtab-input-suggest (&optional files)
 (let (file REGEXP BOUND NOERROR anytxt exprs i n i1 i2 s)
  (if (not files)
   (setq files
    (directory-files (file-name-as-directory (sangram-filename "" "SL"))))
  )
  ; files is a list of strings.
  ; we want the strings of form "subanta-form-gender.txt"
  (setq BOUND nil)
  (setq NOERROR t)
  (setq anytxt "\\([a-zA-Z0-9]+\\)")
  (setq REGEXP (format "%s-%s-%s\.txt" anytxt anytxt anytxt))
  (setq n 3)
  (while files
   (setq file (car files)) 
   (setq files (cdr files))
   (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (when (search-forward-regexp REGEXP BOUND NOERROR)
     (setq exprs (make-vector n nil))
     (setq i 1)
     (while (<= i n)
      (setq i1 (match-beginning i))
      (setq i2 (match-end i))
;      (fol-msg (format "beg=%s, end=%s\n" i1 i2))
      (setq s (buffer-substring i1 i2))
;      (fol-msg (format "expr#%s -> %s\n" i s))
      (aset exprs (1- i) s)
      (setq i (1+ i))
     )
     (fol-msg (format "%s %s %s\n"
		      (elt exprs 0) (elt exprs 1) (elt exprs 2)))
    )
   )
  )
  t
 )
)

(defun SL-dtab-printfile (vfile preproc)
 (let (ans data n record p-field q-field a-field a-field-1 nerr err proc ok)
  (setq data (read-colon-file-validation vfile 0))
  (fol-msg (format "%s %s\n" (san-validation-filename vfile) (length data)))
  (setq n 0)
  (setq nerr 0)
  (setq ans t)
  (cond
   ((equal preproc 'construct)
    (setq proc 'SL-dtab-print-one-construct)
   )
   ((equal preproc 'extract)
    (setq proc 'SL-dtab-print-one-extract)
   )
   (t
    (fol-msg (format "SL-dtab-printfile error: %s\n" preproc))
    (setq data nil)
   )
  )
  
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
;   (fol-msg (format "record=%s\n" record))
   
   (setq q-field (elt record 0)) ;  subanta form gender
   (setq a-field t)
   (condition-case err
    (setq a-field-1 (apply proc q-field))
    (error
     (fol-msg (format "printfile error (%s): %s %s\n" err proc q-field))
     (setq a-field-1 nil)
    )
   )
   (setq a-field (solution a-field))
   (setq a-field-1 (solution a-field-1))
;   (setq ok (equal a-field a-field-1))
   (when (and nil (not ok))
    ; some problem here
    (setq ans nil) ; overall test fails
;     (fol-msg (format "line %s error: %s %s %s %s\n"
; 		     n proc q-field a-field a-field-1))
    (setq nerr (1+ nerr))
    (when (< 50 nerr)
     (fol-msg (format "more than %s errors. quitting.\n" 50))
     (setq data nil)
    )
   )
;    (when ok
;     (fol-msg (format "SL-dtabs agree: %s\n" q-field))
;    )
  )
  (when nil
   (fol-msg (format "%s errors in %s records in SL-dtab-validation of %s\n"
		    nerr n vfile))
  )
  (setq ans (if (equal nerr 0) t nil))
 )
)
(defun SL-dtab-print-one-construct (subanta form gender)
 (let (dtab title)
  (setq dtab (SL-dtab-construct subanta form gender))
  (setq title (format "%s %s %s: " subanta form gender))
  (fol-msg (format "%s\n" title))
  (SL-dtab-print dtab)
  t
 )
)
(defun SL-dtab-print-one-extract (subanta form gender)
 (let (dtab title)
  (setq dtab (SL-dtab-extract subanta form gender))
  (setq title (format "%s %s %s: " subanta form gender))
  (fol-msg (format "%s\n" title))
  (SL-dtab-print dtab)
  t
 )
)

(defun SL-dtabs1-make ()
 ; 12-31-03. Constructs declension tables in directory 'SL/dtabs1'
 ; using files 'SL/dtest/Dtest-harness.txt' and
 ; 'SL/dtest/Dtest-compare.txt'
 ; These files correspond to the nominal forms from 'stems.txt'
 ; and were obtained from P. Scharf - they are thus from the SL
 ; morphology.
 (let (bufin-c hdata nhdata bufout-keys cdata ncdata)
  ; get bufin-h : Dtest-harness.txt
  (let (dirname tabname filename)
   (setq tabname "Dtest-harness.txt")
   (setq dirname "SL/dtest")
   (setq filename (sangram-filename tabname dirname))
   (setq hdata (read-colon-file filename 0))
  )
  ; get bufin-c : Dtest-compare.txt
  (let (dirname tabname filename)
   (setq tabname "Dtest-compare.txt")
   (setq dirname "SL/dtest")
   (setq filename (sangram-filename tabname dirname))
   (setq bufin-c (find-file-noselect filename nil))
   (setq cdata (read-colon-buffer bufin-c 0 '|))
   (fol-msg (format "cdata length=%s\n" (length cdata)))
  )
  ; get and initialize bufout-keys: SL-dtest.txt
  (let (dirname tabname filename)
   (setq tabname "SL-dtest.txt")
   (setq dirname "SL/dtest")
   (setq filename (sangram-filename tabname dirname))
   (setq bufout-keys (find-file-noselect filename nil))
   (with-current-buffer bufout-keys
    (erase-buffer)
   )
  )
  ; must go through 'harness', 'compare' is in that order.
  (setq nhdata 0)
  (while hdata
   (let (d nd d0 d1 )
    (setq d (car hdata))
    (setq hdata (cdr hdata))
    ; e.g. d=[(load META_GRAMMAR)]
    (setq d0 (elt d 0)) ;  (load META_GRAMMAR)
    (when (equal (elt d0 0) 'decline)
     (setq nhdata (1+ nhdata)) ; for indexing into 'compare'
     (setq d1 (cdr d0))
      ; e.g. d1 = (praTama m) or
      ; d1 = (nayat n "v1 prap"), etc
     (let (subanta gender form form1)
      (setq subanta (elt d1 0))
      (setq gender (elt d1 1))
      (setq form (elt d1 2)) ; nil or a string
      (cond
       ((equal form nil) (setq form1 'noun))
       ((equal form "v1 prap") (setq form1 'prap1))
       ((equal form "v2 prap") (setq form1 'prap2))
       ((equal form "v3 prap") (setq form1 'prap3))
       ((equal form "v4 prap") (setq form1 'prap1))
       ((equal form "v5 prap") (setq form1 'prap5))
       ((equal form "v6 prap") (setq form1 'prap6))
       ((equal form "v7 prap") (setq form1 'prap5))
       ((equal form "v8 prap") (setq form1 'prap5))
       ((equal form "v9 prap") (setq form1 'prap5))
       ((equal form "v10 prap") (setq form1 'prap1))
       ((equal form 'fap) (setq form1 'fap))
       ((equal form 'prap) (setq form1 'prap)) 
       ((equal form "v8 d prap") (setq form1 'prap3d))
       ((equal form "v1 i prap") (setq form1 'prap3i))
       ((equal form 'pfap) (setq form1 'pfap))
       ((equal form "comp adj") (setq form1 'compadj))
       ((equal form 'pron) (setq form1 'pron))
       ((equal form "cardinal num") (setq form1 'card))
       (t
	(fol-msg (format "could not match form: %s %s %s\n"
			 subanta gender form))
	(setq form1 nil)
       )
      )
      (when (member form1 '(prap prap3d prap3i))
       (fol-msg (format "%s %s %s ; warning (%s) \n"
			subanta form1 gender form))
      )
      (let ()
       (with-current-buffer bufout-keys
	(insert (format "%s %s %s\n" subanta form1 gender))
       )
      )
      ;access and process corresponding part of 'compare' (cdata)
      (let (n n1 n2 i d  bufout)
       ; initialize output file
       (let (dirname tabname filename)
	(setq tabname (format "%s-%s-%s.txt" subanta form1 gender))
	(message tabname)
	(setq dirname "SL/dtabs1")
	(setq filename (sangram-filename tabname dirname))
	(setq bufout (find-file-noselect filename nil))
	(with-current-buffer bufout
	 (erase-buffer)
	)
       )
       (setq n (+ 3 (* (1- nhdata) 12)))
       (setq n1 (+ n 1))
       (setq n2 (+ n 8))
       (setq i n1)
       (while (<= i n2)
	(setq d (elt cdata i))
	; d is like [(1) (devaH) (devO) (devAH) nil]
	(setq d
	 (mapcar
	  (lambda (x) (if x x ""))
	  d
	 )
	)
	(let (anytxt case-pat sing-pat dual-pat plur-pat end-pat s)
	 (setq anytxt (format "%s" (solution (elt d 0))))
	 (setq case-pat (concat "<tr><th><b>" anytxt "</b></th>"))

	 (setq anytxt (format "%s" (solution (elt d 1))))
	 (setq sing-pat (concat "<td>" anytxt "</td>"))

	 (setq anytxt (format "%s" (solution (elt d 2))))
	 (setq dual-pat (concat "<td>" anytxt "</td>"))

	 (setq anytxt (format "%s" (solution (elt d 3))))
	 (setq plur-pat (concat "<td>" anytxt "</td>"))
	 (setq end-pat "</tr>")
	 (setq s (concat case-pat sing-pat dual-pat plur-pat end-pat))
	 (with-current-buffer bufout
	  (insert (format "%s\n" s))
	 )
	)
	(setq i (1+ i))
       )
       (with-current-buffer bufout
	(save-buffer)
       )
       (kill-buffer bufout)
;       (setq hdata nil)
      )
     )
    )
   )
  )
  ; write out keys
  (with-current-buffer bufout-keys
   (save-buffer)
  )
  t
 )
)
(defun SL-dtest-validation (vfile)
 "use files in 'dtabs1' directory for extraction"
 (let (dir ans)
  (setq dir SL-dtab-directory)
  (setq SL-dtab-directory "SL/dtabs1")
  (setq ans (SL-dtab-validation vfile))
  (setq SL-dtab-directory dir)
  ans
 )
)

(defun SL-extract-compare-dtabs-dtabs1-one (subanta form gender)
 (let (ans dtab1 dtab2 title dir)
  (setq dir SL-dtab-directory)
  (setq SL-dtab-directory "SL/dtabs")
  (setq dtab1 (SL-dtab-extract subanta form gender))
  (setq SL-dtab-directory "SL/dtabs1")
  (setq dtab2 (SL-dtab-extract subanta form gender))
  (setq SL-dtab-directory dir)
  (setq title (format "%s %s %s: " subanta form gender))
  (if dtab1
   (setq ans (SL-dtab-validate-one-compare
	     title
	     "dtabs" dtab1
	     "dtabs1" dtab2))
   (setq ans 'NA)
  )
  (when ans
    (fol-msg (format "%s ok\n" title))
  )
  
  ans
 )
)
(defun SL-extract-compare-dtabs-dtabs1 (vfile)
 (let (ans data n record p-field q-field a-field a-field-1 nerr proc err ok
	   newdata)
  (setq data (read-colon-file-validation vfile 0))
;  (fol-msg (format "%s %s\n" (san-validation-filename vfile) (length data)))
  (setq n 0)
  (setq nerr 0)
  (setq ans t)
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
;   (fol-msg (format "record=%s\n" record))
   (setq proc 'SL-extract-compare-dtabs-dtabs1-one)
   (setq q-field (elt record 0)) ;  subanta form gender
   (setq a-field t)
   (condition-case err
    (setq a-field-1 (apply proc q-field))
    (error
     (fol-msg (format "validation error (%s): %s %s\n" err proc q-field))
     (setq a-field-1 nil)
    )
   )
   (setq a-field (solution a-field))
   (setq a-field-1 (solution a-field-1))
   (setq ok (equal a-field a-field-1))
   (when (equal a-field-1 'NA)
    (setq newdata (append newdata (list record)))
    (setq ok t)
   )
   (when (not ok)
    ; some problem here
    (setq ans nil) ; overall test fails
;     (fol-msg (format "line %s error: %s %s %s %s\n"
; 		     n proc q-field a-field a-field-1))
    (setq nerr (1+ nerr))
    (when (< 50 nerr)
     (fol-msg (format "more than %s errors. quitting.\n" 50))
     (setq data nil)
    )
   )
;    (when ok
;     (fol-msg (format "SL-dtabs agree: %s\n" q-field))
;    )
  )
  (fol-msg (format "%s errors in %s records in SL-extract-compare-dtabs-dtabs1 of %s\n"
		    nerr n vfile))
  (mapcar
   (lambda (y)
    (let (x)
     (setq x (elt y 0))
     (fol-msg (format "%s %s %s\n" (elt x 0) (elt x 1) (elt x 2)))
    )
   )
   newdata
  )
  (setq ans (if (equal nerr 0) t nil))
 )
)
(defun SL-extract-copy-dtabs1-dtabs (vfile)
 (let (ans data n record p-field q-field a-field a-field-1 nerr proc err ok
	   newdata)
  (setq data (read-colon-file-validation vfile 0))
;  (fol-msg (format "%s %s\n" (san-validation-filename vfile) (length data)))
  (setq n 0)
  (setq nerr 0)
  (setq ans t)
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
   (let (subanta form gender tabname filename1 filename2 buf)
    (setq record (elt record 0))
    (setq subanta (elt record 0))
    (setq form (elt record 1))
    (setq gender (elt record 2))
    (setq tabname (format "%s-%s-%s.txt" subanta form gender))
    (setq filename1 (sangram-filename tabname "SL/dtabs"))
    (setq filename2 (sangram-filename tabname "SL/dtabs1"))
    (when (not (file-exists-p filename1))
     (setq buf (find-file-noselect filename1 nil))
     (with-current-buffer buf
      (insert (format "<source=Dtest-compare>\n"))
      (insert-file-contents filename2)
      (save-buffer)
     )
     (kill-buffer buf)
    )
   )
  )
  t
 )
)

(defun SL-dtab-print (tab)
 (mapcar 
  (lambda (r)
   (let (y)
    (setq y (mapconcat (lambda (x) (format "%s" x)) r " "))
    (fol-msg (format "%s\n" y))
   )
  )
  tab
 )
 t
)
