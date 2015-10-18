; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 

; Mbh.el
; begun 03-26-04. See Mbhdoc.txt

(defvar Mbh-dir "SL/Ramopakhyana")
(defun Mbh-buf (filename)
 "filename should be a string.
 "
 (find-file-noselect 
  (sangram-filename filename Mbh-dir))
)
(defun Mbh-buffer ()
 (find-file-noselect (sangram-filename "MBhRamaGrammar.rtf" Mbh-dir))
)
(defun Mbhu-buffer ()
 (find-file-noselect 
  (sangram-filename "MBhRamaGrammar-unanalyzed.rtf" Mbh-dir))
)
(defun construct-Mbhu (&optional bufin)
 "Count 'unanalyzed' records in 'Mbh-buffer',
  and save them in 'Mbhu-buffer'
 "
 (let (regexp more nline nfields nfound bufout fields)
  (if (not bufin) (setq bufin (Mbh-buffer)))
  (setq bufout (Mbhu-buffer))
  (with-current-buffer bufout (erase-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (setq more t)
   (setq nline 0)
   (setq nfound 0)
   (while more
    (setq fields (read-buffer-fields "[\t]"))
    (setq nline (1+ nline))
    (setq nfields (length fields))
    (cond
     ((not (elt fields 1))
      ; skip nil field
     )
     ((string-match "[.]" (elt fields 1))
      ; compound analysis. Skip
     )
     ((< nfields 6)
      (fol-msg (format "line#%s has %s fields\n" nline nfields))
     )
     (t
      (setq nfound (1+ nfound))
      (let (s)
       (setq s (current-line))
       (with-current-buffer bufout (insert (format "%s\n" s)))
      )
     )
    )
    (forward-line)
    (setq more (and more (< (point) (point-max))))
   )
   (with-current-buffer bufout (save-buffer 0))
   (fol-msg (format "%s lines processed\n" nline))
   (fol-msg (format "%s lines were as expected\n" nfound))
   (fol-msg (format " These were put into file %s\n" bufout))
  )
 )
)
(defun Mbhu-lexid-distrib ()
 "get the distribution of lexids (field 6) from 'Mbhu-buffer'.
  Put result into 'Mbhu-lexid.txt'.
 "
 (let (bufin bufout more nline nfound fields distrib)
  (setq bufout (Mbh-buf "Mbhu-lexid.txt"))
  (with-current-buffer bufout (erase-buffer))
  (setq bufin (Mbhu-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (setq more t)
   (setq nline 0)
   (setq nfound 0)
   (while (< (point) (point-max))
    (setq fields (read-buffer-fields "[\t]"))
    (let (s gen n)
     (setq s (elt fields 5)) ; a string
     (setq gen (intern s)) ; as strings vary in plist
     (setq n (plist-get distrib gen))
     (if (not n) (setq n 1) (setq n (1+ n)))
     (setq distrib (plist-put distrib gen n))
    )
    (forward-line)
   )
   (with-current-buffer bufout
    (while distrib
     (let (x n)
      (setq x (car distrib))
      (setq distrib (cdr distrib))
      (setq n (car distrib))
      (setq distrib (cdr distrib))
      (insert (format "%5s %s\n"  n x))
     )
    )
    (save-buffer 0)
   )
  )
 t
 )
)
(defun Mbhu-validate1 (lexreg)
 "
 "
 (let (bufin bufout more nline nfound fields nagree)
  (setq bufout (Mbh-buf "temp.txt"))
  (with-current-buffer bufout (erase-buffer))
  (setq bufin (Mbhu-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (setq more t)
   (setq nline 0)
   (setq nfound 0)
   (setq nagree 0)
   (while (and more (< (point) (point-max)))
    (setq fields (read-buffer-fields "[\t]"))
    (setq nline (1+ nline))
    (let (lex inflected analysis base)
     (setq lex (elt fields 5)) ; a string
     (when (string-match lexreg lex)
      (setq nfound (1+ nfound))
      (message (format "%s %s" nfound nline))
      (setq inflected (elt fields 2))
      (setq analysis (elt fields 3))
      (setq base (elt fields 4))
      (if (Mbhu-validate1-help inflected analysis base lex)
       (setq nagree (1+ nagree))
       (progn
	(fol-msg (format "DIFF @line %s: %s %s %s %s\n"
		      nline inflected analysis base lex))
       )
      )
     )
    )
    (forward-line)
    (when nil ; dbg
     (if (< 10 nfound) (setq more nil))
    )
   )
   ;
  )
  (fol-msg (format "%s %s %s (nline nfound nagree)\n" nline nfound nagree))
 t
 )
)

(defun Mbhu-validate1-help (inflected analysis-in base lex)
 "All arguments are strings.
 "
 (let (ans expls analysis)
  ; analysis-in may have alternates. We use only the first alternate.
  (setq analysis (car (Mbhu-alternates-list analysis-in)))
  (setq expls (Sx1 inflected))
  (when (not expls)
   (fol-msg (format "NO EXPL: %s %s %s %s\n" inflected analysis-in base lex))
  )
  (mapcar
   (lambda (e)
    (when (Mbhu-validate1-help-1a analysis lex base e)
     (setq ans (cons e ans))
    )
   )
   expls
  )
  (when nil
   (fol-msg (format "CHK1: %s %s %s %s\n"
		      inflected analysis base lex))
   (mapcar
    (lambda (e) (fol-msg (format "CHK1A: %s\n" e)))
    expls
   )
  )
  ans
 )
)
(defun Mbhu-alternates-list (s)
 "'s' is a string, which may use a '/' to indicate alternates.
  We return a list of strings. 
  If there is no '/', the list has the one element 's'.
  Here are examples:
    vt4/8m -> vt4m vt8m
    m/n7s -> m7s n7s
    m/f/n1s -> m1s f1s n1s
 "
; vt4/8m  vt 4/8 m
; m/f/n1s '' m/f/n 1s
 (let (ans pfx sfx ibeg iend i c thisans)
  (when (string-match "\\(./\\)+" s)
   (setq ibeg (match-beginning 0))
   (setq iend (1+ (match-end 0)))
   (when nil ; dbg
    (fol-msg (format "string=%s\n" (substring s ibeg iend)))
   )
   (setq pfx (substring s 0 ibeg))
   (setq sfx (substring s iend))
   (setq i ibeg)
   (while (< i iend)
    (setq c (elt s i))
    (setq thisans (concat pfx (char-to-string c) sfx))
    (setq ans (cons thisans ans))
    (setq i (+ i 2)) ; skip '/'
   )
  )
  (if ans 
   (nreverse ans)
   (list s)
  )
 )
)
(defun Mbhu-validate1-help-1a (analysis lex base e)
 "1st 3  arguments are strings.
 "
 (let (ans a-type e-analysis e-lex e-base e-arg e-type)
  ; e like one of
  ;  (janamejaya (S NOUN a M) 1S <MW=jana-m-ejaya,100061,1>)
  ;  (gam (V 1 P PRE) 3S <MW=gam\,83767\,1>)
  (setq e-base (symbol-name (elt e 0)))
  (setq e-arg (elt e 1))
  (setq e-type (elt e-arg 0)) ; symbol
  (setq a-type (elt analysis 0))
  (cond
   ((member a-type '(?m ?f ?n)) 
    ; substantive
    (when (equal e-type 'S)
     (let (gender pn)
      (setq e-lex (elt e-arg 1)) ; 
      (setq gender (elt e-arg 3))
      (setq pn (elt e 2))
      ; 'pn' = [1-8][sdp]
      ;  has '8' for a case number; 'analysis' uses 'v' (vocative)
      ; instead.  Change the 8 to v, if present.
      ; pn, a symbol, is changed first to a string
      (setq pn (symbol-name pn))
      (when (equal (elt pn 0) ?8)
       (aset pn 0 ?v)
      )
      (setq e-analysis (format "%s%s" gender pn))
      (setq e-analysis (downcase e-analysis))
      (when nil ; dbg)
       (when (equal analysis e-analysis)
        (when (not (equal base e-base))
	 (fol-msg (format "baseCHK: %s %s\n" base e-base))
        )
       )
      )
      (cond
       ((and (equal analysis e-analysis) (equal base e-base)) (setq ans t))
       ((not (equal analysis e-analysis)) (setq ans nil))
       ((and (equal (substring base 0 -1) (substring e-base 0 -1))
	     (equal (substring base -1) "a") (equal (substring base -1) "A")
	)
	; Sometimes the base of a feminine form ending in 'A' is specified
	; as ending in 'a'.
	; This difference is not counted as material, though it is noted
	(setq ans t)
	(fol-msg (format "WARN:  f. base in 'a': %s %s %s \n"
			 analysis lex base))
       )
      )
     )
    )
   )
   ((member a-type '(?i))
    ; indeclinable
    (when (equal e-type 'I)
     (let ()
     )
    )
   )
   ((member a-type '(?1 ?2 ?3))
    ; conjugated verb
    (when (equal e-type 'VV)
     (let ()
     )
    )
   )
   (t
    ; unrecognized
    (fol-msg (format "Mbhu-validate1-help-1a (ERR): %s\n" analysis))
   )
  )
  ans
 )
)
(defun Mbhu-validate2 (altlex &optional dbg)
 "'altlex' is a string
 "
 (let (bufin bufout more nline nfound fields nagree)
  (setq bufout (Mbh-buf "temp.txt"))
  (with-current-buffer bufout (erase-buffer))
  (setq bufin (Mbhu-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (setq more t)
   (setq nline 0)
   (setq nfound 0)
   (setq nagree 0)
   (while (and more (< (point) (point-max)))
    (setq fields (read-buffer-fields "[\t]"))
    (setq nline (1+ nline))
    (let (lex inflected analysis base alex)
     (setq lex (elt fields 5)) ; a string
     (setq alex (Mbhu-convert-lex lex))
     (when (equal altlex alex)
      (setq nfound (1+ nfound))
      (message (format "%s %s" nfound nline))
      (setq inflected (elt fields 2))
      (setq analysis (elt fields 3))
      (setq base (elt fields 4))
      (if (Mbhu-validate2-help inflected analysis base lex alex dbg)
       (setq nagree (1+ nagree))
       (progn
	(fol-msg (format "DIFF @line %s: %s %s %s %s\n"
		      nline inflected analysis base lex))
       )
      )
     )
    )
    (forward-line)
    (when dbg ; dbg
     (if (< 10 nfound) (setq more nil))
    )
   )
   ;
  )
  (fol-msg (format "%s %s %s (nline nfound nagree)\n" nline nfound nagree))
 t
 )
)
(defun Mbhu-validate2-help (inflected-in analysis-in base-in lex-in &optional alex dbg)
 "All arguments are strings.
 "
 (let (ans analysis)
;  (fol-msg (format "lex-in=%s\n" lex-in))
  (cond
   ((equal alex "V")
    (Mbhu-validate2-V-0 inflected-in analysis-in base-in lex-in dbg)
   )
   ((equal alex "I")
    (Mbhu-validate2-I-0 inflected-in analysis-in base-in lex-in)
   )
   (t ; assume subanta
    (Mbhu-validate2-S-0 inflected-in analysis-in base-in lex-in)
   )
  )
 )
)
(defun Mbhu-validate2-V-0 (inflected-in analysis-in verb-in lex-in
					&optional dbg)
 "All inputs are strings, coded acc. to SL conventions.
  This routine applies to verbs.
  'verb-in' has one of two forms: 'pfx%root' or '%root'.
  'analysis-in' has two parts: pnv t (person-number-voice tense)
  'lex-in' encodes the  classes and 'voices' governing the inflection of
    the root.
  The routine 
 "
 (let (ans lex analysis SL-dhaatu SL-upasargas cps cps-main cps-alt alt
	SL-p SL-n SL-v SL-c SL-tense parts inflected-bases)
  (let (x y z upas pfx)
   ; parts will have two string elements, the first will be "" for
   ; an unprefixed verb.
   (setq parts (read-string-fields verb-in "%"))
   (setq SL-dhaatu (intern (elt parts 1)))
   (setq pfx (elt parts 0))
   (if (equal (elt parts 0) "")
    (setq SL-upasargas nil)
    (progn
     (setq upas (read-string-fields pfx " "))
     (setq SL-upasargas (mapcar 'intern upas))
    )
   )
  )
  ; inflected
  (let (upa-forms upa-form upas base)
   (setq upa-forms (SL-upasarga-forms inflected-in))
   ; upa-forms is a list, each element of which is a list of 3 elements:
   ;  1st element is a list of upasarga symbols
   ;  2nd element is a base symbol
   ;  3rd element is a list that is not used here
   (if (not SL-upasargas)
    (let (x)
     (setq x (intern inflected-in))
     (setq inflected-bases (list x))
    )
    (while upa-forms
     (setq upa-form (car upa-forms))
     (setq upa-forms (cdr upa-forms))
     (setq upas (elt upa-form 0))
     (when (equal upas SL-upasargas)
      (setq base (elt upa-form 1))
      (setq inflected-bases (cons base inflected-bases))
     )
    )
   )
  )
  ; lex-in
  ; 'cps' is a list of elements (class pada), class a number, pada = P or A
  (setq cps-main (Mbhu-extract-V-lex lex-in))
  (setq cps-alt (Mbhu-complete-classes cps-main))
;  (fol-msg (format "cps %s -> %s\n" cps-main cps-alt))
  ; analysis-in
  (setq parts (read-string-fields analysis-in " "))
  (setq SL-p (char-to-string (elt (elt parts 0) 0))) ;1 2 3 (person)
  (setq SL-n (char-to-string (elt (elt parts 0) 1))) ;s d p (number)
  (setq SL-v (char-to-string (elt (elt parts 0) 2)))
  (setq SL-tense (elt parts 1))
  (setq SL-c (elt parts 2)) ; may be "c" indicating causal form
  (when dbg
   (fol-msg (format "%s %s %s -> (%s %s) %s (%s %s %s) %s\n"
		   verb-in lex-in analysis-in SL-upasargas SL-dhaatu  cps
		   SL-p SL-n SL-v SL-tense))
   (fol-msg (format "inflected: %s -> %s\n" inflected-in inflected-bases))
  )
  ; get every possible conjugation table for the given root using
  ; (a) each (class pada) in 'cps'
  ; (b) each tense
  ; (c) each voice 'ACTIVE' or 'PASSIVE'
  (setq cps cps-main)
  (setq alt nil)
  (while (and (not ans) cps)
   (let (cp class pada voice tense tenses dhaatu ctab upasargas derived-type)
    (setq cp (car cps))
    (setq cps (cdr cps))
    (if (equal SL-c "c") (setq derived-type 'CAUSAL))
    (setq class (elt cp 0))
    (setq pada (elt cp 1))
    (cond
     ((and (equal pada 'P) (equal SL-v "a"))
      (setq voice 'ACTIVE)
     )
     ((and (equal pada 'A) (equal SL-v "m"))
      (setq voice 'ACTIVE)
     )
     ((and alt (or (equal SL-v "a") (equal SL-v "m")))
      (setq voice 'ACTIVE)
     )
     ((equal SL-v "p")
      (setq voice 'PASSIVE)
     )
     (t
      (setq voice nil)
     )
    )
    (setq tenses (SL-tense-tran SL-tense))
    (while (and (not ans) tenses voice)
     (setq tense (car tenses))
     (setq tenses (cdr tenses))
     (setq dhaatu (translate-SLP1-ITRANS SL-dhaatu))
     (setq upasargas nil)
     (setq ctab
      (construct-conjtab1a dhaatu class pada upasargas tense voice
			   derived-type)
     )
     (setq ctab (solution ctab))
     (setq ctab (mapcar-LE 'translate-ITRANS-SLP1 ctab))
     (when dbg
      (fol-msg (format "construct-conjtab1a %s %s %s %s %s %s %s\n"
		dhaatu class pada upasargas tense voice derived-type))
			   
      (fol-msg (format "ctab=%s\n" ctab))
     )
     (let (ibase ibases)
      (setq ibases inflected-bases)
      (while (and (not ans) ibases)
       (setq ibase (car ibases))
       (setq ibases (cdr ibases))
       (setq ans (Mbhu-validate2-V-2 ibase ctab SL-p SL-n))
       (when (and ans alt)
	(fol-msg (format "ALT-PADA-WARNING (%s): %s %s %s %s\n"
			 cp inflected-in analysis-in verb-in lex-in))
       )
      )
     )
    )
    (when (and (not ans) (not cps) (not alt))
     (setq cps cps-alt)
     (setq alt t)
;     (fol-msg (format "trying alt: %s\n" cps-alt))
    )
   )
   
  )
  ans
 )
)
(defun SL-tense-tran (SL-tense)
 "SL-tense is a string.
  Returns a list of symbols;
  Except for aorists, there is just one symbol in the list.
  The SL-tense 'aor' returns the list
     (lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7).
  The symbols 'aop' (aorist optative) and 'asb' (aorist subjunctive)
  also return this list.
 "
    (let (x y z more SL-tenseup tense)
     (setq tense nil)
     (if (stringp SL-tense)
      (setq SL-tenseup (upcase SL-tense)) ; string
     )
     (setq x all-tenses-SL-plist)
     (setq more t)
     (while (and more x)
      (setq y (car x)) ; ejf encoding of tense
      (setq x (cdr x))
      (setq z (car x)) ; SL encoding, in upper case (a symbol)
      (setq x (cdr x))
      (when (or
	     (equal (symbol-name z) SL-tenseup)
	     (and (equal SL-tenseup "POP") (equal z 'OPT))
	    )
       (setq tense y)
       (setq more nil)
      )
     )
     (when (member SL-tense '("aor" "aop" "asb"))
      (setq tense '(lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7))
     )
     (when (not tense)
      (fol-msg (format "UNKNOWN TENSE: %s\n" SL-tense))
     )
     (when (not (listp tense)) (setq tense (list tense)))
     tense
    )
)
(defun Mbhu-validate2-V-2 (inflected tab SL-p SL-n)
 (let (ans g isubs p-in n-in)
;   (fol-msg (format "%s %s %s %s\n" inflected SL-p SL-n tab))
   (setq isubs (Mbhu-validate2-match inflected tab))
   (setq p-in (string-to-number SL-p))
   (setq n-in (intern (upcase SL-n)))
   (while (and (not ans) isubs)
    (let (i pn thisans s p n)
     (setq i (car isubs))
     (setq isubs (cdr isubs))
     (setq pn (elt person-number-set i)) ; [1 S]
     (setq p (elt pn 0))
     (setq n (elt pn 1))
     (setq ans (and (equal p p-in) (equal n n-in)))
    )
   )
  ans
 )
)

(defun Mbhu-validate2-I-0 (inflected-in analysis-in subanta-in lex-in)
 "All inputs are strings, coded acc. to SL conventions.
  This routine applies to indeclineables
 "
 t
)
(defun Mbhu-validate2-S-0 (inflected-in analysis-in subanta-in lex-in
			    &optional dbg)
 "All inputs are strings, coded acc. to SL conventions.
  This routine applies to subantas.
 "
 (let (ans lex analysis SL-subanta val inflected lsub)
  (cond
   ((equal subanta-in "mad")
    (setq subanta-in "asmad")
   )
   ((equal subanta-in "tvad")
    (setq subanta-in "yuzmad")
   )
  )
  (setq SL-subanta (intern subanta-in)) ; symbol
  (setq lex (Mbhu-convert-lex lex-in)) ; string
  (setq lex (intern lex)) ; to symbol
  (setq val (Mbhu-validate2-S-1 SL-subanta lex))
  (setq lsub (length subanta-in))
  (cond
   ((and (<= 2 lsub)
	 (equal (substring subanta-in -2) "tf")
	 (equal (substring analysis-in 1 2) "v")
	 (equal (substring inflected-in -1) "H")
    )
    (setq inflected (intern (concat (substring inflected-in 0 -1) "r")))
   )
   (t
    (setq inflected (intern inflected-in))
   )
  )
  (when dbg (fol-msg (format "CHK: val=%s\n" val)))
  (setq analysis (Mbhu-validate2-S-2 inflected (elt val 1)))
  (when dbg (fol-msg (format "CHK: (%s) analysis=%s\n" inflected analysis)))
  (setq ans (Mbhu-validate2-S-compare analysis-in analysis))
  (when dbg (fol-msg (format "CHK: ans=%s\n" ans)))
  (when (and (not ans) (equal lex 'adj))
   ; try as noun, use recursive call
   (setq ans
     (Mbhu-validate2-S-0 inflected-in analysis-in subanta-in "noun" dbg))
   (when ans
    ; print warning.
    (fol-msg (format "Mbhu-validate2-S-0 (adj -> noun): %s %s %s\n"
		     inflected-in analysis-in subanta-in))
   )
  )
  ans
 )
)
(defun Mbhu-validate2-S-1 (SL-subanta lex)
 "Inputs are symbols.
  Extends  'SL-construct-subanta1' in 'construct' to handle the
  case when 'lex' = 'noun'.
 "
 (let (ans)
  (cond
   ((equal lex 'noun)
    (let (form vals ans1 subname altsub val)
     (setq ans1 (SL-construct-subanta1 SL-subanta 'm))
     (setq form (car ans1))
     (setq val (cadr ans1))
     (setq vals (append vals val))
     (setq ans1 (SL-construct-subanta1 SL-subanta 'f))
     (setq val (cadr ans1))
     (setq vals (append vals val))
     (setq ans1 (SL-construct-subanta1 SL-subanta 'n))
     (setq val (cadr ans1))
     (setq vals (append vals val))
     ; feminine nouns in 'A' are often quoted with a base in 'a'.
     ; Similarly, some adjectives ending in 'a' have a feminine in 'I'.
     ; Since our underlying algorithms don't know this, we have to take
     ; these into account here.
     (setq subname (symbol-name SL-subanta))
     (when (equal (substring subname -1) "a")
      (setq altsub (intern (concat (substring subname 0 -1) "A")))
      (setq ans1 (SL-construct-subanta1 altsub 'f))
      (setq val (cadr ans1))
      (setq vals (append vals val))
      (setq altsub (intern (concat (substring subname 0 -1) "I")))
      (setq ans1 (SL-construct-subanta1 altsub 'f))
      (setq val (cadr ans1))
      (setq vals (append vals val))
     )
     (setq ans (list form vals))
    )
   )
   ((equal lex 'card)
    (let (sname form lex1)
     (setq sname (symbol-name SL-subanta))
     (setq lex1 'pron) ; card
     (cond
      ((equal SL-subanta 'tridaSa) ; a special form of 30
       (setq lex1 'noun) ; leave 'form' nil
      )
      ((equal SL-subanta 'triMSat) ; a special form of 30
       (setq lex1 'noun) ; leave 'form' nil
      )
      ((and (<= 7 (length sname))
	       (equal (substring sname -7) "sahasra"))
       (setq lex1 'noun)
      )
      ((member SL-subanta '(kowi kowI))
       (setq lex1 'noun)
      )
      ((or (member SL-subanta '(paYca zaz sapta nava ))
	  (and (<= 4 (length sname))
	       (equal (substring sname -4) "daSa"))
       )
       (setq form 'CARD)
      )
      ((equal SL-subanta 'azwa)
       (setq form 'IRR)
      )
     )
     (cond
      ((equal SL-subanta 'tridaSa)
       ; a special form of 30. leave base as is
      )
      ((equal SL-subanta 'trimSat)
       ; a special form of 30. leave base as is
      )
      ((or (member SL-subanta '(paYca sapta azwa nava))
	  (and (<= 4 (length sname))
	       (equal (substring sname -4) "daSa"))
       )
       (setq SL-subanta (intern (concat sname "n")))
      )
      
     )
     (if (equal lex1 'noun)
      ; recursive call
      (setq ans (Mbhu-validate2-S-1 SL-subanta lex1))
      (setq ans (or (SL-construct-subanta1 SL-subanta lex1 form)))
     )
;     (fol-msg (format "CHKX: %s %s %s\n" SL-subanta lex1 form))
    )
   )
   ((equal lex 'ord)
    (let (form lex1)
     (setq lex1 'pron)
     (setq form 'ORD)
     (setq ans (or (SL-construct-subanta1 SL-subanta lex1 form)))
    )
   )
   (t 
    (setq ans (SL-construct-subanta1 SL-subanta lex))
;    (fol-msg (format "%s %s -> %s\n" SL-subanta lex ans))
   )
  )
  ans
 )
)
(defun Mbhu-validate2-S-2 (inflected val)
 (let (ans g tab isubs)
  (while val
   (setq g (car val))
   (setq val (cdr val))
   (setq tab (car val))
   (setq val (cdr val))
   (setq isubs (Mbhu-validate2-match inflected tab))
   (while isubs
    (let (i pn thisans s p n)
     (setq i (car isubs))
     (setq isubs (cdr isubs))
     (setq pn (elt case-number-set i)) ; [1 S]
     (setq p (elt pn 0))
     (setq n (elt pn 1))
     (if (equal p 8) (setq p 'v)) ; Mbh uses 'v' for vocative
     (setq s (format "%s%s%s" g p n)) ; m1S
     (setq s (downcase s))
     (setq thisans (intern s))
     (setq ans (cons thisans ans))
    )
   )
  )
  ans
 )
)
(defun Mbhu-validate2-match (inflected tab)
 (let (n i ans x)
  (setq n (length tab))
  (setq i 0)
  (while (< i n)
   (setq x (elt tab i))
   (if (or (equal inflected x)
	   (and (listp x) (member inflected x))
       )
    (setq ans (cons i ans))
   )
   (setq i (1+ i))
  )
 ans
 )
)
(defun Mbhu-convert-lex (lex-in)
 "lex-in is a string. Returns a string.
 "
 (let (bufin ans regexp)
  (setq bufin (Mbh-buf "Mbhu-adjlexid.txt"))
  (setq regexp (format ":%s:" lex-in))
;  (fol-msg (format "regexp=%s\n" regexp))
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (when (search-forward regexp nil t)
;    (fol-msg (format "chk: %s\n" (current-line)))
    (let (p1 p2 s c)
;     (setq s (buffer-substring (match-beginning 1) (match-end 1)))
     (setq p1 (point))
     (end-of-line)
     (setq p2 (point))
     (setq s (buffer-substring p1 p2))
     (setq c (elt s 0))
     (cond
      ((equal c ?S)
       (setq ans (substring s 2)) ; skip 'S:'
      )
      (t
       (setq ans (substring s 0 1)) ; V or I
      )
     )
    )
   )
  )
  ans
 )
)
(defun Mbhu-validate2-S-compare (analysis-in analyses)
 "analysis-in is a string, possibly with multiple options separted by '/'.
 'analyses' is a list of symbols.
 "
 (let (ain a)
  (setq ain (Mbhu-alternates-list analysis-in))
  (setq a (car ain)) ; only match first
  (if (member (intern a) analyses)
   t
   nil
  )
 )
)
(defun Mbhu-extract-V-lex (lex-in)
 "lex-in is a string appearing as a verb lexical identifier in Mbhu,
  e.g. 'vt4/8m'.
  From the 'Mbhu-lexid-V.txt' file, match lex-in and get correspond
  class-pada, e.g. '4 8 m'.
  Convert to list '(4 8 A)  (m->A, a->P).
  Convert this to class-pada pair list e.g. ((4 A) (8 A))
 "
 (let (bufin ans regexp)
  (setq bufin (Mbh-buf "Mbhu-lexid-V.txt"))
  (setq regexp (format "%s:" lex-in))
;  (fol-msg (format "regexp=%s\n" regexp))
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (when (search-forward regexp nil t)
;    (fol-msg (format "chk: %s\n" (current-line)))
    (let (p1 p2 s slist x)
;     (setq s (buffer-substring (match-beginning 1) (match-end 1)))
     (setq p1 (point))
     (end-of-line)
     (setq p2 (point))
     (goto-char p1)
     (while (< (point) p2)
      (setq x (read (current-buffer))) ; a symbol or number)
      (cond
       ((equal x 'm) (setq x 'A)) ; middle = Atmanepada
       ((equal x 'a) (setq x 'P)) ; active = Parasmaipada
       ((equal x 'p) (setq x 'P)) ; active = Parasmaipada (exceptional)
      )
      (setq slist (cons x slist))
     )
     (setq slist (nreverse slist))
     (setq ans (normalize-class-padas slist))
    )
   )
  )
  ans
 )
)
(defun Mbhu-complete-classes (cps-in)
 "Given a list of class-pada pairs 'cps-in', return another such
  list containing (c P) if only (c A) is in cps-in, and
  containing (c A) if only (c P) is in in cps-in.
  Examples:  
   ((4 A) (8 A)) -> ((4 P) (8 P))
   ((4 A) (8 A) (4 P)) -> ((8 P))
   ((4 A) (8 Aa) (4 P) (8 P)) -> nil
 "
 (let (ans c p q cp cp1 cps)
  (setq cps cps-in)
  (while cps
   (setq cp (car cps))
   (setq cps (cdr cps))
   (setq c (elt cp 0))
   (setq p (elt cp 1))
   (setq q (if (equal p 'A) 'P 'A))
   (setq cp1 (list c q))
   (when (not (member cp1 cps-in))
    (setq ans (cons cp1 ans))
   )
  )
  (setq ans (nreverse ans))
  ans
 )
)

(defun Mbh-verse (num &optional opt)
 "Returns  the different words in a
  verse of rAmopAKyAna. The input 'num' is a
  verse number as 3257001.
  The variable 'opt' governs the form of the returned answer:
  nil (or absent): a list, elements are strings representing the words 
  1  a string with the words separated by a space
 "
 (let (ans reg fields)
  (setq reg (format "^%s" num))
  (with-current-buffer (Mbhu-buffer)
   (goto-char 1)
   (while (search-forward-regexp reg nil t)
    (beginning-of-line)
    (setq fields (read-buffer-fields "[\t]"))
    (setq ans (cons (elt fields 2) ans))
    (end-of-line)
   )
  )
  (setq ans (nreverse ans))
  (when ans
   (cond
    ((not opt))
    ((equal opt 1)
     (setq ans (mapconcat 'concat ans " "))
    )
   )
  )
  ans
 )
)
