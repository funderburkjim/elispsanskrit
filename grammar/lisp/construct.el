; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; construct.el  
; begun 09-10-03 from functions within gram2.el
(defvar noisy-load nil)
(defvar join-array-method nil) ; 06-14-04
(defun maximal-prefix-sym (tab-in &optional min-pfx-len)
 (let (ans1 ans)
  (setq ans1 (maximal-prefix tab-in min-pfx-len))
  (setq ans (mapcar 'sym-without-space ans1))
  ans
 )
)
(defun maximal-prefix (tab-in &optional min-pfx-len)
 ; tab-in should be a sequence that is flattenable (this is quite
 ; general). Once flattened, the elements should be symbols encoded
 ; in the ITRANS scheme.
 (let (ans n i tab tabi toks)
  (if (not min-pfx-len) (setq min-pfx-len 2))
  (setq tab (flatten tab-in)) ; a list
  ; remove 'nil' and duplicates
  (let (all tmp)
   (setq all tab)
   (setq tab nil)
   (while all
    (setq tmp (car all))
    (setq all (cdr all))
    (if tmp (setq tab (append-if-new tab tmp)))
   )
  )
  (setq n (length tab))
; get list of tokens
  (setq toks (make-vector n nil))
  (setq i 0)
  (while (< i n)
   (setq tabi (elt tab i))
   (aset toks i (car (ITRANS-parse-words-1 (symbol-name tabi))))
   (setq i (1+ i))
  )
  (setq ans (maximal-prefix-helper toks min-pfx-len))
  ans
 )
)
(defun maximal-prefix-helper (toks min-pfx-len)
 ; toks is an array of sequences (it could be an array of strings)
 (let (ans len1 len2 len more prevans)
  (setq len1 min-pfx-len)
;  (setq len2 (* 2 len1))
  (setq len2 30) ; 30 some arbitrarily chosen large number for a length
  (setq len len1)
  (setq ans (maximal-prefix-helper1 toks len))
  (setq prevans ans)
  (setq len (1+ len))
  (while (and  (<= len len2) (= (length ans) (length prevans)))
   (setq prevans ans)
   (setq ans (maximal-prefix-helper1 toks len))
   (setq len (1+ len))
  )
  prevans
 )
)
(defun maximal-prefix-helper1 (toks min-pfx-len)
 ; toks is an array of sequences (it could be an array of strings)
 (let (ans n i tok m thisans)
  (setq n (length toks))
  (setq i 0)
  (while (< i n)
   (setq tok (elt toks i))
   (setq i (1+ i))
   (setq m (min (length tok) min-pfx-len))
   (setq thisans (substring tok 0 m))
   (setq ans (append-if-new ans thisans))
  )
  ans
 )
)

(defun dcpu-sym (dhaatu class pada upasargas)
 (let (ans upasarga)
  (cond
   ((and class pada)
    (setq ans (sym-without-space (vector dhaatu '- class pada)))
    (while upasargas
     (setq upasarga (car upasargas))
     (setq upasargas (cdr upasargas))
     (setq ans (sym-without-space (vector ans '- upasarga)))
    )
   )
   (class
    ; class present, pada absent (= nil) 
    (setq ans (sym-without-space (vector dhaatu '- class)))
   )
   (t ; class and pada absent. just use dhaatu
    (setq ans dhaatu)
   )
  )
  ans
 )
)
(defun sym-dcpu (sym)
 (let (sparts dhaatu class pada upasarga upasargas)
  (setq sparts (gen-word-list (symbol-name sym) "-")) ; a vector
;  (fol-msg (format "sparts=%s\n" sparts))
  ; 1st is dhaatu, 2nd is classPada rest (if any) are upasargas
  (setq dhaatu (intern (elt sparts 0)))
  (when (< 1 (length sparts))
   (setq class (string-to-number (elt sparts 1))) ; the ending P/A ignored
  )
  (when (<= 2 (length sparts) ) ; pada, upasargas absent
   (setq pada (intern (substring (elt sparts 1) -1))) ; last char 
   (if (not (member pada '(P A))) (setq pada nil))
   (setq upasargas
    (mapcar
     'intern
     (substring sparts 2)
    )
   )
  )
  (list dhaatu class pada upasargas)
 )
)


(defun load-subfields (tabnames dirname &optional appendflag useX)
 (let (tabname filename)
  (if (not (listp tabnames)) (setq tabnames (list tabnames)))
  (while tabnames
   (setq tabname (car tabnames))
   (setq tabnames (cdr tabnames))
   (setq filename (sangram-filename tabname dirname))
   (load-subfields-1 filename appendflag useX)
  )
 )
)
(defun load-subfields-1 (filename  appendflag useX)
 (let (buf key val more dhaatus subkey nrec err nkeep nkeep1)
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (setq nrec 0)
   (if (not (or appendflag useX)) (setq nkeep 0))  ; otherwise, nil
   (while more
    (condition-case err
     (progn
      (setq key "<not read>")
      (setq subkey "<not read>")
      (setq val "<not read>")
      (setq key (read buf)) 
      (setq subkey (read buf)) 
      (setq val (read buf))
      (if (not (listp subkey)) (setq subkey (list subkey)))
      (cond
       (useX
	(if appendflag
	 (XsanAppendElt2 key subkey val)
	 (Xsanput2 key subkey val)
	)
       )
       (t
        (if appendflag
	 (sanAppendElt2 key subkey val)
	 (setq nkeep1 (load-subfields-keep key subkey val))
	)
       )
      )
;      (sanput2 key subkey val)
      (setq nrec (1+ nrec))
      (if (and nkeep nkeep1) (setq nkeep (+ nkeep 1)))
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error(%s): %s %s %s\n" err key subkey val))
      )
     )
    )
   )
   (kill-buffer nil)
  )
  (if nkeep
   (fol-msg (format "load-subfields : %s  %s (%s)\n"
		   filename nrec nkeep))
   (fol-msg (format "load-subfields : %s %s\n"
		   filename nrec))
  )
  t
 )
)
(defun load-subfields-keep (key subkey val)
 ; subkey assumed to be a list
 (let (nkept class-keys)
  ; class-keys are those verbal subtypes that do not
  ; depend on the 'pada' of the root. Generally, they
  ; are all the 'passive' constructs.
  (setq class-keys
   '(laT-P la~N-P loT-P vidhili~N-P
     liT-p-P liT-r-P lRiT-P lRi~N-P luT-P aashiirli~N-P
     lu~N1-P lu~N2-P lu~N3-P lu~N4-P lu~N5-P lu~N6-P lu~N7-P
     PASSPART PPPART
    )
  )
  (cond
   ((not (equal (car subkey) 'dhaatu))
    (sanput2 key subkey val)
    (setq nkept t)
   )
   ((member (elt subkey 1) '(Eng-def forms))
    (sanput2 key subkey val)
    (setq nkept t)
   )
   ((member (elt subkey 1) class-keys)
    (setq nkept (load-subfields-keep-class key subkey val))
   )
   (t
    (setq nkept (load-subfields-keep-classPada key subkey val))
   )
  )
  nkept
 )
)
(defun load-subfields-keep-class (key subkey val)
; key assumed to be a 'dcpu' form (dhaata-classPada-upasargas)
 (let (nkept dcpu dhaatu class pada upasargas val1 key1)
  (setq dcpu (sym-dcpu key))
  (setq dhaatu (elt dcpu 0))
  (setq class (elt dcpu 1))
  (setq pada (elt dcpu 2))
  (setq upasargas (elt dcpu 3))
  (setq key1 (dcpu-sym dhaatu class nil nil)) ; key w/o pada or upasargas
  (setq val1 (sanget2 key1 subkey))
  (cond
   ((not val1)
    ; value not present. Store it under key1
    (sanput2 key1 subkey val)
    (setq nkept t)
   )
   ((equal val val1)
    ; the value for this pada-upasarga and subkey is the
    ; same as for the subkey with no pada-upasarga.
    ; no action required. 'nkept' retains its 'nil' value
   )
   (t
    ; val1 is present, but differs
    ; store 'val' under 'key'
    (sanput2 key subkey val)
    (setq nkept t)
    (if noisy-load (fol-msg (format "pada check: %s %s\n" key subkey)))
   )

  )
  nkept  
 )
)
(defun load-subfields-keep-classPada (key subkey val)
; key assumed to be a 'dcpu' form (dhaata-classPada-upasargas)
 (let (nkept dcpu dhaatu class pada upasargas val1 key1)
  (setq dcpu (sym-dcpu key))
  (setq dhaatu (elt dcpu 0))
  (setq class (elt dcpu 1))
  (setq pada (elt dcpu 2))
  (setq upasargas (elt dcpu 3))
  (cond
   ((not upasargas)
    ; no upasargas present. Enter 'val' under 'key'
    (sanput2 key subkey val)
    (setq nkept t)
   )
   (t
    ; upasargas present. 
    (setq key1 (dcpu-sym dhaatu class pada nil)) ; key w/o upasargas
    (setq val1 (sanget2 key1 subkey))
    (cond
     ((not val1)
      ; have not yet entered data for key or key1. Put 'val' under 'key1'
      (sanput2 key1 subkey val)
      (setq nkept t)
     )
     ((equal val val1)
      ; the value for this upasarga and subkey is the
      ; same as for the subkey with no upasarga.
      ; no action required. 'nkept' retains its 'nil' value
     )
     (t
      ; this value is peculiar to the upasarga.
      ; Enter 'val' under 'key'
      (sanput2 key subkey val)
      (setq nkept t)
      (if noisy-load (fol-msg (format "upasarga check: %s %s\n" key subkey)))
     )
    )
   )
  )
  nkept
 )
)
(defun construct-cleartemp (&optional OBARRAY)
 (if (not OBARRAY) (setq OBARRAY obarray))
 (mapatoms 'construct-cleartemp1 OBARRAY)
)
(defun construct-cleartemp1 (sym)
 (if (get sym 'Sangram) (sanput sym 'temp nil))
)

(defun construct-dhaatukosha (&optional intab indir outtab outdir)
 (construct-cleartemp)
 (let (nrec irec recs fields nfields dhaatus)
  (if (not intab) (setq intab "dhaatukosha.txt"))
  (if (not indir) (setq indir "inputs"))
  (if (not outtab) (setq outtab intab))
  (if (not outdir) (setq outdir "construct"))
  (setq nfields 3)
  (setq recs (read-colon-file-table intab nfields indir))
  (setq nrec (length recs))
  (fol-msg (format "nrec=%s\n" nrec))
  (setq irec 0)
  (while (< irec nrec)
   (setq fields (elt recs irec))
   (let (dhaatu prop val class pada a~Nga new-Eng-def class-pada-info
		upasargas)
    ; 1. unpack 'fields' to other variables
    ; assume fields[0] is a list of length 1 whose member is a symbol
;    (fol-msg (format "fields=%s\n" fields))
    (let (field0) ; either (dhaatu) or (upasarga dhaatu)
      ; or (upasarga1 upasarga2 ... dhaatu)
     (setq field0 (elt fields 0))
     (cond
      ((= 1 (length field0))
       (setq dhaatu (elt field0 0))
       (setq upasargas nil)
      )
      (t
       (let (vec0 n0)
	(setq vec0 (vconcat field0))
	(setq n0 (length vec0))
	(setq upasargas (substring vec0 0 (1- n0)))
	(setq dhaatu (substring vec0 -1)) ; a vector
	(setq dhaatu (elt dhaatu 0)) ; the element, a symbol
	(setq upasargas (append upasargas nil)) ; a list
       )
      )
     )
    )
    (setq class (elt (elt fields 1) 0)) ; class is a number from 1 to 10
    (setq pada (elt (elt fields 1) 1)) ; P or A or U
    (setq new-Eng-def (gather-fields (elt fields 2) ', ))
    ; set class-pada-info
    (setq class-pada-info nil)
    (cond
     ((equal pada 'U)
      (setq class-pada-info (list
			(vector class 'P upasargas new-Eng-def)
			(vector class 'A upasargas new-Eng-def)))
     )
     ((member pada '(P A))
      (setq class-pada-info (list (vector class pada upasargas new-Eng-def)))
     )
     (t ; error
      (fol-msg (format "pada error: %s %s\n%s\n"
		       intab indir fields))
      (setq irec nrec) ; stop outer loop
     )
    )
     (when class-pada-info
;      (if (not (member dhaatu dhaatus))
;       (sanput dhaatu 'temp nil)
;      )
     (setq dhaatus (append-if-new dhaatus dhaatu))
     (mapcar
      (lambda (x)
       (sanAppendElt dhaatu 'temp x)
;       (fol-msg (format "%s %s %s\n" irec dhaatu x))
      )
      class-pada-info
     )
;     (if (< 10 irec) (setq irec nrec))
    )
   )
   (setq irec (1+ irec))
  )
  ; post-processing
  (let (fileout bufout subkey key dhaatu)
   (setq fileout (sangram-filename outtab outdir))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (with-current-buffer bufout
    (erase-buffer)
;     (setq key 'GLOBAL)
;     (setq subkey 'dhaatus)
;     (insert (format "%s %s %s\n" key subkey dhaatus))
    (while dhaatus
     (setq dhaatu (car dhaatus))
     (setq dhaatus (cdr dhaatus))
     (let (keys key data datum class pada upasargas new-Eng-def)
      (setq data (sanget dhaatu 'temp))
      (while data
       (setq datum (car data))
       (setq data (cdr data))
       (setq class (elt datum 0))
       (setq pada (elt datum 1))
       (setq upasargas (elt datum 2))
       (setq new-Eng-def (elt datum 3))
       (setq upasargas (flatten upasargas))
       (setq key (dcpu-sym dhaatu class pada upasargas))
       (setq keys (append-if-new keys key))
       (setq subkey '(V DEF))
       (insert (format "%s %s %s\n" key subkey new-Eng-def))
      )
;      (setq subkey '(dhaatu forms))
;      (insert (format "%s %s %s\n" dhaatu subkey keys))
     )
    )
    (save-buffer)
   )
   (kill-buffer bufout)
  )
  t
 )
)
(defun all-prefix-files (pfx sfx dirin)
 (let (dir files  file ans nsfx npfx)
;  (setq dirin "inputs")
  (setq nsfx (length sfx))
  (setq dir (file-name-as-directory (sangram-filename "" dirin)))
  ; nil indicates rel names
  (setq files (directory-files dir nil pfx))
;  (fol-msg (format "chk: files=%s\n" files))
  (setq npfx (length pfx))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (cond
    ((not (equal (substring file 0 npfx) pfx)))
    ((not (equal (substring file (- nsfx)) sfx)))
    (t ; ok 
     (setq ans (append-if-new ans file))
    )
   )
  )
  ans
 )
)
(defun all-dhaatukosha-inputs ()
 (all-prefix-files "dhaatukosha" ".txt" "inputs")
)
(defun all-subanta-inputs ()
 (all-prefix-files "subanta" ".txt" "inputs")
)
(defun strip-pfx-sfx (s pfx sfx)
 "arguments should be strings.
  When the string 's' starts with 'pfx' and
  ends with 'sfx', the string in the middle is 
  returned.
  Otherwise, nil is returned.
 "
 (let (ans len-s len-pfx len-sfx)
  (cond
   ((not (stringp s)))
   ((not (stringp pfx)))
   ((not (stringp sfx)))
   ((progn
     (setq len-s (length s))
     (setq len-pfx (length pfx))
     (setq len-sfx (length sfx))
     nil ; so this step fails
    )
   )
   ((not (<= len-pfx len-s)))
   ((not (<= len-sfx len-s)))
   (t
    (setq s (substring s len-pfx))
    (when (<= len-sfx len-s)
     (setq ans (substring s 0 (- len-sfx)))
    )
   )
  )
  ans
 )
)
(defun construct-dhaatukosha-all ()
 (let (files file)
  (setq files (all-dhaatukosha-inputs))
  (mapcar 'construct-dhaatukosha files)
 )
)
(defun extract-dhaatukosha-forms (tabname &optional dirname)
 (let (filename buf dirname ans more key subkey val nrec err)
  (if (not dirname) (setq dirname "construct"))
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (setq nrec 0)
   (while more
    (condition-case err
     (progn
      (setq key (read buf)) 
;      (setq subkey (read buf)) 
;      (setq val (read buf))
      (end-of-line)
      (setq nrec (1+ nrec))
      (if (member key ans)
       (fol-msg (format "duplicate key (%s) at record %s\n" key nrec))
       (setq ans (append ans (list key)))
      )
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
   )
   (kill-buffer nil)
  )
  ans
 ) 
)

(defun construct-dhaatu-forms1 (&optional form-files)
 (if (not form-files)
     (setq form-files (all-dhaatukosha-inputs)))
 (apply 'append
    (mapcar 'extract-dhaatukosha-forms form-files)
   )
)
(defun construct-dhaatu-forms-all (&optional outtab outdir)
 (construct-cleartemp)
 (let (forms keys dhaatu outtab  fileout bufout nform dhaatus)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "dhaatu-forms.txt"))
  (setq forms (construct-dhaatu-forms1))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
;  (fol-msg (format "forms=%s\n" forms))
  (setq nform 0)
  (while forms
   (let (form keyval key val)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (let (tmp)
     (setq tmp (sym-dcpu form))
     (setq dhaatu (car tmp))
    )
    (sanAppendElt dhaatu 'temp form)
    (setq dhaatus (append-if-new dhaatus dhaatu))
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (while dhaatus
    (setq dhaatu (car dhaatus))
    (setq dhaatus (cdr dhaatus))
    (let (form keyval key val)
     (setq key dhaatu)
     (setq val (sanget dhaatu 'temp))
     (insert (format "%s %s %s\n" dhaatu '(D FORM) val))
    )
   )
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun dhaatukosha-forms-sorted ()
 (let (forms1 forms dhaatu class pada upasargas dcpu1 dcpu2)
  (setq forms1 (construct-dhaatu-forms1))
  ; remove elements with upasargas
;  (fol-msg (format "#forms1 = %s\n" (length forms1)))
  (while forms1
   (let (f)
    (setq f (car forms1))
    (setq forms1 (cdr forms1))
    (setq dcpu1 (sym-dcpu f))
    (if (not (elt dcpu1 3)) (setq forms (cons f forms)))
   )
  )
;  (fol-msg (format "#forms = %s\n" (length forms)))
  (setq forms (sort forms 'sanforms-lt))
  forms
 )
)
(defun sanforms-lt (form1 form2)
 (let (dcpu1 dcpu2)
  (setq dcpu1 (sym-dcpu form1))
  (setq dcpu2 (sym-dcpu form2))
;  (fol-msg (format "chk: %s %s\n" dcpu1 dcpu2))
  (cond
   ((ITRANS-lt-sym (elt dcpu1 0) (elt dcpu2 0)) t) ; compare dhaatus
   ((not (equal (elt dcpu1 0) (elt dcpu2 0))) nil) ;
   ((< (elt dcpu1 1) (elt dcpu2 1)) t) ; compare class numbers 
   ((and (equal (elt dcpu1 2) 'P) (equal (elt dcpu2 2) 'A)) t) ; P < A
  )
 )
)

(defvar all-voices 
 '(ACTIVE PASSIVE)
)
(defvar all-tenses 
 '(
   laT    ; present
   la~N   ; imperfect  takes  'a
   loT    ; imperative
   vidhili~N ; optative
   liT-p  ; periphrastic perfect
   liT-r  ; reduplicative perfect
   lRiT   ; simple future (2nd future)
   lRi~N  ; conditional (takes 'a')
   luT    ; periphrastic future (1st future) 
   aashiirli~N ; benedictive
   lu~N1  ; 1st aorist (takes 'a)
   lu~N2  ; 1st aorist (takes 'a)
   lu~N3  ; 1st aorist (takes 'a)
   lu~N4  ; 1st aorist (takes 'a)
   lu~N5  ; 1st aorist (takes 'a)
   lu~N6  ; 1st aorist (takes 'a)
   lu~N7  ; 1st aorist (takes 'a)
  )
)
(defvar all-tenses-SL-plist
 '(
   laT    PRE ; present
   la~N   IPF ; imperfect  takes  'a
   loT    IPV ; imperative
   vidhili~N OPT ; optative
   liT-p  PPF ; periphrastic perfect  PPF ACTN
   liT-r  PRF ; reduplicative perfect
   lRiT   FUT ; simple future (2nd future)
   lRi~N  CON ; conditional (takes 'a')
   luT    PFT; periphrastic future (1st future) 
   aashiirli~N  BEN ; benedictive (could not find in Scharf)?
   lu~N1  AOR1 ; 1st aorist (takes 'a)
   lu~N2  AOR2 ; 2nd aorist (takes 'a)
   lu~N3  AOR3 ; 3rd aorist (takes 'a)
   lu~N4  AOR4 ; 4th aorist (takes 'a)
   lu~N5  AOR5; 5th aorist (takes 'a)
   lu~N6  AOR6 ; 6th aorist (takes 'a)
   lu~N7  AOR7 ; 7th aorist (takes 'a)
  )
)
(defun construct-join-arrays (old new)
 (cond
  ((not join-array-method) (join-arrays old new))
  ((equal join-array-method 1)
   ; assume old is a list. Append new to it
   (append old (list new))
  )
 )
)
(defun construct-conjtabs (formtab outtab &optional outdir)
 ; 1-30-04. Input transliteration is SLP. Also construct output in SLP form.
 ; 3-23-04. 1st arg is now a text file name. 
 ;  Each of its lines is a list, (dhaatu class pada dict)
 (let (keys fileout bufout nform ok bufin more)
  (if (not outdir) (setq outdir "construct"))
  (setq ok t)
  (when ok
   (setq fileout (sangram-filename outtab outdir))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (with-current-buffer bufout
    (erase-buffer)
   )
  )
  (setq bufin (find-file-noselect (sangram-filename formtab outdir) 't))
  (setq nform 0)
  (with-current-buffer bufin
   (setq more t)
   (goto-char 1)
   (while (and more (< (point) (point-max)))
    (let (form keyval key val dcpu dhaatu class pada upasargas
	      SLdhaatu dict tmp)
     (setq tmp (read (current-buffer)))
     (forward-line)
     (setq SLdhaatu (elt tmp 0))
     (setq class (elt tmp 1))
     (setq pada (elt tmp 2))
     (setq dict (elt tmp 3))
     (setq nform (1+ nform))
     (message (format "%s %s" nform SLdhaatu))
     (setq dhaatu (translate-SLP1-ITRANS SLdhaatu))
     (mapcar
      (lambda (tense)
       (construct-conjtab-helper bufout tense dhaatu class pada upasargas dict)
      )
      all-tenses
     )
    )
   )
  )
  ;post-processing
  (when ok
   (with-current-buffer bufout
    (save-buffer 0)
    (kill-buffer nil)
   )
  )
  ok
 )
)

(defun construct-conjtab-helper (bufout tense dhaatu class pada upasargas dict)
 (let (form keyval key val)
    (mapcar
     (lambda (voice)
      (cond
       ((and (member tense '(laT la~N loT vidhili~N))
	     (member class '(1 4 6 10))
	     (equal  voice 'ACTIVE)
	)
        (setq keyval
	 (construct-conjtab-base1a tense dhaatu class pada upasargas voice))
       )
       ((and (member tense '(laT la~N loT vidhili~N))
	     (equal voice 'PASSIVE)
	)
	(setq keyval
	 (construct-conjtab-base1a tense dhaatu class pada upasargas voice))
       )
       ((equal tense 'luT) ; periphrastic future
	(setq keyval
	 (construct-conjtab-base-luT dhaatu class pada upasargas voice))
       )
       ((equal tense 'lRiT) ; simple future
	(setq keyval
	 (construct-conjtab-base-lRiT dhaatu class pada upasargas voice))
       )
       ((equal tense 'aashiirli~N) ; benedictive
	(setq keyval
	 (construct-conjtab-base-aashiirli~N
	    dhaatu class pada upasargas voice))
       )
       ((equal tense 'lRi~N) ; conditional
	(setq keyval
	 (construct-conjtab-base-lRi~N
	    dhaatu class pada upasargas voice))
       )
       ((equal tense 'liT-p) ; periphrastic perfect
	(setq keyval
	 (construct-conjtab-base-liT-p
	    dhaatu class pada upasargas voice))
       )
       ((member tense '(lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7)) ;aorist
	(setq keyval
	 (construct-conjtab-base-lu~N tense
	    dhaatu class pada upasargas voice))
       )
       (t
        (setq keyval
         (construct-conjtab1a dhaatu class pada upasargas tense voice))
       )
      )
      (when keyval
       (when (equal voice 'PASSIVE)
	(setq pada 'PV)
       )
       (setq key dhaatu)
       (setq val keyval)
;       (fol-msg (format "chk: %s %s\n" key val))
       (with-current-buffer bufout
	(let (subkey base tense1)
;	 (setq subkey (list 'V class pada tense))
	 (setq tense1 (plist-get all-tenses-SL-plist tense))
	 (setq subkey (list 'V class pada tense1))
	 (cond
	  ((and (listp val) (equal (elt val 0) 'BASE))
	   (setq base (solution (elt val 1)))
	   (SL-construct-output-base bufout base key subkey dict)
	  )
	  (t
	   (setq keyval (solution keyval))
	   (SL-construct-output1 bufout key subkey keyval dict)
	  )
	 )
        )
       )
      )
     )
     all-voices
    )
 )
)
(defun construct-conjtab-base-luT (dhaatu class pada upasargas voice)
 (let (ctab base ans tense)
  (setq tense 'luT)
  (setq ctab (construct-conjtab1a dhaatu class pada upasargas tense voice))
  (when ctab
   (let (celts celt s sb b)
    (setq celts (elt ctab 0)) ; sym or list of syms
    (if (not (listp celts)) (setq celts (list celts)))
    (while celts
     (setq celt (car celts))
     (setq celts (cdr celts))
     (setq s (symbol-name celt))
     (setq sb (substring s 0 -2)) ; drop the 'aa'
     (setq b (intern sb))
     (setq base (append base (list b)))
    )
   )
   (setq ans (list 'BASE base))
  )
  (when (and nil ctab) ; (and t ctab) for checking
   (let ( ctab1 b ctab0 endings base0)
    (if (equal voice 'PASSIVE)
     (setq endings (plist-get2 conjtab-endings (list tense 'A)))
     (setq endings (plist-get2 conjtab-endings (list tense pada)))
    )
    (setq base0 base)
    (while base0
     (setq b (car base0))
     (setq base0 (cdr base0))
     (setq ctab0
      (mapcar (lambda (e) (sym-concat b e)) endings)
     )
     (setq ctab0 (vconcat ctab0)) ; make an array
     (setq ctab1 (if ctab1 (join-arrays ctab1 ctab0) ctab0))
    )
    (when (not (equal ctab ctab1))
     (fol-msg (format "err (%s): %s %s %s %s %s\n %s\n %s\n"
		      tense dhaatu class pada upasargas voice
		      ctab ctab1))
    )
   )
  )
  ans
 )
)
(defun construct-conjtab-base-lRiT (dhaatu class pada upasargas voice)
 (let (ctab base ans tense)
  (setq tense 'lRiT)
  (setq ctab (construct-conjtab1a dhaatu class pada upasargas tense voice))
  (when (and (member dhaatu '(kLip vRit vRidh shRidh syand))
	 (or (equal pada 'A) (equal voice 'PASSIVE))
	 (member tense '(lRiT lRi~N))
	)
   ;; Kale 484 p. 301
   ; The roots (above) optionally take parasmaipada terminations in
   ; the Second Future, Conditional, and Desiderative.
   ; They reject the augment 'i' when parasmaipada terminations are taken.
   (setq ans ctab)
   (setq ctab nil)
  )
  (when ctab
   (let (celts celt s sb b)
    (setq celts (elt ctab 0)) ; sym or list of syms
    (if (not (listp celts)) (setq celts (list celts)))
    (while celts
     (setq celt (car celts))
     (setq celts (cdr celts))
     (setq s (symbol-name celt))
     (setq sb (substring s 0 -4)) ; drop the 'yati' or 'yate'
     (setq b (intern sb))
     (setq base (append base (list b)))
    )
   )
   (setq ans (list 'BASE base))
  )
  (when (and t ctab) ; (and t ctab) for checking
   (let ( ctab1 b ctab0 endings base0)
    (if (equal voice 'PASSIVE)
     (setq endings (plist-get2 conjtab-endings (list tense 'A)))
     (setq endings (plist-get2 conjtab-endings (list tense pada)))
    )
    (setq base0 base)
    (while base0
     (setq b (car base0))
     (setq base0 (cdr base0))
     (setq ctab0
      (mapcar (lambda (e) (sym-concat b e)) endings)
     )
     (setq ctab0 (vconcat ctab0)) ; make an array
     (setq ctab1 (if ctab1 (join-arrays ctab1 ctab0) ctab0))
    )
    (when (not (equal ctab ctab1))
     (fol-msg (format "err (%s): %s %s %s %s %s: NOT USING BASE\n %s\n %s\n"
		      tense dhaatu class pada upasargas voice
		      ctab ctab1))
     (setq ans ctab)
    )
   )
  )
  ans
 )
)

(defun construct-conjtab-base-aashiirli~N (dhaatu class pada upasargas voice)
 (let (ctab base ans tense)
  (setq tense 'aashiirli~N)
  (setq ctab (construct-conjtab1a dhaatu class pada upasargas tense voice))
  (when (and (member dhaatu '(kLip vRit vRidh shRidh syand))
	     (or (equal pada 'A) (equal voice 'PASSIVE))
	     (member tense '(lRiT lRi~N))
	)
   ;; Kale 484 p. 301
   ; The roots (above) optionally take parasmaipada terminations in
   ; the Second Future, Conditional, and Desiderative.
   ; They reject the augment 'i' when parasmaipada terminations are taken.
   (setq ans ctab)
   (setq ctab nil)
  )
  (when ctab
   (let (celts celt s sb b)
    (setq celts (elt ctab 0)) ; sym or list of syms
    (if (not (listp celts)) (setq celts (list celts)))
    (while celts
     (setq celt (car celts))
     (setq celts (cdr celts))
     (setq s (symbol-name celt))
     (if (or (equal pada 'A) (equal voice 'PASSIVE))
      (setq sb (substring s 0 -6)) ; drop the 'iiShTa'
      (setq sb (substring s 0 -4)) ; drop the 'yaat'
     )
     (setq b (intern sb))
     (setq base (append base (list b)))
    )
   )
   (setq ans (list 'BASE base))
  )
  (when (and t ctab) ; (and t ctab) for checking
   (let ( ctab1 b ctab0 endings base0)
    (if (equal voice 'PASSIVE)
     (setq endings (plist-get2 conjtab-endings (list tense 'A)))
     (setq endings (plist-get2 conjtab-endings (list tense pada)))
    )
    (setq base0 base)
    (while base0
     (setq b (car base0))
     (setq base0 (cdr base0))
     (setq ctab0
      (mapcar (lambda (e) (sym-concat b e)) endings)
     )
     (setq ctab0 (vconcat ctab0)) ; make an array
     (setq ctab1 (if ctab1 (join-arrays ctab1 ctab0) ctab0))
    )
    (when (not (equal ctab ctab1))
     (fol-msg (format "err (%s): %s %s %s %s %s: NOT USING BASE\n %s\n %s\n"
		      tense dhaatu class pada upasargas voice
		      ctab ctab1))
     (setq ans ctab)
    )
   )
  )
  ans
 )
)
(defun construct-conjtab-base-lRi~N (dhaatu class pada upasargas voice)
 (let (ctab base ans tense)
  (setq tense 'lRi~N)
  (setq ctab (construct-conjtab1a dhaatu class pada upasargas tense voice))
  (when (and (member dhaatu '(kLip vRit vRidh shRidh syand))
	     (or (equal pada 'A) (equal voice 'PASSIVE))
	     (member tense '(lRiT lRi~N))
	)
   ;; Kale 484 p. 301
   ; The roots (above) optionally take parasmaipada terminations in
   ; the Second Future, Conditional, and Desiderative.
   ; They reject the augment 'i' when parasmaipada terminations are taken.
   (setq ans ctab)
   (setq ctab nil)
  )
  (when ctab
   (let (celts celt s sb b)
    (setq celts (elt ctab 0)) ; sym or list of syms
    (if (not (listp celts)) (setq celts (list celts)))
    (while celts
     (setq celt (car celts))
     (setq celts (cdr celts))
     (setq s (symbol-name celt))
     (if (or (equal pada 'A) (equal voice 'PASSIVE))
      (setq sb (substring s 0 -4)) ; drop the 'yata'
      (setq sb (substring s 0 -3)) ; drop the 'yat'
     )
     (setq b (intern sb))
     (setq base (append base (list b)))
    )
   )
   (setq ans (list 'BASE base))
  )
  (when (and t ctab) ; (and t ctab) for checking
   (let ( ctab1 b ctab0 endings base0)
    (if (equal voice 'PASSIVE)
     (setq endings (plist-get2 conjtab-endings (list tense 'A)))
     (setq endings (plist-get2 conjtab-endings (list tense pada)))
    )
    (setq base0 base)
    (while base0
     (setq b (car base0))
     (setq base0 (cdr base0))
     (setq ctab0
      (mapcar (lambda (e) (sym-concat b e)) endings)
     )
     (setq ctab0 (vconcat ctab0)) ; make an array
     (setq ctab1 (if ctab1 (join-arrays ctab1 ctab0) ctab0))
    )
    (when (not (equal ctab ctab1))
     (fol-msg (format "err (%s): %s %s %s %s %s: NOT USING BASE\n %s\n %s\n"
		      tense dhaatu class pada upasargas voice
		      ctab ctab1))
     (setq ans ctab)
    )
   )
  )
  ans
 )
)
(defun construct-conjtab-base-liT-p (dhaatu class pada upasargas voice)
 (let (keyval)
  (when (periphrastic-liT-P dhaatu class)
   (setq keyval
    (if (equal voice 'PASSIVE)
     (periphrastic-base dhaatu class 'A)
     (periphrastic-base dhaatu class pada)
    )
   )
   (when keyval
    (setq keyval (list 'BASE keyval))
   )
  )
  keyval
 )
)
(defun construct-conjtab-base-lu~N (tense dhaatu class pada upasargas voice)
; NOTE: 10-16-03: I am so unsure of the construction of the PASSIVE aorist 
;  (except for the 3S form), that I include ONLY the 3S form here.
 (let (ctab base ans  endings)
  (setq ctab (construct-conjtab1a dhaatu class pada upasargas tense voice))
  (when ctab
   (let (celts celt s sb b e n elen)
    (if (equal voice 'PASSIVE)
     (progn
      (setq endings (plist-get2 conjtab-endings (list tense 'PA)))
      (if (not endings)
       (setq endings (plist-get2 conjtab-endings (list tense 'A)))
      )
      (setq e (elt endings 0)) ; the 3S ending
      (setq elen (length (symbol-name e)))
     )
     (progn
      (setq endings (plist-get2 conjtab-endings (list tense pada)))
      (setq e (elt endings 0)) ; the 3S ending
      (setq elen (length (symbol-name e)))
     )
    )
    
    (setq celts (elt ctab 0)) ; sym or list of syms
    (if (not (listp celts)) (setq celts (list celts)))
    (while celts
     (setq celt (car celts))
     (setq celts (cdr celts))
     (setq s (symbol-name celt))
     (setq sb (substring s 0 (- elen))) ; drop the 
     (setq b (intern sb))
     (setq base (append base (list b)))
    )
   )
   (setq ans (list 'BASE base))
  )
  (when (and t ctab (not (equal voice 'PASSIVE))) ; (and t ctab) for checking
   (let ( ctab1 b ctab0  base0)
    (setq base0 base)
    (while base0
     (setq b (car base0))
     (setq base0 (cdr base0))
     (setq ctab0
      (mapcar
       (lambda (e1)
	(let (ans)
	 (if (not (listp e1)) (setq e1 (list e1)))
	 (setq ans (mapcar (lambda (e) (sym-concat b e)) e1))
	 (setq ans (solution ans))
	 ans
	)
       )
       endings
      )
     )
     (setq ctab0 (vconcat ctab0)) ; make an array
     (setq ctab1 (if ctab1 (join-arrays ctab1 ctab0) ctab0))
    )
    (when (not (equal ctab ctab1))
     (when (not (member tense '(lu~N4)))
      (fol-msg (format "BASE NOT USED (%s): %s %s %s %s %s\n"
		      tense dhaatu class pada upasargas voice))
     )
     (setq ans ctab)
    )
    (when (and (equal ctab ctab1) (member tense '(lu~N4)))
     (fol-msg (format "BASE USED: (%s): %s %s %s %s %s\n"
		      tense dhaatu class pada upasargas voice))
    )
   )
  )
  ans
 )
)
(defun construct-conjtab-base1a (tense dhaatu class pada upasargas voice &optional dbg)
 (let (keyval)
  
  (setq keyval
    (construct-conjbase1a dhaatu class pada upasargas voice dbg)
   )
  (when keyval
   (when (equal tense 'la~N)
    (fol-msg (format "construct-conjtab-base1a has an error\n"))
    ; error is when initial value of a key is a vowel.
    (setq keyval
     (mapcar
      (lambda (x) (sym-concat 'a x))
      keyval 
     )
    )
   )
   (setq keyval (list 'BASE keyval))
  )
 )
)
(defun construct-conjtab1a (dhaatu class pada upasargas tense 
   &optional voice derived-type &optional dbg)
 (let ()
  (when dbg
   (fol-msg (format "construct-conjtab1a %s\n" (list dhaatu class pada upasargas tense 
    voice derived-type)))
  )
  (cond
   ((equal derived-type 'CAUSAL)
    (causal-conjtab1a dhaatu class pada upasargas tense voice dbg)
   )
   ((member tense '(laT la~N loT vidhili~N))
    (construct-conjtab1a-spcltense dhaatu class pada upasargas tense voice dbg)
   )
   ((member tense '(liT-p liT-r lRiT lRi~N luT aashiirli~N
		    lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7))
    (construct-conjtab1a-gentense dhaatu class pada upasargas tense voice dbg)
   )
   (t
    nil
   )
  )
 )
)

(defun construct-conjtab1a-spcltense (dhaatu class pada upasargas tense voice &optional dbg)
 (let (bases b ctabs ctab)
  (setq bases (construct-conjbase1a dhaatu class pada upasargas voice dbg))
  (when dbg
   (fol-msg (format " construct-conjtab1a-spcltense %s\nbase=%s\n" (list dhaatu class pada upasargas tense voice ) bases))
  )
  (if (not (listp bases)) (setq bases (list bases)))
  (while bases
   (setq b (car bases))
   (setq bases (cdr bases))
   (if (arrayp b) (setq b (sym-without-space b)))
   (setq ctab
    (conjugation-tab b tense class pada dhaatu voice dbg))
;   (setq ctabs (append-if-new ctabs ctab))
   (if ctab (setq ctabs (construct-join-arrays ctabs ctab)))
  )
  ctabs
 )
)
(defun construct-conjtab1a-gentense (dhaatu class pada upasargas tense voice &optional dbg)
  (when dbg
   (fol-msg (format "construct-conjtab1a-gentense %s\n" (list dhaatu class pada upasargas tense voice )))
  )
 (let (ctabs lc lcnum)
  (setq lc (substring (symbol-name tense) -1)) ; last letter, as string
  (setq lcnum (string-to-number lc)) ; 0 except for lu~N1..7
  (cond
   ((equal lc "p") ; periphrastic perfect
    (if (periphrastic-liT-P dhaatu class dbg)
     (setq ctabs
      (conjugation-tab-liT-p upasargas class pada dhaatu voice dbg))
    )
   )
   ((equal lc "r") ; reduplicative perfect
    (if (reduplicative-liT-P dhaatu class)
     (setq ctabs
	 (conjugation-tab-liT-r upasargas class pada dhaatu voice dbg))
     )
    )
    ((member lcnum '(1 2 3 4 5 6 7)) ; aorist variety
     (setq ctabs
      (conjugation-tab-aorist upasargas class pada dhaatu lcnum voice dbg))
    )
    ((equal tense 'lRiT)
     (setq ctabs
       (conjugation-tab-lRiT upasargas class pada dhaatu voice dbg))
    )
    ((equal tense 'lRi~N)
       (setq ctabs
	(conjugation-tab-lRi~N
	 upasargas class pada dhaatu voice dbg))
      )
      ((equal tense 'luT)
       (setq ctabs
	(conjugation-tab-luT
	 upasargas class pada dhaatu voice dbg))
      )
      ((equal tense 'aashiirli~N)
       (setq ctabs
	(conjugation-tab-aashiirli~N
	 upasargas class pada dhaatu voice dbg))
      )
     )
  ctabs
 )
)
(defun construct-conjbase1a (dhaatu class pada upasargas &optional voice dbg)
 (let (base)
  (when dbg
   (fol-msg (format " construct-conjbase1a %s\n" (list dhaatu class pada upasargas voice )))
  )
  (cond
   ((equal voice 'PASSIVE)
    (setq base (construct-conjpassbase1a dhaatu class pada upasargas dbg))
    (if (not (listp base)) (setq base (list base)))
   )
   (t
    (cond
     ((member class '(1 4 6 10))
      (setq base (class-a-base dhaatu class pada dbg))
      (setq base (mapcar 'sym-without-space base))
     )
     ((member class '(2))
      (setq base (list upasargas)) ; ??? 06-10-04 What weirdness is this?
     )
     (t
      (setq base (class-b-base dhaatu class pada upasargas dbg))
      (setq base (mapcar 'sym-without-space base))
     )
    )
   )
  )
  base
 )
)


(defun construct-causalconjtabs (formtab outtab &optional outdir)
 ; 1-30-04. Input transliteration is SLP. Also construct output in SLP form.
 ; 3-23-04. 1st arg is now a text file name. 
 ;  Each of its lines is a list, (dhaatu class pada dict)
 (let (keys fileout bufout nform ok bufin more)
  (if (not outdir) (setq outdir "construct"))
  (setq ok t)
  (when ok
   (setq fileout (sangram-filename outtab outdir))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (with-current-buffer bufout
    (erase-buffer)
   )
  )
  (setq bufin (find-file-noselect (sangram-filename formtab outdir) 't))
  (setq nform 0)
  (with-current-buffer bufin
   (setq more t)
   (goto-char 1)
   (while (and more (< (point) (point-max)))
    (let (form keyval key val dcpu dhaatu class pada upasargas
	      SLdhaatu dict tmp)
     (setq tmp (read (current-buffer)))
     (forward-line)
     (setq SLdhaatu (elt tmp 0))
     (setq class (elt tmp 1))
     (setq pada (elt tmp 2))
     (setq dict (elt tmp 3))
     (setq nform (1+ nform))
     (message (format "%s %s" nform SLdhaatu))
     (setq dhaatu (translate-SLP1-ITRANS SLdhaatu))
     (mapcar
      (lambda (tense)
       (construct-conjtab-helper bufout tense dhaatu class pada upasargas dict)
      )
      all-tenses
     )
    )
   )
  )
  ;post-processing
  (when ok
   (with-current-buffer bufout
    (save-buffer 0)
    (kill-buffer nil)
   )
  )
  ok
 )
)
(defun causal-conjtab1a (dhaatu class pada upasargas tense 
   &optional voice dbg)
 (let (ans)
  (setq ans (cond
   ((member tense '(laT la~N loT vidhili~N))
    (causal-conjtab1a-spcltense dhaatu class pada upasargas tense voice)
   )
   ((member tense '(liT-p liT-r lRiT lRi~N luT aashiirli~N
		    lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7))
    (causal-conjtab1a-gentense dhaatu class pada upasargas tense voice)
   )
   (t
    nil
   )
  ))
  (setq ans (solution ans))
 ; check
 ; (causal-conjtab1a-chk dhaatu class pada upasargas tense voice ans)
 )
)
(defun causal-conjtab1a-chk (dhaatu class pada upasargas tense voice ans)
  (let (bases base i n ok bname ename lenbname enames es suffixes sfx
	      thissfx)
   (setq bases (causal-conjtab1a-bases
    dhaatu class pada upasargas tense voice))
   (when (and ans (not bases))
    (fol-msg (format "no base : %s %s %s %s %s %s \n"
		   dhaatu class pada upasargas tense voice ))
   )
   (when bases
    (setq base (elt bases 0)) ; bases, if non-nil, is a singleton
    (setq bname (symbol-name base))
    (setq lenbname (length bname))
    (setq n (length ans))
    (setq suffixes (make-vector n nil))
    (setq i 0)
    (setq ok t)
    (while (and ok (< i n))
     (setq es (elt ans i))
     (if (not (listp es)) (setq es (list es)))
;     (fol-msg (format "chk: (elt ans %s) = %s\n" i es))
     (setq thissfx nil)
     (while (and ok es)
      (setq e (car es))
      (setq es (cdr es))
      (when (not (symbolp e))
       (fol-msg (format "not a symbol: %s\n" e))
       (setq e 'xxx)
      )
      (setq ename (symbol-name e))
      (when (or (<= (length ename) lenbname)
	       (not (equal bname (substring ename 0 lenbname)))
	   )
       (fol-msg (format "err : %s %s %s %s %s %s -> %s != %s\n"
		   dhaatu class pada upasargas tense voice ename bname))
       (setq ok nil)
      )
      (when ok
       (setq sfx (substring ename lenbname))
       (setq thissfx (append thissfx (list (intern sfx))))
      )
     )
     (when ok
      (setq thissfx (solution thissfx))
      (aset suffixes i thissfx)
     )
     (setq i (1+ i))
    )
    (when ok
     (let (p)
      (setq p (if (equal voice 'PASSIVE) 'PA pada))
      (sanput2 'TEMPENDINGS (list tense p) suffixes)
;      (fol-msg (format "(%s %s %s %s)\n" tense voice pada suffixes))
     )
    )
   )
  )
  t
)
(defun causal-conjtab1a-spcltense (dhaatu class pada upasargas tense voice)
 (let (bases b ctabs ctab nilpada causalclass)
  (setq bases (causal-base1b dhaatu class nilpada upasargas voice));symbols
  (if (not (listp bases)) (setq bases (list bases)))
  (setq causalclass 10)
  (while bases
   (setq b (car bases))
   (setq bases (cdr bases))
   (if (arrayp b) (setq b (sym-without-space b)))
   (setq ctab
    (conjugation-tab b tense causalclass pada dhaatu voice))
;   (setq ctabs (append-if-new ctabs ctab))
   (if ctab
    (setq ctabs (construct-join-arrays ctabs ctab))
   )
  )
  ctabs
 )
)
(defun causal-conjtab1a-bases (dhaatu class pada upasargas tense 
   &optional voice)
 (let (bases base ans)
  (setq bases
   (causal-conjtab1a-bases-a dhaatu class pada upasargas tense voice))
  (cond
   (t (setq ans bases)) ; 5-9-04
   ((equal (length bases) 1)
     (setq base (car bases))
   )
   ((equal (length bases) 2)
    (if (equal (elt bases 0) (sym-concat (elt bases 1) 'ay))
     (setq base (elt bases 1))
    )
   )
  )
  (if base (setq ans (list base)))
  ans
 )
)
(defun causal-conjtab1a-gentense (dhaatu class pada upasargas tense voice)
 (let (ctabs ctab lc lcnum bases base nilpada causalclass thefunc base1)
  (if (not (member voice '(ACTIVE PASSIVE)))
    (setq voice 'ACTIVE)
  )
  (setq lc (substring (symbol-name tense) -1)) ; last letter, as string
  (setq lcnum (string-to-number lc)) ; 0 except for lu~N1..7
  (setq thefunc (intern-soft (format "conjugation-tab-%s" tense)))
  (setq bases (causal-bases-gentense
    dhaatu class pada upasargas tense voice))
  (when nil
   (fol-msg (format "bases=%s\n" bases))
  )
  ; now, get the conj tables
  (when (member tense '(liT-p luT lRiT lRi~N aashiirli~N lu~N3))
   (setq causalclass 11)
   (setq ctabs nil)
   
   (while bases
    (setq base (car bases))
    (setq bases (cdr bases))
;    (fol-msg (format "base=%s\n" base))
    (if (equal tense 'lu~N3)
     (setq ctab
      (conjugation-tab-aorist upasargas causalclass pada 
	     dhaatu lcnum  voice)
     )
     (setq ctab
      (funcall thefunc upasargas causalclass pada base voice)
     )
    )
    (when nil
     (fol-msg (format "base=%s => ctab=%s\n" base ctab))
    )
    (setq ctabs (construct-join-arrays ctabs ctab))
   )
  )
  ctabs
 )
)
(defun causal-conjtab1a-bases-a (dhaatu class pada upasargas tense 
   &optional voice)
 (let (bases)
  (setq bases
      (cond
       ((member tense '(laT la~N loT vidhili~N))
        (setq bases
	 (causal-base1b dhaatu class pada upasargas voice)
	)
       )
       (t
	(setq bases
	 (causal-bases-gentense dhaatu class pada upasargas tense voice)
	)
       )
      )
  )
  ; use prefix 'a' for those tenses that still require it
  ; Note the 'lu~N3' already has it.
  (when (member tense '(la~N lRi~N))
   (setq bases
    (mapcar
     (lambda (base) (sym-concat 'a base))
     bases
    )
   )
  )
  bases
 )
)
(defun causal-bases-gentense (dhaatu class pada upasargas tense voice)
 (let (ctabs ctab lc lcnum bases base nilpada causalclass thefunc bases1)
  (setq causalclass 11)
  (if (not (member voice '(ACTIVE PASSIVE)))
    (setq voice 'ACTIVE)
  )
  (setq lc (substring (symbol-name tense) -1)) ; last letter, as string
  (setq lcnum (string-to-number lc)) ; 0 except for lu~N1..7
  (cond
   ((let (tok lc)
     ; return nil for 'lu~N3 when root ends in vowel. Logic is known
     ; to be faulty.
     (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
     (setq lc (elt (substring tok -1) 0))
     ; when vowel, this step succeeds without assigning 'bases',
     ; so bases is nil
     (if (and (vowel-P lc) (equal tense 'lu~N3)) t nil) 
    )
   )  
   ((and (equal voice 'ACTIVE) (equal tense 'lu~N3))
    (let (toks)
     (setq toks (aorist-causal-base dhaatu class pada upasargas nil))
     (setq bases
      (mapcar
       (lambda (tok)
	(setq base (sym-without-space (vconcat [a] tok)))
       )
       toks
      )
     )
    )
   )
   ((and (equal voice 'PASSIVE) (equal tense 'lu~N3))
    (let (ctab ctab3s tok)
     (setq ctab
      (conjugation-tab-aorist upasargas causalclass pada 
	     dhaatu 3  voice)
     )
     (setq ctab3s (elt ctab 0))
     (when (listp ctab3s)
      ;(fol-msg (format "warning: dropping extra lu~N3: %s %s -> %s\n" dhaatu pada ctab3s))
      (setq ctab3s (car ctab3s))
     )
     (setq tok (car (ITRANS-parse-words-1 (symbol-name ctab3s))))
     ; drop the 'i'
     (setq tok (substring tok 0 -1))
     (setq base (sym-without-space tok))
     (setq bases (list base))
    )
   )
   ((equal voice 'ACTIVE)
    (setq bases (causal-base1b dhaatu class nilpada upasargas voice))
   )
   ((equal voice 'PASSIVE)
    (cond
     ((equal tense 'liT-p)
      (setq bases (causal-base1b dhaatu class nilpada upasargas 'ACTIVE))
     )
     ((member tense '(luT lRiT lRi~N))
      (setq bases (causal-base1b dhaatu class nilpada upasargas 'ACTIVE))
     )
     ((equal tense 'aashiirli~N)
      (setq bases (causal-base1b dhaatu class nilpada upasargas 'ACTIVE))
;      (fol-msg (format "chk: bases=%s\n" bases))
     )
     (t
      (setq bases (causal-base1b dhaatu class nilpada upasargas voice))
     )
    )
   )
  )
  (when nil
   (fol-msg (format " from causal-base1b, bases=%s\n" bases))
  )
  ; In some cases, Kale shows an alternate form with the causal 'ay'
  ; dropped
  (cond
   ((equal voice 'PASSIVE)
    (when (or (equal tense 'luT) ; periphrastic future
	      (equal tense 'lRiT) ; simple future
	      (equal tense 'lRi~N) ; conditional
	      (equal tense 'aashiirli~N) ; benedictive
	   )
     (setq bases1 bases)
     (setq bases nil)
     (while bases1
      (setq base (car bases1))
      (setq bases1 (cdr bases1))
      (setq bases (append-if-new bases base))
      (let (tok tok1)
       (setq tok (car (ITRANS-parse-words-1 (symbol-name base))))
       (setq tok1 (substring tok 0 -2))
       (setq base (sym-without-space tok1))
      )
      (setq bases (append-if-new bases base))
     )
    )
   )
   ((equal voice 'ACTIVE)
    (when (and (equal tense 'aashiirli~N) ; benedictive
	       (equal pada 'P)
	  )
     ; drop the 'ay' in this case
     (setq bases1 bases)
     (setq bases nil)
     (while bases1
      (setq base (car bases1))
      (setq bases1 (cdr bases1))
      (let (tok tok1)
       (setq tok (car (ITRANS-parse-words-1 (symbol-name base))))
       (setq tok1 (substring tok 0 -2))
       (setq base (sym-without-space tok1))
      )
      (setq bases (append-if-new bases base))
     )
    )
   )
  )
  bases
 )
)
(defun causal-base1b (dhaatu class pada upasargas voice)
 (let (toks b ans)
  (setq toks (causal-base dhaatu class pada upasargas))
  (when (equal voice 'PASSIVE)
   (setq toks 
    (mapcar
     (lambda (tok) (vconcat (substring tok 0 -2) [y]))
     toks
    )
   )
  )
  (mapcar 'sym-without-space toks)
 )
)

(defun construct-seT-code1a (dhaatu class pada upasargas &optional dtype dbg)
; (fol-msg (format "construct-seT-code1a: %s\n" dhaatu))
; This is from Antoine2#134. Compare Kale#458
; (1) First general rule.
;   The following classes of roots are 'seT' (insert 'i'):
;     All roots of the 10th conjugation and
;     all roots of the derivative conjugations (#144),
;     all roots ending with consonants,
;     all roots ending with the long vowel 'RI'
;     all roots ending with the long vowel 'uu'
;  Kale #458. (a): Roots of more than one syllable.
; (2) Second general rule.
;     The following classes of roots are 'aniT' (do not insert 'i'):
;     All roots of the first 9 conjugations ending in vowels other
;      than the long vowels 'RI' or 'uu'.
; The following list of 102 roots comprise exceptions to (1),
; in that they are 'aniT' (do not insert 'i')
; (3) Third general rule.
;     The following roots are 'veT' (optionally insert 'i'):
;  NOTE: Kale has a much longer list (see below)
; Based on Kale (p. 296-7), the following roots are 'veT'
; Here is the verse from Kale:
; svaratiH suuyate suute pa~nchame navame cha dhu~n .
; tanaktirvRishchatishchaantaavanaktishcha tanaktinaa .. 1 ..
; maarShTi maarjati anteShu daantau klidyatisyandate .
; radhyatiH sedhatirdhaantau paantaaH pa~nchaiva kalpate .. 2 ..
; gopaayatistRipyatishcha trapate dRipyatistathaa .
; maantau kShaamyatiH kShamate.ashnute klishnaati nashyati .. 3 ..
; shaantaasrayothaakShatishcha niShkuShNaatishcha takShatiH .
; tvakShatishcha Shakaaraantaa hyatha haantaashcha gaahate .. 4 ..
; padadvaye guuhatishcha Rikaaroopaantyagarhate .
; tRihatitRiMhatidruhyatayo vRihatimuhyatii .. 5 ..
; vRihistRihii snihyasnuhyaavete veTkaa hi dhaatavaH .
; ajantaanaaM tu thalyeve ved syaadanyatra sarvadaa .. 6 ..
; NOTE: These couplets ass well as those bearing on the 2nd variety
; of the Aorist are composed by Mr. ChintaamaNa Aatmaarama Kelkar, the
; present learned Shaastri at the Poona Training College.

; ADDENDA: Some changes:
;  vah:  from aniT to veT (ref Whitney, example Scharf)
; shii: removed from 'seT-exceptions', put in 'veT-exceptions' (Whitney)
 (let (ans seT-exceptions aniT-exceptions veT-exceptions)
 (when dbg
  (fol-msg (format "construct-seT-code1a %s\n" (list dhaatu class pada upasargas  dtype)))
 )
  (setq seT-exceptions '(daridraa shri shvi Dii yu ru nu snu kShu
	                 kShNu uurNu jaagRi vRi))
  (setq aniT-exceptions '(shak pach much rich vach vich sich prachCh tyaj
			  nij bhaj bha~nj bhuj bhrasj masj muj yaj yuj
			  ra~nj ruj vij sRij sa~nj sva~nj ad kShud khid
			  Chid tud nud pad bhid vid shad sad skand svid
			  krudh kShudh budh bandh yudh ; radh
			  rudh vyadh
			  shudh saadh sidh man han aap kShip tap ; trap
			  dRip lip lup vap shap sRip svap yabh labh rabh
			  gam nam yam ram krush daMsh dish dRish mRish
			  rish rush lish vish spRish kRiSh tuSh tviSh
			  duSh dviSh piSh puSh mRiSh viSh shiSh shuSh
			  shliSh ghas vas dah dih duh nah ruh lih  mih))
  (setq veT-exceptions '(radh muh druh snih nash lubh sah kRit nRit prii dhRi
			 kLip syand tRip klid klish kSham gaah gup guh takSh
			 trap tRih mRij vrashch svRi kuSh gRih stRih bRih
			 vRih
			 vah shii))
  (cond
   ((equal class 10) (setq ans 'seT))
;   ((equal class 11) (setq ans 'seT))
   ((equal dtype 'c)  (setq ans 'seT)) ; causal
   ((member dhaatu seT-exceptions) (setq ans 'seT))
   ((member dhaatu aniT-exceptions) (setq ans 'aniT))
   ((member dhaatu veT-exceptions) (setq ans 'veT))
   (t
    (let (tok lc ans0 types)
      (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
      (setq lc (elt (substring tok -1) 0)) ; last character
      (let (wparts parts)
	(setq wparts (word-parts tok))
	(setq parts (elt wparts 0))
	(setq types (elt wparts 1))
	)
      (cond
       ((consonant-P lc) (setq ans0 'seT))
       ((member lc '(RI uu)) (setq ans0 'seT))
       ((> (length types) 3) ; word of more than 1 syllable
	(setq ans0 'seT)
	)
       (t (setq ans0 'aniT))
       )
      (setq ans ans0)
      )
    )
  )
  (when dbg ; dbg
   (fol-msg (format "construct-seT-code1a : %s %s %s %s %s -> %s\n"
	     dhaatu class pada upasargas dtype ans))
  )
  (when nil
   (fol-msg (format "construct-seT-code1a %s => %s\n" (list dhaatu class pada upasargas dtype) ans))
  )
  ans
 )
)
(defun construct-seTPERF-code1a (dhaatu class pada upasargas &optional dbg)
; it is assumed that the 8 exceptional roots (kRi ... shru) are
; handled elsewhere. This routine does not check for them
; Kale #495. Compare Antoine2#109
; This pertains to the perfect tense.
; Special rules about the admission of the intermediate 'i' before
; the terminations 'va ma tha se vahe mahe dhve'
; (a) All roots, whether 'seT' or 'aniT', admit 'i' except:
;     kRi sRi bhRi vRi
;     stu dru sru shru
;  The eight roots just mentioned are aniT. 
;  In addition to this general rule, there are some modifications
;  applicable only to the ending 'tha'.
;  Before 'tha':
;      the root 'kRi' (only with prefix 'sam') admits 'i'.
;      the root 'vRi' admits 'i'.
;      aniT roots ending in 'Ri', except the root 'Ri', reject 'i'
;      aniT roots with a final vowel other that 'Ri' admit 'i' optionally
;      aniT roots with a penultimate 'a' admit 'i' optionally
;      the root 'Ri' admits 'i' necessarily (Kale #515)
;kRi : aniT seT sam
; the case  'sam-kRi' is handled in function 'conjugation-tab-liT'
; ADDENDA: 
; 1.  based on example on p. 314,
;  the aniT root'sRij' admits 'i' optionally before 
; 'th' (otherwise, it would be required)
; Also, it is treated as seT before other consonants
; 2. Kale p. 318
; In the perfect, 'shvi' is optionally considered as 'shu'.
; 'shvi' is considered 'seT' before 'tha' and other consonants.
; Based on the table in Kale, this is also the way 'shu' is
; considered.  Otherwise, 'shu' was 'seT veT' for perfect
; Kale #508, p. 319. The roots 'svRi', 'suu' amd 'dhuu' admit
; 'i' necessarily before consonantal terminations except 'tha';
; before 'tha' they admit 'i' optionally.
; Note: Kale has these roots mentioned in the general category
; of 'veT' roots. However, in my general classification of seT-code,
; 'dhuu' and 'suu' show as 'seT', and 'svRi' shows as 'aniT'
; Kale #508, p. 319
; My algorithm gives different results for the following verbs. 
; So I explicitly state the answer (based on Kale)
; All these are classified as general 'veT' by Kale, but this
; often disagrees with my classification
; 'ta~nj' is stated to be 'same as ta~nch' in Kale dhaatukosha.
; Thus, I do not include it
; the case  'nir-kuSh' is handled in function 'conjugation-tab-liT'
;nir kuSh : veT veT  Handle
; Kale #515. 'ad', 'Ri', and 'vye' admit 'i' necessarily before 'tha'
; Since 'ghas' is a substitute for 'ad' in the Perfect, it too 
; admits 'i' necessarily before 'tha'
; Kale #512, p. 325

 (let (tok n lc ans0 ans seT-codes ans1 ans2 exception-plist)
  (when dbg
   (fol-msg (format "construct-seTPERF-code1a %s\n" (list dhaatu class pada upasargas)))
  )
  ; ans1 pertains to all endings other than 'tha'
  ; ans2 pertains to the ending 'tha'
  (setq exception-plist '(
     kRi (aniT aniT)
     sRi (aniT aniT)
     bhRi (aniT aniT)
     vRi (aniT seT)
     stu (aniT aniT)
     dru (aniT aniT)
     sru (aniT aniT)
     shru (aniT aniT)
     sRij (seT veT)
     shu (seT seT)
     svRi (seT veT)
     dhuu (seT veT)
     suu (seT veT)
     ta~nch (veT seT)
     vrashch (veT veT)
     mRij (veT veT)
     a~nj (veT veT)
     klid (veT veT)
     syand (veT veT)
     radh (veT veT)
     jabh (veT veT)
     sidh (veT veT)
     kLip (veT veT)
     tRip (veT veT)
     dRip (veT veT)
     trap (veT veT)
     kSham (veT veT)
     ash (veT veT)
     klish (veT veT)
     nash (veT veT)
     akSh (veT veT)
     tvakSh (veT veT)
     takSh (veT veT)
     gaah (veT veT)
     gRih (veT veT)
     guh (veT veT)
     tRih (veT veT)
     tRiMh (veT veT)
     druh (veT veT)
     muh (veT veT)
     stRih (veT veT)
     vRih (veT veT)
     snih (veT veT)
     snuh (veT veT)
     ad (seT seT)
     ghas (seT seT)
     vye (seT seT)
     Ri (seT seT)
     phal (seT seT)
  ))
  (setq ans1 'seT)
  (setq ans2 'seT) ; some overrides to this below
;  (setq seT-codes (sanget2 dhaatu '(dhaatu seT-code)))
  (setq seT-codes (construct-seT-code1a dhaatu class pada upasargas nil dbg))
  (setq seT-codes (solution seT-codes))
  (if (not (listp seT-codes)) (setq seT-codes (list seT-codes)))
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0)) ; last character
  (setq n (length tok))
  ;(fol-msg (format "check: sew_codes=%s, tok=%s,lc=%s,n=%s\n" seT-codes tok lc n))
  (cond
   ((and (member 'aniT seT-codes) (equal lc 'Ri) (not (equal dhaatu 'Ri)))
    (setq ans2 'aniT)
   )
   ((and (member 'aniT seT-codes) (vowel-P lc))
    (setq ans2 'veT)
   )
   ((and (member 'aniT seT-codes)
	 (< 1 n)
	 (equal (elt (substring tok -2) 0) 'a))
    (setq ans2 'veT)
    ;(fol-msg (format "check: ans2=%s\n" ans2))
   )
   ; 08-02-03 : ends with : consonant(s) + 'a' +  Compound final consonant
   ((and (member 'aniT seT-codes)
	 (< 3 n)
	 (consonant-P (elt (substring tok -1) 0))
	 (consonant-P (elt (substring tok -2) 0))
	 (equal (elt (substring tok -3) 0) 'a)
	 (consonant-P (elt (substring tok -4) 0)))
    (setq ans2 'veT)
;    (fol-msg (format "check cpd consants: %s\n" dhaatu))
   )
  )
  (setq ans (list ans1 ans2))
  ;(fol-msg (format "check: ans=%s\n" ans))
  (let (excep)
   (setq excep (plist-get exception-plist dhaatu))
   (when excep (setq ans excep))
  )
  ans
 )
)
(defun construct-conjpassbase1a (dhaatu class pada upasargas &optional dbg)
; Kale 591.
; (a) 'ya' is added to the root, which is weak, i.e., no
;    guna or vrddhi substitute takes place before it
; (b) Before 'ya', roots undergo the same changes as they
;    do before the P terminations of the Benedictive
;   NOTE: There are some exceptions to this
;   - han : in benedictive, base is 'vagh', but passive base is 'hany'
; (c) The final 'aa' (original or substituted) changes to 'ii' in cases:
;     'daa' (to give) (class = 1,3)
;     'de do dhaa dhe maa gai so'
;     'paa' (to drink) (class = 1)
;     'haa' (to abandon) (class = 3, pada=P)
; Kale 593 
; (a) The roots 'khan jan tan san' optionally drop their 
;   'n' and at the same time lengthen their 'a'
; (b) 'shii' (to lie down) -> shayy
;     'shvi' -> shuuy
; (c) 'uuh' shortens its 'uu' when a preposition is prefixed to it
; (d) 'bruu' uses the passive of 'vach'
;     'as' (class=2)uses the passive of 'bhuu'
;     'ghas' uses the passive of 'ad'
;     'aj' uses the passive of 'vii'
; ADDENDA:
;  'sthaa' -> 'sthiiy'
;  'jyaa' -> 'jiiy'
;  'pai' -> paay
;  'pan' -> pany, panaayy
;  'paN' -> paNy, paNaayy
;  'gup' -> gupy gopy gopaayy
;  'kam' -> kam, kaamy
;  'Rit' -> Rity, Ritiiy
;  'vichCh' -> vichChy, vichChaayy
;  'div' -> 'diivy'
;  'han' -> 'hany'
; based on Antoine2 (appendix class 10),
; class 10 usually forms its passive base
; by (a) forming the class10 conjugational base,
;    (b) changing the ending 'ay' to 'y'
 (let (ans tok base)
  (when dbg
   (fol-msg (format "construct-conjpassbase1a %s\n" (list dhaatu class pada upasargas )))
  )

  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (cond
   ((member dhaatu '(khan jan tan san))
    (setq base (list
     tok
     (vconcat (substring tok 0 -2) [aa])
    ))
   )
   ((equal dhaatu 'shii) (setq base [sh a y]))
   ((equal dhaatu 'shvi) (setq base [sh uu]))
   ((and (equal dhaatu 'uuh) upasargas) (setq base [u h]))
   ((equal dhaatu 'bruu) (setq base [u ch]))
   ((and (equal dhaatu 'as) (equal class 2)) (setq base [bh uu]))
   ((equal dhaatu 'ghas) (setq base [a d]))
   ((equal dhaatu 'aj) (setq base [v ii]))
   ((equal dhaatu 'jyaa) (setq base [j ii]))
   ((equal dhaatu 'sthaa) (setq base [s th ii]))
   ((equal dhaatu 'pai) (setq base [p aa]))
   ((equal dhaatu 'pan) (setq base '([p a n] [p a n aa y])))
   ((equal dhaatu 'paN) (setq base '([p a N] [p a N aa y])))
   ((equal dhaatu 'gup) (setq base '([g u p] [g o p] [g o p aa y])))
   ((equal dhaatu 'kam) (setq base '([k a m] [k aa m])))
   ((equal dhaatu 'Rit) (setq base '([Ri t] [Ri t ii])))
   ((equal dhaatu 'vichCh) (setq base '([v i ch Ch] [v i ch Ch aa y])))
   ((equal dhaatu 'div) (setq base [d ii v]))
   ((equal dhaatu 'han) (setq base [h a n]))
   ((equal class 10)
    (let (cb)
     (setq cb (construct-conjbase1a dhaatu class pada upasargas)) ; a list
     (setq base
      (mapcar
       (lambda (b) ; b is a symbol
        (setq b (car (ITRANS-parse-words-1 (symbol-name b))))
        (substring b 0 -2); drop the [a y]
       ) 
       cb
      )
     )
    )
   )
   ((or (member dhaatu '(de do dhaa dhe maa gai so))
	(and (equal dhaatu 'daa) (member class '(1 3)))
	(and (equal dhaatu 'paa) (equal class 1))
	(and (equal dhaatu 'haa) (equal class 3) (equal pada 'P))
    )
    (setq base (vconcat (substring tok 0 -1) [ii]))
   )
;    ((member (elt (substring tok -1) 0) '(aa))
;     ; benedictive logic does not always do this
;     (setq base (vconcat (substring tok 0 -1) [aa]))
;    )
   (t
    (setq base (benedictive-base dhaatu class 'P upasargas nil dbg))
    ; This will be a list of length 2 only in case of
    ;Kale 585. final 'aa' (original or substituted), if it be
    ; preceded by a conjunct consonant, is changed to 'e' optionally:
    ; In this case, we drop the 1st element (with the 'e')
    (when (and (listp base) (equal (length base) 2))
     (setq base (elt base 1))
    )
    ; some 'aa' words go to a single 'e'. We change these back
    (when (and (equal (substring tok -1) [aa])
 	       (arrayp base)
	       (equal (substring base -1) [e])
 	  )
     (setq base (vconcat (substring base 0 -1) [aa]))
    )
   )
  )
  (if (not (listp base)) (setq base (list base)))
  (setq ans
   (mapcar
    (lambda (b)
     (setq b (vconcat b [y]))
     (setq b (sym-without-space b))
    )
    base
   )
  )
  (setq ans (solution ans))
  ans
 )
)

(defun load-dhaatukosha (files) 
  (mapcar
   (lambda (x)
    (load-subfields x "construct")
   )
   files
  )
)

(defun Xconstruct (tabname dirname outtab outdir)
 (let (filename buf key val more dhaatus subkey nrec err Xval keys)
  (construct-cleartemp)
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (setq nrec 0)
   (while more
    (condition-case err
     (progn
      (setq key (read buf)) 
      (setq subkey (read buf)) 
      (setq val (read buf))
      (if (not (listp subkey)) (setq subkey (list subkey)))
      (setq Xval (Xalgorithm val))
      (sanAppendElt key 'temp (list subkey Xval))
      (setq keys (append-if-new keys key))
      (message (symbol-name key))
      (setq nrec (1+ nrec))
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
   )
   (kill-buffer nil)
  )
  (fol-msg (format "Xconstruct : %s %s %s\n"
		   tabname dirname nrec))
  ;post-processing
  (let (fileout bufout subkey key dhaatu val)
   (setq fileout (sangram-filename outtab outdir))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (fol-msg (format "#keys=%s\n" (length keys)))
   (with-current-buffer bufout
    (erase-buffer)
    (while keys
     (setq key (car keys))
     (setq keys (cdr keys))
     (let (data datum subkey1 form vals)
      (setq data (sanget key 'temp))
      (while data
       (setq datum (car data))
       (setq data (cdr data))
       (setq form (elt datum 0)) ; (dhaatu <tense>) 
       (setq vals (elt datum 1)) ; (X1 ... Xn)
       (while vals
	(setq val (car vals)) ; a symbol X1 
	(setq vals (cdr vals))
	(insert (format "%s %s %s\n" val form key))
       )
      )
     )
    )
    (save-buffer)
    (kill-buffer nil)
   )
  )
  t
 )
)

(defun Xalgorithm (xin)
 (let (ans1 ans xflat nflat nans)
  (setq xflat (flatten xin))
  (setq ans (maximal-prefix-sym xflat))
  (setq nflat (length xflat))
  (setq nans (length ans))
  (when (< (/ nflat 2) nans)
   ; just use everything
   (setq ans xflat)
  )
  ans
 )
)

(defun getdhaatu (dhaatu class pada upa-syms subkey)
 ;recursive routine
 (let (form ans pada1)
  (if (not (listp subkey)) (setq subkey (list subkey)))
  (setq form (dcpu-sym dhaatu class pada upa-syms))
  (setq ans (sanget2 form (cons 'dhaatu subkey)))
  (when (and (not ans) class pada)
   (when (not ans)
    (when upa-syms
     ; try with no upasargas
     (setq ans (getdhaatu dhaatu class pada nil subkey))
    )
    (when (not ans)
     (when pada
      ; try with no pada upasargas
      (setq ans (getdhaatu dhaatu class nil nil subkey))
     )
     (when (not ans)
     ; try with just the dhaatu
      (setq ans (getdhaatu dhaatu nil nil nil subkey))
     )
    )
   )
  )
  ans
 )
)
(defun getdhaatu-ver0 (dhaatu class pada upa-syms subkey)
 ;recursive routine
 (let (form ans pada1)
  (setq form (dcpu-sym dhaatu class pada upa-syms))
  (setq ans (sanget2 form (list 'dhaatu subkey)))
  (when (and (not ans) class pada)
   (setq pada1 (if (equal pada 'P) 'A 'P)) ; use other pada
   (setq form (dcpu-sym dhaatu class pada1 upa-syms))
   (setq ans (sanget2 form (list 'dhaatu subkey)))
   (when (not ans)
    (when upa-syms
     ; try with no upasargas
     (setq ans (getdhaatu dhaatu class pada nil subkey))
    )
    (when (not ans)
     ; try with just the dhaatu
     (setq ans (getdhaatu dhaatu nil nil nil subkey))
    )
   )
  )
  ans
 )
)
(defun get-seTPERF-code (dhaatu class pada upa-syms)
 (getdhaatu dhaatu class pada upa-syms 'seTPERF-code)
)
(defun get-seT-code (dhaatu class pada upa-syms)
 (getdhaatu dhaatu class pada upa-syms 'seT-code)
)

(defun sandefs (dhaatu)
 (let (forms form ans thisans)
  (setq forms (sanget2 dhaatu '(dhaatu forms)))
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq thisans (sanget2 form '(dhaatu Eng-def)))
   (setq thisans (solution thisans))
   (fol-msg (format "%s : %s\n" form thisans))
  )
  t
 )
)
(defun construct-init ()
 (fol-msg (format "BEGIN construct-init...\n"))
 (fol-msg (format "fol-ob-init\n"))
 (fol-ob-init)
; (parse_INIT)
 (fol-msg (format "init-transliteration\n"))
 (init-transliteration)
 (fol-msg (format "init-Sangram\n"))
 (init-Sangram)
 (fol-msg (format "init-sets\n"))
 (init-sets)
 (fol-msg (format "init-properties\n"))
 (init-properties)
 (fol-msg (format "init-vowelstrength\n"))
 (init-vowelstrength)
 (fol-msg (format "init-semivowels\n"))
 (init-semivowels)
 ; initialize Sandhi
 ;(fol-msg (format "\n"))
 ;(message (format "begin initialize sandhi"))
; (Sandhi-init) initializes from scratch
; (Sandhi-unload) creates the loadable file of next
 (fol-msg (format "Sandhi-load-init\n"))
 (Sandhi-load-init)
 ;(message (format "end initialize sandhi"))
 ; initialize endings
 (fol-msg (format "Subanta-initAll\n"))
 (Subanta-initAll)
 (fol-msg (format "init-sup\n"))
 (init-sup) ; nominal endings
 (fol-msg (format "init-vsup\n"))
 (init-vsup); verbal endings  laT, la~N , loTh , vidhili~N
; (load-library "C:/sanskrit/grammar/lisp/irreg")
 ;(fol-msg (format "construct-init: begin irreg load...\n"))
 ;(my-load-library "grammar/lisp/irreg") ; commented out 10-16-2015
 (fol-msg (format "irregs-init\n"))
 (irregs-init) ; added 10-16-2015
 (fol-msg (format "END construct-init\n"))
)

(defun explore-Xconjgentab (&optional dhaatus)
 (let (dhaatu ndif nform)
  (when (not dhaatus)
   (setq dhaatus (sanget2 'GLOBAL '(dhaatus)))
  )
  (setq ndif 0)
  (setq nform 0)
  (while dhaatus
   (let (dhaatu forms)
    (setq dhaatu (car dhaatus))
    (setq forms (sanget2 dhaatu '(dhaatu forms)))
    (while forms
     (let (form val form1 val1 l l1)
      (setq form (car forms))
      (setq nform (1+ nform))
      (setq val (Xsanget2 form '(dhaatu conjgentab)))
      (let (tmp dhaatu class pada)
       (setq tmp (sym-dcpu form))
       (setq form1 (dcpu-sym (elt tmp 0) (elt tmp 1) (elt tmp 2) nil))
      )
      (setq val1 (Xsanget2 form1 '(dhaatu conjgentab)))
      (when (not (equal val val1))
       (setq l (length val))
       (setq l1 (length val1))
       (fol-msg (format "%s %s %s %s\n" form form1 l l1))
       (setq ndif (1+ ndif))
      )
     )
     (setq forms (cdr forms))
    )
   )
   (setq dhaatus (cdr dhaatus))
  )
  (fol-msg (format "%s differences in %s forms\n" ndif nform))
 )
)
(defun explore-Xconjtab (&optional dhaatus)
 (let (dhaatu ndif nform)
  (when (not dhaatus)
   (setq dhaatus (sanget2 'GLOBAL '(dhaatus)))
  )
  (setq ndif 0)
  (setq nform 0)
  (while dhaatus
   (let (dhaatu forms)
    (setq dhaatu (car dhaatus))
    (setq forms (sanget2 dhaatu '(dhaatu forms)))
    (while forms
     (let (form val form1 val1 l l1)
      (setq form (car forms))
      (setq nform (1+ nform))
      (setq val (Xsanget2 form '(dhaatu conjtab)))
      (let (tmp dhaatu class pada)
       (setq tmp (sym-dcpu form))
       (setq form1 (dcpu-sym (elt tmp 0) (elt tmp 1) (elt tmp 2) nil))
      )
      (setq val1 (Xsanget2 form1 '(dhaatu conjtab)))
      (when (not (equal val val1))
       (setq l (length val))
       (setq l1 (length val1))
       (fol-msg (format "%s %s %s %s\n" form form1 l l1))
       (setq ndif (1+ ndif))
      )
     )
     (setq forms (cdr forms))
    )
   )
   (setq dhaatus (cdr dhaatus))
  )
  (fol-msg (format "%s differences in %s forms\n" ndif nform))
 )
)
(defun explore-dhaatu-tenses (&optional dhaatus)
 (let (dhaatu ndif nform all-tenses)
  (when (not dhaatus)
   (setq dhaatus (sanget2 'GLOBAL '(dhaatus)))
  )
  (setq all-tenses '(
   laT la~N loT vidhili~N
   liT-p liT-r lRiT lRi~N luT aashiirli~N
   lu~N1 lu~N2 lu~N3 lu~N4 lu~N5 lu~N6 lu~N7
  ))
  (setq ndif 0)
  (setq nform 0)
  (while dhaatus
   (let (dhaatu forms tenses tense)
    (setq dhaatu (car dhaatus))
    (setq forms (sanget2 dhaatu '(dhaatu forms)))
    (while forms
     (let (form val form1 val1 v v1)
      (setq form (car forms))
      (setq nform (1+ nform))
      (setq val (sanget2 form '(dhaatu)))
      (let (tmp dhaatu class pada)
       (setq tmp (sym-dcpu form))
       (setq form1 (dcpu-sym (elt tmp 0) (elt tmp 1) (elt tmp 2) nil))
      )
      (setq val1 (sanget2 form1 '(dhaatu)))
      (setq tenses all-tenses)
      (while tenses
       (setq tense (car tenses))
       (setq tenses (cdr tenses))
       (setq v (plist-get val tense))
       (setq v1 (plist-get val1 tense))
       (when (not (equal v v1))
	(let (c c1)
	 (setq c (if v t))
	 (setq c1 (if v1 t))
         (fol-msg (format "%s %s %s %s %s\n" form form1 tense c c1))
         (setq ndif (1+ ndif))
	)
       )
      )
     )
     (setq forms (cdr forms))
    )
   )
   (setq dhaatus (cdr dhaatus))
  )
  (fol-msg (format "%s differences in %s forms\n" ndif nform))
 )
)
(defvar participle-data
 '(
   prespart ; present active participle
   passpart ; present passive participle
   pppart   ; past passive participle
   ippart   ; indeclinable past passive participle
   inf      ; infinitive
   potpart  ; potential passive participle
   rppart   ; reduplicative perfect participle
   perppart ; periphrastic perfect participle
   futpart  ; future active participle
   futppart ; future passive participle
  )
)
(defvar participle-data-1
 '(
 PRESP PRAP ; prespart ; present participle active (SL= PRMP for pada=A)
 PASSP PRPP ; passpart ; present passive participle
 PPP   PPP  ; pppart   ; past passive participle
 PAP   PAP  ; papart   ; past active participle (add 'av' to pppart)
 IPP   IPP  ; ippart   ; indeclinable past passive participle (absolutive)
 IPPA  IPPA ; ? ipparta ; indeclinable past passive participle with prefixes
 INF   INF  ; inf      ; infinitive
 POTP  PPOT ; ? potpart  ; potential passive participle (gerundive)
 RPP   RPP  ; ? rppart   ; reduplicative perfect participle
 FUTP  FAP  ; ? FMP futpart  ; future active participle
 FUTPP FPP  ; futppart ; future passive participle
 )
)
(defun construct-participles-all (&optional outtab outdir)
 (let (forms workouttab)
  (setq forms (extract-dcpforms)) ; dhaatu-classPada
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "participles.txt"))
  (setq workouttab (concat "work-" outtab))
  (construct-participles forms workouttab outdir)
  (construct-participles-reformat outtab workouttab outdir)
 )
)
(defun construct-participles-reformat (outtab intab &optional outdir)
 "Modifies the file 'intab' constructed by 'construct-participles',
  changing it to 'outtab'. Both files are in directory 'outdir.'
 "
 (let (fileout bufout filein bufin more)
  (if (not outdir) (setq outdir "construct"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq filein (sangram-filename intab outdir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (let (more key root parm V class pada part B strength out parmout
	 partout dict)
    (setq more t)
    (while more
     (setq key (read (current-buffer)))
     (setq root (read (current-buffer)))
     (setq dict (read (current-buffer)))
     (setq parm (read (current-buffer)))
     (setq V (elt parm 0))
     (setq class (elt parm 1))
     (setq pada (elt parm 2))
     (setq part (elt parm 3))
     (setq partout (plist-get participle-data-1 part))
     (setq B (read (current-buffer)))
     (setq strength nil)
     (setq parmout (list root class pada partout))
     (cond
      ((member part '(IPP IPPA INF))
       ; indeclineables
       (setq out (format "%s = %s (I %s) B\n" key dict parmout))
      )
      ((and (equal part 'PRESP) (equal pada 'A))
       (setq out (format "%s =a %s (S ADJ a %s) B\n" key dict parmout))
      )
      ((and (equal part 'PRESP) (equal pada 'P))
       (setq strength (read (current-buffer)))
       (setq out (format "%s =t %s (S ADJ %s-%s %s) B\n"
			 key dict partout strength parmout))
      )
      ((member part '(PASSP PPP POTP FUTP FUTPP))
       (setq out (format "%s =a %s (S ADJ a %s) B\n" key dict parmout))
      )
      ((equal part 'PAP)
       (setq out (format "%s =at %s (S ADJ at %s) B\n"
			 key dict parmout))
      )
      ((equal part 'RPP)
      )
      (t (setq out nil)
      )
     )
     (if out (with-current-buffer bufout (insert out)))
     (forward-line)
     (setq more (and more (< (point) (point-max))))
    )
   )
  )
  (with-current-buffer bufout
   (save-buffer)
   ;(kill-buffer nil)
  )
 )
)
(defun construct-participles-reformat1 (outtab &optional outdir)
 "removes (partial) duplicate participle lines.
  The lines are sorted.
  Two sample lines:
    ARy =a <MW=aR,3169,1> (S ADJ a (aR 1 P PPOT)) B
    ARy =a <MW=aR,3169,1> (S ADJ a (aR 4 A PPOT)) B
  These are compared thru          ^
  Since identical, the first is kept. It is somewhat arbitrary
   which is kept.
 "
 (let (s fileout ndup bufout)
  (if (not outdir) (setq outdir "construct"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (setq ndup (remove-partial-duplicates "([^()]+("))  
   ;
   (save-buffer 0)
   (kill-buffer nil)
  )
  (fol-msg (format "%s duplicates removed from %s\n" ndup outtab))
 )
)
(defun construct-participles (formtab outtab &optional outdir)
; 3-23-04. 1st arg is now a text file name. See 'construct-conjtabs'
 (fol-msg (format "construct-participles: WARNING: 'perppart' not done\n"))
 (let (keys SL-dhaatu fileout bufout nform bufin more)
  (if (not outdir) (setq outdir "construct"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq bufin (find-file-noselect (sangram-filename formtab outdir) 't))
  (setq nform 0)
  (with-current-buffer bufin
   (setq more t)
   (goto-char 1)
   (while (and more (< (point) (point-max)))
    (let (form keyval key val tmp SL-dhaatu class pada dict tmp)
     (setq tmp (read (current-buffer)))
     (forward-line)
     (setq SL-dhaatu (elt tmp 0))
     (setq class (elt tmp 1))
     (setq pada (elt tmp 2))
     (setq dict (elt tmp 3))
     (setq nform (1+ nform))
     (message (format "%s %s" nform SL-dhaatu))
     (construct-participles1 bufout SL-dhaatu class pada dict)
    )
   )
  )
 
  ;post-processing
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-participles1 (bufout SL-dhaatu class pada &optional dict) 
 (condition-case err
  (construct-participles1-helper bufout SL-dhaatu class pada dict)
  (error
   (fol-msg (format "%s %s %s: error = %s\n" SL-dhaatu class pada err))
  )
 )
)
(defun construct-participles1-helper (bufout SL-dhaatu class pada 
       &optional dict)
 (let (key subkey keyval upasargas val base dhaatu)
  (setq dhaatu (translate-SLP1-ITRANS SL-dhaatu))
  (setq key dhaatu)
  ;1. present participle (base)
  ; when pada = P, val = ((<base-syms> <strength-sym>) or
  ;                         (<base-sym> <strength-sym>)
  ; when pada = A, val = (<base-syms>)
  (setq val (construct-prespart-base-alt dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'PRESP)) ;PRAP
  (if (equal pada 'P)
   (let (bases strength SL-key SL-base)
    (setq bases (elt val 0))
    (setq strength (elt val 1))
    (if (not (listp bases)) (setq bases (list bases)))
    (when (not bases)
     (fol-msg (format "prespart missing: %s %s %s\n" dhaatu class pada))
    )
    (setq SL-key (translate-ITRANS-SLP1 key))
    (mapcar
     (lambda (base)
      (setq SL-base (translate-ITRANS-SLP1 base))
      (with-current-buffer bufout
       (insert (format "%s %s %s %s %s %s\n"
		SL-base SL-key dict subkey 'B strength))
      )
     )
     bases
    )
   )
   ; pada = A
   (SL-construct-output-base bufout val key subkey dict)
  )
  ;2. passpart.
  ; val = (<base-syms>)
  (setq val (construct-passpart-base-alt dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'PASSP)) ;PRPP
  (SL-construct-output-base bufout val key subkey dict)
  ;3. pppart (past passive participle)
  ; 'construct-pppart1a' in 'kta.el'
  (setq val (construct-pppart1a dhaatu class pada upasargas))
  (setq val (sym-delete-last val)) ; remove trailing 'a'
  (setq subkey (list 'V class pada 'PPP)) ; PPP
  (SL-construct-output-base bufout val key subkey dict)
  ;4. papart (past active participle)
  ; add 'av' to pppart
  (setq val
   (mapcar (lambda (x) (sym-concat x 'av)) val)
  )
  (setq subkey (list 'V class pada 'PAP)) ;PAP
  (SL-construct-output-base bufout val key subkey dict)
  ;5. ippart (indeclineable past passive participle) 
  (setq val (construct-ippart1a-tvaa dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'IPP)) ;IPP (gerundive?)
  (SL-construct-output-base bufout val key subkey dict)
  ;6. ipparta (past passive participle with prefixes) 
  (setq val (construct-ippart1a-ya dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'IPPA))
  (SL-construct-output-base bufout val key subkey dict)
  ;7. inf (infinitive)
  ; function 'construct-inf1a' is in file 'kta.el'
  (setq val (construct-inf1a dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'INF)) ;INF
  (SL-construct-output-base bufout val key subkey dict)
  ;8. potpart (potential passive participle)
  ; function 'construct-potpart1a' is in file 'kta.el'
  (setq val (construct-potpart1a dhaatu class pada upasargas))
  (setq val (sym-delete-last val)) ; remove trailing 'a'
  (setq subkey (list 'V class pada 'POTP))  ;PPOT ??
  (SL-construct-output-base bufout val key subkey dict)
  ;9. rppart (reduplicate past participle).
  (setq val (construct-rppart1a dhaatu class pada upasargas))
  (if (equal pada 'P)
   ; val = ((<strong weak>))
   (while val ; many do not have
;    (fol-msg (format "%s %s %s -> %s\n" dhaatu class pada val))
    (setq subkey (list 'V class pada 'RPP))  ;RPP ?
    (let (basevals base SL-key SL-base SL-basevals)
     (setq basevals (car val))
     (setq val (cdr val))
     (setq base (Xalgorithm basevals))
     (setq base (solution base))
     (setq SL-key (translate-ITRANS-SLP1 key))
     (setq SL-base (translate-ITRANS-SLP1 base))
     (setq SL-basevals (translate-ITRANS-SLP1 basevals))
     (with-current-buffer bufout
       (insert (format "%s %s %s %s %s %s\n"
		       SL-base SL-key dict subkey 'B SL-basevals))
     )
    )
   )
   ; pada = A
   (when val (SL-construct-output-base bufout val key subkey dict))
  )
  ;10. futpart : future participle (active).
  ; function 'construct-futpart1a' is in file 'gram2-future.el'
  ; Only the 'base' is constructed. 
  (setq val (construct-futpart1a dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'FUTP))  ;FAP  (FMP)
  (SL-construct-output-base bufout val key subkey dict)
  ;11. futppart : future participle (passive).
  ; function 'construct-futppart1a' is in file 'gram2-future.el'
  ; Only the 'base' is constructed. 
  (setq val (construct-futppart1a dhaatu class pada upasargas))
  (setq subkey (list 'V class pada 'FUTPP)) ;FPP
  (SL-construct-output-base bufout val key subkey dict)
 )
)
(defun sym-delete-last (sym)
 (let (val ans)
  (setq val sym)
  (if (not (listp val)) (setq val (list val)))
  (setq ans
   (mapcar
    (lambda (x)
     ; remove last char
     (let (tok)
      (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
      (setq tok (substring tok 0 -1))
      (sym-without-space tok)
     )
    )
    val
   )
  )
;  (fol-msg (format "val=%s, ans=%s\n" val ans))
  ans
 )
)
(defun construct-prespart1a-alt (dhaatu class pada upasargas &optional dbg)
 (let (ans praatipadikas dtab gender genders thisans ps p dtabs)
  (when dbg
   (fol-msg (format "construct-prespart1a-alt %s\n" (list dhaatu class pada upasargas)))
  )
  (setq praatipadikas
	(construct-prespart-base-alt dhaatu class pada upasargas))
  (when dbg
   (fol-msg (format "construct-prespart1a-alt: praatipadikas=%s\n"
		    praatipadikas))
  )
  (setq genders '(M N F))
  (if (not praatipadikas) (setq genders nil))
  (while genders
   (setq gender (car genders))
   (setq genders (cdr genders))
   (setq ps praatipadikas)
   (setq dtabs nil)
   (while ps
    (setq p (car ps))
    (setq ps (cdr ps))
    (setq dtab (declension-pres-part-alt p gender class pada))
    (setq dtabs (construct-join-arrays dtabs dtab))
   )
   (setq dtabs (solution dtabs))
   (setq thisans (list gender dtabs))
   (setq ans (append ans thisans))
  )
  ans
 )
)
(defun declension-pres-part-alt (praatipadikas gender class pada)
 (if (equal pada 'P)
  (declension-pres-part-P-alt praatipadikas gender class)
  (declension-pres-part-A-alt praatipadikas gender class)
 )
)
(defun declension-pres-part-P-alt (praatipadikas gender &optional class)
 ; praatipadikas = (base <S/W/SW>)
 (let ( praatipadika ans1 ans endings weak strong code)
  (setq code (elt praatipadikas 1))
  (setq endings (plist-get2 PRESPART-endings (list 'P code gender )))
  (setq weak (elt praatipadikas 0))
  (when weak
   (setq ans
    (mapcar
     (lambda (e1)
      (if (not (listp e1)) (setq e1 (list e1)))
      (solution
       (mapcar
        (lambda (e) (sym-concat weak e))
	e1
       )
      )
     )
     endings
    )
   )
  )
  (when ans (setq ans (vconcat ans)))
  ans
 )
)
(defun declension-pres-part-A-alt (praatipadika gender &optional class)
 ; praatipadikas = base e.g, vartamaan
 ; for gender=M, the sups are sup-M-a
 ; for gender=N, the sups are sup-N-a
 ; for gender=F, the sups are sup-F-aa
 (let (ans1 ans endings)
  (setq endings (plist-get2 PRESPART-endings (list 'A gender)))
   (setq ans1 
    (mapcar
     (lambda (e)
      (sym-concat praatipadika e)
     )
     endings
    )
   )
   (setq ans1 (vconcat ans1)) ; make an array
   (if ans1
    (setq ans (construct-join-arrays ans ans1))
    (setq ans ans1)
   )
  ans
 )
)
(defun construct-prespart-base-alt (dhaatu class pada upasargas)
 (cond
  ((equal pada 'P)
   (construct-prespart-base-alt-P dhaatu class pada upasargas)
  )
  ((equal pada 'A)
   (construct-prespart-base-alt-A dhaatu class pada upasargas)
  )
  (t nil)
 )
)
(defun construct-prespart-base-alt-P (dhaatu class pada upasargas)
 (let (ans ctabs tense-sym voice-sym femtype ans1)
  (setq tense-sym 'laT)
  (setq voice-sym 'ACTIVE)
  (setq ctabs (construct-conjtab1a  dhaatu class pada upasargas tense-sym))
  (setq ctabs (solution ctabs))
  (if (not (listp ctabs)) (setq ctabs (list ctabs)))
  (let (i m ctab ctabelt)
   (when ctabs
    (setq ctab (elt ctabs 0))
    (setq m (length ctabs))
    (setq i 1)
    (while (< i m)
     (setq ctab (join-arrays ctab (elt ctabs i)))
     (setq i (1+ i))
    )
    (setq ctabelt (elt ctab 2))
    (setq ans1
      (construct-prespart-base-alt-P-1 ctabelt dhaatu class))
    (setq ans (append ans ans1))
   )
  )
  ans
 )
)
(defun construct-prespart-base-alt-P-1 (ctabelt dhaatu class)
; Note: AntoineI-#83 says
; (1) Verbs taking parasmaipada terminations form their present
;   participle active in 'at'. To form it, the termination
;   'anti' of the 3rd pers. plur of the present active is
;   replaced by 'at'.
; (2) Verbs taking atmanepada terminations form their present
;   participle active in 'maana'. To form it, the termination
;   'te' of the 3rd pers. sing. of the present tense is replaced
;   by 'maana'.
; (2) is correct only for the a-conjugations.
; For the non-a conjugations, the ending 'aana' replaces the
; termination 'ate' of the 3rd pers. PLURAL of the present tense.
; An equivalent alternate of (2) for the 'a' conjugations is
; to replace the termination 'nte' of the 3rd pers. plur.
; of the present tense with 'maana'.
; Since the 3rd pers PLURAL of the present tense can be made
; to work with either, and is required for non-a conjugations,
; that is chosen for both a and non-a conjugations
; This is consistent with Goldman (weak stem of non-a conjugations)
; and Kale (#669, p. 418) says to use 3P of present tense.
 (let (ans femtype c conjelts conjelt)
    (setq conjelts ctabelt) ; 3rd pers plur present tense for P/A
    (if (not (listp conjelts)) (setq conjelts (list conjelts)))
    (while conjelts
     (setq conjelt (car conjelts))
     (setq conjelts (cdr conjelts))
     (let (tok stok wtok s w strong weak )
      ; conjelt is 3rd pers PLUR of present active
      ; this construction follows Goldman (15.7)
      (setq tok (car (ITRANS-parse-words-1 (symbol-name conjelt))))
;      (fol-msg (format "chk: %s\n" tok))
      (when (equal (substring tok -2) [u H])
       ; this encountered with vid (class 2)
       ; one form of its 3P present tense is viduH
       ; I want to pretend this is vidanti,
       ; so remove the [u H] and replace with [a n t i]
       (setq tok (substring tok 0 -2)) ;
       (setq tok (vconcat tok [a n t i]))
;       (fol-msg (format "chk: (construct-prespart-base-alt-P-1): %s\n" tok))
      )
      (setq stok (substring tok 0 -1)) ; strong base (remove 'i')
      ; stok ends in either 'ant' or 'at' or
      ; 'aant' ('yaa' and similar class 2 ending in 'aa', but not 'daridraa')
      ; In the case 'ant' and 'aant', we remove the last 3 letters
      ; In the case 'at', we also remove the last 2 letters.
      (if (member (substring stok -3) '([a n t] [aa n t])) 
       (setq wtok (substring stok 0 -2)) ; remove [n t]
       (setq wtok (substring stok 0 -1)) ; remove [t]: class=3, daridraa,etc
      )
      (setq strong (sym-without-space stok))
      (setq weak (sym-without-space wtok))
      (cond
       ((member class '(1 4 10)) (setq femtype 'S))
       ((equal class 3) (setq femtype 'VW))
       ((equal class 6) (setq femtype 'SW))
       ((member dhaatu '(shaas jakSh chakaas daridraa jaagRi))
	(setq femtype 'VW)
       )
       ((equal class 2)
        (cond
	 ((equal (substring stok -3) [aa n t])
	  (setq femtype 'SW)
	 )
	 (t
	  (setq femtype 'W)
	 )
	)
       )
       (t (setq femtype 'W))
      )
      (setq ans (append ans (list (list weak femtype))))
     )
    )
;     (let (bases)
;      (fol-msg (format "chk: %s\n" ans))
;      (setq bases (mapcar 'car ans))
;      (setq bases (solution bases))
;      (setq ans (list bases femtype))
;     )
    ans
 )
)
(defun construct-prespart-base-alt-A (dhaatu class pada upasargas)
 (let (ans ctabs tense-sym voice-sym)
  (setq tense-sym 'laT)
  (setq voice-sym 'ACTIVE)
  (setq ctabs (construct-conjtab1a  dhaatu class pada upasargas tense-sym))
  (setq ctabs (solution ctabs))
  (if (not (listp ctabs)) (setq ctabs (list ctabs)))
  (let (ctab  praatipadikas ctabelt)
   (while ctabs
    (setq ctab (car ctabs))
    (setq ctabs (cdr ctabs))
    (setq ctabelt (elt ctab 2))
    (setq praatipadikas
       (construct-prespart-base-alt-A-1 ctabelt dhaatu class))
    (setq ans (append ans praatipadikas))
   )
  )
  ans
 )
)
(defun construct-prespart-base-alt-A-1 (ctabelt dhaatu class )
 (let (ans conjelt Eng-def procname conjelts)
  (setq conjelts ctabelt) ; 3rd pers plur present tense for P/A
  (if (not (listp conjelts)) (setq conjelts (list conjelts)))
  (setq procname "construct-prespart-base-alt-A-1")
  (while conjelts
   (setq conjelt (car conjelts))
   (setq conjelts (cdr conjelts))
   (let (tok stok wtok s w strong weak femtype record)
      ; conjelt is 3rd pers plural of present active
      ; this construction follows Goldman (15.8) , Kale (#669)
      ; conjelt assumed to end in 'ante' for a-conjugations
      ; and 'ate' for non-a-conjugations.
      (setq tok (car (ITRANS-parse-words-1 (symbol-name conjelt))))
      (cond
       ((member class '(1 4 6 10))
	;conjelt ends in 'ante'. Remove 'nte', leave the 'a'
        (setq stok (substring tok 0 -3)) ; remove 'nte', leave the 'a'
;        (setq stok (vconcat stok [m aa n]))
        (setq stok (conjugation-join stok [m aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((member class '(5 8 9))
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((equal class 2)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((equal class 3)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
	(cond
	 ((equal dhaatu 'aas) ; Kale #670(a)
          (setq stok (conjugation-join stok [ii n a]))
	  (setq stok (substring stok 0 -1)) ; remove 'a'
	 )
	 (t
          (setq stok (conjugation-join stok [aa n a]))
	  (setq stok (substring stok 0 -1)) ; remove 'a'
	 )
	)
       )
       ((equal class 7)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
      )
      (setq ans (list (sym-without-space stok)))
     )
  )
  ans
 )
)
(defun construct-prespart-base (dhaatu class pada upasargas)
 (cond
  ((equal pada 'P)
   (construct-prespart-base-P dhaatu class pada upasargas)
  )
  ((equal pada 'A)
   (construct-prespart-base-A dhaatu class pada upasargas)
  )
  (t nil)
 )
)
(defun construct-prespart-base-P (dhaatu class pada upasargas)
; Note: AntoineI-#83 says
; (1) Verbs taking parasmaipada terminations form their present
;   participle active in 'at'. To form it, the termination
;   'anti' of the 3rd pers. plur of the present active is
;   replaced by 'at'.
; (2) Verbs taking atmanepada terminations form their present
;   participle active in 'maana'. To form it, the termination
;   'te' of the 3rd pers. sing. of the present tense is replaced
;   by 'maana'.
; (2) is correct only for the a-conjugations.
; For the non-a conjugations, the ending 'aana' replaces the
; termination 'ate' of the 3rd pers. PLURAL of the present tense.
; An equivalent alternate of (2) for the 'a' conjugations is
; to replace the termination 'nte' of the 3rd pers. plur.
; of the present tense with 'maana'.
; Since the 3rd pers PLURAL of the present tense can be made
; to work with either, and is required for non-a conjugations,
; that is chosen for both a and non-a conjugations
; This is consistent with Goldman (weak stem of non-a conjugations)
; and Kale (#669, p. 418) says to use 3P of present tense.
 (let (ans ctabs tense-sym voice-sym)
  (setq tense-sym 'laT)
  (setq voice-sym 'ACTIVE)
  (setq ctabs (construct-conjtab1a  dhaatu class pada upasargas tense-sym))
  (setq ctabs (solution ctabs))
  (if (not (listp ctabs)) (setq ctabs (list ctabs)))
  (let (i m ctab c conjelt praatipadikas)
   (when ctabs
    (setq ctab (elt ctabs 0))
    (setq m (length ctabs))
    (setq i 1)
    (while (< i m)
     (setq ctab (join-arrays ctab (elt ctabs i)))
     (setq i (1+ i))
    )
    (setq conjelt (elt ctab 2)) ; 3rd pers plur present tense for P/A
    (setq praatipadikas (pres-part-praatipadikas conjelt class pada dhaatu))
    (setq ans praatipadikas)
   )
  )
  ans
 )
)
(defun construct-prespart-base-A (dhaatu class pada upasargas)
 (let (ans ctabs tense-sym voice-sym)
  (setq tense-sym 'laT)
  (setq voice-sym 'ACTIVE)
  (setq ctabs (construct-conjtab1a  dhaatu class pada upasargas tense-sym))
  (setq ctabs (solution ctabs))
  (if (not (listp ctabs)) (setq ctabs (list ctabs)))
  (let (i m ctab c conjelt praatipadikas)
   (when ctabs
    (setq ctab (elt ctabs 0))
    (setq m (length ctabs))
    (setq i 1)
    (while (< i m)
     (setq ctab (join-arrays ctab (elt ctabs i)))
     (setq i (1+ i))
    )
    (setq conjelt (elt ctab 2)) ; 3rd pers plur present tense for P/A
    (setq praatipadikas (pres-part-praatipadikas conjelt class pada dhaatu))
    (setq ans praatipadikas)
   )
  )
  ans
 )
)
(defun pres-part-praatipadikas (conjelt class pada dhaatu)
 ; returns (list weak strong femtype) when pada = P
 (when (and conjelt (listp conjelt))
  (mapcar
   (lambda (c) (pres-part-praatipadikas c class pada dhaatu))
   conjelt
  )
 )
 (when (and conjelt (not (listp conjelt)))
  (let (Eng-def procname ans)
  (setq procname "pres-part-praatipadikas")
   (cond
    ((equal pada 'P)
     (let (tok stok wtok s w strong weak femtype Eng-def-str record)
      ; conjelt is 3rd pers PLUR of present active
      ; this construction follows Goldman (15.7)
      (setq tok (car (ITRANS-parse-words-1 (symbol-name conjelt))))
      (setq stok (substring tok 0 -1)) ; strong base
      (if (equal (substring stok -3) [a n t]) ; the usual case
       (setq wtok (vconcat (substring stok 0 -3) [a t]))
       (setq wtok stok) ; when class = 3
      )
      (setq strong (sym-without-space stok))
      (setq weak (sym-without-space wtok))
      (cond
       ((member class '(1 4 10)) (setq femtype 'S))
       ((equal class 6) (setq femtype 'SW))
       ((and (equal class 2)
	     (let (dname)
	      (setq dname (symbol-name dhaatu))
	      (and (<= 2 (length dname))
	       (equal (substring dname -2) "aa"))
	     )
	)
	(setq femtype 'SW)
       )
       (t (setq femtype 'W))
      )
      (setq ans (list weak strong femtype))
     )
    )
    ((equal pada 'A)
     (let (tok stok wtok s w strong weak femtype record)
      ; conjelt is 3rd pers plural of present active
      ; this construction follows Goldman (15.8) , Kale (#669)
      ; conjelt assumed to end in 'ante' for a-conjugations
      ; and 'ate' for non-a-conjugations.
      (setq tok (car (ITRANS-parse-words-1 (symbol-name conjelt))))
      (cond
       ((member class '(1 4 6 10))
	;conjelt ends in 'ante'. Remove 'nte', leave the 'a'
        (setq stok (substring tok 0 -3)) ; remove 'nte', leave the 'a'
;        (setq stok (vconcat stok [m aa n]))
        (setq stok (conjugation-join stok [m aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((member class '(5 8 9))
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((equal class 2)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
       ((equal class 3)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
	(cond
	 ((equal dhaatu 'aas) ; Kale #670(a)
          (setq stok (conjugation-join stok [ii n a]))
	  (setq stok (substring stok 0 -1)) ; remove 'a'
	 )
	 (t
          (setq stok (conjugation-join stok [aa n a]))
	  (setq stok (substring stok 0 -1)) ; remove 'a'
	 )
	)
       )
       ((equal class 7)
	; conjelt ends in 'ate'. Remove this. add 'aan'
	(setq stok (substring tok 0 -3))
        (setq stok (conjugation-join stok [aa n a]))
	(setq stok (substring stok 0 -1)) ; remove 'a'
       )
      )
      (setq ans (list (sym-without-space stok)))
     )
    )
    (t
     (fol-msg (format "(%s) Error : %s %s %s\n" procname conjelt class pada))
     (setq ans nil)
    )
   )
  ans
 ))
)


(defun construct-passpart (&optional forms outtab Xouttab outdir)
 ; does not depend on upasarga
 ; 10-11-03 : output base only 
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "passpart.txt"))
  (if (not Xouttab) (setq Xouttab "Xpasspart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    ; don't need upasargas or pada
    (setq key (dcpu-sym dhaatu class pada nil)) 
    (when (not (sanget key 'temp))
     ; a plist, with keys M, F, N
     ; each elt of form (gender dtab)
;     (fol-msg (format "chk: %s %s %s %s\n"
;		      dhaatu class pada upasargas))
     (setq keyval (construct-passpart-base-alt dhaatu class pada upasargas))
     (sanput key 'temp keyval)
     (let (subkey Xval Xvals)
;     (setq subkey (list 'dhaatu 'PASSPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'PASSPART))
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq val (elt keyval 0))
      (setq Xvals val)
      (if (not (listp Xvals)) (setq Xvals (list Xvals)))
;      (setq Xvals (Xalgorithm val))
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-passpart-base-alt   (dhaatu class pada upasargas)
 (let (ans praatipadikas  thisans bases)
  (setq praatipadikas (construct-conjpassbase1a dhaatu class pada upasargas))
  (if (not (listp praatipadikas)) (setq praatipadikas (list praatipadikas)))
  (setq bases
   (mapcar
    (lambda (x)
     ; x is a symbol (ending in 'y', e.g. 'budhy'
     ; we append 'amaan' to get the base
     (let (tok stok)
      (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
      (setq stok (conjugation-join tok [a m aa n a]))
      (setq stok (substring stok 0 -1)) ; remove final 'a'
      (sym-without-space stok)
     )
    )
    praatipadikas
   )
  )
  bases
 )
)
(defun construct-passpart1a (dhaatu class pada upasargas)
 (let (ans praatipadikas dtab gender genders thisans bases)
  (setq praatipadikas (construct-conjpassbase1a dhaatu class pada upasargas))
  (if (not (listp praatipadikas)) (setq praatipadikas (list praatipadikas)))
  (setq bases
   (mapcar
    (lambda (x)
     ; x is a symbol (ending in 'y', e.g. 'budhy'
     ; we append 'amaan' to get the base
     (declension-join x 'amaan)
    )
    praatipadikas
   )
  )
  (setq genders '(M F N))
  (while (and genders bases)
   (setq gender (car genders))
   (setq genders (cdr genders))
   (setq dtab (declension-pres-part bases gender 4 'A))
   (setq thisans (list gender dtab))
   (setq ans (append ans thisans))
  )
  ans
 )
)

(defun construct-pppart (&optional forms outtab Xouttab outdir)
 ; function 'construct-pppart1a' is in file 'kta.el'
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "pppart.txt"))
  (if (not Xouttab) (setq Xouttab "Xpppart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a past-passive participle (with the ending 'a')
     (setq keyval (construct-pppart1a dhaatu class pada upasargas))
     (if (not (listp keyval)) (setq keyval (list keyval)))
     (setq keyval
      (mapcar
       (lambda (x)
	; remove the trailing 'a'
        (let (tok)
	 (setq tok (car (ITRANS-parse-words-1 (symbol-name x))))
	 (setq tok (substring tok 0 -1))
	 (sym-without-space tok)
        )
       )
       keyval
      )
     )
     (sanput key 'temp keyval)
     (let (subkey Xval Xvals)
;     (setq subkey (list 'dhaatu 'PPPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'PPPART))
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq Xvals keyval)
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
     ; do separate for PAPART (past participle active)
     (let (subkey Xval Xvals)
;     (setq subkey (list 'dhaatu 'PAPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'PAPART))
      (setq keyval
       (mapcar
	(lambda (x) (sym-concat x 'av))
        keyval
       )
      )
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq Xvals keyval)
      (if (not (listp Xvals)) (setq Xvals (list Xvals)))
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-ippart (&optional forms outtab Xouttab outdir)
 ; function 'construct-ippart1a' is in file 'kta.el'
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "ippart.txt"))
  (if (not Xouttab) (setq Xouttab "Xippart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a past-passive participle (with the ending 'a')
     (setq keyval (construct-ippart1a dhaatu class pada upasargas))
;    (sanput key 'temp keyval)
     (let (subkey Xval Xvals)
;     (setq subkey (list 'dhaatu 'IPPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'IPPART))
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq Xvals keyval)
      (if (not (listp Xvals)) (setq Xvals (list Xvals)))
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       ; remove the trailing 'a'
       (let (tok)
	(setq tok (car (ITRANS-parse-words-1 (symbol-name Xval))))
	(setq tok (substring tok 0 -1))
	(setq Xval (sym-without-space tok))
       )
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)

(defun construct-inf (&optional forms outtab Xouttab outdir)
 ; function 'construct-inf1a' is in file 'kta.el'
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "inf.txt"))
  (if (not Xouttab) (setq Xouttab "Xinf.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a past-passive participle (with the ending 'a')
     (setq keyval (construct-inf1a dhaatu class pada upasargas))
;    (sanput key 'temp keyval)
     (let (subkey Xval Xvals)
;     (setq subkey (list 'dhaatu 'INF))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'INF))
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq Xvals keyval)
      (if (not (listp Xvals)) (setq Xvals (list Xvals)))
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       ; remove the trailing 'a'
       (let (tok)
	(setq tok (car (ITRANS-parse-words-1 (symbol-name Xval))))
	(setq tok (substring tok 0 -1))
	(setq Xval (sym-without-space tok))
       )
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-potpart (&optional forms outtab Xouttab outdir)
 ; function 'construct-potpart1a' is in file 'kta.el'
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "potpart.txt"))
  (if (not Xouttab) (setq Xouttab "Xpotpart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a past-passive participle (with the ending 'a')
     (setq keyval (construct-potpart1a dhaatu class pada upasargas))
;    (sanput key 'temp keyval)
     (let (subkey Xval Xvals)
;      (setq subkey (list 'dhaatu 'POTPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'POTPART))
      (with-current-buffer bufout
       (insert (format "%s %s %s\n" key subkey keyval))
      )
      (setq Xvals keyval)
      (if (not (listp Xvals)) (setq Xvals (list Xvals)))
      (while Xvals
       (setq Xval (car Xvals)) ; a symbol
       (setq Xvals (cdr Xvals))
       ; remove the trailing 'a'
       (let (tok)
	(setq tok (car (ITRANS-parse-words-1 (symbol-name Xval))))
	(setq tok (substring tok 0 -1))
	(setq Xval (sym-without-space tok))
       )
       (with-current-buffer Xbufout
        (insert (format "%s %s %s\n" Xval subkey key))
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)

(defun rppart-doc ()
 "Kale 675. 
  - The participle of the Perf. Par. and Atm. is formed by adding 
    'vas' and 'aana' generally to the form of the root which it assumes
    before the termination of the 3rd per. plural.
  - If this form consists of one syllable only or when the root ends
    in 'aa', 'vas' has the intermediate 'i' added to it.
  - 'i' is optionally prefixed to 'vas' in the case of the roots
    'gam han dRish vish (vid 6)'.
  -  The roots 'jan khan gam  han', when they do not take 'i', form
     this participle from that base which they would assume before
     the 2nd person sing. termination Par.
  - In the Par., 
    - 'jan' -> 'jajanvas'
    - 'khan' -> 'chakhanvas'
    - 'gam' -> 'jagmivas' or 'jaganvas'
    - 'han' -> 'jaghnivas' or 'jaghanvas'
  - In the case of roots beginning with 'a', 'n' is not inserted as
    it is in the perfect, e.e. 'a~nj' -> 'aajivas'
  - These participles are declined as in #124 ('declension-general-vas-ADJ')
 Kale 676.
  Roots ending in 'RI' (including 'tRI jRI') form their perfect participle
  irregularly.
  Parasmaipada: 
   - 'vas' is added to the root
   - The root undergoes the change of #394:
      When preceded by a labial or 'v', 'RI' changes to 'uur' (long
      since followed by the consonant 'v' of 'vas')
      Otherwise, 'RI' changes to 'iir'
   - This form is reduplicated
  Atmanepada:
   - The root is first reduplicated and 'aana' afterwards added, the
     final 'RI' changing as before.
  NOTE: in this case, [i r] or [u r] replaces 'RI', since the following
     letter is a vowel (the 'aa' of 'aana')
 Kale 677.
  The Perfect participle is not frequently used.
  The only participles that are most commonly to be met with
  are those formed from the roots 'sad vas sthaa shru'
 "
)
(defun construct-rppart (&optional forms outtab Xouttab outdir)
 ; function 'construct-rppart1a' is in file 'kta.el'
 ; reduplicative perfect participle. Only the 'base'
 ; is constructed. Based on Kale #675-8
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "rppart.txt"))
  (if (not Xouttab) (setq Xouttab "Xrppart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a base 
     (setq keyval (construct-rppart1a dhaatu class pada upasargas))
     (when keyval
;     (sanput key 'temp keyval)
      (let (subkey Xval Xvals)
;       (setq subkey (list 'dhaatu 'RPPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'RPPART))
       (with-current-buffer bufout
        (insert (format "%s %s %s\n" key subkey keyval))
       )
       (setq Xvals keyval)
       (if (not (listp Xvals)) (setq Xvals (list Xvals)))
       (setq Xvals (flatten Xvals))
       (while Xvals
        (setq Xval (car Xvals)) ; a symbol
        (setq Xvals (cdr Xvals))
        (with-current-buffer Xbufout
         (insert (format "%s %s %s\n" Xval subkey key))
        )
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-rppart1a (dhaatu class pada upasargas)
; reduplicative perfect participle
; when pada = 'P', return a list of pairs of symbols,
;    the first symbol of a pair ends in 'v', the second ends in 'uSh'
; when pada = 'A', return a list of symbols (ending in 'aan' or 'aaN')
 (let (ans1 ans)
  (setq ans1 (construct-rppart1b dhaatu class pada upasargas))
  (cond
   ((not ans1)) ; ans will be nil
   ((equal pada 'A) (setq ans ans1))
   ((equal pada 'P)
    (setq ans (mapcar 'construct-rppart1a-mod ans1))
   )
  )
  ans
 )
)
(defun construct-rppart1a-mod (x)
 ; x is a symbol ending in 'v'.
 ; return a list of 2 symbols, the 1st is 'x'
 ; the 2nd is the prefix for weak endings (ends in 'uSh')
 ; compare to logic in 'declension-general-vas-ADJ'
 (let (ans tok ptok1 wtok w)
  (setq tok  ; [ch a k Ri v] [j a g m i v] [v i d v]
   (car (ITRANS-parse-words-1 (symbol-name x))))
  (setq ptok1 (substring tok 0 -1)) ; [ch a k Ri] [j a g m i] [v i d]
  (if (equal (substring ptok1 -1) [i])
   (setq wtok (vconcat (substring ptok1 0 -1) [u Sh])) ; [j a g m u Sh]
   (setq wtok (declension-join ptok1 [u Sh])) ; [ch a k r u Sh] [v i d u Sh]
  )  
  (setq w (sym-without-space wtok))
  (list x w)
 )
)
(defun construct-rppart1b (dhaatu class pada upasargas)
; reduplicative perfect participle
; The form returned is a list of symbols, where
;  when pada = 'P', the symbol ends in 'v', so when 'as' is 
;  appended, the form ending in 'vas' needed by 'declension-general-vas-ADJ'
;  is available.
;  when pada = 'A', the symbol ends in 'aan', the form
;   needed for the declension ending in 'a'.
 (when (reduplicative-liT-P dhaatu class)
  (let (ans tok base  seT-code ntok lc pc ans1 x y wparts parts types
	ending)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   (setq ntok (length tok))
   (if (< 0 ntok) (setq lc (elt (substring tok -1) 0)))
   
   (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
   (setq base
    (let (bitab dhaatu1)
     ; Example for nil 1 'P nil: x=(([n i n ii] NA))
     ; so 'x' is a list of pairs;
     ; in each pair, the 1st elt is a tok-array, the second a
     ; symbol (NA or seT or aniT)
     (cond
      ((and (equal (elt tok 0) 'a)
	      (nasal-P pc)
       )
       (setq dhaatu1
         (sym-without-space
		     (vconcat (substring tok 0 -2) (substring tok -1))))
      )
      ((and (equal lc 'RI) (equal pada 'P))
       ; Kale 676
       (let (fc x y)
	(setq x (substring tok 0 -1)) ; drop the 'RI'
	(if (< 0 ntok) (setq fc (elt tok 0)))
        (if (or (labial-P fc) (equal pc 'v))
	 (setq y (vconcat x [uu r]))
	 (setq y (vconcat x [ii r]))
	)
	(setq dhaatu1 (sym-without-space y))
       )
      )
      (t (setq dhaatu1 dhaatu)
      )
     )
     (setq bitab (liT-main-get-bitab upasargas class pada dhaatu1))
     (setq x (elt bitab 2)) ; 3rd pers plur
     (setq y (mapcar 'car x)) ; just keep the tok-arrays
     (when (and (equal lc 'RI) (equal pada 'A))
       ; Kale 676
      (let (fc x z)
       (setq y (reduplicate-perfect tok))
       (setq x (car y)) ; assume 'y' has only one form in this case
;	(fol-msg (format "x=%s\n" x))
       (setq x (substring x 0 -1)) ; drop the 'RI'
       (if (< 0 ntok) (setq fc (elt tok 0)))
       (if (or (labial-P fc) (equal pc 'v))
        (setq z (vconcat x [u r]))
        (setq z (vconcat x [i r]))
       )
       (setq y (list z))
      )
     )
     y
    )
   )
   (while base
    (setq x (car base)) ; x is a tok-array
    (setq base (cdr base))
    
    (cond
     ((equal pada 'P)
      (setq ending [v a])
      (setq seT-code 'aniT)
      (setq wparts (word-parts x))
      (setq parts (elt wparts 0))
      (setq types (elt wparts 1))
      (cond
       ((member types '("V" "VC" "CV" "CVC")) ; 1 syllable
        (setq seT-code 'seT)
       )
       ((member lc '(aa e i o))
	(setq seT-code 'seT)
       )
      )
      (setq y (perfect-join1 x seT-code ending))
      (setq y (substring y 0 -1)) ; drop the last [a]
     )
     ((equal pada 'A)
      (setq ending [aa n a])
      (setq seT-code 'aniT)
      (setq y (perfect-join1 x seT-code ending))
      (setq y (substring y 0 -1)) ; drop the last [a]
     )
    )
    (when y
     (setq ans1 (sym-without-space y))
     (setq ans (append-if-new ans ans1))
    )
   )
   ; exceptions
   (cond
    ((equal pada 'P)
     (cond
      ((equal dhaatu 'bhuu) (setq ans '(babhuuv))) ; otherwise, 'babhuuvv'
      ((equal dhaatu 'jan) (setq ans '(jajanv)))
      ((equal dhaatu 'khan) (setq ans '(chakhanv)))
      ((equal dhaatu 'gam) (setq ans '(jagmiv jaganv)))
      ((equal dhaatu 'han) (setq ans '(jaghniv jaghanv)))
      ((equal dhaatu 'vid) (setq ans '(vividv vividiv)))
      ((equal dhaatu 'dRish) (setq ans '(dadRishiv dadRishv)))
      ((equal dhaatu 'vish) (setq ans '(vivishiv vivishv)))
     )
    )
   )
   ans
  )
 )
)

(defun perppart-doc ()
 "Kale 675. 
  - The participle of the Perfect Par. and Atm. is formed by adding 
    'vas' and 'aana' generally to the form of the root which it assumes
    before the termination of the 3rd per. plural.
  - If this form consists of one syllable only or when the root ends
    in 'aa', 'vas' has the intermediate 'i' added to it.
  - 'i' is optionally prefixed to 'vas' in the case of the roots
    'gam han dRish vish (vid 6)'.
  -  The roots 'jan khan gam  han', when they do not take 'i', form
     this participle from that base which they would assume before
     the 2nd person sing. termination Par.
  - In the Par., 
    - 'jan' -> 'jajanvas'
    - 'khan' -> 'chakhanvas'
    - 'gam' -> 'jagmivas' or 'jaganvas'
    - 'han' -> 'jaghnivas' or 'jaghanvas'
  - In the case of roots beginning with 'a', 'n' is not inserted as
    it is in the perfect, e.e. 'a~nj' -> 'aajivas'
  - These participles are declined as in #124 ('declension-general-vas-ADJ')
 Kale 676.
  Roots ending in 'RI' (including 'tRI jRI') form their perfect participle
  irregularly.
  Parasmaipada: 
   - 'vas' is added to the root
   - The root undergoes the change of #394:
      When preceded by a labial or 'v', 'RI' changes to 'uur' (long
      since followed by the consonant 'v' of 'vas')
      Otherwise, 'RI' changes to 'iir'
   - This form is reduplicated
  Atmanepada:
   - The root is first reduplicated and 'aana' afterwards added, the
     final 'RI' changing as before.
  NOTE: in this case, [i r] or [u r] replaces 'RI', since the following
     letter is a vowel (the 'aa' of 'aana')
 Kale 677.
  The Perfect participle is not frequently used.
  The only participles that are most commonly to be met with
  are those formed from the roots 'sad vas sthaa shru'
 Kale 678.
  The participles of the Periphrastic Perfect Par. and Atm. are
  formed by the addition of the Perf. Participial forms (as above)
  of the auxiliary verbs 'kRi as bhuu' to the base in 'aam'.
 "
)
(defun construct-perppart (&optional forms outtab Xouttab outdir)
 ; function 'construct-perppart1a' is in file 'kta.el'
 ; reduplicative perfect participle. Only the 'base'
 ; is constructed. Based on Kale #675-8
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "perppart.txt"))
  (if (not Xouttab) (setq Xouttab "Xperppart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a base 
     (setq keyval (construct-perppart1a dhaatu class pada upasargas))
     (when keyval
;     (sanput key 'temp keyval)
      (let (subkey Xval Xvals)
;       (setq subkey (list 'dhaatu 'PERPPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'PERPPART))
       (with-current-buffer bufout
        (insert (format "%s %s %s\n" key subkey keyval))
       )
       (setq Xvals keyval)
       (if (not (listp Xvals)) (setq Xvals (list Xvals)))
       (setq Xvals (flatten Xvals))
       (while Xvals
        (setq Xval (car Xvals)) ; a symbol
        (setq Xvals (cdr Xvals))
        (with-current-buffer Xbufout
         (insert (format "%s %s %s\n" Xval subkey key))
        )
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defvar perppart1a-data nil)
(defun construct-perppart1a (dhaatu class pada upasargas)
; periphrastic perfect participle
; Kale doesn't say explicitly, but I assume that the form of the
; participle ('vas' or 'a') follows the 'pada' (P or A), as it
; does with the reduplicative perfect participle. 
; In particular, recall the comment regarding the conjugational
; 'liT' forms:
   ; Kale #525 p. 329.
   ;  When the forms of 'kRi' are added, a parasmaipada root takes the
   ;  parasmaipada forms, and an atmanepadi one takes the atmanepadi forms.
   ;  NOTE: When the forms of 'bhuu' or 'as' are added, the parasmaipadi forms
   ;   are used, regardless of the form of the root
; Thus, when 'pada' = 'P', I return 'vas' bases using 'kRi bhuu as'
; When pada = 'A', I return an 'a' bases using 'kRi bhuu as'
; The form returned is a list , where
;  when pada = 'P', each list item is a pair of symbols, the first
;    ending in 'v' and the second ending in 'uSh'
;  when pada = 'A', the symbol ends in 'aan', the form
;   needed for the declension ending in 'a'.
 (if (not perppart1a-data) (init-perppart1a-data))

 (when (periphrastic-liT-P dhaatu class)
  (let (ans tok base  seT-code ntok lc pc ans1 x y wparts parts types
	ending suffixes)
   (setq base (periphrastic-base dhaatu class pada)); a list
;   (setq base (solution base))
   
   (setq suffixes
    (if (equal pada 'P) (elt perppart1a-data 0) (elt perppart1a-data 1))
   )
   (setq ans
    (mapcar
     (lambda (s)
      (if (not (listp s)) (setq s (list s)))
      (solution
       (mapcar
	(lambda (x)
	 (solution
	  (mapcar
	   (lambda (b1)
	    (conjugation-join-sym b1 x)
	   )
	   base
	 )
	 )
	)
	s)
      )
     )
     suffixes
    )
   )
   ans
  )
 )
)
(defun init-perppart1a-data ()
 (setq perppart1a-data
  (list 
   (mapcar
    (lambda (pair)
     (mapcar
      (lambda (x) (conjugation-join-sym 'aam x))
      pair
     )
    )
    (list
     (solution (construct-rppart1a 'kRi 8 'P nil))
     (solution (construct-rppart1a 'bhuu 1 'P nil))
     (solution (construct-rppart1a 'as 2 'P nil))
    )
   )
  (list
   (conjugation-join-sym 'aam (solution (construct-rppart1a 'kRi 8 'A nil)))
;   (conjugation-join-sym 'aam (solution (construct-rppart1a 'bhuu 1 'A nil)))
;   (conjugation-join-sym 'aam (solution (construct-rppart1a 'as 2 'A nil)))
   (mapcar
    (lambda (x) (conjugation-join-sym 'aam x))
    (solution (construct-rppart1a 'bhuu 1 'P nil))
   )
   (mapcar
    (lambda (x) (conjugation-join-sym 'aam x))
    (solution (construct-rppart1a 'as 2 'P nil)) 
   )
  )
 ))
)

(defun construct-futpart (&optional forms outtab Xouttab outdir)
 ; function 'construct-futpart1a' is in file 'gram2-future.el'
 ; future participle (active). 
 ; Only the 'base' is constructed. 
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "futpart.txt"))
  (if (not Xouttab) (setq Xouttab "Xfutpart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a base 
     (setq keyval (construct-futpart1a dhaatu class pada upasargas))
     (when keyval
;     (sanput key 'temp keyval)
      (let (subkey Xval Xvals)
;       (setq subkey (list 'dhaatu 'FUTPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'FUTPART))
       (with-current-buffer bufout
        (insert (format "%s %s %s\n" key subkey keyval))
       )
       (setq Xvals keyval)
       (if (not (listp Xvals)) (setq Xvals (list Xvals)))
       (while Xvals
        (setq Xval (car Xvals)) ; a symbol
        (setq Xvals (cdr Xvals))
        (with-current-buffer Xbufout
         (insert (format "%s %s %s\n" Xval subkey key))
        )
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)

(defun construct-futppart (&optional forms outtab Xouttab outdir)
 ; function 'construct-futppart1a' is in file 'gram2-future.el'
 ; future participle (active). 
 ; Only the 'base' is constructed. 
 (construct-cleartemp)
 (let (keys dhaatu fileout bufout Xfileout Xbufout nform)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "futppart.txt"))
  (if (not Xouttab) (setq Xouttab "Xfutppart.txt"))
  (if (not forms) (setq forms (construct-dhaatu-forms1)))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq Xfileout (sangram-filename Xouttab outdir))
  (setq Xbufout (find-file-noselect Xfileout 't)) ; 't suppresses warning
  (with-current-buffer Xbufout
   (erase-buffer)
  )
  (setq nform 0)
  (while forms
   (let (form keyval key val tmp dhaatu class pada upasargas)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
    (setq tmp (sym-dcpu form))
    (setq dhaatu (elt tmp 0))
    (setq class (elt tmp 1))
    (setq pada (elt tmp 2))
    (setq upasargas (elt tmp 3))
    (setq key form)
;    (setq key (dcpu-sym dhaatu class pada nil)) ; don't need upasargas
    (when t ;(not (sanget key 'temp))
     ; keyval is a base 
     (setq keyval (construct-futppart1a dhaatu class pada upasargas))
     (when keyval
;     (sanput key 'temp keyval)
      (let (subkey Xval Xvals)
;       (setq subkey (list 'dhaatu 'FUTPPART))
       (setq key dhaatu)
       (setq subkey (list 'dhaatu class pada 'FUTPPART))
       (with-current-buffer bufout
        (insert (format "%s %s %s\n" key subkey keyval))
       )
       (setq Xvals keyval)
       (if (not (listp Xvals)) (setq Xvals (list Xvals)))
       (while Xvals
        (setq Xval (car Xvals)) ; a symbol
        (setq Xvals (cdr Xvals))
        (with-current-buffer Xbufout
         (insert (format "%s %s %s\n" Xval subkey key))
        )
       )
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (with-current-buffer Xbufout
   (save-buffer)
   (kill-buffer nil)
  )
  t
 )
)
(defun construct-verb-all ()
 (let (sfx files)
  ; files = ("dhaatukosha.txt" "dhaatukosha1.txt" ... )
  (setq files (all-dhaatukosha-inputs))
  (mapcar 
   (lambda (file)
    (setq sfx (strip-pfx-sfx file "dhaatukosha" ".txt"))
    (construct-verb-one sfx) 
   )
   files
  )
 )
)
(defun construct-verb-one (dhaatu-sfx)
 "dhaatu-sfx is a number (e.g., 9) used to generate a
  filename ('dhaatukosha9.txt') appearing in directory 'inputs'.
  Function 'construct-dhaatukosha' will create a file by the same
  name ('dhaatukosha9.txt') in directory 'construct'. 
  The keys in this file and the other 'dhaatukosha#.txt' files in
  directory 'construct' are combined by function 
  '(construct-dhaatu-forms-all)' to make file 'dhaatu-forms.txt'.
  The verbal construction functions are indirectly specified in
  the global variable 'construct-verb-one-data', and include tenses
  and participial forms.
  For each  verbal construction function (e.g. 'prespart')
  there is created a data and key file, e.g.
   'prespart9.txt' and 'Xprespart9.txt';
  This is accomplished by the function named, e.g.,
   'construct-prespart'
 "
 (let (dhaatu-filename dhaatu-pfx)
  (construct-init)
  (setq dhaatu-pfx (format "dhaatukosha%s" dhaatu-sfx))
  (setq dhaatu-filename (format "%s.txt" dhaatu-pfx))
  (construct-dhaatukosha dhaatu-filename)
  (construct-dhaatu-forms-all)
 )
 (mapcar
  (lambda (fun-sfx)
   (construct-verb-one-a dhaatu-sfx fun-sfx)
  )
  construct-verb-one-data
 )
)
(defvar construct-verb-one-data
 '(prespart ; present active participle
   passpart ; present passive participle
   pppart   ; past passive participle
   ippart   ; indeclinable past passive participle
   inf      ; gerund
   potpart  ; potential passive participle
   rppart   ; reduplicative perfect participle
   perppart ; periphrastic perfect participle
   futpart  ; future active participle
   futppart ; future passive participle
   laT    ; present
   la~N   ; imperfect  (takes 'a')
   loT    ; imperative
   vidhili~N ; optative
   liT-p  ; periphrastic perfect
   liT-r  ; reduplicative perfect
   lRiT   ; simple future (2nd future)
   lRi~N  ; conditional (takes 'a')
   luT    ; periphrastic future (1st future) 
   aashiirli~N ; benedictive
   lu~N1  ; 1st aorist (takes 'a)
   lu~N2  ; 1st aorist (takes 'a)
   lu~N3  ; 1st aorist (takes 'a)
   lu~N4  ; 1st aorist (takes 'a)
   lu~N5  ; 1st aorist (takes 'a)
   lu~N6  ; 1st aorist (takes 'a)
   lu~N7  ; 1st aorist (takes 'a)
  )
)
(defun filter-dhaatu-forms (forms option)
 (let (ans form dcpu dhaatu class pada upasargas form1)
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq dcpu (sym-dcpu form))
   (setq dhaatu (elt dcpu 0))
   (setq class (elt dcpu 1))
   (setq pada (elt dcpu 2))
   (setq upasargas (elt dcpu 3))
   (cond
    ((equal option 1)
     ; used by 'construct-verb-one-a'. Ignore upasargas
     (setq form1 (dcpu-sym dhaatu class pada nil))
     (setq ans (append-if-new ans form1))
    )
    (t )
   )
  )
  ans
 )
)
(defun construct-verb-one-a (dhaatu-sfx fun-sfx)
 (fol-msg (format "%s\n" (current-time-string)))
 (let (dhaatu-pfx dhaatu-filename forms form-function-names)
  (setq dhaatu-pfx (format "dhaatukosha%s" dhaatu-sfx))
  (setq dhaatu-filename (format "%s.txt" dhaatu-pfx))
  (setq forms (construct-dhaatu-forms1 (list dhaatu-filename)))
  (setq forms (filter-dhaatu-forms forms 1)) ;01-20-04
    (let (fun-name outtab Xouttab outdir)
     (setq outdir "construct")
     (setq fun-name (sym-concat 'construct- fun-sfx))
     (setq outtab (format "%s%s.txt" fun-sfx dhaatu-sfx))
     (setq Xouttab (concat "X" outtab))
     (fol-msg (format "funcall %s <%s> %s %s %s\n"
		      fun-name dhaatu-filename outtab Xouttab outdir ))
     (funcall fun-name forms outtab Xouttab outdir)
    )
 )
 (fol-msg (format "%s\n" (current-time-string)))
)
(defun construct-verb-one-a1 (dhaatu-sfxes fun-sfx)
 (mapcar 
  (lambda (dhaatu-sfx) (construct-verb-one-a dhaatu-sfx fun-sfx))
  dhaatu-sfxes
 )
)

(defun construct-verb-causal-one (dhaatu-sfx)
 (fol-msg (format "%s\n" (current-time-string)))
 (let (dhaatu-pfx dhaatu-filename forms form-function-names fun-sfx)
  (setq fun-sfx 'causal-conjtab)
  (setq dhaatu-pfx (format "dhaatukosha%s" dhaatu-sfx))
  (setq dhaatu-filename (format "%s.txt" dhaatu-pfx))
  (setq forms (construct-dhaatu-forms1 (list dhaatu-filename)))
  
    (let (fun-name outtab Xouttab outdir)
     (setq outdir "construct")
     (setq fun-name (sym-concat 'construct- fun-sfx))
     (setq outtab (format "%s%s.txt" fun-sfx dhaatu-sfx))
     (setq Xouttab (concat "X" outtab))
     (fol-msg (format "funcall %s <%s> %s %s %s\n"
		      fun-name dhaatu-filename outtab Xouttab outdir ))
     (funcall fun-name forms outtab Xouttab outdir)
    )
 )
 (fol-msg (format "%s\n" (current-time-string)))
)

(defun load-construct-all ()
 ; load constructed data
 (fol-msg (format "%s\n" (current-time-string)))
 (load-dhaatukosha (all-dhaatukosha-inputs))
 (load-subfields "dhaatu-forms.txt" "construct")
 (mapcar 
  'load-construct-all-verbal
  construct-verb-one-data
 )
 (fol-msg (format "%s\n" (current-time-string)))
 (load-subanta-files)
 (load-causal-files)
 (fol-msg (format "%s\n" (current-time-string)))
)
(defun load-subanta-files ()
 (let (files)
  ; files = ("subanta.txt" "subanta1.txt" ... )
  (setq files (all-subanta-inputs))
  (mapcar 
   (lambda (file)
    (message (format "loading %s" file))
    (load-subfields file "construct")
    (load-subfields (concat "X" file) "construct" t t)
   )
   files
  )
 )
)
(defun all-dhaatukosha-suffixes ()
 (let (dhaatu-files dhaatu-file sfx file pfxes sfxes)
  (setq dhaatu-files (all-dhaatukosha-inputs))
  (while dhaatu-files
   (setq dhaatu-file (car dhaatu-files))
   (setq dhaatu-files (cdr dhaatu-files))
   (setq sfx (strip-pfx-sfx dhaatu-file "dhaatukosha" ".txt"))
   (setq sfxes (append sfxes (list sfx)))
  )
  sfxes
 )
)
(defun load-construct-all-verbal (pfx)
 (let (sfx file pfxes sfxes)
  (message (format "load-construct-all-verbal: %s" pfx))
  (setq sfxes (all-dhaatukosha-suffixes))
  
  (load-subfields
   (mapcar (lambda (sfx1) (format "%s%s.txt" pfx sfx1)) sfxes)
   "construct" nil nil)
  (load-subfields
   (mapcar (lambda (sfx1) (format "X%s%s.txt" pfx sfx1)) sfxes)
   "construct" t t)
 )
)
(defun load-causal-files ()
 (let (file pfx sfxes)
  (setq pfx "causal-conjtab")
  (message (format "load-causal-files ...: "))
  (setq sfxes (all-dhaatukosha-suffixes))
  (load-subfields
   (mapcar (lambda (sfx1) (format "%s%s.txt" pfx sfx1)) sfxes)
   "construct" nil nil)
  (load-subfields
   (mapcar (lambda (sfx1) (format "X%s%s.txt" pfx sfx1)) sfxes)
   "construct" t t)
 )
)

(defun construct-subanta (intab &optional indir outtab outdir dbg)
 ; 01-30-04: assume input transliteration form is SLP.
 ; Output is constructed with transliteration form SLP.
 (let (nrec irec recs fields nfields procname ok subantas fileout bufout
       bufin filein)
  (setq procname "construct-subanta")
  (if (not indir) (setq indir "inputs"))
  (if (not outtab) (setq outtab intab))
  (if (not outdir) (setq outdir "construct"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
    (goto-char 1)
  )

  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
    (erase-buffer)
  )
  (setq irec 0)
  (setq ok 't)
  (while (and (condition-case err (progn (read bufin) t) (error nil))  ok)
   (with-current-buffer bufin
    (setq fields (read-colon-line-fields (current-line) ':))
    (forward-line)
   )
   (setq irec (1+ irec))
   (let (subanta prop val new-vals gender form praatipadikas 
		 gender0 irregs processed base  forms ierr
		 SLsubanta lexinfo dict)
    ; 1a. unpack 'fields' to other variables
    ; assume fields[0] is a list of length 1 whose member is a symbol
    (setq SLsubanta (elt (elt fields 0) 0))
    (setq lexinfo (elt fields 1))
    (setq dict (elt fields 2))
    (setq dict (solution dict))
    (message (format "%s %s" irec SLsubanta))
    (setq subanta (translate-SLP1-ITRANS SLsubanta))
    (setq forms (convert-subanta-lexinfo subanta lexinfo))
    (while forms
     (let (tmp)
      (setq tmp (car forms))
      (setq forms (cdr forms))
      (setq form (car tmp))
      (setq gender (cadr tmp))
     )
;     (construct-subanta-helper subanta form gender)
     (setq praatipadikas nil)
     ; 1c. construct new-vals
     (setq processed nil)
     (when ok
      (when (not (member gender '(M F N ADJ ADJI PRON CARD ORD ADJt)))
       ;gender error
       (setq ierr 'gender)
       (setq ok nil)
      )
     )
     ; 2 provide a base for many easy forms
     (when ok
      (setq base (construct-subanta-base subanta gender form))
     )
     (when dbg
      (fol-msg (format "chk: ok=%s, fields=%s, subanta gender form=%s %s %s, base=%s\n"
		      ok fields subanta gender form base))
     )
     (when base
      (let (val1 val key subkey subkey1 keyval subkey2 val2 Xvals Xval SLbase)
       ; base should be an atom (e.g., 'raam' for "raama M a")
       (setq key subanta)
       (if (member gender '(M F N))
	; case of NOUNs with a base
	(setq subkey (list 'S 'NOUN form gender))
	; case of ADJs with a base.
	; 12-12-03: Only give one entry, rather than a
	; separate entry for each gender
	(setq subkey (list 'S 'ADJ form))
       )
       ; 01-22-04. Don't put definition here.
       ; 03-21-04. Put a dictionary reference in 
       (SL-construct-output-base bufout base key subkey dict)
      )
      (setq processed t) ; so next variation will be skipped
     )

     (when (and ok (not processed))
      (let (val1 val key subkey subkey1 keyval subkey2 val2 Xvals Xval)
       (condition-case err
        (progn
         (setq val (construct-subanta1 subanta gender form dbg))
	 (setq ok (if val t nil))
         (when dbg
          (fol-msg (format "construct-subanta1 returns: %s %s %s -> %s\n" subanta gender form val))
         )
         ; val has form ( (type form) (M array F array N array))
         ; where type is one of NOUN ADJ PRON etc
         ; and form has many values.
         ; e.g. for 'raamaH' ((M a) (M [raamaH ...] 
         ; Each array is a declension table
         (setq subkey1 (elt val 0)) ; (type form)
         (setq val1 (elt val 1)) ; (M array F array N array)
	 (setq ok nil)
         (mapcar
	  (lambda (gender)
	   (setq val2 (plist-get val1 gender))
	   (when val2
            (setq key subanta)
	    (setq subkey2 (cons 'S subkey1)) ; (subanta <type> <form>)
	    (setq subkey (append subkey2 (list gender)))
	    (setq keyval val2)
	    (SL-construct-output1 bufout key subkey keyval dict)
	    (setq ok t)
	   )
	  )
	  '(M F N)
         )
        )
	(error (setq ierr 'subanta1) (setq ok nil))
       )
       )
      )
      (when (not ok)
       (fol-msg (format "error (%s) #%s: %s\n" ierr (1- irec) fields))
       (setq ok t) ; keep processing
      )
     )
   (when nil
    (when (< 10 irec)
     (setq ok nil)
     (fol-msg (format "stopping after 10 records\n"))
    )
   )
  )
  )
  ; 2.
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)   
  )
;  (fol-msg (format "length=%s\n" (length subantas)))
  t
 )
)
(defun convert-subanta-lexinfo (subanta lexinfo)
 (let (forms form gender)
  ;(fol-msg (format "convert-subanta-lexinfo: subanta=%s, lexinfo=%s\n" subanta lexinfo))
    (let (formin tmp formsin forminfo)
     (setq forminfo (cdr lexinfo))
     (setq formin (elt forminfo 0))
     (setq formin (intern (upcase (symbol-name formin))))
     (cond
      ((equal formin 'MF) (setq formsin '(M F)))
      ((equal formin 'MN) (setq formsin '(M N)))
      ((equal formin 'FN) (setq formsin '(F N)))
      ((equal formin 'MFN) (setq formsin '(M F N)))
      ((equal formin 'ADJT) (setq formsin (list 'ADJt)))
      (t (setq formsin (list formin)))
     )
     (when (equal formin 'ADJ)
      (setq tmp (subanta-convert-form subanta formin))
      (if tmp
       (progn
        (setq form (elt tmp 1))
	(setq forms (list (list form formin)))
	(setq formsin nil) ; inhibit next phase attempt
       )
       (progn ; arrange to try as MFN
	(setq formsin '(M F N))
       )
      )
     )
     
     (while formsin
      (setq formin (car formsin))
      (setq formsin (cdr formsin))
      (setq tmp (subanta-convert-form subanta formin))
      (if (member formin '(M F N))
       (setq gender formin)
       (setq gender (elt tmp 0))
      )
      (setq form (elt tmp 1))
      (setq forms (cons (list form gender) forms))
     )
    )
  forms
 )
)
(defun subanta-convert-form (subanta formin)
 (let (tok len lc form type subanta-type-data monosyllabicp)
  ;(fol-msg (format "subanta-convert-form subanta=%s, formin=%s\n" subanta formin))
  (setq subanta-type-data (init-subanta-type-data))
  (setq form formin)
  (setq type (plist-get subanta-type-data subanta))
  (setq tok (car (ITRANS-parse-words-1 (symbol-name subanta))))
  (setq lc (elt (substring tok -1) 0)) ; last character
  (setq len (length tok))

  (when type
   ; must do some adjustments
;   (fol-msg (format "CHK: %s %s %s\n" lc form type))
   (cond
    ((and (equal lc 'a)
	  (member form '(M F N ADJ))
	  (member type '(c d))
     )
     (setq type 'a)
    )
    ((and (equal subanta 'sama) (equal formin 'ADJ))
     (setq type nil) ; so normal adjective processing will occur
    )
   )
  )
  (when (not type)
;   (setq N-flag (sandhi-n-N tok)) ; true if endings with 'N' are needed
   (let (tmp types parts)
    (setq tmp (word-parts tok))
    (setq parts (elt tmp 0))
    (setq types (elt tmp 1))
    (setq monosyllabicp (equal (length types) 2))
   )
   (cond
   ((and (member form '(NOUN M F)) (equal lc 'Ri)) ; noun ending in 'Ri
    ; enter 'Ri-R' (relation type in 'subanta-type-init')
    (setq type 'Ri-A)
    (setq form 'NOUN)
   )
   ((and (member form '(NOUN N)) (equal lc 'Ri)) ; neuter noun ending in 'Ri
    (setq type lc)
    (setq form 'NOUN)
   )
   ((and (equal form 'ORD) (member subanta '(prathama dvitiiya tRitiiya)))
    (setq form 'PRON)
    (setq type 'ORDa)
   )
   ((and (equal form 'ORD) (member subanta '(turiiya turya)))
    (setq form 'PRON)
    (setq type 'ORDb)
   )
   ((equal form 'ORD)
    (setq form 'PRON)
    (setq type 'ORD)
   )
   ((equal form 'CARD)
    (setq form 'PRON)
    (setq type 'CARD)
   )
   ((equal form 'ADJI)
    (setq form 'ADJ)
    (setq type 'aI)
   )
   ((equal form 'ADJt)
    (setq form 'ADJ)
    (setq type 'at)
   )
   ((and (<= 4 len)
	 (member (substring tok -3) '([m a n] [v a n] ))
    )
    ; 03-21-04
    (if (consonant-P (elt (substring tok -4 -3) 0))
     (setq type 'manC)
     (setq type 'manV)
    )
   )
   ((and (<= 2 len)
	 (member (substring tok -2) '([i n] [a n] [a ch] [aa ch])))
    (setq type (sym-without-space (substring tok -2)))
   )
   ((and (<= 3 len)
	 (member (substring tok -3) '([m a t] [v a t] ))
    )
;    (setq type 'at) ; 03-19-04
    (setq type 'mat) ; 06-20-04
   )
   ((and (<= 5 len)
	 (member (substring tok -5) '([m a h a t] ))
    )
    (setq type 'mat) ; 05-20-04
   )
   ((and (<= 3 len)
	 (member (substring tok -3) '([v a s] ))
	 (not (member subanta '(dhvas vishravas))) ;vishravas
    )
    (setq type (sym-without-space (substring tok -3)))
   )
   ((and (<= 4 len)
	 (or 
	  (member (substring tok -4)
		 '([ii y a s] [e y a s] [uu y a s]))
	  (equal tok [j y aa y a s])
	 )
     )
    ; 4-19-04: vihaayas does not decline like this, I think, but as '1cons'
    ; for 'jyAyas', see Antoine 1, p. 103.
    ; There are some adjectives in 'DAyas', I think those are '1cons'.
    (setq type 'iiyas)
   )
   ((and (member form '(M F N)) (consonant-P lc))
    (setq type '1cons)
   )
   ((and (string-match 
	  (concat (make-regexp '(Nii nii dhii krii hrii bhii shrii)) "$")
	  (symbol-name subanta)
	 )
	 (or (and (equal form 'F) monosyllabicp)
	     (equal form 'M)
	 )
     )
    ; root nouns ending in 'ii' (M or F)
    (setq type 'ii1)
   )
   ((and (equal form 'M) (equal lc 'ii))
    ; kaumudii
    (setq type 'ii1)
   )
   ((and (string-match 
	  (concat (make-regexp '(bhuu bruu luu)) "$")
	  (symbol-name subanta)
	 )
	 (or (and (equal form 'F) monosyllabicp)
	     (equal form 'M)
	 )
     )
    ; root nouns ending in 'uu' (M or F)
    (setq type 'uu1)
   )
   ((vowel-P lc)
    (setq type (sym-without-space (vector lc)))
   )
   (t
    (setq type nil)
   )
   )
  )
  (when (equal form 'CARD) 
   (setq form 'PRON)
   (when (not (equal type 'IRR)) (setq type 'CARD))
  )
  (if type
   (list form type)
  )
 )
)
(defun init-subanta-type-data ()
 '(  ; a plist 
   kiyat vat
   iyat  vat
   mahat mat

;    danta IRR
;    paada IRR
;    maasa IRR
;    nishaa IRR
;    naasikaa IRR
;    kroShTu IRR
;    pathin IRR

;   ahan IRR
 ;  shvan IRR
;   yuvan IRR
;   maghavan IRR
;   vRitrahan IRR
   dos IRR
   aashis IRR
   puMs IRR
   anaDuh IRR
   ap IRR

;   dhii ii1
;   shrii ii1
;   hrii ii1
;   bhii ii1

   vaatapramii ii0 ; M nouns in 'ii'- Kale #76, p.42
   yaayii ii0
   papii ii0

;   strii ii2

;   lakShmii ii3
;   tantrii ii3
;   tarii ii3

;   bhuu uu1
;   bhruu uu1
   khalapuu uu1

   asmad IRR
   yuShmad IRR
   bhavat vat
   idam IRR
   adas IRR

;   eka IRR
   eka adj  ; treated as a pronominal adjective
   dvi IRR
   tri IRR
   chatur IRR
   ShaSh IRR
   aShTan IRR
   ; pa~nchan IRR
   yad b
   tad b
   etad b
   kim b
   tyad b

   pitRi Ri-R
   bhraatRi Ri-R
   jaamaatRi Ri-R
;   bhartRi Ri-R
   maatRi Ri-R
   duhitRi Ri-R
   nanaandRi Ri-R
   nanandRi Ri-R
   nRi Ri-R
   yaatRi Ri-R ; Kale p. 50 (a husband's brother's wife)
   devRi Ri-R ; a husband's brother (esp. younger)
   kiyat vat
   iyat vat

   mahat mat
   vaach 1cons

   ; sarva and pronominal adjectives like it have form 'adj'
   sarva adj
   vishva adj
   sama adj
   sima adj
   ubha adj
   ubhaya adj
;   sva adj
;   antara adj

   ; anya, and others like it, have type 'a'. This is almost
   ; same as 'sarva', except neuter 1S, 2S (and vS) which end in 'at'
   anya a
   anyatara a
   itara a
   katara a
   katama a
   ekatama a

   ;Antoine2#174, p. 114
   ;like 'sarva' except that in the following forms they optionally
   ;have a form like nouns in 'a':
   ; m5s m7s m1p n5s n7s 
   puurva c
   avara c
   dakShiNa c
   uttara c
   adhara c
   para c
   apara c
   antara c
   sva c

   ;Kale 152, p. 99. 'nema' (half) is a pronoun, but it is optionally
   ; declined like 'rAma' in the nom. pl. mas.; in other respects it
   ; is declined like 'sarva'
   nema d
  )
)

(defun SL-construct-output-base (bufout bases key subkey &optional dict)
 (let (key1)
  (setq bases (translate-ITRANS-SLP1 bases))
  (setq key (translate-ITRANS-SLP1 key))
  (if (not (listp bases)) (setq bases (list bases)))
  (mapcar
   (lambda (base)
    (setq key1 (replace-prefix base key '=))
    (with-current-buffer bufout
     (insert (format "%s %s %s %s %s\n" base key1 dict subkey 'B))
    )
   )
   bases
  )
 )
)
(defun SL-construct-output1 (bufout key subkey keyval &optional dict)
 (let (Xvals Xval)
  (setq key (translate-ITRANS-SLP1 key))
;  (setq subkey (translate-ITRANS-SLP1 subkey))
  (setq Xvals (Xalgorithm keyval))
  (setq keyval (translate-ITRANS-SLP1 keyval))
  (setq Xvals (translate-ITRANS-SLP1 Xvals))
  (if (equal (length Xvals) 1)
   ; simple prefix
   (with-current-buffer bufout
    (setq Xval (car Xvals))
    (let (key1 keyval1)
     (setq key1 (replace-prefix Xval key '=))
     (setq keyval1 (replace-prefix Xval keyval '=))
     (insert (format "%s %s %s %s %s\n" Xval key1 dict subkey keyval1))
    )
   )
   ; no simple prefix.
   ; Make each non-nil elt of keyval a search elt.
   ; assumes keyval is an array
   (with-current-buffer bufout
    (let (n i x y)
     (setq n (length keyval))
     (setq i 0)
     (while (< i n)
      (setq x (elt keyval i))
      (if (not (listp x)) (setq x (list x)))
      (while x ; skip nil
       (setq y (car x))
       (setq x (cdr x))
       (insert (format "%s %s %s %s %s\n" y key dict subkey i ))
      )
      (setq i (1+ i))
     )
    )
   )
  )
 )
)
(defun replace-prefix0 (pfx val r)
 "Assume arguments are symbols.
  When, using the associated strings, pfx is the initial segment of
  val, replace this initial segment with 'r', then return the associated
  symbol. For instance,
  pfx = 'rAm', val = 'rAmasya', r = '=' -> '=asya'.
  If pfx is not the initial segment of val, return val
 "
 (let (ans-s)
  (setq ans-s (replace-prefix0-string 
   (symbol-name pfx) (symbol-name val) (symbol-name r)))
  (if ans-s
   (intern ans-s)
  )
 )
)
(defun replace-prefix (pfx vals r)
 (mapcar-LE 
  (lambda (val)
   (replace-prefix0 pfx val r)
  )
  vals
 )
)
(defun replace-prefix0-string (pfx val r)
 "Assume arguments are strings.
  When pfx is the initial segment of
  val, replace this initial segment with 'r', and return result.
  pfx = 'rAm', val = 'rAmasya', r = '=' -> '=asya'.
  If pfx is not the initial segment of val, return val
 "
 (let (n sfx)
  (setq case-fold-search nil)
  (if (equal (string-match pfx val) 0)
   (progn
    (setq n (length pfx))
    (setq sfx (substring val n))
    (concat r sfx)
   )
   val
  )
 )
)

(defun construct-output-base (bufout bases key subkey)
 (let ()
  (if (not (listp bases)) (setq bases (list bases)))
  (when (not bases)
   (fol-msg (format "missing base: %s %s\n" key subkey))
  )
  (mapcar
   (lambda (base)
    (with-current-buffer bufout
     (insert (format "%s %s %s %s\n" base key subkey 'B))
    )
   )
   bases
  )
 )
)
(defun construct-output1 (bufout key subkey keyval)
 (let (Xvals Xval)
	  (setq Xvals (Xalgorithm keyval))
	  (if (equal (length Xvals) 1)
	   ; simple prefix
           (with-current-buffer bufout
	    (setq Xval (car Xvals))
	    (insert (format "%s %s %s %s\n" Xval key subkey keyval))
           )
	   ; no simple prefix.
	   ; Make each non-nil elt of keyval a search elt.
	   ; assumes keyval is an array
	   (with-current-buffer bufout
	    (let (n i x y)
	     (setq n (length keyval))
	     (setq i 0)
	     (while (< i n)
	      (setq x (elt keyval i))
	      (if (not (listp x)) (setq x (list x)))
	      (while x ; skip nil
	       (setq y (car x))
	       (setq x (cdr x))
	       (insert (format "%s %s %s %s\n" y key subkey i))
	      )
	      (setq i (1+ i))
	     )
	    )
           )
	  )
 )
)
(defun construct-subanta-base (subanta gender form)
 ; this will return 'nil' if the declension is not known
 ; to be formed by concatenating (without sandhi) a base
 ; with a collection of endings
 ; subanta, gender and form are all symbols
 (let ()
  (cond
   ((sanget 'Subanta-irreg subanta) nil)
   ((not (member (list gender form)
			'((M a) (N a) (F aa) (ADJ a)
			  (M i) (N i) (F i) (ADJ i)
		          (M u) (N u) (F u) (ADJ u)
			  (M at) (N at) (F at) (ADJ at)
			  (F ii)
			  (ADJ Ri) (N Ri) (M Ri-A) (F Ri-A)
			  (M Ri-R) (F Ri-R)
			  (M in) (N in) (F in) (ADJ in)
			  (ADJ manC) (ADJ manV)
			  (M manC) (F manC) (N manC)
			  (M manV) (F manV) (N manV)
			  (ADJ aI)
			  (F ii)
			 ))
    )
    nil
   )
   (t
    (subanta-base subanta gender form)
   )
  )
 )
)
(defun subanta-base (citation-sym gender form &optional dbg)
 (let (praatipadikas citation-tok n gf ans len )
  (setq gf (list gender form))
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq len (length citation-tok))

  (setq  praatipadikas (cond
   ((<= len 1) nil)
   ((member form '(manC manV))
    ; 03-21-04
    ; the base drops last 2 chars 'an'
    (list (sym-without-space (substring citation-tok 0 -2)))
   )
   ((member gf '((M ii) (M ii1)))
    ; Antoine2, Section 12.
    ; Kale #77 p.43
    (let (tok1 sym1 sym2 tok2 last1)
     (setq tok1 (substring citation-tok 0 -1)) ; remove [ii]
     (setq sym1 (sym-without-space tok1))
     (setq last1 (elt (substring tok1 -1) 0))
     (cond
      ((equal (length tok1) 1)
       (setq tok2 (vconcat tok1 [i y]))
      )
      ((equal gf '(M ii1))
       (cond
	((member last1 '(n N))
          ; senaaniiH -> senaany, agraNiiH -> agraNy
         (setq tok2 (vconcat tok1 [y]))
        )
	(t
         (setq tok2 (vconcat tok1 [i y]))
	)
       )
      )
      (t
       ; sudhiiH -> sudhiy
       (setq tok2 (vconcat tok1 [i y]))
      )
     )
     (setq sym2 (sym-without-space tok2))
     (list sym1 sym2)
    )
   )
   ((member gf '((F ii1) (F ii2) (F uu1)
		  (M uu1) (M uu)))
    ; assumes 'monosyllabic for F' has been checked.
    ; See function 'subanta-convert-form' in 'construct.el'.
    ; Antoine2, Section 24. Monosyllabic Feminine nouns in 'ii' and 'uu' are 
    ; referenced as of type 'ii1' or 'uu1'. They are declined
    ; (a) before terminations beginning with a vowel, like stems
    ;     ending with a consonant 
    ; (b) and before consonant terminations like 'nadii' or 'vadhuu'
    ; However, for cases 4-7 before terminations beginning with a vowel, they
    ; have an optional form using the consonantal stem with the
    ; terminations of 'nadii' or 'vadhuu'
    ; I give the citation form as 'dhii' or 'bhuu'; Apte dictionary gives the
    ; citation form as the nominative singular (e.g. 'dhiiH' or 'bhuuH')
    ; Section 25. monosyllaabic feminine noun 'strii' (type ii2)
    ; This has no visarga in 1S. It takes the terminations of 'nadii' with
    ; the consonantal stems before terminations beginning with vowels. However,
    ; it has optional forms in cases 2S and 2P. 

   (let (tok1 sym1 sym2 tok2 last)
     (setq last (elt (substring citation-tok -1) 0))
     (setq tok1 (substring citation-tok 0 -1)) ; remove [ii] or [uu]
     (setq sym1 (sym-without-space tok1))
     (cond
      ((equal last 'ii)
       (setq tok2 (vconcat tok1 [i y]))
      )
      ((equal last 'uu)
       (setq tok2 (vconcat tok1 [u v]))
      )
     )
     (setq sym2 (sym-without-space tok2))
     (list sym1 sym2)
    )
   )
   ; ((member gf '((M ii1) (M ii2) (M uu1) (M uu)))
;     ; Kale #77, p. 43. Root nouns in 'I' or 'U' m.f.n.
;     ; (a) The ending 'i' or 'u', short or long, of nouns derived from
;     ;    roots with the affix 'kvip' (o) and of the noun 'bhuu', is
;     ;    changed to 'iy' or 'uv' before the vowel terminations (Pan. VI.4.77).
;     ;    The feminine nouns of this description are optionally declined
;     ;    like 'nadii' in the Da. Ab. Gen. and Loc. singulars, and Gen. plur.
;     ; (b) BUT, if the ending 'i' or 'u' of a many-voweled noun having a
;     ;    root at the end
;     ;    - be not preceded by a radical conjunct consonant, or
;     ;    - the root noun is preceded by 
;     ;      -  a preposition termed 'gati' (i.e., as added to the root)
;     ;      -  or a word governed by the root
;     ;    then the ending 'i' or 'ii' is changed to 'y', and
;     ;         the ending 'u' or 'uu' is changed to 'v'.
;     ;   However, this does not apply to the noun 'sudhii' or
;     ;     to nouns ending in 'bhuu'.

;    (let (tok1 sym1 sym2 tok2 last)
;      (setq last (elt (substring citation-tok -1) 0))
;      (setq tok1 (substring citation-tok 0 -1)) ; remove [ii] or [uu]
;      (setq sym1 (sym-without-space tok1))
;      (cond
;       ((equal last 'ii)
;        (setq tok2 (vconcat tok1 [i y]))
;       )
;       ((equal last 'uu)
;        (cond
; 	((equal (substring citation-tok -2) [bh uu])
;          (setq tok2 (vconcat tok1 [u v]))
; 	)
; 	((and (<= 3 len) (equal (substring citation-tok  -3) [b r uu]))
; ;         (setq tok2 (vconcat tok1 [u v]))
; 	)
; 	((equal (length citation-tok) 2)
;          ;(setq tok2 (vconcat tok1 [u v]))
; ;	 (setq tok2 tok1)
; 	)
; 	(t
; ;	 (setq tok2 (vconcat tok1 [v]))
; 	)
;        )
;       )
;      )
;      (if tok2
;       (progn
;        (setq sym2 (sym-without-space tok2))
;        (list sym1 sym2)
;       )
;       (list sym1)
;      )
;     )
;    )
   ((and (equal form 'IRR)
	 (member citation-sym '(dos aashis))
    )
    ; Antoine volume 2: Chapter 8
    (let (tok1 sym1 sym2 tok2 last)
     (setq last (elt (substring citation-tok -1) 0))
     (setq tok1 (substring citation-tok 0 -1)) ; remove [ii] or [uu]
     (setq sym1 citation-sym)
     (setq tok2 (vconcat tok1 [H]))
     (setq sym2 (sym-without-space tok2))
     (list sym2)
    )
   )
   ((and (equal form 'IRR)
	 (equal citation-sym 'ap)
    )
    ; Antoine volume 2: Chapter 8
    (list  'aap)
   )
   ((equal citation-sym 'go)
    ; Antoine2, Section 54: go 
    ; (Kale) vrddhi ('au') is substituted for  'o'  of words ending
    ; in 'o' in the first five inflections (and in 8th), except for
    ; 2S and 2P, where 'aa' is substuted. 
    ; Nouns ending in 'au' are regularly declined.
    ; The 'normal' endings are used.
    (list 'g)
   )
   ((and (equal citation-sym 'sat) (equal gender 'ADJ))
    '(sat sant W)
   )
   ((and (equal citation-sym 'asat) (equal gender 'ADJ))
    '(asat asant W)
   )
   ((equal form 'at) ; 4-14-04
    (list (sym-without-space (substring citation-tok 0 -2)))
   )
   ((member gf gender-form-data-2)
    (list (sym-without-space (substring citation-tok 0 -2)))
   )
   ((member gf gender-form-data-1)
    (list (sym-without-space (substring citation-tok 0 -1)))
   )
   ((member gf gender-form-data-4)
    (list (sym-without-space (substring citation-tok 0 -4)))
   )
   ((member gf gender-form-data-3)
    (if (<= 3 (length citation-tok)) ; 'ach' forms
     (list (sym-without-space (substring citation-tok 0 -3)))
     nil
    )
   )
   (t citation-sym) ; possibly should be nil
  ))
  (setq ans (solution praatipadikas))
  (when nil ;
   (fol-msg (format "chk (subanta-base): %s %s %s -> %s\n"
	    citation-sym gender form ans))   
  )
  ans
 )
)

(defun construct-subanta1a (subanta formin gender)
 ; Used by SL-dtab-construct in 'validation.el'
 (let (form1 dtab1 gen-or-type dtab form)
  (let (tmp)
   (if (equal formin 'NOUN)
    (setq tmp (subanta-convert-form subanta gender))
    (setq tmp (subanta-convert-form subanta formin))
   )
   (setq form1 (elt tmp 1))
   (setq form (elt tmp 0))
  )
  (setq gen-or-type
   (cond
    ((equal form 'NOUN) gender)
    (t form)
   )
  )
  (when  nil
   (fol-msg (format "chk: %s %s %s -> %s %s %s\n"
		subanta formin gender  subanta gen-or-type form1))
  )
  (setq dtab1 (construct-subanta1 subanta gen-or-type form1))
  (when nil ; dbg
   (fol-msg (format "chk1: %s %s %s -> %s %s %s\n"
		subanta formin gender  subanta gen-or-type form1))
   (fol-msg (format "dtab1=%s\n" dtab1))
  )
  ; dtab1 = ((form form1) (M dtab-M F dtab-F N dtab-Nn))
  (setq dtab (plist-get (elt dtab1 1) gender))
 )
)
(defun construct-subanta1 (subanta gen-or-mtype form &optional dbg)
 ;(setq dbg t)
 (when dbg
  (fol-msg (format "construct-subanta1: %s %s %s\n"
		   subanta gen-or-mtype form))
 )
 (let (ans new-praatipadikas  gender thisans info info1 type dtab
	   M1P-opt)
  (let (tok)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name subanta))))
   (when (or (member subanta '(prathama charama alpa ardha katipaya))
	     (and (<= 4 (length tok))
		  (equal (substring tok -4) [t a y a])
	     )
	 )
    ;Kale 156, p. 190. In this case, the nom. plu. is optionally formed
    ;like that of pronouns. 'aaH' is optionally replaced by 'e'
    ; Note: This applies only in M case (I think)
    (setq M1P-opt (sym-without-space (vconcat (substring tok 0 -1) [e])))
   )
  )
  (if (and (equal gen-or-mtype 'ADJ) (equal form 'at))
   (setq info (construct-subanta2-ADJ-at subanta gen-or-mtype form)) 
   (setq info (construct-subanta2 subanta gen-or-mtype form dbg))
  )
  (when dbg
   (fol-msg (format "construct-subanta1: info=%s\n" info))
  )
  (if (member gen-or-mtype '(M F N))
   (setq type 'NOUN)
   (setq type gen-or-mtype)
  )
  
  (while info
   (setq info1 (car info))
   (setq info (cdr info))
   (cond
    ((and (equal gen-or-mtype 'PRON) (equal subanta 'bhavat))
     (let (info2)
      (setq info2 (list (elt info1 0) (elt info1 1) (elt info1 2)))
;      (fol-msg (format "chk: %s\n" (cons subanta info2)))
      (setq dtab (apply 'declension-pron (cons subanta info2)))
     )
    )
    (t
     (let (args)
      (setq args (cons subanta info1))
      (setq args (append args (list dbg)))
      (setq dtab (apply 'declension-citation1 args))
     )
    )
   )
   (when dbg
    (fol-msg (format "chk1: %s -> %s\n" (cons subanta info1) dtab))
   )
   (setq gender (elt info1 1))
   (when M1P-opt 
    (let (old new)
     (setq old (elt dtab 2))
     (if (not (listp old)) (setq old (list old)))
     (setq new (append old (list M1P-opt)))
     (aset dtab 2 new)
    )
   )
   (setq thisans (list gender dtab))
   (setq ans (append ans thisans))
   (when dbg
    (fol-msg (format "construct-subanta1\n  thisans=%s\n  ans=%s\n\n" thisans ans))
   )
  )
  (setq ans (list (list type form) ans))
 )
)
(defun construct-subanta2 (citation-sym  genderin form &optional dbg)
 (let (genders gender iform irregs x-new other-info praatipadikas)
  (when dbg ; dbg
   (fol-msg (format "construct-subanta2: %s %s %s\n" citation-sym  genderin form))
  )
  (setq praatipadikas
     (subanta-base citation-sym genderin form dbg))
  (when dbg
   (fol-msg (format "construct-subanta2: %s %s %s, praatipadikas=%s \n" citation-sym  genderin form praatipadikas))
  )
  (when (and (equal citation-sym 'bhavat) (equal genderin 'PRON))
    (setq praatipadikas 'bhav)
  )
  (setq gender genderin)
  (if (member gender '(M F N))
   (setq genders (list gender))
   (progn
    (setq genders '(M F N))
    ; PRON IRR -> IRR-PRON
;    (setq form (intern-soft (format "%s-%s" form gender)))
    (setq form (intern (format "%s-%s" form gender)))
    (when (equal form 'vat-PRON) ; case of 'bhavat'
     (setq form 'vat)
    )
   )
  )
  (while genders
   (setq gender (car genders))
   (setq genders (cdr genders))
   (setq iform (intern-soft (format "%s-%s" gender form)))
   (setq irregs (sanget2 'Subanta-irreg (list citation-sym iform)))
   
   (setq x-new (list praatipadikas gender form irregs))
   (setq other-info (append-if-new other-info x-new))
  )
  (when dbg ; dbg
   (fol-msg (format "construct-subanta2: %s %s %s -> %s\n"
		    citation-sym  genderin form other-info))
  )
  other-info
 )
)
(defun construct-subanta2-ADJ-at (citation-sym  genderin form &optional dbg)
 (let (genders gender iform irregs x-new other-info praatipadikas 
       citation-tok len strong weak SW)
  (setq genderin 'ADJ)
  (setq form 'at)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq len (length citation-tok))

  (when dbg
   (fol-msg (format "construct-subanta2: %s %s %s\n"
		    citation-sym  genderin form))
  )
  (setq weak citation-sym)
  (setq strong (sym-without-space
		(vconcat (substring citation-tok 0 -1) [n t])))
  (setq SW 'S)
  (setq praatipadikas
   (list
    (list weak  strong SW)
   )
  )
  (setq genders '(M F N))
  
  (while genders
   (setq gender (car genders))
   (setq genders (cdr genders))
   (setq iform (intern-soft (format "%s-%s" gender form)))
;   (setq irregs (sanget2 'Subanta-irreg (list citation-sym iform)))
   
   (setq x-new (list praatipadikas gender 'at-ADJ irregs))
   (setq other-info (append-if-new other-info x-new))
  )
  (when nil ; dbg
   (fol-msg (format "construct-subanta2: %s %s %s -> %s\n"
		    citation-sym  genderin form other-info))
  )
  other-info
 )
)
(defun SL-construct-subanta1 (SL-subanta SL-gender &optional SL-form dbg)
 "SL-subanta is a citation form in SLP1 transliteration (a symbol)
  SL-gender is a lower case 'm f n'  (a symbol)
  SL-form (if present) is a string

 "
 ;(setq dbg t)
 (when dbg
  (fol-msg (format "SL-construct-subanta1: %s %s %s\n"
		   SL-subanta SL-gender SL-form))
 )
 (let (subanta gender form ans1 ans temp1 temp x)
  (setq subanta (translate-SLP1-ITRANS SL-subanta))
  (when nil
   (fol-msg (format "SL-subanta=%s -> subanta=%s\n" SL-subanta subanta))
  )
  (setq gender (intern (upcase (symbol-name SL-gender))))
  (if (equal gender 'ADJT) (setq gender 'ADJt))
  (setq form SL-form)
  (when (not form)
   (let (forms)
    (setq forms (convert-subanta-lexinfo subanta (list 'S gender)))
    (setq form (caar forms))
    (cond
     ((member form '(manC manV)) (setq form 'an))
     ((equal gender 'ADJt) (setq gender 'ADJ))
     ((equal form 'at) (setq form 'mat))
    )
   )
   (when dbg
    (fol-msg (format "SL-construct-subanta1: %s %s %s -> %s %s -> %s\n" 
              SL-subanta SL-gender SL-form subanta gender form))
   )
  )
  (setq ans1 (construct-subanta1 subanta gender form dbg))
  (when dbg; dbg
   (fol-msg (format " from construct-subanta1: %s %s %s -> %s\n"
		    subanta gender form ans1))
  )
  (when ans1
   ; (type form) e.g. (NOUN a) -> (noun a)
   (setq temp1 (car ans1))
   (setq ans1 (cdr ans1))
   (setq temp (intern (downcase (symbol-name (car temp1))))) ; NOUN -> noun
   (setq ans (cons (list temp (elt temp1 1)) ans))
   ; plist of values
   (setq temp1 (car ans1)) ; a plist (M [val] F [val] N [val])
   (setq temp nil)
   (while temp1
    ; gender - change to lower case
    (setq x (car temp1))
    (setq temp1 (cdr temp1))
    (setq x (intern (downcase (symbol-name x))))
    (setq temp (cons x temp))
    ; value - translate from ITRANS to SLP1
    (setq x (car temp1))
    (setq temp1 (cdr temp1))
    (setq x (mapcar-LE 'translate-ITRANS-SLP1 x))
    (setq temp (cons x temp))
   )
   (setq temp (nreverse temp))
   (setq ans (cons temp ans))
   (setq ans (nreverse ans))
  )
  ans
 )
)

(defun construct-subanta-all ()
 (let (files)
  ; files = ("subanta.txt" "subanta1.txt" ... )
  (setq files (all-subanta-inputs))
  (mapcar 
   (lambda (file)
    (construct-subanta file)
   )
   files
  )
 )
)

(defun construct-dcpforms (intab &optional outtab indir outdir)
 ;01-23-04.
 ;03-23-04. 'intab' is a file of input verb forms,
 ;  like 'inputs/mw-verb.txt'. 
 ; A sample line is 'kFt : V 10 P A : <MW=kFt,74067,1>'
 (let (fileout bufout filein bufin  more)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "dcpforms.txt"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (if (not indir) (setq indir "inputs"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char (point-min))
   (setq more t)
   (while (and more (< (point) (point-max)))
    (condition-case err
     (let (forms form name n i k newforms s fields dhaatu forms dict pdhaatu
	    formsin cps upasargas)
      ; assume data like 'kFt : V 10 P A : <MW=kFt,74067,1>'
      (setq fields (read-colon-line-fields (current-line) ':))
      (setq pdhaatu (elt fields 0)) ; a list of symbols, last is dhaatu
      (let (e)
       (while pdhaatu
        (setq e (car pdhaatu))
        (if (not (cdr pdhaatu)) ; last element
         (setq dhaatu e)
	 (setq upasargas (cons e upasargas))
        )
        (setq pdhaatu (cdr pdhaatu))
       )
       (setq upasargas (nreverse upasargas))
      )
      (setq formsin (cdr (elt fields 1))) ; skip 'V'
      (setq dict (solution (elt fields 2)))
      (setq cps (normalize-class-padas formsin)) ; ((class pada)...)
      (when nil; dbg
       (fol-msg (format "fields=%s\n" fields))
       (fol-msg (format "cps=%s\n" cps))
      )
      (mapcar
       (lambda (cp)
	(with-current-buffer bufout
         (insert (format "%s\n" (append (cons dhaatu cp) (list dict))))
	)
       )
       cps
      )
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
    (forward-line)
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)
  )
  (with-current-buffer bufin
   (kill-buffer nil)
  )
  t
 )
)

(defun additional-preverbforms (intab intab1 outtab &optional outdir)
 (let (fileout bufout filein bufin  more s s1 bufin1 filein1 found
       nfound indir)
  (setq nfound 0)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "dcpforms.txt"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (if (not indir) (setq indir "construct"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't))
  (setq filein1 (sangram-filename intab1 indir))
  (setq bufin1 (find-file-noselect filein1 't))
  (with-current-buffer bufout
   (erase-buffer)
  )
  (with-current-buffer bufin1
   (goto-char 1)
   (while (< (point) (point-max))
    (setq s (current-line))
    (forward-line)
    (setq s1 (substring s 0 (string-match "<" s)))
    (save-excursion
     (with-current-buffer bufin
      (goto-char 1)
      (setq found (search-forward-regexp (concat "^" s1) nil t))
     )
    )
    (when (not found)
     (setq nfound (1+ nfound))
     (message (format "%s" nfound))
     (save-excursion
      (with-current-buffer bufout
       (insert (format "%s\n" s))
      )
     )
    )
    
   )
  )
  
  ;post-processing
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)
  )
  (fol-msg (format "%s new verb forms found\n" nfound))
  t
 )
)
(defun merge-dcpforms (outtab intabs &optional indir outdir)
 ;03-23-04. merge files created by 'construct-dcpforms', removing
 ; duplicate lines.
 (let (fileout bufout filein bufin  more intab s s1 ndup)
  (setq ndup 0)
  (if (not outdir) (setq outdir "construct"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (if (not indir) (setq indir "construct"))
  (with-current-buffer bufout
   (erase-buffer)
   (while intabs
    (setq intab (car intabs))
    (setq intabs (cdr intabs))
    (setq filein (sangram-filename intab indir))
    (insert-file filein)
    (goto-char (point-max))
   )
   (setq ndup (remove-partial-duplicates "<"))
   (save-buffer 0)
   (kill-buffer nil)
  )
  (fol-msg (format "%s duplicates\n" ndup))
  t
 )
)
(defun remove-partial-duplicates ( regexp)
 "Operates in current buffer.
  Sorts the lines.
  Compare consecutive lines from the beginning to point matched by 'regexp';
  delete 2nd line of the sub-parts are identical.
  Assume 'regexp' will match within each line.
  Return # of duplicates removed
 "
 (let (s s1 ndup i i1)
   (setq ndup 0)
   (sort-lines nil (point-min) (point-max))
   (goto-char 1)
   (setq s (current-line))
   (string-match regexp s)
   (setq i (match-end 0))
   (if i
    (progn
     (setq s (substring s 0 i))
     (forward-line)
    )
    (progn
     (fol-msg (format "remove-partial-duplicates (ERROR): %s %s\n"
         regexp s))
     (goto-char (point-max))
    )
   )
   (while (< (point) (point-max))
    (setq s1 (current-line))
    (string-match regexp s1)
    (setq i (match-end 0))
    (if i
     (progn
      (setq s1 (substring s1 0 i))
      (if (equal s s1)
       (progn
        (setq ndup (1+ ndup))
	(kill-line 1)
       )
       (progn
        (setq s s1)
	(forward-line)
       )
      )
     )
     (progn
      (fol-msg (format "remove-partial-duplicates (ERROR): %s %s\n"
         regexp s1))
      (goto-char (point-max))
     )
    )
    
   )
   ndup
  )
)
(defun normalize-class-padas (cps)
 "'cps' should be a list, containing numbers (1-10) and symbols 'P' 'A' 'PA'.
  A complicated assumption is made on the ordering of elements if 'cps'.
  A list is return, each-element of which is a list '(class pada)).
  Examples: (1 P) -> ((1 P))
  (1 P A) -> ((1 P) (1 A))
  (1 2 P) -> ((1 P) (2 P)) 
 "
 (let (ans cs c e cps0)
  (setq cps0 cps)
  (while cps
   (setq e (car cps))
   (cond
    ((numberp e)
     (setq cs (cons e cs))
     (setq cps (cdr cps))
    )
    ((not cs) ; error condition
     (fol-msg (format "normalize-class-padas error: %s\n" cps0))
     (setq ans nil)
     (setq cps nil)
    )
    (t
     (setq cs (nreverse cs))
     (while (and cps (not (numberp (car cps))))
      (setq e (car cps))
      (mapcar
       (lambda (c)
	(cond
	 ((member e '(P A))
	  (setq ans (cons (list c e) ans))
	 )
	 ((equal e 'PA)
	  (setq ans (cons (list c 'P) ans))
	  (setq ans (cons (list c 'A) ans))
	 )
	 (t
	  (fol-msg (format "normalize-class-padas error2: %s\n" cps0))
	  (setq cps nil)
	 )
	)
	 
       )
       cs
      )
      (setq cps (cdr cps))
     )
     (setq cs nil)
    )
   )
  )
  (nreverse ans)
 )
)
(defun extract-dcpforms (&optional outtab outdir)
 ;01-23-04.
 ;read file constructed by 'construct-dcpforms'.
 ;return list of forms in it. (dhaatu-classPada)
 (let (fileout bufout more ans)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "dcpforms.txt"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (goto-char (point-min))
   (setq more t)
   (while more
    (condition-case err
     (let (form)
      ; assume data = '<form-sym>'
      (setq form (read bufout))
      (end-of-line)
;      (setq ans (append ans (list form)))
      (setq ans (cons form ans)) ; faster
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s\n" err))
      )
     )
    )
   )
  )
  ;post-processing
  (with-current-buffer bufout
   (kill-buffer nil)
  )
  (nreverse ans) ; return to original order
 )
)


(defun construct-avyayapada-all ()
 (construct-avyayapada "avyayapada.txt")
 t
)
(defun construct-avyayapada (tabname &optional outtab outdir)
 (let (nrec irec recs fields nfields fileout bufout) 
  (if (not outtab) (setq outtab tabname))
  (if (not outdir) (setq outdir "construct"))
  (setq nfields 3)
  (setq recs (read-colon-file-table tabname nfields "inputs"))
  (setq nrec (length recs))
  ;(fol-msg (format "nrec=%s\n" nrec))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq irec 0)
  (while (< irec nrec)
   (setq fields (elt recs irec))
   (setq irec (1+ irec))
   (let (key type dict)
    ; 1a. unpack 'fields' to other variables
    ; assume fields[0] is a list of length 1 whose member is a symbol
    (setq key (elt (elt fields 0) 0))
    (setq type (elt fields 1)) ; initial elt shld be 'I'
;    (setq new-Eng-def (gather-fields (elt fields 2) ', ))
    (setq dict (solution (elt fields 2)))
    (with-current-buffer bufout
     (insert (format "%s %s %s (I) B\n"
		key "=" dict))
    )
   )
  )
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)   
  )
  t
 )
);end


(defun Sandhi-construct ()
 (Sandhi-init)
 (Sandhi-unload)
 (Sandhi-load-init)
)

(defun construct-mw-all ()
 (fol-msg (format "%s\n" (current-time-string)))
 (construct-subanta "MW-noun.txt")
 (fol-msg (format "%s\n" (current-time-string)))
 (construct-subanta "MW-adj.txt")
 (fol-msg (format "%s\n" (current-time-string)))
 (construct-avyayapada "MW-ind.txt")
 (construct-subanta "MW-PCO.txt")
 (fol-msg (format "%s\n" (current-time-string)))
; --- verbs
 (construct-dcpforms "MW-verb.txt" "dcpforms-MW-verb.txt")
 (construct-dcpforms "MW-preverb.txt" "dcpforms-MW-preverb.txt")
 (additional-preverbforms "dcpforms-MW-verb.txt"
   "dcpforms-MW-preverb.txt"
   "dcpforms-MW-additional.txt")
 (merge-dcpforms "dcpforms-MW.txt" 
  '("dcpforms-MW-verb.txt" "dcpforms-MW-additional.txt"))
 (construct-conjtabs "dcpforms-MW.txt" "conjtabs-MW.txt")

 (construct-participles "dcpforms-MW.txt" "work-participles-MW.txt")
 (construct-participles-reformat "participles-MW.txt"
   "work-participles-MW.txt")

)

(defun SL-conjtab (root class evoice upas sltense dtype &optional dbg)
 "arguments are symbols, except 'class', which is a number.
  dtype = derived root type. Meaningful values are:
   c = causal
 "
 (let (dhaatu pada upasargas tenses voice derived-type ans err
	      tense ctab)
  (when dbg
   (fol-msg (format "SL-conjtab %s\n" (list root class evoice upas sltense dtype)))
  )
  (setq dhaatu (translate-SLP1-ITRANS root))
  (setq upasargas (mapcar 'translate-SLP1-ITRANS upas))
  (cond 
   ((equal evoice 'a) (setq pada 'P) (setq voice 'ACTIVE))
   ((equal evoice 'm) (setq pada 'A) (setq voice 'ACTIVE))
   ((equal evoice 'p) (setq pada 'P) (setq voice 'PASSIVE))  ; pada irrelevant
   (t (fol-msg
       (format "SL-conjtab: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
    (setq err t)
   )
  )
  ;
  (cond
   ((equal dtype 'c) (setq derived-type 'CAUSAL))
  )
  ;
  (setq tenses (SL-tense-tran (symbol-name sltense)))
  (while (and (not err) tenses voice)
   (setq tense (car tenses))
   (setq tenses (cdr tenses))
   (setq upasargas nil)
   (setq ctab
      (construct-conjtab1a dhaatu class pada upasargas tense voice
			   derived-type dbg)
   )
   (setq ctab (solution ctab))
   (setq ctab (mapcar-LE 'translate-ITRANS-SLP1 ctab))
   (when nil ; dbg
      (fol-msg (format "construct-conjtab1a %s %s %s %s %s %s %s\n"
		dhaatu class pada upasargas tense voice derived-type))
			   
      (fol-msg (format "ctab=%s\n" ctab))
   )
   (setq ans (construct-join-arrays ans ctab))
;   (if (equal join-array-method 1)
;     (setq ans (append ans ctab))
;     (setq ans (join-arrays ans ctab))
;   )
;   (setq ans (cons ctab ans))
  )
  ans
 )
)
(defun SL-prespart-base (root class evoice)
 "arguments are symbols, except 'class', which is a number.
 "
 (let (dhaatu pada upasargas voice ans err bases)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (setq upasargas nil)
  (cond 
   ((equal evoice 'a) (setq pada 'P) (setq voice 'ACTIVE))
   ((equal evoice 'm) (setq pada 'A) (setq voice 'ACTIVE))
   ((equal evoice 'p) (setq pada 'P) (setq voice 'PASSIVE))  ; pada irrelevant
   (t (fol-msg
       (format "SL-conjtab: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
    (setq err t)
   )
  )
  ;
  (cond
   ((equal evoice 'a)
    (setq bases (construct-prespart-base-alt dhaatu class pada upasargas))
    ; bases is a list of pairs, each pair has form (<base> <strength>).
    ; <base> needs to be coded back into SLP1, but <strength> does not
    (setq ans
     (mapcar
      (lambda (x) ; x = (<base> <strength>)
       (list (translate-ITRANS-SLP1 (car x)) (cadr x))
      )
      bases
     )
    )
   )
   ((equal evoice 'm)
    (setq bases (construct-prespart-base-alt dhaatu class pada upasargas))
    (setq ans (mapcar 'translate-ITRANS-SLP1 bases))
   )
   ((equal evoice 'p)
    (setq bases (construct-passpart-base-alt dhaatu class pada upasargas))
    (setq ans (mapcar 'translate-ITRANS-SLP1 bases))
   )
  )
  ans
 )
)
(defun SL-prespart-base1 (root class evoice ctabeltin)
 "arguments are symbols, except 'class', which is a number.
 "
 (let (dhaatu pada upasargas voice ans err bases ctabelt)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (setq ctabelt (translate-SLP1-ITRANS ctabeltin))
  (setq upasargas nil)
  (cond 
   ((equal evoice 'a) (setq pada 'P) (setq voice 'ACTIVE))
   ((equal evoice 'm) (setq pada 'A) (setq voice 'ACTIVE))
   ((equal evoice 'p) (setq pada 'P) (setq voice 'PASSIVE))  ; pada irrelevant
   (t (fol-msg
       (format "SL-conjtab: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
    (setq err t)
   )
  )
  ;
  (cond
   ((equal evoice 'a)
    (setq bases (construct-prespart-base-alt-P-1 ctabelt dhaatu class))
    ; bases is a list of pairs, each pair has form (<base> <strength>).
    ; <base> needs to be coded back into SLP1, but <strength> does not
    (setq ans
     (mapcar
      (lambda (x) ; x = (<base> <strength>)
       (list (translate-ITRANS-SLP1 (car x)) (cadr x))
      )
      bases
     )
    )
   )
   ((equal evoice 'm)
    (setq bases (construct-prespart-base-alt-A-1 ctabelt dhaatu class))
    (setq ans (mapcar 'translate-ITRANS-SLP1 bases))
    (when nil
     (fol-msg (format "chk: %s %s %s -> %s %s\n"
		     ctabelt dhaatu class bases ans))
    )
   )
   ((equal evoice 'p)
    (setq bases (construct-prespart-base-alt-A-1 ctabelt dhaatu 4))
    (setq ans (mapcar 'translate-ITRANS-SLP1 bases))
   )
  )
  ans
 )
)
(defun SL-prespart-declension (root class evoice ctabeltin)
 "arguments are symbols, except 'class', which is a number.
  Returns list of pairs, where each pair has form (<gender> <dtab>),
  where <gender> is one of m/f/n,
  and <dtab> is a declension array of 24 elements or a list of such arrays,
  the individual items being in SLP1 form.
 "
 (let (dhaatu pada upasargas voice ans bases ctabelt
       genders gender ps p dtabs dtab thisans class1 pada g)
  (setq join-array-method 1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (setq ctabelt (translate-SLP1-ITRANS ctabeltin))
  (setq upasargas nil)
  ; genders ps p dtabs gender thisans
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
    (setq bases (construct-prespart-base-alt-P-1 ctabelt dhaatu class1))
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
    (setq bases (construct-prespart-base-alt-A-1 ctabelt dhaatu class1))
   )
   ((equal evoice 'p)
    (setq pada 'A)
    (setq class1 4)
    (setq bases (construct-prespart-base-alt-A-1 ctabelt dhaatu class1))
   )
   (t 
    (fol-msg
     (format "Err. arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
  (setq genders '(M F N))
  (if (not bases) (setq genders nil))
  (while genders
   (setq gender (car genders))
   (setq genders (cdr genders))
   (setq ps bases)
   (setq dtabs nil)
   (while ps
    (setq p (car ps))
    (setq ps (cdr ps))
    (setq dtab (declension-pres-part-alt p gender class1 pada))
;    (fol-msg (format "chk: %s %s %s %s -> %s\n" p gender class1 pada dtab))
    (setq dtabs (construct-join-arrays dtabs dtab))
   )
   (setq dtabs (solution dtabs))
   (setq g (intern (downcase (symbol-name gender))))
   (setq thisans (list g (translate-ITRANS-SLP1 dtabs)))
   (setq ans (cons thisans ans))
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-ppfactn (root class evoice &optional dtype)
 "construct periphrastic action noun, if one is applicable"
 (let (dhaatu pada voice ans class1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
   )
   ((equal evoice 'p)
    (setq pada 'A)
    (setq class1 class)
   )
   (t (fol-msg
       (format "SL-ppfactn: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
;  (fol-msg (format "%s %s -> %s\n"
;		   dhaatu class (periphrastic-liT-P dhaatu class)))
  (when (periphrastic-liT-P dhaatu class dtype)
   (let (ans1 ans2)
    (setq ans1 (periphrastic-base dhaatu class pada dtype))
;    (fol-msg (format "chk: %s\n" ans1))
    (if (not (listp ans1)) (setq ans1 (list ans1)))
    (while ans1
     (setq ans2 (car ans1))
     (setq ans1 (cdr ans1))
     ; convert to SLP1, add 'Am'
     (setq ans2 (translate-ITRANS-SLP1 ans2))
     (setq ans2 (sym-concat ans2 'Am))
     (setq ans (cons ans2 ans))
    )
   )
  )
  (setq ans (nreverse ans))
  (when nil ; dbg
   (fol-msg (format "SL-ppfactn: %s %s %s %s -> %s\n"
		   root class evoice dtype ans))
  )
  ans
 )
)
(defun SL-inf (root class evoice &optional dtype)
 "construct infinitive. Assume root and evoice are symbols using
  SLP conventions, class is a number, dtype, if present is one of
  the dervived type symbols. 
  The algorithm recomputes the conjugation table of the periphrastic
  future, then replaces the 'A' of the 3s with 'um'
 "
 (let (ans thisans sltense upas tok luTs luT ntok lc ctab ctabs)
  (setq sltense 'pft)
  (setq ctabs (SL-conjtab root class evoice upas sltense dtype));array list
  (setq ctabs (solution ctabs))
  (if (not (listp ctabs)) (setq ctabs (list ctabs)))
;  (fol-msg (format "ctabss=%s\n" ctabs))
  (mapcar
   (lambda (ctab) ;ctab is an array
    (setq luT (elt ctab 0)) ; a symbol or a list of symbols
    (if (not (listp luT)) (setq luT (list luT)))
    (setq luTs (append luTs luT))
   )
   ctabs
  )
;  (fol-msg (format "chk luTs: %s\n" luTs))
;  (setq luTs (elt ctab 0)) ; 3S
;  (when (not (listp luTs)) (setq luTs (list luTs)))
  (while luTs
   (setq luT (car luTs))
   (setq luTs (cdr luTs))
   ; luT is expected to end in 'A
   (setq tok (symbol-name luT)) ; string
   (setq ntok (length tok))
   ; tok ends in 
   (when (< 0 ntok)
    (setq lc (substring tok -1)) ; string consisting of last character
    (cond
     ((equal lc "A")
      ; replace "A" with "um"
      (setq thisans (concat (substring tok 0 -1) "um"))
     )
     (t ; unexpected. write message
      (fol-msg (format "SL-inf err: %s %s %s %s: %s\n"
         root class evoice dtype luT))
      (setq thisans nil)
     )
    )
   )
   (when thisans (setq thisans (intern thisans)))
   (setq ans (append-if-new ans thisans))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun SL-abs (root class evoice &optional dtype)
 "construct absolutives, for unprefixed and prefixed root"
 (let (dhaatu pada voice ans class1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
   )
   ((equal evoice 'p)
    (setq pada 'A)
    (setq class1 class)
   )
   (t (fol-msg
       (format "SL-abs: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
  ;'construct-ippart1a-tvaa' is in 'kta.el'
   (let (ans1 ans1a ans1b ans2 upasargas)
    (setq ans1a (construct-ippart1a-tvaa dhaatu class pada upasargas dtype))
    (if (not (listp ans1a)) (setq ans1a (list ans1a)))
    (setq ans1b (construct-ippart1a-ya dhaatu class pada upasargas dtype))
    (if (not (listp ans1b)) (setq ans1b (list ans1b)))
    (when (eq 0 1) ; older version
     (setq ans1 (append ans1a ans1b))
     (while ans1
      (setq ans2 (car ans1))
      (setq ans1 (cdr ans1))
      (setq ans2 (translate-ITRANS-SLP1 ans2))
      (setq ans (cons ans2 ans))
     )
    )
    (setq ans (list ans1a ans1b))
    (setq ans (mapcar-LE 'translate-ITRANS-SLP1 ans))
   )
;  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-ppp (root class evoice &optional dtype)
 "past passive participle. Returns a list of citation forms,
  e.g., (kfta) or (gata)
 "
 (let (dhaatu pada voice ans class1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
   )
   (t (fol-msg
       (format "SL-ppp: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
  ;'construct-pppart1a' is in 'kta.el'
   (let (ans1 ans2 upasargas)
    (setq ans1 (construct-pppart1a dhaatu class pada upasargas dtype))
;    (fol-msg (format "chk: %s\n" ans1))
    (if (not (listp ans1)) (setq ans1 (list ans1)))
    (while ans1
     (setq ans2 (car ans1))
     (setq ans1 (cdr ans1))
     (setq ans2 (translate-ITRANS-SLP1 ans2))
     (setq ans (cons ans2 ans))
    )
   )
  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-ppp-declension (ppp)
 "'ppp' is citation form in SLP1 format, e.g. 'kfta', 'gata'
  Returns list of pairs, where each pair has form (<gender> <dtab>),
  where <gender> is one of m/f/n,
  and <dtab> is a declension array of 24 elements or a list of such arrays,
  the individual items being in SLP1 form.
 "
 (let (ans citation ans1 ans2 dtab2 g2 dtab g thisans)
  (setq citation (translate-SLP1-ITRANS ppp))
  (setq ans1 (construct-subanta1 citation 'ADJ 'a))
  ; ans1 has form ((ADJ a) (M dtab F dtab N dtab))
  (setq ans2 (elt ans1 1))
  (while ans2
   (setq g2 (car ans2))
   (setq ans2 (cdr ans2))
   (setq dtab2 (car ans2))
   (setq ans2 (cdr ans2))
   (setq g (intern (downcase (symbol-name g2)))) ; e.g. M->m
   (setq dtab (translate-ITRANS-SLP1 dtab2))
   (setq thisans (list g dtab))
   (setq ans (cons thisans ans))
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-pap-declension (ppp)
 "'pap' is citation form in SLP1 format, e.g. 'kfta', 'gata'. Note
  That the input is same as for 'SL-ppp-declension'.
  Returns list of pairs, where each pair has form (<gender> <dtab>),
  where <gender> is one of m/f/n,
  and <dtab> is a declension array of 24 elements or a list of such arrays,
  the individual items being in SLP1 form.
 "
 (let (ans citation ans1 ans2 dtab2 g2 dtab g thisans genders)
  (let (ppp1 s s1)
   (setq s (symbol-name ppp)) ; gata
   (setq s1 (substring s 0 -1)) ; gat
   (setq ppp1 (intern s1)) ; 'gat
   (setq citation (translate-SLP1-ITRANS ppp1))
  )
  (setq genders '(M F N))
;  (setq ans1 (construct-subanta1 citation 'ADJ 'a))
  ; ans1 has form ((ADJ a) (M dtab F dtab N dtab))
;  (setq ans2 (elt ans1 1))
  (while genders
   (setq g2 (car genders))
   (setq genders (cdr genders))
   (setq dtab2 (declension-perf-part-active (list citation) g2))
   (setq g (intern (downcase (symbol-name g2)))) ; e.g. M->m
   (setq dtab (translate-ITRANS-SLP1 dtab2))
   (setq thisans (list g dtab))
   (setq ans (cons thisans ans))
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-gerundive (root class evoice)
 "potential passive participle. Returns a list of citation forms,
  e.g., 
 "
 (let (dhaatu pada voice ans class1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
   )
   (t (fol-msg
       (format "SL-gerundive: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
  ;'construct-potpart1a' is in 'kta.el'
   (let (ans1 ans2 upasargas)
    (setq ans1 (construct-potpart1a dhaatu class pada upasargas))
;    (fol-msg (format "chk: %s\n" ans1))
    (if (not (listp ans1)) (setq ans1 (list ans1)))
    (while ans1
     (setq ans2 (car ans1))
     (setq ans1 (cdr ans1))
     (setq ans2 (translate-ITRANS-SLP1 ans2))
     (setq ans (cons ans2 ans))
    )
   )
  (setq ans (nreverse ans))
  ans
 )
)
(defun SL-gerundive-declension (gerundive)
 "'gerundive' is citation form in SLP1 format, e.g. 'kartavya'
  Returns list of pairs, where each pair has form (<gender> <dtab>),
  where <gender> is one of m/f/n,
  and <dtab> is a declension array of 24 elements or a list of such arrays,
  the individual items being in SLP1 form.
 "
 (let (ans citation ans1 ans2 dtab2 g2 dtab g thisans)
  (setq citation (translate-SLP1-ITRANS gerundive))
  (setq ans1 (construct-subanta1 citation 'ADJ 'a))
  ; ans1 has form ((ADJ a) (M dtab F dtab N dtab))
  (setq ans2 (elt ans1 1))
  (while ans2
   (setq g2 (car ans2))
   (setq ans2 (cdr ans2))
   (setq dtab2 (car ans2))
   (setq ans2 (cdr ans2))
   (setq g (intern (downcase (symbol-name g2)))) ; e.g. M->m
   (setq dtab (translate-ITRANS-SLP1 dtab2))
   (setq thisans (list g dtab))
   (setq ans (cons thisans ans))
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun old-construct-all ()
 (fol-msg (format "%s\n" (current-time-string)))
 (construct-init)
 (construct-dhaatukosha-all)
 (construct-dhaatu-forms-all)
; (construct-dcpforms) ; dcpforms.txt
 (construct-conjtabs-all)
 (construct-participles-all)
 (construct-causalconjtabs-all)
 (construct-subanta-all)
 (construct-avyayapada-all)
 (fol-msg (format "%s\n" (current-time-string)))
)
(defun construct-conjtabs-all (&optional outtab outdir)
 (let (forms)
  (setq forms (extract-dcpforms)) ; dhaatu-classPada
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "conjtabs.txt"))
  (construct-conjtabs forms outtab outdir)
 )
)
(defun construct-causalconjtabs-all (&optional outtab outdir)
 (let (forms)
  (setq forms (extract-dcpforms)) ; dhaatu-classPada
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "causalconjtabs.txt"))
  (old-construct-causalconjtabs forms outtab outdir)
 )
)
(defun old-construct-causalconjtabs (formsin outtab outdir)
; (construct-cleartemp)
 (let (keys all-tenses all-pada-voices fileout bufout nform forms
       Xfileout Xbufout ok tense all-pada-voice-tenses)
  (setq all-pada-voices
	'((P ACTIVE)
	  (A ACTIVE)
	  (P PASSIVE))
  )
  (setq all-tenses '(
   laT    ; present
   la~N   ; imperfect  takes
   loT    ; imperative
   vidhili~N ; optative
   liT-p  ; periphrastic perfect
;*   liT-r  ; reduplicative perfect
   lRiT   ; simple future (2nd future)
   lRi~N  ; conditional (takes 'a')
   luT    ; periphrastic future (1st future) 
   aashiirli~N ; benedictive
;*   lu~N1  ; 1st aorist (takes 'a)
;*   lu~N2  ; 1st aorist (takes 'a)
   lu~N3  ; 1st aorist (takes 'a)
;*   lu~N4  ; 1st aorist (takes 'a)
;*   lu~N5  ; 1st aorist (takes 'a)
;*   lu~N6  ; 1st aorist (takes 'a)
;*   lu~N7  ; 1st aorist (takes 'a)
  ))
  (let (ans vp thisans)
   (setq ans nil)
   (mapcar
    (lambda (tense)
     (mapcar
      (lambda (vp)
       (setq thisans (append vp (list tense)))
       (setq ans (append ans (list thisans)))
      )
      all-pada-voices
     )
    )
    all-tenses
   )
   (setq all-pada-voice-tenses ans)
  )
  (setq ok t)
  (when ok
   (setq fileout (sangram-filename outtab outdir))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (with-current-buffer bufout
    (erase-buffer)
   )
  )
  ; construct forms from formsin. keep distinct (dhaatu class) records
  (setq forms nil)
;  (fol-msg (format "formsin=%s\n" formsin))
  (while formsin
   (let (form keyval key val dcpu dhaatu class pada upasargas)
    (setq form (car formsin))
    (setq formsin (cdr formsin))
    (setq dcpu (sym-dcpu form))
    (setq dhaatu (elt dcpu 0))
    (setq class (elt dcpu 1))
;    (setq forms (append-if-new forms (list dhaatu class)))
    (setq forms (cons (list dhaatu class) forms))
   )
  )
  (setq forms (nreverse forms))
;  (fol-msg (format "forms=%s\n" forms))
  (setq nform 0)
  (while forms
   (let (form keyval key val dcpu dhaatu class pada upasargas voice SL-dhaatu)
    (setq form (car forms))
    (setq forms (cdr forms))
    (setq nform (1+ nform))
    (message (format "%s %s" nform form))
;    (setq dcpu (sym-dcpu form))
    (setq SL-dhaatu (elt form 0))
    (setq dhaatu (translate-SLP1-ITRANS SL-dhaatu))
    (setq class (elt form 1))
    (mapcar
     (lambda (vpt)
      (setq pada (elt vpt 0))
      (setq voice (elt vpt 1))
      (setq tense (elt vpt 2))
      (setq keyval (causal-conjtab1a-bases
        dhaatu class pada upasargas tense voice)
      )

      (when keyval
       (setq key dhaatu)
       (when (equal voice 'PASSIVE)
	(setq pada 'PV)
       )
       (with-current-buffer bufout
	(let (subkey base)
	 ; update outtab
        (setq subkey (list 'V 'C class pada tense))
	(setq key dhaatu)
	(setq base (solution keyval))
	(SL-construct-output-base bufout base key subkey)
       )
      )
     )
     )
     all-pada-voice-tenses
    )
   )
  )
  ;post-processing
  (when ok
   (with-current-buffer bufout
    (save-buffer)
    (kill-buffer nil)
   )
  )
  (fol-msg (format "ok=%s\n" ok))
  ok
 )
)

(defun old-SL-inf (root class evoice)
 "construct infinitive"
 (let (dhaatu pada voice ans class1)
  (setq dhaatu (translate-SLP1-ITRANS root))
  (cond
   ((equal evoice 'a)
    (setq pada 'P)
    (setq class1 class)
   )
   ((equal evoice 'm)
    (setq pada 'A)
    (setq class1 class)
   )
   ((equal evoice 'p)
    (setq pada 'A)
    (setq class1 class)
   )
   (t (fol-msg
       (format "SL-inf: arg 3 should be one of 'a,m,p' : %s %s %s\n"
	       root class evoice))
   )
  )
  ;'construct-inf1a' is in 'kta.el'
   (let (ans1 ans2 upasargas)
    (setq ans1 (construct-inf1a dhaatu class pada upasargas))
;    (fol-msg (format "chk: %s\n" ans1))
    (if (not (listp ans1)) (setq ans1 (list ans1)))
    (while ans1
     (setq ans2 (car ans1))
     (setq ans1 (cdr ans1))
     (setq ans2 (translate-ITRANS-SLP1 ans2))
     (setq ans (cons ans2 ans))
    )
   )
  (setq ans (nreverse ans))
  ans
 )
)