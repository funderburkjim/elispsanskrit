; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
(defvar explain-dbg nil)
(defvar explain-pflag t)
(defvar explain-pchk nil)
(defvar explain-ans nil)
(defvar explain-verbosity nil) ; faster but misses som
(defvar explain-shortest-flag t)
(defvar upasarga-Implied-flag nil)
(defvar explain-upasarga-alwaysTry t)
(defvar explain-exclude-types nil)
(defvar explain-verb-forms)
(defvar explain-part-forms)
(defvar explain-ADJ-forms)
(defvar explain-upasarga-forms)
(defvar explain-other-forms)
(defvar explain-pr-forms)
(defvar explain-forms)
(defvar save-explain-forms)
(defvar save-explain-upasarga-forms)
(defvar explain-forms-aagama)
(defvar declension-buffer nil)
(defvar declension-buffer-key nil)
(defvar explain-knowlev nil)
(defvar explain-step2-num 0)
(defvar explain1-cur nil)
(defvar explain1-cur1 nil)
(defvar explain1-cpd nil)
(defvar explain1-cpd1 nil)
(defvar explain1-cur-pr nil)
(defvar explain1-cpd-pr nil)

(defvar cpd-flags nil)
(defvar explain-dhaatu-plist nil)
(defvar cpd-form nil)
(defun explain-chapsII (n1 &optional n2)
 (let (sfxes i)
  (if (or (not (numberp n2)) (not (numberp n1)))
   (setq sfxes (list n1))
   (progn
    (setq i n1)
    (while (<= i n2)
     (setq sfxes (append sfxes (list i)))
     (setq i (1+ i))
    )
   )
  )
  (while sfxes
   (setq i (car sfxes))
   (setq sfxes (cdr sfxes))
   (explain-file (format "explainsenII%s.txt" i))
  )
 )
)
(defun explain-chaps (n1 &optional n2)
 (let (sfxes i)
  (if (or (not (numberp n2)) (not (numberp n1)))
   (setq sfxes (list n1))
   (progn
    (setq i n1)
    (while (<= i n2)
     (setq sfxes (append sfxes (list i)))
     (setq i (1+ i))
    )
   )
  )
  (while sfxes
   (setq i (car sfxes))
   (setq sfxes (cdr sfxes))
   (let (savelev)
    (setq savelev explain-knowlev)
;    (setq explain-knowlev 'Antoine1)
;     (if (or (and (numberp i) (<= 1 i) (<= i 17))
; 	    (equal i '14a))
;       (exclude-form 'PART)
;     )
    (explain-file (format "explainsen%s.txt" i))
    (init-explain-forms)
    (setq explain-knowlev savelev)
   )
  )
 )
)
(defun explain-chk (chap)
 (let (qfile file1 file2 comp)
  (setq explain-pchk t)
  (setq explain-pflag t)
  (explain-chaps chap chap)
;  (setq qfile (format "explainsen%s.txt" chap))
;  (fol-msg (format "qfile= %s\n" qfile))
;  (explain-file qfile)
  (setq explain-pchk nil)
  (setq file1 (format "chk-explainsen%s.txt" chap))
  (setq file2 (format "ans-explainsen%s.txt" chap))
  (setq comp (compare-table-files file1 file2))
  (if comp
   (fol-msg (format "%s  =  %s\n" file1 file2))
  )
 )
)
(defun explain-chk-chaps (n1 n2)
 (let (i )
  (setq i n1)
  (while (<= i n2)
   (explain-chk i)
   (setq i (1+ i))
  )
 )
)
(defun read-explain-file (file)
 (let (ans n i j s sen s1)
  (setq s (read-colon-file-table file 0 "explain"))
  (setq s (mapcar 'flatten s))
  (while s
   (setq s1 (car s))
   (setq s (cdr s))
   (setq sen (list s1))
   (when (not (equal (car s1) 'sen))
    (fol-msg (format "error from read-explain-file %s : %s\n" file s1))
    (setq s nil)
    (setq ans nil)
    (setq sen nil)
   )
   (while (and s (not (equal (car (car s)) 'sen)))
    (setq sen (append sen (list (car s))))
    (setq s (cdr s))
   )
   (when sen (setq sen (read-explain-file1a sen)))
   (when sen (setq ans (append ans (list sen))))
  )
  ans
 )
)
(defun read-explain-file1a (s)
 (let (ans n i j sen s1 s2)
  (while s
   (setq s1 (car s))
   (setq s (cdr s))
   (setq sen (list s1))
   (while (and s (not (numberp (car (car s)))))
    (setq s2 (car s))
    (when (listp s2) (setq s2 (cdr s2)))
    (setq sen (append sen (list s2)))
    (setq s (cdr s))
   )
   (when sen
    (setq sen (flatten sen))
    (setq ans (append ans (list sen)))
   )
  )
  ans
 )
)

(defun compare-table-files (file1 file2)
 (let (s1 s2 n1 n2 ans err)
  (setq s1 (read-explain-file file1))
  (setq s2 (read-explain-file file2))
;  (setq ans (equal s1 s2))
  (setq n1 (length s1))
  (setq n2 (length s2))
  (when (not (equal n1 n2))
   (fol-msg (format "ERROR: %s != %s\n" file1 file2))
   (fol-msg (format "n1 = %s,  n2 = %s\n" n1 n2))
  )
  (when (equal n1 n2)
   (let (i x1 x2 nerr merr xerr)
     (setq merr 5)
     (setq nerr 0)
     (setq i 0)
     (while (and (< i n1) (< i n2) (< nerr merr))
      (setq x1 (elt s1 i))
      (setq x2 (elt s2 i))
      (setq xerr (compare-table-files1a x1 x2))
      (when xerr
       (when (not err)
	(fol-msg (format "ERROR: %s != %s\n" file1 file2))
	(setq err t)
       )
       (mapcar
	(lambda (y)
	 (let (y1 y2)
	  (setq y1 (elt y 0)) (setq y2 (elt y 1))
          (fol-msg (format " err(%s):\n  %s\n  %s\n" (1+ i) y1 y2))
	 )
	)
	xerr
       )
       (setq nerr (1+ nerr))
      )
      (setq i (1+ i))
     )
    )
  )
  ans
 )
)
(defun compare-table-files1a (x y)
 (let (ans)
  (cond
   ((equal x y) (setq ans nil))
   ((or (not (listp x)) (not (listp y))) (setq ans (list x y)))
   ((not (equal (length x) (length y))) (setq ans (list x y)))
   (t
    ; temporary logic
    (let (x1 y1 thisans x2 y2)
     (while x
      (setq x1 (car x))
      (setq x (cdr x))
      (setq y1 (car y))
      (setq y (cdr y))
      (when (not (equal x1 y1))
       (setq x2 (copy-sequence x1))
       (setq y2 (copy-sequence y1))
       (setq x2 (compare-table-files1b x2))
       (setq y2 (compare-table-files1b y2))
       (cond 
	((and (listp y2) (listp x2)
	      (< (length y2) (length x2))
	      y2 ; so y2 not empty
	 )
	 ; temporary logic
;	 (fol-msg (format "x2=%s, y2=%s\n" x2 y2))
	 (let (y3)
	  (while y2
	   (setq y3 (car y2))
	   (setq y2 (cdr y2))
	   (when (not (member y3 x2))
	    (setq thisans (list x1 y1))
	    (setq ans (append ans (list thisans)))
	    (setq y2 nil) ; end loop
	   )
	  )
	 )
	)
        (t ; original logic
	 (setq x2 (sort x2 'lt-stringRep))
	 (setq y2 (sort y2 'lt-stringRep))
	 (when (not (equal x2 y2))
	  (setq thisans (list x1 y1))
	  (setq ans (append ans (list thisans)))
	 )
	)
       )
       
      )
     )
    )
   )
   (t ; original logic
    (let (x1 y1 thisans x2 y2)
     (while x
      (setq x1 (car x))
      (setq x (cdr x))
      (setq y1 (car y))
      (setq y (cdr y))
      (when (not (equal x1 y1))
       (setq x2 (copy-sequence x1))
       (setq y2 (copy-sequence y1))
       (setq x2 (compare-table-files1b x2))
       (setq y2 (compare-table-files1b y2))
       (setq x2 (sort x2 'lt-stringRep))
       (setq y2 (sort y2 'lt-stringRep))
       (when (not (equal x2 y2))
        (setq thisans (list x1 y1))
        (setq ans (append ans (list thisans)))
       )
      )
     )
    )
   )
  )
  ans
 )
)
(defun compare-table-files1b (x)
 (let (ans y)
  (setq ans x)
  (when (listp x)
   (setq ans nil)
   (while x
    (setq y (car x))
    (setq x (cdr x))
    (when (not (member y explain-pr-forms))
     (setq ans (append ans (list y)))
    )
   )
  )
  ans
 )
)
(defun lt-stringRep (x y)
 (string< (format "%s" x) (format "%s" y))
)

(defun explain-all-files ()
 (mapcar 'explain-file
  '(
   "explainsen2.txt" "explainsen3.txt" "explainsen4.txt"
   "explainsen5.txt" "explainsen6.txt" "explainsen7.txt"
   "explainsen8.txt" "explainsen9.txt" "explainsen10.txt"
   "explainsen11.txt"
   )
 )
)
(defun explain-file (vfile)
 (let (bufout fileout bufsave data)
  (if explain-pchk
   (setq fileout (concat "chk-" vfile))
   (setq fileout (concat "ans-" vfile))
  )
  (if explain-pchk
   (fol-msg (format "explain. Using check file %s\n" fileout))
  )
  (setq fileout (sangram-filename fileout "explain"))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq bufsave fol-msg-buffer)
  (setq fol-msg-buffer bufout)
  (setq data (read-colon-file-table vfile 0 "explain"))
;  (setq data (read-colon-file-table vfile 2))
  (explain-file1 data)
  (with-current-buffer bufout
   (save-buffer)
  )
  (kill-buffer bufout)
  (setq fol-msg-buffer bufsave)
  t
 )
)
(defun explain-file1 (data)
 (let (n record p-field q-field old-pflag optional proc)
  ;
;  (fol-msg (format "data has %s records\n" (length data)))
  (setq n 0)
  (setq old-pflag explain-pflag)
  (setq explain-pflag 't)
  (while data
   (setq record (car data))
   (setq data (cdr data))
   (setq n (1+ n))
   (setq p-field (elt record 0))
   (setq proc (elt p-field 0))
   (setq q-field (elt record 1))
   (setq optional
     (if (< 2 (length record))
      (car (elt record 2))))
   (let (chapnum sennum sen)
    (setq chapnum (elt p-field 0))
    (setq sennum (elt p-field 1))
    (setq sen (mapconcat 'symbol-name q-field " "))
    (message (format "sen %s.%s" chapnum sennum))
    (fol-msg (format "sen %s.%s %s\n" chapnum sennum sen))
    (explain sen optional)
    (fol-msg (format "\n"))
   )
  )
  (setq explain-pflag old-pflag)
  t
 )
)
(defun explain-chap-unknowns (chap)
 (let (file recs nrec irec  nans fields)
  (setq nans 0)
  (setq file (format "ans-explainsen%s.txt" chap))
  (setq recs (read-colon-file-table file 0 "explain"))
  (setq nrec (length recs))
  (setq irec 0)
  (while (< irec nrec)
   (setq fields (elt recs irec))
   (setq irec (1+ irec))
   (mapcar
    (lambda (x)
     (when (equal x '(unknown)) (setq nans (1+ nans)))
    )
    fields
   )
  )
  nans
 )
)
(defun explain-chap-unknowns-all ()
 (let (chaps)
  (setq chaps [2 3 4 5 6 7 8 9 10 11 12 13 14 14a 15 
   16 17 18 19 20 21 22 23 24 25 26])
  (mapcar
   (lambda (chap)
    (let (n)
     (setq n (explain-chap-unknowns chap))
     (if (not (equal n 0))
       (fol-msg (format "%s unknowns in chapter %s\n" n chap))
     )
    )
   )
   chaps
  )
  nil
 )
)
(defun explain-Antoine1 (&optional chk)
 (mapcar 
  (lambda (x)
   (if chk
    (message (format "checking Antoine1-Chap %s" x))
   )
   (fol-msg (format "%s\n" (current-time-string)))
   (if chk
    (explain-chk x)
    (explain-chaps x)
   )
   (fol-msg (format "%s\n" (current-time-string)))
  )
  '(2 3 4 5 6 7 8 9 10 11 12 13 14 14a 15 
   16 17 18 19 20 21 22 23 24 25 26)
 )
)
(defun explain-pr (ans)
 (let (ians nans subid nexpl indent-str)
  (setq explain1-cur-pr nil)
  (setq explain1-cpd-pr nil)
  (setq indent-str "")
  (setq subid " abcdefghi")
  (setq nans (length ans))
  (setq ians 0)
  (while (< ians nans)
   (let (x sym expls)
    (setq x (elt ans ians))
    (setq ians (1+ ians))
    (setq sym (elt x 0))
    (setq expls (elt x 1))
    (setq nexpl (length expls))
    (if (not expls)
     (fol-msg (format "%s%s. %s : %s\n" indent-str ians sym 'unknown))
     (explain-pr0 indent-str ians sym expls)
    )
   )
  )
 t
 )
)
(defun explain-pr0 (pfxin ians sym expls)
 (let (explpfx jexpl)
  (if (equal pfxin "")
   (progn
    (setq explpfx (format "%s" ians))
    (fol-msg (format "%s %s = READINGS: %s\n" explpfx sym expls))
   )
   (progn
    (setq explpfx (format "%s.%s" pfxin ians))
    (fol-msg (format "%s %s = PAIR: %s\n" explpfx sym expls))
   )
  )
  
  (setq jexpl 0)  
  (mapcar
   (lambda (expl)
    (setq jexpl (1+ jexpl))
    (explain-pr1 explpfx jexpl expl)
   )
   expls
  )
  t
 )
)
(defun explain-pr1 (pfxin jexpl expl)
 (let (j w x1 explpfx explained)
  (setq explpfx pfxin)
  (setq explpfx (format "%s*%s" pfxin jexpl))
  (setq explained (plist-get explain1-cur-pr expl))
  (if explained
   (progn
    (fol-msg (format "%s %s << (see above) \n" explpfx expl))
    (setq x1 nil)
   )
   (progn
    (setq x1 (plist-get explain1-cur expl))
    (let (x2)
     (setq x2
      (mapcar
       (lambda (x) (substring x 1))
       x1
      )
     )
     (fol-msg (format "%s %s << %s\n" explpfx expl x2))
    )
    (setq j 0)
    (while x1
     (let (x2 pfx x3 x4 pfx3 k)
      (setq x2 (car x1))
      (setq x1 (cdr x1))
      (setq j (1+ j))
      (setq pfx (format "%s+%s" explpfx j))
      (cond
       ((or (not (vector x2)) (<= (length x2) 1))
	(fol-msg (format "%s (ERROR) %s = %s\n"  pfx expl x2))
       )
       ((and (< 2 (length x2)) (not (equal (elt x2 0) 'NORM)))
	(fol-msg (format "%s (ERROR) %s = %s\n"  pfx expl x2))
       )
       ((and (equal (length x2) 2) (equal (elt x2 0) 'CPD))
	(let (w)
	 (setq w (elt x2 1))
	 (if (plist-get explain1-cur-pr w)
	  (progn
	   (fol-msg (format "%s %s -> (see above)\n" pfx expl))
	  )
	  (progn
	   (setq x3 (plist-get explain1-cpd w))
	   (let (pfx1)
	    (setq pfx1 (format "%s %s CPD->" pfx expl))
	    (explain-pr-CPD-item pfx1 x3)
	   )
	   (setq explain1-cur-pr (plist-put explain1-cur-pr w t))
	  )
	 )
	)
       )
       ((and (equal (length x2) 2) (equal (elt x2 0) 'CPD))
	(let (w)
	 (setq w (elt x2 1))
	 (if (plist-get explain1-cpd-pr w)
	  (progn
	   (fol-msg (format "%s %s CPD-> (see above)\n" pfx expl))
	  )
	  (progn
	   (setq x3 (plist-get explain1-cur1 w))
	   (let (pfx1)
	    (setq pfx1 (format "%s %s CPD(%s)->" pfx expl w))
	    (explain-pr-item pfx1 x3)
	   )
	   (setq explain1-cpd-pr (plist-put explain1-cpd-pr w t))
	  )
	 )
	)
       )
       
       ((and (equal (length x2) 2) (equal (elt x2 0) 'CPD))
	(let (w wlist)
	 (setq w (elt x2 1))
         (setq wlist (explain-CPD-straighten w))
	 (setq wlist nil) ; dbg
	 (if wlist
	  (progn 
	   (fol-msg (format "%s %s -> CPD: %s\n"  pfx expl wlist))
	   (setq pfx3 (format "%s" pfx))
	   (explain-pr-CPD pfx3 wlist)
	  )
	  (progn
	   (setq x3 (plist-get explain1-cpd w))
	   (fol-msg (format "%s %s ->> CPD: %s\n"  pfx expl x3))
	   (let (pfx1 k)
	    (setq k 0)
	    (mapcar
	     (lambda (x4)
	      (setq k (1+ k))
	      (setq pfx1 (format "%s.%s %s +>" pfx k expl))
	      (fol-msg (format "%s %s\n" pfx1 x4))
;	      (setq w (elt x4 1))
;              (setq wlist (explain-CPD-straighten w))
;	      (explain-pr-item pfx1 )
	     )
	     x3
	    )
	   )
	  )
	 )
        )
       )
       ((and (equal (length x2) 2) (equal (elt x2 0) 'NORM))
	(let (w)
	 (setq w (elt x2 1))
	 (if (plist-get explain1-cur-pr w)
	  (progn
	   (fol-msg (format "%s %s -> (see above)\n" pfx expl))
	  )
	  (progn
	   (setq x3 (plist-get explain1-cur1 w))
	   (let (pfx1)
	    (setq pfx1 (format "%s %s ->" pfx expl))
	    (explain-pr-item pfx1 x3)
	   )
	   (setq explain1-cur-pr (plist-put explain1-cur-pr w t))
	  )
	 )
	)
       )
       (t ; (and (< 2 (length x2)) (equal (elt x2 0) 'NORM))
	(setq x3 (substring x2 1))
	(explain-pr0 explpfx j expl x3)
       )
      )
     )
    )
    (setq explain1-cur-pr (plist-put explain1-cur-pr expl t))
   )
  )
 )
)
(defun explain-pr-item (pfx defs)
 (let (x y z defs1 pfx1 def)
  (setq defs1 (explain-pr-keyval-modify-a defs))
  (setq def (car defs1))
  (setq defs1 (cdr defs1))
  (setq def (flatten def))
  (fol-msg (format "%s %s\n" pfx def))
  (setq pfx1 (make-string (length pfx) (elt " " 0))) ; blank
  (while defs1
   (setq def (car defs1))
   (setq defs1 (cdr defs1))
   (setq def (flatten def))
   (fol-msg (format "%s %s\n" pfx1 def))
  )
  t
 )
)
(defun explain-pr-CPD-item (pfx cpds)
 (let (cpd k x pfx1 w1 w2 def)
  (fol-msg (format "%s CPDS %s\n" pfx cpds))
;  (setq cpds nil)
  (setq k 0)
  (while cpds
   (setq cpd (car cpds))
   (setq cpds (cdr cpds))
   ; cpd has form [CPD w1 w2]
   (setq k (1+ k))
   (setq pfx1 (format "%s.%s" pfx k))
   (fol-msg (format "%s %s\n" pfx1 cpd))
   (setq w1 (elt cpd 1))
   (setq w2 (elt cpd 2))
   (explain-pr-CPD pfx1 (list w1))
   (explain-pr-CPD pfx1 (list w2))
  )
  t
 )
)
(defun explain-CPD-straighten (csym)
 (let (cprev c wlist w more ok)
  (setq cprev csym)
  (setq more t)
  (setq ok t)
  (while more
   (setq c (plist-get explain1-cpd cprev))
   (cond
    ((not c)
     (setq wlist (append wlist (list cprev)))
     (setq more nil)
    )
    ((not (and (listp c) (equal (length c) 1) (vectorp (elt c 0))))
     ;there are multiple interpretations. Must do things the long way
;     (fol-msg (format "explain-pr-CPD err: %s %s %s\n" csym cprev c))
     (setq more nil)
     (setq ok nil)
    )
    (t
     (setq c (solution c))
     (setq w (elt c 1))
     (setq wlist (append wlist (list w)))
     (setq cprev (elt c 2))
    )
   )
  )
  (if ok wlist)
 )
)
(defun explain-pr-CPD (pfx wlist)
 (let (j def w)
  (setq j 0)
  (while wlist
   (setq w (car wlist))
   (setq wlist (cdr wlist))
   (setq j (1+ j))
   (if (plist-get explain1-cpd-pr w)
    (progn
     (fol-msg (format "%s.%s %s >> (see above)\n" pfx j w))
    )
    (progn
     (let (pfx1)
      (setq pfx1 (format "%s.%s %s >>" pfx j w))
      (setq def (plist-get explain1-cpd1 w))
      (if def
       (progn
        (explain-pr-item pfx1 def)
        (setq explain1-cpd-pr (plist-put explain1-cpd-pr w t))
       )
       (let (cpds)
	(setq cpds (plist-get explain1-cpd w))
	(explain-pr-CPD-item pfx1 cpds)
       )
      )
     )
    )
   )
  )
  t
 )
)
(defun explain-pr-keyval-modify-a (xin)
 (let (x ans)
  (setq x (solution xin))
  (if (symbolp (car x)) (setq x  (list x)))
  (setq ans (mapcar 'explain-pr-keyval-modify x))
  ans
 )
)
(defun explain-pr-keyval-modify (xlist)
 (let (ans vec x y i n z)
  (setq vec (vconcat xlist))
  (setq n (length vec))
  (setq ans (make-vector n nil))
  (setq i 0)
  (while (< i n)
   (setq x (elt vec i))
   (aset ans i x)
   (setq i (1+ i))
   (when (< i n)
    (setq y  (elt vec i))
;    (fol-msg (format "pr-keyval-modify %s: y=%s\n" i y))
    (setq z (explain-pr-keyval-modify1 y))
;    (fol-msg (format "pr-keyval-modify %s: z=%s\n" i z))
    (aset ans i z)
   )
   (setq i (1+ i))
  )
  (append ans nil) ; returns a list
 )
)
(defun explain-pr-keyval-modify1 (x)
 ; assume x is a list of elements, 
 ; assume each element has the form
 ; (key val)
 ; we want to combine elements with the same keys
 (let (ifx ans keys n)
  (setq keys nil)
  (mapcar
   (lambda (x0)
    (let (key val this)
     (setq key (elt x0 0))
     (setq this (vector key nil))
     (setq keys (append-if-new keys this))
;    (if (not (member this keys))
;     (setq keys (append keys (list this)))
;    )
    )
   )
   x
  )
  (setq keys (vconcat keys)) ; convert to vector
  (setq n (length keys))
  (mapcar
   (lambda (x0)
    (let (key i val)
     (setq key (elt x0 0))
     (setq val (elt x0 1))
     (setq i 0)
    (while (< i n)
      (let (this old-val w)
       (setq this (elt keys i))
       (when (equal key (elt this 0))
	(setq i n) ; breaks outer loop
	(setq old-val (elt this 1))
	(when (not (member val old-val))
	 (setq old-val (append old-val (list val)))
	 (aset this 1 old-val)
	)
       )
      )
      (setq i (1+ i))
     )
    )
   )
   x
  )
  ; generally, (elt keys i) is a pair (key val)
  ; when val = nil, we change this to (key)
  (let (kv key i val)
   (setq i 0)
   (while (< i n)
    (setq kv (elt keys i))
    (setq key (elt kv 0))
    (setq val (elt kv 1))
    (setq val (solution val))
    (when (not val)
     (setq kv (list key))
     (aset keys i kv)
    )
    (setq i (1+ i))
   )
  )
  ; change vector to list
  (append keys nil) 
 )
)

(defun explain (string &optional optional)
 (setq explain-step2-num 0)
 (setq explain1-cur nil)
 (setq explain1-cur1 nil)
 (setq explain1-cpd nil)
 (setq explain1-cpd1 nil)
 (setq cpd-flags nil)
 (let (ans seps syms nsym isym verbose)
  ; seps is a list.
  ; each element of seps is a list,
  ; whose elements represent the sandhi possibilities for
  ; the corresponding word of 'string'.
  (setq verbose explain-verbosity)
  (let (tmp)
   (setq tmp (sandhi-separate3-str string optional))
   (setq syms (elt tmp 0))
   (setq seps (elt tmp 1))
  )
  (setq nsym (length syms))
  (setq ans (make-vector nsym nil))
  (setq isym 0)
  (while (< isym nsym)
   (let (word-list sym)
    (setq word-list (elt seps isym))
    (setq sym (elt syms isym))
    (let (thisans ans1 w)
     (setq thisans nil)
     (while word-list
      (setq w (car word-list))
      (setq word-list (cdr word-list))
      (setq ans1 (explain-exactly-1 w verbose))
      (when ans1
       (setq thisans (append thisans (list w)))
      )
     )
     (when thisans
      (when nil ;(not verbose)
       (setq thisans (explain-exactly-1-shortest thisans))
      )
     )
     (aset ans isym (list sym thisans))
    )
   )
   (setq isym (1+ isym))
  )
  (setq explain-ans ans)
  (when explain-pflag
   (explain-pr ans)
   (setq ans t)   
  )
  ans
 )
)
(defun explain-exactly-1 (sym1 &optional verbose)
; (explain-declension-init sym1)
 (when nil; explain-dbg
  (fol-msg (format "explain-exactly-1: %s (verbose=%s)\n" sym1 verbose))
 )
 (let (ans1 ans ans2a ans2b sym1a
       Xans2a Xans2b)
  (setq sym1a (praatar-modify sym1))
  ; next is for efficiency. when an avagraha is present,
  ; (explain1 sym1) will never work.
  (cond 
   ((string-match "\\.a" (symbol-name sym1)) ; avagraha present.
    (setq ans1 (explain-pair sym1 verbose))
    (when ans1
      (setq ans ans1)
    )
   )
   (t
    (setq ans2a (explain-single sym1))
    (setq ans2b (explain-pair sym1 verbose))
    (setq Xans2a ans2a)
    (setq Xans2b ans2b)
    (cond
     ((and ans2a ans2b) (setq ans (list Xans2a Xans2b)))
     (ans2a (setq ans Xans2a))
     (ans2b (setq ans Xans2b))
    )
   )
  )
  (if ans t nil)
 )
)
(defun explain-single (sym)
 (explain1 sym)
)
(defun explain1 (sym1)
 (when nil ;dbg
  (fol-msg (format "explain1: %s\n" sym1))
 )
 (let (ans ans1 ans2)
  (setq ans1 (plist-get explain1-cur sym1))
  (when (not ans1)
   (setq ans1 (explain1a sym1))
   (when ans1
    (setq explain1-cur (plist-AppendElt explain1-cur sym1 (vector 'NORM sym1)))
   )
  )
;  (setq ans2 (plist-get explain1-cpd sym1))
  (when (not ans2)
   (setq ans2 (explain-cpd sym1))
   (when ans2
    (setq explain1-cur (plist-AppendElt explain1-cur sym1 (vector 'CPD sym1)))
   )
  )
  (setq ans (or ans1 ans2))
  (if ans t nil)
 )
)
(defun explain-cpd (sym &optional verbose)
 (when nil ;  explain-dbg
  (fol-msg (format "explain-cpd : %s (verbose=%s)\n" sym verbose))
 )
 (let (seps ans)
  (setq ans nil)
  (setq cpd-flags (cons t cpd-flags))
  (setq seps (cpd-separate sym))
  (while seps
   (let (sep w1 w2 ans1 ans2 ans3 thisans)
    (setq sep (car seps))
    (setq seps (cdr seps))
    (setq w1 (elt sep 0))
    (setq w2 (elt sep 1))
    (setq thisans
     (and
      (not (equal w1 sym)) ; sym already explained as a single word
      (not (equal w2 sym)) ; avoid infinite loops
      (setq ans1 (explain-cpd1 w1 'first))
      (progn
       (setq ans2 (explain-cpd1 w2 'last))
       (setq ans3 (explain-cpd w2 verbose))
       (or ans2 ans3)
        
      )
     )
    )
    (when thisans
     (setq explain1-cpd
      (plist-AppendElt explain1-cpd sym (vconcat [CPD] sep))
     )
;     (fol-msg (format "chk: %s -> %s\n" sym sep))
     (setq ans t)
    )
   )
  )
  (setq cpd-flags (cdr cpd-flags))
  ans
 )
)
(defun explain-cpd1 (sym1 cpd-form-in)
 (let (ans ans1)
  (setq ans (plist-get explain1-cpd1 sym1))
  (when (not ans)
   (setq cpd-form cpd-form-in)
   (setq ans1 (explain-str (symbol-name sym1) explain-forms nil))
   (setq cpd-form nil)
   (mapcar
    (lambda (thisans)
     (when (not (equal (car thisans) 'VERB))     
      (setq ans (append ans (list thisans)))
     )
    )
    ans1
   )
   (when ans
    (setq explain1-cpd1 (plist-AppendElt explain1-cpd1 sym1 ans))
   )
  )
  (if ans t nil)
 )
)

(defun explain1a (sym1)
 (if explain-dbg (fol-msg (format "explain1a: %s\n" sym1)))
 (let (string ans1 ans2 thisans upa-forms form pfx sfx s-a~Nga)
  (setq string (symbol-name sym1))
  ; 03-27-03. The former logic tried upasarga explanation only
  ; when there was no other explanation.
  ; In the case of "adhiiye", the explanation of passive imperfect 1S of
  ; "dhaa" was available; thus, the explanation of present 1S of
  ; "adhi i" was not tried.
  ; The flag 'explain-upasarga-alwaysTry' must be true for this to work
  (setq ans1 (explain-str string explain-forms nil))
  (if ans1
   (setq explain1-cur1 (plist-AppendElt explain1-cur1 sym1 ans1))
  )
  (setq upa-forms (upasarga-forms string))
  (while upa-forms
   (setq form (car upa-forms))
   (setq upa-forms (cdr upa-forms))
   (setq pfx (elt form 0)) ; list of prefixes, as symbols
   (setq sfx (elt form 1)) ; verbal form to be explained, as a symbol
   (setq s-a~Nga (symbol-name sfx))
   (setq ans2 (explain-str s-a~Nga explain-upasarga-forms pfx))
   (while ans2
    (setq thisans (car ans2))
    (setq explain1-cur1 (plist-AppendElt explain1-cur1 sym1 thisans))
    (setq ans2 (cdr ans2))
    (setq ans1 (append-if-new ans1 thisans))
   )
  )
  (if ans1 t nil)
 )
)
(defun explain-pair (sym &optional verbose)
 (let (ans)
  (setq ans (explain-pair1 sym verbose))
  ans
 )
)
(defun explain-pair1 (sym &optional verbose)
 (let (seps ans)
  (setq ans nil)
  (setq seps (sandhi-separate sym))
  (while seps 
   (let (sep w1 w2 ans1 ans2 ans3 thisans)
    (setq sep (car seps))
    (setq seps (cdr seps))
    (setq w1 (elt sep 0))
    (setq w2 (elt sep 1))
    (setq thisans
     (and
      (not (equal w1 sym)) ; sym already explained as a single word
      (not (equal w2 sym)) ; avoid infinite loops
      (progn
       (setq ans1 (explain1 w1))
      )
      (progn
       (setq ans2 (explain1 w2))
       (setq ans3 (explain-pair1 w2 verbose))
       (or ans2 ans3)
      )
     )
    )
    (when thisans 
     (setq explain1-cur
      (plist-AppendElt explain1-cur sym (vconcat [NORM] sep))
     )
     (setq ans t)
    )	      
   )
  )
  ans
 )
)

(defun cpd-separate (sym)
 (let (ans seps1 seps2 seps3)
  (setq seps1 (vowel-separate sym))
  (setq seps2 (cons-separate sym))
  (setq seps3 (sandhi-separate sym))
  (setq ans (append seps1 seps2 seps3))
  ans
 )
)
(defun vowel-separate (sym)
 (let (ans toks wp forms types n i j form1 form2 sym1 sym2 thisans)
  (setq toks (car (ITRANS-parse-words-1 (symbol-name sym))))
  (setq wp (word-parts toks))
  (setq forms (elt wp 0))
  (setq types (elt wp 1))
  (setq n (length types))
  
  (setq i (if (equal (elt types 0) ?V) 0 1))
  (while (< (1+ i) n)
   (setq j (1+ i))
   (setq form1 (substring forms 0 j))
   (setq form2 (substring forms j))
   (setq sym1 (sym-without-space (vconcat (flatten form1))))
   (setq sym2 (sym-without-space (vconcat (flatten form2))))
   (setq thisans (vector sym1 sym2))
   (setq ans (append ans (list thisans)))
   (setq i (+ i 2))
  )
  ans
 )
)
(defun cons-separate (sym)
 (let (ans toks wp forms types n i j form1 form2 sym1 sym2 thisans f)
  (setq toks (car (ITRANS-parse-words-1 (symbol-name sym))))
  (setq wp (word-parts toks))
  (setq forms (elt wp 0))
  (setq types (elt wp 1))
  (setq n (length types))
  (setq i 1)
  (while (< (1+ i) n)
   (setq f (elt forms i))
   (when (and (equal (elt types i) ?C) (< 1 (length f)))
    ; split after the 1st cons
    (setq j (1+ i))
    (setq form1 (substring forms 0 i))
    (setq form2 (substring forms j))
    (setq form1 (vconcat (flatten form1)))
    (setq form2 (vconcat (flatten form2)))
    (setq form1 (vconcat form1 (substring f 0 1)))
    (setq form2 (vconcat (substring f 1) form2))
    (setq sym1 (sym-without-space form1))
    (setq sym2 (sym-without-space form2))
    (setq thisans (vector sym1 sym2))
    (setq ans (append ans (list thisans)))
   )
   (setq i (1+ i))
  )
  ans
 )
)

(defun explain-exactly-1-shortest (expls)
; (fol-msg (format "shortest : expls=%s\n" expls))
 (setq expls (explain-exactly-1-shortest-remove-aa expls))
 (if explain-shortest-flag
  (let (expl expls-vec nexpls iexpls exp1-found)
   (setq expls-vec (vconcat expls))
   (setq nexpls (length expls-vec))
   ; each elt of expls-vec has form
   ; expl =  (EXPX1 sym (EXP1-or-EXPPAIR ans1 ans2 ...))
   ; phase 1a. See if there is an EXP1 type answer
   ;   If so, this is shorter than any EXPPAIR type answer
   (setq exp1-found nil)
   (setq iexpls 0)
   (while (< iexpls nexpls)
    (setq expl (elt expls-vec iexpls))
    (setq iexpls (1+ iexpls))
    (when (equal (elt (elt expl 2) 0) 'EXP1)
     (setq exp1-found t)
    )
   )
   ; phase 1b. When exp1-found, discard all EXPPAIR
   (when exp1-found
    (let (newexpls)
     (setq iexpls 0)
     (while (< iexpls nexpls)
      (setq expl (elt expls-vec iexpls))
      (setq iexpls (1+ iexpls))
      (when (equal (elt (elt expl 2) 0) 'EXP1)
       (setq newexpls (append newexpls (list expl)))
      )
     )
     (setq expls-vec (vconcat newexpls))
     (setq nexpls (length expls-vec))
    )
   )
   ; phase 2. minimize length within each EXPPAIR explanation (if any left)
   (setq iexpls 0)
   (while (< iexpls nexpls)
    (setq expl (elt expls-vec iexpls))
    ; when EXP1, no change to expl
    ; when EXPPAIR, those of ans1 ans2 ... of more than
    ; minimal length are discarded
    (when (and (equal (elt (elt expl 2) 0) 'EXPPAIR) exp1-found)
     (aset expls-vec iexpls nil)
    )
    (when (and (equal (elt (elt expl 2) 0) 'EXPPAIR) (not exp1-found))
     (let (sym anslist minlen newanslist x newexpl)
      (setq sym (elt expl 1))
      (setq anslist (cdr (elt expl 2)))
      (setq minlen (apply 'min (mapcar 'length anslist)))
      (setq newanslist nil)
      (while anslist
       (setq x (car anslist))
       (setq anslist (cdr anslist))
       (if (= (length x) minlen)
	 (setq newanslist (append newanslist (list x))))
      )
      (setq newexpl (list 'EXPX1 sym (cons 'EXPPAIR newanslist)))
      (aset expls-vec iexpls newexpl)
     )
    )
    (setq iexpls (1+ iexpls))
   )
   ; turn expls-vec back to a list, and return it
   (append expls-vec nil)
  )
 )
)

(defun explain-exactly-1-shortest-remove-aa (expls)
  (let (expl expls-vec nexpls iexpls exp1-found newans)
   (setq expls-vec (vconcat expls))
   (setq nexpls (length expls-vec))
   ; each elt of expls-vec has form
   ; expl =  (EXPX1 sym (EXP1-or-EXPPAIR ans1 ans2 ...))
   ; phase 1a. See if there is an EXP1 type answer
   ;   If so, this is shorter than any EXPPAIR type answer
   (setq exp1-found nil)
   (setq iexpls 0)
   (while (< iexpls nexpls)
    (setq expl (elt expls-vec iexpls))
    (setq iexpls (1+ iexpls))
    (when (equal (elt (elt expl 2) 0) 'EXP1)
     (setq exp1-found t)
    )
   )
   (setq exp1-found nil)
   ; phase 1b. When exp1-found, discard all EXPPAIR when 
   (when exp1-found
    (let (newexpls)
     (setq iexpls 0)
     (while (< iexpls nexpls)
      (setq expl (elt expls-vec iexpls))
      (setq iexpls (1+ iexpls))
      (when (equal (elt (elt expl 2) 0) 'EXP1)
       (setq newexpls (append newexpls (list expl)))
      )
     )
     (setq expls-vec (vconcat newexpls))
     (setq nexpls (length expls-vec))
    )
   )
   ; phase 2. minimize length within each EXPPAIR explanation (if any left)
   (setq iexpls 0)
   (while (< iexpls nexpls)
    (setq expl (elt expls-vec iexpls))
    ; when EXP1, no change to expl
    ; when EXPPAIR, those of ans1 ans2 ... of more than
    ; minimal length are discarded
    
    (if (and (equal (elt (elt expl 2) 0) 'EXPPAIR) )
     (let (sym anslist minlen newanslist x newexpl subsymbols)
      (setq sym (elt expl 1))
      (setq anslist (cdr (elt expl 2)))
;      (setq anslist (solution anslist))
      (setq newanslist nil)
      (while anslist
       (setq x (car anslist))
       (setq anslist (cdr anslist))
;       (fol-msg (format "chk: x=%s\n" x))
       (setq subsymbols
	(mapcar
	 (lambda (x1)
	  (if (and (listp x1) (symbolp (car x1))) (car x1))
	 )
         x
	)
       )
       (when (shortest-check subsymbols)
	 (setq newanslist (append newanslist (list x)))
       )
      )
      (when newanslist
       (setq newexpl (list 'EXPX1 sym (cons 'EXPPAIR newanslist)))
       (setq newans (append newans (list newexpl)))
      )
     )
     (progn
      ; not a pair. Keep
      (setq newans (append newans (list expl)))
     )
    )
    (setq iexpls (1+ iexpls))
   )
   newans
  )
)
(defun shortest-check (syms)
 (let (sym ok)
  (setq ok t)
  (while (and syms ok)
   (setq sym (car syms))
   (setq syms (cdr syms))
   (setq ok (not (member sym '
    (aa aama aana ama aya aamaH aami
     aiH aaram anaama aaNi aaNa ati aavaH
     aanam aakaH syati aiva aane
     e aapi
    ))))
  )
  ok
 )
)

(defun praatar-modify (sym1)
 (cond
  ((equal sym1 'praataH) 'praatar)
  ((equal sym1 'punaH) 'punar)
  (t sym1)
 )
)
(defun explain-str (s theforms &optional extra)
 (when nil ; dbg
  (fol-msg (format "explain-str: %s %s %s\n" s theforms extra))
 )
; 08-31-03. For the moment, 'aagama' logic is unneeded, as 
; pointers exist for verb forms like 'la~N'
 ; this handles 'aagama' (the 'a' used in imperfect and some general tenses)
 ; We first separate 'theforms' into two sets:
 ; theforms1 = those requiring no modification of 's' due to prefix like 'a'
 ; theforms2 = those requiring such a modification
 ; This distinction is determined with the constant 
 ; 'explain-forms-aagama'
 ; We call explain-str-step1 immediately for theforms1, using 's'
 ; When theforms2 is present, another step is required:
 ;  1. call 'inverse-aagama' to get a list of strings which might
 ;     have led to 's' by adding the prefix 'a'
 ;  2. For each such string 's1', call explain-str-step1 using
 ;     theforms1 with 's' as the string to explain, and 's1' as
 ;     the string to use for lookups.
 (let (theforms1 theforms2 ans)
  ; determine theforms1 , theforms2
  (let (forms f)
   (setq forms theforms)
   (while forms
    (setq f (car forms))
    (setq forms (cdr forms))
    (setq explain-forms-aagama nil) ; this makes there to be no aagamas
    (if (member f explain-forms-aagama)
     (setq theforms2 (append-if-new theforms2 f ))
     (setq theforms1 (append-if-new theforms1 f ))
    )
   )
  )
  ; 2. Make the explain-str-step1 call for theforms1
  (when theforms1
   (setq ans (explain-str-step1-incr s s theforms1 extra))
  )
  ; 3. Note: this step empty when no 'aagamas'
  (when theforms2
   (let (s-aagamas s-aagama thisans)
    (setq s-aagamas (inverse-aagama s))
    (while s-aagamas
     (setq s-aagama (car s-aagamas))
     (setq s-aagamas (cdr s-aagamas))
     (setq ans (explain-str-step1-incr s s-aagama theforms2 extra))
     (if thisans (setq ans (append-if-new ans thisans)))
    )
   )
  )
  ans
 )
)
(defvar san-key-sfx nil)
(defun explain-str-step1-incr (s s0 theforms &optional extra)
 (when nil ; dbg
  (fol-msg (format "step1-incr: %s %s : %s %s\n"
		   s s0 theforms extra))
 )
 (let (s-sym san-info san-key s1 n j ans ans1 thisans s0-tok s2)
  ; s-sym = the thing to explain, as a symbol
  (setq s-sym (intern s))
  (setq s0-tok (car (ITRANS-parse-words-1 s0)))
  (setq n (length s0-tok))
  (setq j 1)
  (while (<= j n)
   (setq san-key (sym-without-space (substring s0-tok 0 j)))
   
   (if (= j n)
    (setq san-key-sfx nil)
    (progn
     (setq san-key-sfx (sym-without-space (substring s0-tok j)))
    )
   )
   (setq j (1+ j))
   ; alternate spellings not otherwise explained
   (cond
    ((equal san-key 'praataH) (setq san-key 'praatar))
    ((equal san-key 'punaH) (setq san-key 'punar))
   )
   (setq san-info (Xsangetall san-key))
   (when san-info
;    (fol-msg (format "chk: %s\n" san-info))
    (setq ans1 (explain-str-step2 s-sym san-key san-info theforms extra))
    (while ans1
     (setq thisans (car ans1))
     (setq ans1 (cdr ans1))
     (setq ans (append-if-new ans thisans))
    )
   )
  )
  ans
 )
)
(defun explain-str-step2 (s-sym san-key san-key-info expforms &optional extra)
 (setq explain-step2-num (1+ explain-step2-num))
 (when  explain-dbg
  (fol-msg (format "explain-str-step2: %s %s %s\n" s-sym san-key san-key-info))
 )
 (let (formans fname fname-sym ans xtype x xargs preargs thisans san-info
       info-key info-data)
 ; say-key-info is a plist, whose keys are one of
 ; 'dhaatu subanta avyayapada'
  (while san-key-info
   (setq info-key (car san-key-info))
   (setq san-key-info (cdr san-key-info))
   (setq info-data (car san-key-info))
   (setq san-key-info (cdr san-key-info))
;   (fol-msg (format "chk: %s %s\n" info-key info-data))
   
   (when (member info-key expforms)
    (setq ans nil)
    (cond
     ((equal info-key 'dhaatu)
      (let (sub-info-key sub-info-data dhaatu-form-data dhaatu-form-datum)
;       (fol-msg (format "info-data=%s\n" info-data))
       (while info-data
	(setq sub-info-key (car info-data))
	(setq info-data (cdr info-data))
	(setq sub-info-data (car info-data))
	(setq info-data (cdr info-data))
	(setq dhaatu-form-data (plist-get explain-dhaatu-plist sub-info-key))
	(when nil
	 (fol-msg (format "chka: %s %s %s\n"
		sub-info-key sub-info-data dhaatu-form-data))
	)
        (when (equal sub-info-key 'CAUSAL) 
         (setq formans
	       (explain-str-step2-CAUSAL formans
		 s-sym san-key sub-info-data  extra))
	 (setq dhaatu-form-data nil) ; so next part not executed
	)
	(if (and dhaatu-form-data (symbolp (elt dhaatu-form-data 0)))
	  (setq dhaatu-form-data (list dhaatu-form-data))
	)
	(while dhaatu-form-data
	 (setq dhaatu-form-datum (car dhaatu-form-data))
	 (setq dhaatu-form-data (cdr dhaatu-form-data))
	 (setq xtype (elt dhaatu-form-datum 0))
	 (cond
	  ((equal xtype 'VERB)
	   (let (tense-sym voice-sym upa-syms)
	    (setq tense-sym sub-info-key)
	    (setq voice-sym (elt dhaatu-form-datum 1))
	    (setq upa-syms extra)
	    (setq ans (explain-str-VERB
             s-sym san-key sub-info-data tense-sym voice-sym upa-syms))
	   )
	  )
	  ((equal xtype 'PART)
	   (let (tense-sym voice-sym upa-syms subkey subkey-id proc)
	    (setq subkey sub-info-key)
	    (setq tense-sym (elt dhaatu-form-datum 1))
	    (setq voice-sym (elt dhaatu-form-datum 2))
	    (setq subkey-id (elt dhaatu-form-datum 3))
	    (setq proc (elt dhaatu-form-datum 4))
	    (setq upa-syms extra)
	    (setq ans (explain-str-PART
              s-sym san-key sub-info-data tense-sym voice-sym upa-syms))
	    (when nil
             (fol-msg (format "ppp: %s %s %s -> %s\n"
	       tense-sym voice-sym upa-syms ans))
	    )
	   )
	  )
	 )
	 (setq formans (update-form-ans formans ans xtype))
	)
       )
      )
     )
     ((equal info-key 'subanta)
      (let (sub-info-key sub-info-data dhaatu-form-data)
;       (fol-msg (format "info-data=%s\n" info-data))
       (while info-data
	(setq sub-info-key (car info-data))
	(setq info-data (cdr info-data))
	(setq sub-info-data (car info-data))
	(setq info-data (cdr info-data))
;	(setq dhaatu-form-data (plist-get explain-dhaatu-plist sub-info-key))
;	(setq xtype (elt dhaatu-form-data 0))
	(setq xtype sub-info-key)
	(setq ans (explain-str-subanta s-sym san-key sub-info-data xtype))
	(when (and (not ans) (equal xtype 'ADJ))
	 ; try to explain as a degree (COMPAR or SUPERL) of an ADJ
	 (setq ans (explain-ADJ-degree s-sym san-key sub-info-data))
	)
	(setq formans (update-form-ans formans ans xtype))
       )
      )
     )
     ((equal info-key 'avyayapada)
      (setq ans (explain-str-INDECL s-sym san-key (list info-key info-data)))
      (setq xtype 'INDECL)
      (setq formans (update-form-ans formans ans xtype))
     )
    )
    
   )
  )
  formans
 )
)
(defun explain-str-step2-CAUSAL
   (formans s-sym san-key info-data  extra)
 (let (sub-info-key sub-info-data dhaatu-form-data dhaatu-form-datum
       xtype x xargs preargs thisans ans )
;       (fol-msg (format "info-data=%s\n" info-data))
       (while info-data
	(setq sub-info-key (car info-data))
	(setq info-data (cdr info-data))
	(setq sub-info-data (car info-data))
	(setq info-data (cdr info-data))
	(setq dhaatu-form-data (plist-get explain-dhaatu-plist sub-info-key))
	(when  nil
	 (fol-msg (format "chkb: %s %s %s %s %s\n"
		s-sym san-key sub-info-key sub-info-data dhaatu-form-data))
	)
        
	(if (symbolp (elt dhaatu-form-data 0))
	  (setq dhaatu-form-data (list dhaatu-form-data))
	)
	(while dhaatu-form-data
	 (setq dhaatu-form-datum (car dhaatu-form-data))
	 (setq dhaatu-form-data (cdr dhaatu-form-data))
	 (setq xtype (elt dhaatu-form-datum 0))
	 (cond
	  ((equal xtype 'VERB)
	   (let (tense-sym voice-sym upa-syms)
	    (setq tense-sym sub-info-key)
	    (setq voice-sym (elt dhaatu-form-datum 1))
	    (setq upa-syms extra)
	    (setq ans (explain-str-VERB-CAUSAL
             s-sym san-key sub-info-data tense-sym voice-sym upa-syms))
	   )
	  )
	  ((equal xtype 'PARTXX) ; not implemented
	   (let (tense-sym voice-sym upa-syms subkey subkey-id proc)
	    (setq subkey sub-info-key)
	    (setq tense-sym (elt dhaatu-form-datum 1))
	    (setq voice-sym (elt dhaatu-form-datum 2))
	    (setq subkey-id (elt dhaatu-form-datum 3))
	    (setq proc (elt dhaatu-form-datum 4))
	    (setq upa-syms extra)
	    (setq ans (explain-str-PART
              s-sym san-key sub-info-data tense-sym voice-sym upa-syms))
;            (fol-msg (format "%s %s %s -> %s\n" tense-sym voice-sym upa-syms
;			    ans))
	   )
	  )
	  (t (setq ans nil))
	 )
	 (when ans
	  (when (not (equal xtype 'VERB))
	   (fol-msg (format "chk: unexpected xtype: %s\n" xtype))
	  )
	  (setq formans (update-form-ans formans ans xtype))
	 )
	)
       )
   formans
  )
)
(defun update-form-ans (formans ans xtype)
 (when ans
     (when (member xtype '(VERB))
     ; remove extra parens so answer form like that of
     ; explain-str-upasarga
      (setq ans (mapcar 'solution ans)) 
     )
     (when (not (member xtype '(VERB PART)))
      (setq ans (cons xtype (list ans)))
      (setq formans (append-if-new formans ans))
     )
     (when nil (member xtype '(VERB PART)) ; older method (skipped)
      (setq ans (cons xtype (list ans)))
      (setq formans (append-if-new formans ans))
     )
     (when (member xtype '(VERB PART))
      (while (and t ans)
       (let (thisans thisans1 thisans0 ans0)
	(setq thisans0 (elt (car ans) 0))
	(setq thisans1 nil)
	(while (and ans (equal thisans0 (elt (car ans) 0)))
         (setq thisans (car ans))
         (setq ans (cdr ans))
	 (setq thisans1 (append thisans1 (list thisans)))
	)
        (setq thisans1 (cons xtype (list thisans1)))
        (setq formans (append-if-new formans thisans1))
       )
      )
     )
    )
  formans
)
(defun adjust-dhaatu-forms (forms upa-syms)
 (let (ans forms1 form dhaatu class pada upasargas tmp form1 formfound
	   shortform)
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas (elt tmp 3))
   (cond
    (upasargas
     ; form refers to specific upasarga. To keep it, this must
     ; be the same as upa-syms
     (when (equal upasargas upa-syms)
      (setq forms1 (append-if-new forms1 form))
      (setq formfound form)
      (setq shortform (dcpu-sym dhaatu class pada nil))
     )
    )
    ((not upa-syms)
     ; neither upasargas nor upa-syms is present. Keep provided
     ; forms is in dhaatukosha
     (when (sanget2 form '(dhaatu Eng-def))
      (setq forms1 (append-if-new forms1 form))
     )
    )
    (t
     ; upa-syms is present, but not upasargas
     ; this form with upa-syms appended must be known
     (setq form1 (dcpu-sym dhaatu class pada upa-syms))
     (when (sanget2 form1 '(dhaatu Eng-def))
      (setq forms1 (append-if-new forms1 form))
     )
    )
   )
  )
  (when formfound
   ; upa-syms is present. We found a form with upa-syms explicitly
   ; mentioned.  The short-form must be excluded
   (setq forms forms1)
   (setq forms1 nil)
   (while forms
    (setq form (car forms))
    (setq forms (cdr forms))
    (when (not (equal form shortform))
     (setq forms1 (append-if-new forms1 form))
    )
   )
  )
  forms1
 )
)
(defun explain-str-VERB
 (s-sym san-key san-info-data tense-sym &optional voice-sym upa-syms)
 (let (ans forms form dhaatu class pada upasargas tmp person number
	   Eng-def ctabs ansform thisansform thisans tense)
  ; s-sym = the thing to explain, as a symbol
  ; when voice-sym is 'PASSIVE', tense-sym should end in '-P',
  ; and when voice-sym is not, tense-sym should not
  (setq forms san-info-data)
  (when nil ; dbg
   (fol-msg (format "str-VERB: %s %s %s %s %s\n"
 	    s-sym san-key tense-sym voice-sym upa-syms))
   (fol-msg (format "forms=%s\n" forms))
  )
;   (when (not (explain-knowlev-P 'TENSE tense-sym))
;    (setq forms nil)
;   )
  (when cpd-form
   (setq forms nil)
  )
  (when forms
   (let (stense)
    (setq tense tense-sym)
    (when (equal voice-sym 'PASSIVE)
     (setq stense (symbol-name tense-sym))
     (when (equal (substring stense -2) "-P")
      (setq stense (substring stense 0 -2)) ; drop the '-P'
     )
     (setq tense (intern stense))
    )
   )
  )
  (while forms ;(and forms (not ans)) 
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas (elt tmp 3))
   (when (and (equal upa-syms upasargas)
;	      (explain-knowlev-P 'VERB dhaatu class tense-sym)
	 )
    (setq ctabs (getdhaatu dhaatu class pada upasargas tense-sym))
    (setq ctabs (solution ctabs))
    (if (not (listp ctabs)) (setq ctabs (list ctabs)))
    (setq Eng-def (sanget2 form '(dhaatu Eng-def)))
    
    (let (i m ctab c baseflag found sym-sfx)
     (when ctabs
      (setq ctab (elt ctabs 0))
      (cond
       ((equal ctab 'BASE)
	(setq baseflag t)
	(if (equal voice-sym 'PASSIVE)
	 (progn
	  (setq ctab (plist-get2 conjtab-endings (list tense 'PA)))
	  (if (not ctab)
	   (setq ctab (plist-get2 conjtab-endings (list tense 'A)))
	  )
	 )
	 (progn
	  (if (and (equal pada 'P) (equal tense 'loT))
	   (let (base)
	    (setq base (elt ctabs 1))
	    (if (listp base) (setq base (elt base 0))) ; just use 1st base
;	    (fol-msg (format "chk: %s\n" base))
	    (if (nN-P base)
	     (setq ctab (plist-get2 conjtab-endings '(loT P-N)))
	     (setq ctab (plist-get2 conjtab-endings (list tense pada)))
	    )
	   )
	   (setq ctab (plist-get2 conjtab-endings (list tense pada)))
	  )
	 )
	)
	(setq sym-sfx san-key-sfx)
;	(fol-msg (format "%s %s %s\n" tense pada ctab))
       )
       (t
        (setq m (length ctabs))
        (setq i 1)
        (while (< i m)
         (setq ctab (join-arrays ctab (elt ctabs i)))
         (setq i (1+ i))
        )
       )
      )
     )
     (setq m (length ctab))
     (setq i 0)
     (while (< i m)
      (setq c (elt ctab i))
      (setq found
       (cond
	((not c) nil)
	(baseflag
	 (or (equal sym-sfx c) (and (listp c) (member sym-sfx c)))
	)
	(t (or (equal s-sym c) (and (listp c) (member s-sym c)))
	)
       )
      )
      (when found
       (setq person (elt (elt person-number-set i) 0))
       (setq number (elt (elt person-number-set i) 1))
       (let (v)
	(setq v (if (equal voice-sym 'PASSIVE) voice-sym))
        (setq thisans (explain-VERB-makeans
	     tense dhaatu class pada person
		  number Eng-def upasargas v))
       )
       (setq thisansform
	  (list dhaatu tense-sym class voice-sym person number))
       (when (equal voice-sym 'PASSIVE)
        ; check that we do not have passive of 'A' and 'P'
	(let (temp temp1 found)
	 (setq temp ansform)
	 (setq found nil)
	 (while (and temp (not found))
	  (setq temp1 (car temp))
	  (setq temp (cdr temp))
	  (setq found (equal thisansform temp1))
	 )
	 (if found (setq thisans nil))
	)
       )
       (when thisans
	(setq ans (append-if-new ans thisans))
        (setq ansform (append-if-new ansform thisansform))
       )
      )
      (setq i (1+ i))
     )
    )
   )
  )
  (when (and nil ans) ; dbg
   (fol-msg (format "str-VERB: %s %s %s %s %s\n"
 	    s-sym san-key tense-sym voice-sym upa-syms))
   (fol-msg (format "san-info=%s\n" san-info))
   (mapcar
    (lambda (x)
     (let (form thisans)
      (setq form (elt x 0))
      (setq thisans (elt x 1))
      (fol-msg (format "%s -> %s\n" form thisans))
     )
    )
    ansform
   )
  )
  
  ans
 )
)
(defun explain-str-VERB-CAUSAL
 (s-sym san-key san-info-data tense-sym &optional voice-sym upa-syms)
 (let (ans forms form dhaatu class pada upasargas tmp person number
	   Eng-def ctabs ansform thisansform thisans tense)
  ; s-sym = the thing to explain, as a symbol
  ; when voice-sym is 'PASSIVE', tense-sym should end in '-P',
  ; and when voice-sym is not, tense-sym should not
  (setq forms san-info-data)
  (when nil ; dbg
   (fol-msg (format "str-VERB-CAUSAL: %s %s %s %s %s\n"
 	    s-sym san-key tense-sym voice-sym upa-syms))
   (fol-msg (format "forms=%s\n" forms))
  )
;   (when (not (explain-knowlev-P 'TENSE tense-sym))
;    (setq forms nil)
;   )
  (when cpd-form
   (setq forms nil)
  )
  (when forms
   (let (stense)
    (setq tense tense-sym)
    (when (equal voice-sym 'PASSIVE)
     (setq stense (symbol-name tense-sym))
     (when (equal (substring stense -2) "-P")
      (setq stense (substring stense 0 -2)) ; drop the '-P'
     )
     (setq tense (intern stense))
    )
   )
  )
  (while forms ;(and forms (not ans)) 
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas (elt tmp 3))
   (when (and (equal upa-syms upasargas)
;	      (explain-knowlev-P 'VERB dhaatu class tense-sym)
	 )
    (setq ctabs (getdhaatu dhaatu class pada upasargas
			   (list 'CAUSAL tense-sym)))
    (when nil
     (fol-msg (format "ctabs: %s %s %s %s %s -> %s\n"
		     dhaatu class pada upasargas tense-sym ctabs))
    )
    (setq ctabs (solution ctabs))
    (if (not (listp ctabs)) (setq ctabs (list ctabs)))
    (setq Eng-def (sanget2 form '(dhaatu Eng-def)))
    
    (let (i m ctab c baseflag found sym-sfx)
     (when ctabs
      (setq ctab (elt ctabs 0))
      (cond
       ((equal ctab 'BASE)
	(setq baseflag t)
	(if (equal voice-sym 'PASSIVE)
	 (progn
	  (setq ctab (plist-get2 causal-endings (list tense 'PA)))
	  (if (not ctab)
	   (setq ctab (plist-get2 causal-endings (list tense 'A)))
	  )
	 )
	 (progn
	  (if (and (equal pada 'P) (equal tense 'loT))
	   (let (base)
	    (setq base (elt ctabs 1))
	    (if (listp base) (setq base (elt base 0))) ; just use 1st base
;	    (fol-msg (format "chk: %s\n" base))
	    (if (nN-P base)
	     (setq ctab (plist-get2 causal-endings '(loT P-N)))
	     (setq ctab (plist-get2 causal-endings (list tense pada)))
	    )
	   )
	   (setq ctab (plist-get2 causal-endings (list tense pada)))
	  )
	 )
	)
	(setq sym-sfx san-key-sfx)
;	(fol-msg (format "%s %s %s\n" tense pada ctab))
       )
       (t (fol-msg (format "unexpectedxx CAUSAL\n")) (setq ctab nil)
       )
       (t
        (setq m (length ctabs))
        (setq i 1)
        (while (< i m)
         (setq ctab (join-arrays ctab (elt ctabs i)))
         (setq i (1+ i))
        )
       )
      )
     )
     (setq m (length ctab))
     (setq i 0)
     (while (< i m)
      (setq c (elt ctab i))
      (setq found
       (cond
	((not c) nil)
	(baseflag
	 (or (equal sym-sfx c) (and (listp c) (member sym-sfx c)))
	)
	(t (or (equal s-sym c) (and (listp c) (member s-sym c)))
	)
       )
      )
      (when found
       (setq person (elt (elt person-number-set i) 0))
       (setq number (elt (elt person-number-set i) 1))
       (let (v)
	(setq v 'CAUSAL)
	(if (equal voice-sym 'PASSIVE)
	  (setq v (sym-concat v '-PASSIVE))
        )
        (setq thisans (explain-VERB-makeans
	     tense dhaatu class pada person
		  number Eng-def upasargas v ))
       )
       (setq thisansform
	  (list dhaatu tense-sym class voice-sym person number))
       (when (equal voice-sym 'PASSIVE)
        ; check that we do not have passive of 'A' and 'P'
	(let (temp temp1 found)
	 (setq temp ansform)
	 (setq found nil)
	 (while (and temp (not found))
	  (setq temp1 (car temp))
	  (setq temp (cdr temp))
	  (setq found (equal thisansform temp1))
	 )
	 (if found (setq thisans nil))
	)
       )
       (when thisans
	(setq ans (append-if-new ans thisans))
        (setq ansform (append-if-new ansform thisansform))
       )
      )
      (setq i (1+ i))
     )
    )
   )
  )
  (when (and nil ans)
   (fol-msg (format "str-VERB-CAUSAL: %s %s %s %s %s\n"
 	    s-sym san-key tense-sym voice-sym upa-syms))
   (fol-msg (format "san-info=%s\n" san-info))
   (mapcar
    (lambda (x)
     (let (form thisans)
      (setq form (elt x 0))
      (setq thisans (elt x 1))
      (fol-msg (format "%s -> %s\n" form thisans))
     )
    )
    ansform
   )
  )
  
  ans
 )
)
(defun explain-knowlev-P (xtype &rest args)
 (let ()
;  (fol-msg (format "xtype=%s, args=%s\n" xtype args))
  (cond
   ((equal explain-knowlev 'Antoine1)
    (cond
     ((equal xtype 'VERB)
      ; restrict to conjugation class 1,4,6, or 10
      ; except for passive tenses (laT-P la~N-P loT-P vidhili~N-P)
      (let (dhaatu class tense)
       (setq dhaatu (elt args 0))
       (setq class (elt args 1))
       (setq tense (elt args 2))
       (cond
	((member dhaatu '(kRi as)) t)
	((member class '(1 4 6 10)) t)
	((member tense '(laT-P la~N-P loT-P vidhili~N-P)) t)
	(t nil)
       )
      )
     )
     ((equal xtype 'TENSE)
      (let (tense)
       (setq tense (elt args 0))
       (if (member tense
	    '(laT la~N loT vidhili~N
	      laT-P la~N-P loT-P vidhili~N-P
	     )
	   )
	t
        nil
       )
      )
     )
     (t) ; default
    )
   )
   (t) ; default
  )
 )
)
(defun explain-str-PART
  (s-sym san-key san-info tense-sym &optional voice-sym upa-syms)
 (when nil
  (fol-msg (format "explain-str-PART: %s %s %s %s %s\n"
				  s-sym san-key tense-sym voice-sym upa-syms))
 )
 (let (ans j n s1   s0 thisans theseans genders gender subkey subkey-id)
  (cond
   ((and (equal tense-sym 'laT) (member voice-sym (list nil 'ACTIVE)))
    (setq subkey 'PRESPART)
    (setq subkey-id 'PRES-ACT)
   )
   ((and (equal tense-sym 'laT) (equal voice-sym 'PASSIVE))
    (setq subkey 'PASSPART)
    (setq subkey-id 'PRES-PASS)
   )
   ((and (equal tense-sym 'lRiT) (member voice-sym (list nil 'ACTIVE)))
    (setq subkey 'FUTPART)
    (setq subkey-id 'FUT-ACT)
   )
   ((and (equal tense-sym 'lRiT) (equal voice-sym 'PASSIVE))
    (setq subkey 'FUTPPART)
    (setq subkey-id 'FUT-PASS)
   )
   ((equal tense-sym 'PERF)
     ; kta
    (if (equal voice-sym 'PASSIVE)
     (progn (setq subkey 'PPPART) (setq subkey-id 'PERF-PASS))
     (progn (setq subkey 'PAPART) (setq subkey-id 'PERF-ACT))
    )
   )
   ((equal tense-sym 'POT)
    (setq subkey 'POTPART)
    (setq subkey-id 'POT-PASS)
   )
   ((equal tense-sym 'IPP)
    (setq ans (explain-PART-IPP s-sym san-key san-info upa-syms
				'IPPART 'IPP))
   )
   ((equal tense-sym 'INF)
    (setq ans (explain-PART-INF s-sym san-key san-info upa-syms
				'INF 'INF))
   )
   ((equal tense-sym 'RPERF)
    (setq subkey 'RPPART) ; reduplicated perfect participle
    (setq subkey-id 'RPERF)
   )
   ((equal tense-sym 'PERPERF)
    (setq subkey 'PERPPART) ; periphrastic perfect participle
    (setq subkey-id 'PERPERF)
   )
   (t (setq subkey nil))
  )

  (when subkey
   (if (not subkey-id) (setq subkey-id subkey))
   (setq ans
     (explain-str-PART1 s-sym san-key san-info upa-syms subkey subkey-id))
  )
  ans
 )
)
(defun explain-str-PART1 
  (s-sym san-key san-key-info upa-syms subkey subkey-id)
 (let (ans forms  thisans form tmp dhaatu class pada upasargas
	   data dtab i m delt case number form1 genders gender ok Eng-def
	   san-sfx)
  (setq forms san-key-info)
  (when (and nil forms)
   (fol-msg (format "PART1: %s %s %s %s %s\n"
		    s-sym san-key san-key-info  subkey subkey-id))
  )
  (while forms 
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas upa-syms)
   (setq Eng-def (getdhaatu dhaatu class pada upasargas 'Eng-def))
   (setq ok Eng-def)
   (when (and ok
	      (member subkey '(PRESPART PASSPART PPPART PAPART
			       POTPART FUTPART FUTPPART RPPART
			       PERPPART))
	 )
    (setq ok nil)
    (setq data (getdhaatu dhaatu class pada upasargas subkey))
    (setq genders '(M F N))
    (while genders
     (setq gender (car genders))
     (setq genders (cdr genders))
     (setq san-sfx san-key-sfx)
     (cond
      ((and (equal subkey 'PRESPART) (equal pada 'P))
       (let (base strength)
;	(setq base (elt data 0)) ; a symbol
	(setq strength (elt data 1))
	(setq dtab (plist-get2 PRESPART-endings (list pada strength gender)))
       )
      )
      ((and (equal subkey 'PRESPART) (equal pada 'A))
       (setq dtab (plist-get2 PRESPART-endings (list pada  gender)))
      )
      ((equal subkey 'PASSPART)
       (setq dtab (plist-get2 PASSPART-endings (list gender)))
      )
      ((member subkey '(PPPART PAPART))
       (setq dtab (plist-get2 PPPART-endings (list subkey gender)))
      )
      ((equal subkey 'POTPART)
       (setq dtab (get-ADJ-a-endings san-key gender))
      )
      ((and (equal subkey 'FUTPART) (equal pada 'P))
       (let (strength)
	(setq strength 'SW) ; like verb of class 6
	(setq dtab (plist-get2 PRESPART-endings (list pada strength gender)))
	; unlike with PRESPART, san-key does not end in 'a';
	; i.e., the 'a' is part of of san-key-sfx here.
	; to match properly, we must remove this 'a'
	(let (s)
	 (setq s (symbol-name san-key-sfx))
	 (if (equal (elt s 0) ?a)
	     (setq san-sfx (intern (substring s 1)))
	 )
	)
       )
      )
      ((and (equal subkey 'FUTPART) (equal pada 'A))
       (setq dtab (plist-get2 PRESPART-endings (list pada  gender)))
      )
      ((and (equal subkey 'FUTPPART) )
       (setq dtab (plist-get2 PRESPART-endings (list 'A  gender)))
      )
      ((and (member subkey '(RPPART PERPPART)) (equal pada 'A))
       (setq dtab (plist-get2 PRESPART-endings (list pada  gender)))
      )
      ((and (member subkey '(RPPART PERPPART)) (equal pada 'P))
       (let (s last)
	(setq s (symbol-name san-key))
	(setq last (substring s -1)) ; last char (as string)
	(if (equal last "v")
	 (setq dtab (plist-get2 RPPART-P-endings (list 'v gender)))
	 (setq dtab (plist-get2 RPPART-P-endings (list 'uSh gender)))
	)
       )
      )
     )
     (setq m (length dtab))
     (setq i 0)
     (while (< i m)
      (setq delt (elt dtab i))
      (when (or (and (symbolp delt) (equal san-sfx delt))
                (and (listp delt) (member san-sfx delt)))
       (setq case (elt (elt case-number-set i) 0))
       (setq number (elt (elt case-number-set i) 1))
       (setq thisans (explain-PART-makeans
	 subkey-id upasargas dhaatu Eng-def gender case number class pada))
       (setq ans (append-if-new ans thisans))
      )
      (setq i (1+ i))
     )
    )    
   )
  )
  ans
 )
)
(defun explain-PART-IPP (s-sym a~Nga san-key-info &optional upa-syms
			  subkey subkey-id)
 ; IPP = indeclineble perfect participle (gerund)
 ; when there is a prefix, lyap is used
 ; otherwise, ttvaa is used
 (let (ans forms form tmp dhaatu class pada upasargas Eng-def thisans
       form1 datum ok)
  (setq forms san-key-info)
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas upa-syms)
   (setq datum (getdhaatu dhaatu class pada upasargas subkey))
   (when (or (equal datum s-sym)
	     (and (listp datum) (member s-sym datum))
	 )
    (setq form1 (dcpu-sym dhaatu class pada upasargas))
    (setq Eng-def (sanget2 form1 '(dhaatu Eng-def)))
    (setq ok Eng-def)
    (when ok
      (setq thisans (explain-PART-IPP-makeans
         subkey-id upasargas dhaatu Eng-def class))
      (setq ans (append-if-new ans thisans))
    )
   )
  )
  ans
 )
)
(defun explain-PART-INF (s-sym a~Nga san-key-info &optional upa-syms
			  subkey subkey-id)
 ; INF = indeclineble perfect participle (gerund)
 ; when there is a prefix, lyap is used
 ; otherwise, ttvaa is used
 (let (ans forms form tmp dhaatu class pada upasargas Eng-def thisans
       form1 datum ok)
  (setq forms san-key-info)
  (while forms
   (setq form (car forms))
   (setq forms (cdr forms))
   (setq tmp (sym-dcpu form))
   (setq dhaatu (elt tmp 0))
   (setq class (elt tmp 1))
   (setq pada (elt tmp 2))
   (setq upasargas upa-syms)
   (setq datum (getdhaatu dhaatu class pada upasargas subkey))
   (when (or (equal datum s-sym)
	     (and (listp datum) (member s-sym datum))
	 )
    (setq form1 (dcpu-sym dhaatu class pada upasargas))
    (setq Eng-def (sanget2 form1 '(dhaatu Eng-def)))
    (setq ok Eng-def)
    (when ok
      (setq thisans (explain-PART-INF-makeans
         subkey-id upasargas dhaatu Eng-def class))
      (setq ans (append-if-new ans thisans))
    )
   )
  )
  ans
 )
)
(defun explain-str-subanta (s-sym san-key san-key-info subanta-type)
 (if (equal cpd-form 'first)
   (explain-str-subanta-cpd s-sym san-key san-key-info subanta-type)
   (explain-str-subanta-normal s-sym san-key san-key-info subanta-type)
 )
)
(defun explain-str-subanta-normal (s-sym san-key san-key-info subanta-type)
  ; s-sym = the thing to explain, as a symbol
 (let (ans forms form  tmp case number subanta-form gender
	   Eng-def dtab ansform thisansform thisans)
  (setq forms san-key-info) 
  (when nil ; (and t forms)  dbg
   (fol-msg (format "str-subanta: %s %s %s \n"
 	    s-sym san-key subanta-type))
   (fol-msg (format "   forms=%s\n" forms))
  )
  (while forms ;(and forms (not ans)) 
   (setq subanta-form (car forms))
   (setq forms (cdr forms))
;   (fol-msg (format "subanta-form: %s\n" subanta-form))
   (let (gender-forms gender-form subantas subanta key data Eng-def dtab)
    (setq gender-forms (car forms))
    (setq forms (cdr forms))
;    (fol-msg (format "gender-forms=%s\n" gender-forms))
    (when (and (equal subanta-type 'ADJ)
	       (not (member (car gender-forms) '(M F N)))
	  )
     ; assume only the base is given. Convert to standard form
     (let (base)
      (setq base gender-forms)
      (setq gender-forms (list
        'M base 'F base 'N base))
     )
;     (fol-msg (format "NEW gender-forms=%s\n" gender-forms))
    )
    (while gender-forms
     (setq gender (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     (setq subantas (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     (while subantas
      (setq subanta (car subantas))
      (setq subantas (cdr subantas))
      (setq key (list 'subanta subanta-type subanta-form gender))
      (setq data (sanget2 subanta key))
      (when (and (equal subanta-type 'ADJ) (not data))
       ; different form for ADJ with bases
       (let (data1)
	(setq key (list 'subanta subanta-type subanta-form))
	(setq data1 (sanget2 subanta key))
	(when (and (listp data1) (equal (car data1) 'BASE))
	 (setq data data1)
	)
       )
      )
      (when data
       (if (equal (elt data 0) 'BASE)
	(progn
	 (setq Eng-def (elt data 2))
	 (let (base dtab i m i delt thisans cnlist case number san-sfx)
	  (setq san-sfx san-key-sfx)
	  (setq base (elt data 1))
	  (cond
	   ((or (and (equal subanta-type 'ADJ) (equal subanta-form 'a))
		(member (list gender subanta-form) '((M a) (N a) (F aa)))
	    )
	    (setq dtab (get-ADJ-a-endings san-key gender))
	   )
	   ((or (and (equal subanta-type 'ADJ) (member subanta-form '(i u)))
		(member (list gender subanta-form) '((M i) (N i) (F i)
						     (M u) (N u) (F u)
						    )
			)
	    )
	    (setq dtab
	     (get-explain-subanta-endings subanta-type subanta-form
					  san-key gender))
	   )
	  )
	  (setq ans (explain-substring-subanta1 ans
		      san-sfx dtab subanta gender subanta-form Eng-def)
	  )
	 )
	)
	; non-BASE type
	(progn
         (setq Eng-def (elt data 0))
         (setq dtab (elt data 1))
	 (setq ans (explain-substring-subanta1 ans
		      s-sym dtab subanta gender subanta-form Eng-def)
	 )
	)
       )
      )
     )
    )
   )
  )
  ans
 )
)
(defun explain-str-subanta-cpd (s-sym san-key san-key-info subanta-type)
  ; s-sym = the thing to explain, as a symbol
 (let (ans forms form  tmp case number subanta-form gender
	   Eng-def dtab ansform thisansform thisans)
  (setq forms san-key-info) 
  (when  nil ; (and t forms)  dbg
   (fol-msg (format "str-subanta-cpd: %s %s %s \n"
 	    s-sym san-key subanta-type))
   (fol-msg (format "   forms=%s\n" forms))
  )
  (while forms ;(and forms (not ans)) 
   (setq subanta-form (car forms))
   (setq forms (cdr forms))
   (let (gender-forms gender-form subantas subanta key data Eng-def dtab
	 ok)
    (setq gender-forms (car forms))
    (setq forms (cdr forms))
;    (fol-msg (format "gender-forms=%s\n" gender-forms))
    (when (and (equal subanta-type 'ADJ)
	       (not (member (car gender-forms) '(M F N)))
	  )
     ; assume only the base is given. Convert to standard form
     (let (base)
      (setq base gender-forms)
      (setq gender-forms (list
        'M base 'F base 'N base))
     )
;     (fol-msg (format "NEW gender-forms=%s\n" gender-forms))
    )
    (while gender-forms
     (setq gender (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     (setq subantas (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     (while subantas
      (setq subanta (car subantas))
      (setq subantas (cdr subantas))
      (setq key (list 'subanta subanta-type subanta-form gender))
      (setq data (sanget2 subanta key))
;      (fol-msg (format "chk: %s %s -> %s\n" subanta key data))
      (when (and (equal subanta-type 'ADJ) (not data))
       ; different form for ADJ with bases
       (let (data1)
	(setq key (list 'subanta subanta-type subanta-form))
	(setq data1 (sanget2 subanta key))
	(when (and (listp data1) (equal (car data1) 'BASE))
	 (setq data data1)
	)
       )
      )
      (when data
       (setq ok nil)
       (if (equal (elt data 0) 'BASE)
	(progn
	 (setq Eng-def (elt data 2))
	 (let (base san-sfx)
	  (setq san-sfx san-key-sfx)
	  (setq base (elt data 1))
	  (setq dtab subanta-form)
	  (setq ok (equal san-sfx dtab))
;	  (fol-msg (format "chk1: %s %s -> %s\n" san-sfx dtab ok))
	 )
	)
	; non-BASE type
	(progn
         (setq Eng-def (elt data 0))
	 (cond
	  ((equal subanta 'tat) (setq ok (equal s-sym subanta)))
	  (t
	   (let (tok delt delt1)
            (setq dtab (elt data 1))
	    (setq delt (elt dtab 8)) ; instrumental plural
;	    (fol-msg (format "chk: (%s) %s %s -> %s\n" s-sym subanta key delt))
	    (setq tok (car (ITRANS-parse-words-1 (symbol-name delt))))
	    (when (and (<= 3 (length tok))
		     (equal (substring tok -3) [bh i H])
		  )
	     (setq delt1 (sym-without-space (substring tok 0 -3)))
	     (setq ok (equal s-sym delt1))
	    )
	   )
	  )
	 )
	)
       )
       (when ok
	(setq case 'CPD)
	(setq thisans 
	 (explain-subanta-makeans
	   subanta gender subanta-form case number Eng-def nil)
        )
	(setq ans (append-if-new ans thisans))
       )
      )
     )
    )
   )
  )
  ans
 )
)
(defun explain-substring-subanta1 
   (ans s-match dtab subanta gender subanta-form Eng-def)
 (let (m i thisans cnlist case number delt)
	  (setq m (length dtab))
	  (setq i 0)
	  (while (< i m)
	   (setq delt (elt dtab i))
	   (when (or (and (symbolp delt) (equal s-match delt))
		     (and (listp delt) (member s-match delt)))
	    (setq cnlist (append (elt case-number-set i) nil))
	    (setq case (elt cnlist 0))
	    (setq number (elt cnlist 1))
	    (setq thisans (explain-subanta-makeans
			  subanta gender subanta-form case number Eng-def nil))
	    (setq ans (append-if-new ans thisans))
	   )
	  (setq i (1+ i))
	  )
  ans
 )
)
(defun explain-ADJ-degree (s-sym san-key san-key-info)
 (let (subanta-type ans forms form  tmp case number subanta-form gender
	   Eng-def dtab ansform thisansform thisans)
  ; s-sym = the thing to explain, as a symbol
  ;Antoine1#93.
  ;The comparative and superlative of adjectives are generally
  ; formed by adding the suffixes 'tara' and ;tama' to the stem
  ; as it appears in the instrumental plural masculine.
  ; The new base so formed is declined as an adjective in 'a'
  (setq subanta-type 'ADJ)
;  (setq forms (plist-get2 san-info (list 'subanta subanta-type)))
  (setq forms san-key-info) 
  (when (and nil forms) ; dbg
   (fol-msg (format "ADJ-degree: %s %s %s \n"
 	    s-sym san-key subanta-type))
   (fol-msg (format "   forms=%s\n" forms))
  )
  (while forms ;(and forms (not ans)) 
   (setq subanta-form (car forms))
   (setq forms (cdr forms))
   (let (gender-forms gender-form subantas subanta key data Eng-def dtab
	 i n delt base m3p degree base1 base2 nbase tok tok1 symtok)
    (setq gender-forms (car forms))
    (setq forms (cdr forms))
    (while gender-forms
     (setq gender (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     (setq subantas (car gender-forms))
     (setq gender-forms (cdr gender-forms))
     
     (while (and (equal gender 'M) subantas)
      (setq subanta (car subantas))
      (setq subantas (cdr subantas))
      (setq key (list 'subanta subanta-type subanta-form gender))
      (setq data (sanget2 subanta key))
      (setq degree nil)
      (if (equal (elt data 0) 'BASE)
       (progn
	(setq Eng-def (elt data 2))
	(let (base endings e)
	 (setq base (elt data 1))
	 (setq gender 'M)
	 ; get m3p by joing base with appropriate ending
;	 (fol-msg (format "chk: %s %s %s %s\n" subanta-type subanta-form
;					  san-key gender))
	 (if (equal subanta-form 'a)
	  (setq endings (get-ADJ-a-endings san-key gender))
	  (setq endings
	   (get-explain-subanta-endings subanta-type subanta-form
					  san-key gender))
	 )
	 (setq e (elt endings 8)) ; m3p ending
	 (setq m3p (sym-concat base e))
	)
       )
       (progn
	(setq Eng-def (elt data 0))
	(setq dtab (elt data 1))
	(setq m3p (elt dtab 8)) ; M3P
       )
      )
;      (fol-msg (format "m3p=%s  (%s)\n" m3p Eng-def))
      (when m3p
	 (setq tok (car (ITRANS-parse-words-1 (symbol-name m3p))))
	 (if (equal subanta-form 'a)
	  (setq tok1 (vconcat (substring tok 0 -2) [a]))
	  (setq tok1 (substring tok 0 -3))
	 )
	 (setq base1 (declension-join tok1 [t a r]))
	 (setq base2 (declension-join tok1 [t a m]))
	 (setq nbase (+ (length tok1) 3))
	 (setq symtok (car (ITRANS-parse-words-1 (symbol-name s-sym))))
	 (cond
	  ((< (length symtok) nbase))
	  ((equal base1 (substring symtok 0 nbase))
	   (setq degree 'COMPAR)
	   (setq base base1)
	  )
	  ((equal base2 (substring symtok 0 nbase))
	   (setq degree 'SUPERL)
	   (setq base base2)
	  )
	 )
      )
      (when degree
       (let (newdata dtab-struc newdtab newgender newgenders newsubanta)
	(setq newsubanta (vconcat base [a]))
        (setq newsubanta (sym-without-space newsubanta))
;	(fol-msg (format "chk0: %s %s %s %s\n" gender subantas m3p newsubanta))
        (setq newdata (construct-subanta1 newsubanta 'ADJ 'a))
	(setq dtab-struc (elt newdata 1)) ; (M [] F [] N [])
	(while dtab-struc
	 (setq newgender (car dtab-struc))
	 (setq dtab-struc (cdr dtab-struc))
	 (setq newdtab (car dtab-struc))
	 (setq dtab-struc (cdr dtab-struc))
;	 (fol-msg (format "chk: %s %s\n" newgender newdtab))
	 (setq n (length newdtab))
	 (setq i 0)
	 (while (< i n)
	  (setq delt (elt newdtab i))
	  (when (or (equal s-sym delt)
		   (and (listp delt) (member s-sym delt))
	        )
	   (let (thisans cnlist case number)
	    (setq cnlist (append (elt case-number-set i) nil))
	    (setq case (elt cnlist 0))
	    (setq number (elt cnlist 1))
	    (setq thisans (explain-subanta-makeans 
		  subanta gender subanta-form case number Eng-def
		  degree))
	    (setq ans (append ans (list thisans)))
	   )
	  )
	  (setq i (1+ i))
	 )
	)
       )
      )
     )
    )
   )
  )
  ans
 )
)
(defun explain-subanta-makeans
 (subanta gender form case number Eng-def degree)
 (let (key0 key def val cn)
  (if degree
   (setq key0 (list degree subanta form))
   (setq key0 (list subanta form))
  )
  (setq def (list (solution Eng-def)))
  (setq key (vector key0 def))
  (if (equal case 'CPD)
   (setq cn case)
   (setq cn (sym-without-space (vector case number)))
  )
  (setq val (vector gender cn))
  (setq val (sym-without-space val))
  (list key val)
 )
)

(defun explain-str-INDECL (s-sym san-key san-info)
 (let (s ans other-info Eng-def)
;   (setq s-sym (intern s)) ; the thing to explain, as a symbol
;   (setq avyayapada-data (sanget s-sym 'avyayapada))
;   (setq other-info (plist-get avyayapada-data 'other-info))
  (when (equal s-sym san-key)
   (setq other-info (plist-get2 san-info '(avyayapada)))
;   (fol-msg (format "chk1: %s\n" other-info))
;   (fol-msg (format "otherinfo=%s\n" other-info))
   (when other-info
    (setq other-info (remove-INDECL-upasarga other-info)))
   (when other-info
    (setq ans other-info)
   )
  )
  ans
 )
)

(defun upasarga-forms (s &optional oldans)
 ;s is a string representing a word, coded in ITRANS
 (let (ans ansx)
  (setq ansx (upasarga-forms2 s))
;  (fol-msg (format "upasarga-forms: %s %s\n\n" s ansx))
  (setq ans oldans)
  (let (thisans pfx1 sfx1 def1 ans1 ans2 pfx2 sfx2 def2 candidate)
   (while ansx
    (setq thisans (car ansx))
    (setq ansx (cdr ansx))
    (when (not (member thisans ans))
     (setq pfx1 (elt thisans 0))
     (setq sfx1 (elt thisans 1))
     (setq def1 (elt thisans 2))
     (setq ans (append-if-new ans thisans))
     (setq ans1 (upasarga-forms (symbol-name sfx1) ans))
     (while ans1
      (setq ans2 (car ans1))
      (setq ans1 (cdr ans1))
      (when (not (member ans2 ans))
       (setq pfx2 (elt ans2 0))
       (setq sfx2 (elt ans2 1))
       (setq def2 (elt ans2 2))
       (when (disjoint-P pfx1 pfx2)
        (setq candidate (list (append pfx1 pfx2) sfx2 (append def1 def2)))
        (setq ans (append-if-new ans candidate))
       )
      )
     )
    )
   )
  )
  ans
 )
)
(defun upasarga-forms2 (s)
 ; when 'nir' is a prefix, put another choice with 'nis' as pfx
 ; when 'nis' is a prefix, put another choice with 'nir' as pfx
 (let (ansx ans thisans pfx1 sfx1 def1 thisans1)
  (setq ansx (upasarga-forms1 s))
  (while ansx
   (setq thisans (car ansx))
   (setq ansx (cdr ansx))
   (setq pfx1 (elt thisans 0))
   (setq sfx1 (elt thisans 1))
   (setq def1 (elt thisans 2))
   (cond
    ((equal pfx1 '(nir))
     (setq thisans1 (list '(nis) sfx1 def1))
     (setq ans (append-if-new ans thisans1))
    )
    ((equal pfx1 '(nis))
     (setq thisans1 (list '(nir) sfx1 def1))
     (setq ans (append-if-new ans thisans1))
    )
   )
   (setq ans (append-if-new ans thisans))
  )
  ans
 )
)
(defun upasarga-forms1 (s)
 ;s is a string representing a word
 (let (ans ans0 ans1 ans2)
  (setq ans1 (upasarga-forms-nosandhi s))
  (setq ans2 (upasarga-forms-sandhi s))
  (setq ans0 (append ans1 ans2))
  ; ans0 is a list with elements of form
  ; (pfxsym sfxsym (pfxdef-syms))
  ; 1. when the prefix has an 'r' and the suffix an 'N',
  ;    the 'N' may come from 'n' due to 'n-N' sandhi
  ;    In this case, we should replace sfxsym with the 'n' version
  ; 2. In order to be able to handle multiple upasargas, 
  ; I change this to form
  ; ((pfxsym1) sfxsym ((pfxdef1-syms)))
  ; Since it is convenient to do so here, I
  ; also change the suffix due to inverse n-N sandhi.
  (setq ans nil)
  (mapcar
   (lambda (x)
    (let (thisans pfx sfx df sfx1 pfxtok)
     (setq pfx (elt x 0))
     (setq sfx (elt x 1))
     (setq df (elt x 2))

     (setq pfxtok (car (ITRANS-parse-words-1 (symbol-name pfx))))
     (if (member 'r (append pfxtok nil))
      (setq sfx1 (upasarga-forms1-check pfx sfx))
      (setq sfx1 sfx)
     )
     (setq thisans (list (list pfx) sfx1 (list df)))
     (setq ans (append-if-new ans thisans))
    )
   )
   ans0
  )
  ans
 )
)
(defun upasarga-forms1-check (pfx-sym sfx-sym)
 (let (pfx sfx sfx1-sym N-pos)
  (setq sfx1-sym sfx-sym) ; default response
  ; look for 'N' in sfx.  We can just use string-match.
  ; if found, replace it with 'n'.
  ; then join pfx to this new sfx,
  ; and see if sandhi-n-N is the same as
  ; the simple join of pfx and sfx
  (setq sfx (copy-sequence (symbol-name sfx-sym)))
  (let (old)
   (setq old case-fold-search)
   (setq case-fold-search nil) ; do NOT ignore case in search
   (setq N-pos (string-match "N" sfx)) ; just the first one matters
   (setq case-fold-search old) ; restore to whatever it was
  )
  (when N-pos
   (let (sfx2 ps ps1 sfx2-sym pstok2 pstok3 ps2 ps3)
    (setq sfx2 sfx)
    (aset sfx2 N-pos ?n) ; change it to 'n'
    (setq sfx2-sym (intern sfx2))
    (setq ps2 (sym-without-space (vector pfx-sym sfx2-sym)))
    (setq ps (sym-without-space (vector pfx-sym sfx-sym)))
    (setq pstok2 (car (ITRANS-parse-words-1 (symbol-name ps2))))
    (setq pstok3 (sandhi-n-N pstok2))
    (setq ps3 (sym-without-space pstok3))
    (when (equal ps ps3)
     (setq sfx1-sym sfx2-sym)
    )
   )
  )
  sfx1-sym
 )
)
(defun upasarga-forms-sandhi (s)
 ;s is a string representing a word
 (let (ans pairs)
  (setq ans nil)
  (setq pairs (sandhi-separate (intern s)))
  (while pairs
   (let (pair defs thisans sym1 sym2)
    (setq pair (car pairs)) ; an array of 2 symbols
    (setq sym1 (elt pair 0))
    (setq sym2 (elt pair 1))
    (setq defs (upasarga-P sym1))
    (when defs
     (setq thisans (list sym1 sym2 defs))
     (setq ans (append ans (list thisans)))
    )
   )
   (setq pairs (cdr pairs))
  )
  ans
 )
)
(defun upasarga-forms-nosandhi (s)
 ;s is a string representing a word
 (let (ans n n1 i maxlen thisans defs s1 s2)
  (setq ans nil)
  (setq n (length s))
  (setq n1 (1- n)) ; so
  (setq i 1)
  (while (< i n) ; whole word excluded
   (setq s1 (substring s 0 i))
   (setq defs (upasarga-P (intern s1)))
   (when defs
    (let (sym1 sym2)
     (setq s2 (substring s i)) ; rest of s
     (setq sym1 (intern s1))
     (setq sym2 (intern s2))
     (setq thisans (list sym1 sym2 defs))
    )
    (setq ans (append ans (list thisans)))
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun saveold-upasarga-P (sym)
 
 (let (other-info x found ans)
  (setq other-info (sanget2 sym '(avyayapada)))
;  (when other-info (fol-msg (format "upasarga-P:%s %s\n" sym other-info)))
  (while (and (not found) other-info)
   (setq x (car other-info))
   (setq found (member 'upasarga (car x)))
   (when found (setq ans (car (cdr x))))
   (setq other-info (cdr other-info))
  )
  ans
 )
)
(defun upasarga-P (sym &optional code)
 (let (other-info x found ans)
;  (if (not code) (setq code 'all))
  (if (not code) (setq code 'standard))
  (if (member sym (upasargas-get code)) t nil)
 )
)
(defun SL-upasarga-P (sym &optional code)
 (let (other-info x found ans)
;  (if (not code) (setq code 'all))
  (if (not code) (setq code 'standard))
  (if (member sym (SL-upasargas-get code)) t nil)
 )
)
(defun upasargas-get (code)
 ; coding is ITRANS
 (let (standard_upasargas non-standard_upasargas)
  (setq standard_upasargas '(
   pra ; upasarga ; forward
   paraa ; upasarga ; away , back ; Apte (also nominal prefix)
   apa ; upasarga ; away
   sam ; upasarga ; together , fully
   anu ; upasarga ; after , along
   ava ; upasarga ; down
   nis ; upasarga ; away , out
   nir ; upasarga ; away , out;  for 'nis' before vowels, soft cons.
   dus ; upasarga ; bad , hard
   dur ; upasarga ; bad , hard  ; for 'dus' before vowels, soft cons.
   vi ; upasarga ; apart , without
   aa ; upasarga ; unto , back
   ni ; upasarga ; under
   adhi ; upasarga ; near , unto
   api ; upasarga ; near
   ati ; upasarga ; over , beyond
   su ; upasarga ; good , well
   ut ; upasarga ; up , forth  ; ut
   abhi ; upasarga ; towards
   prati ; upasarga ; towards , against
   pari ; upasarga ; around
   upa ; upasarga ; near
   ))
  (setq non-standard_upasargas '(
   alam  ; upasarga ; enough
   tiras ; upasarga ; across
   aaviSh ; upasarga ; evidently ; with kRi
   pariSh ; upasarga ; around ; with kRi
   saMs   ; upasarga ; sam-before-kRi
  ))
  (cond 
   ((equal code 'standard) standard_upasargas)
   ((equal code 'all) (append standard_upasargas non-standard_upasargas))
  )
 )
)
(defun SL-upasargas-get (code)
 ; coding is SLP1
 (let (standard_upasargas non-standard_upasargas)
  (setq standard_upasargas '(
   pra ; upasarga ; forward
   parA ; upasarga ; away , back ; Apte (also nominal prefix)
   apa ; upasarga ; away
   sam ; upasarga ; together , fully
   anu ; upasarga ; after , along
   ava ; upasarga ; down
   nis ; upasarga ; away , out
   nir ; upasarga ; away , out;  for 'nis' before vowels, soft cons.
   dus ; upasarga ; bad , hard
   dur ; upasarga ; bad , hard  ; for 'dus' before vowels, soft cons.
   vi ; upasarga ; apart , without
   A ; upasarga ; unto , back
   ni ; upasarga ; under
   aDi ; upasarga ; near , unto
   api ; upasarga ; near
   ati ; upasarga ; over , beyond
   su ; upasarga ; good , well
   ut ; upasarga ; up , forth  ; ut
   aBi ; upasarga ; towards
   prati ; upasarga ; towards , against
   pari ; upasarga ; around
   upa ; upasarga ; near
   ))
  (setq non-standard_upasargas '(
   alam  ; upasarga ; enough
   tiras ; upasarga ; across
   Aviz ; upasarga ; evidently ; with kRi
   pariz ; upasarga ; around ; with kRi
   saMs   ; upasarga ; sam-before-kRi
  ))
  (cond 
   ((equal code 'standard) standard_upasargas)
   ((equal code 'all) (append standard_upasargas non-standard_upasargas))
  )
 )
)
(defun SL-upasarga-forms (s)
 ; s in SLP1
 ; 05-10-04: dropped the 3rd field (definition) in the elements
 (let (sym sym1 ans1 ans tmp ans1 pfx1 root1 def1 pfx root def s1)
  (setq sym (intern s))
  (setq sym1 (translate-SLP1-ITRANS sym)) ; in ITRANS
  (setq s1 (symbol-name sym1))
  (setq ans1 (upasarga-forms s1))
  (while ans1
   (setq tmp (car ans1))
   (setq ans1 (cdr ans1))
   (setq pfx1 (elt tmp 0))
   (setq root1 (elt tmp 1))
   (setq def1 (elt tmp 2))
   (setq pfx (translate-ITRANS-SLP1 pfx1))
   (setq root (translate-ITRANS-SLP1 root1))
   (setq def def1)
;   (setq ans (cons (list pfx root def) ans))
   (setq ans (cons (list pfx root) ans))
  )
;  (fol-msg (format "chk1: %s\n" ans))
  (setq ans (SL-upasarga-forms1 ans))
;  (fol-msg (format "chk2\n"))
  (nreverse ans)
 )
)
(defun SL-upasarga-forms1 (old-forms)
 ; 05-10-04. Changed to provide additional alternatives, e.g.
 ;(SL-upasarga-forms "aByaziYcatAm") -> 
 ;  old: (((aBi) aziYcatAm (t)))
 ;  new: (((aBi) aziYcatAm (t)) ((aBi) asiYcatAm (t)))
 ; applies only when these conditions hold:
 ;  - last upasarga ends in 'i' or 'u', 
 ;  - rest of word starts with 'az'.
 (let (ans tmp pfxes root thisans rname)
  (setq ans old-forms) ; all of old answer kept. may be some additional forms
  (while old-forms
   (setq tmp (car old-forms))
   (setq old-forms (cdr old-forms))
   (setq pfxes (elt tmp 0))
   (setq root (elt tmp 1))
   (setq rname (symbol-name root))
;   (fol-msg (format "pfxes=%s, root=%s\n" pfxes root))
   (cond
    ((< (length rname) 3))
    ((not (equal (substring rname 0 2) "az")))
    ((let (lastpfx)
      (setq lastpfx (car (nreverse pfxes)))
      (not (member (substring (symbol-name lastpfx) -1) '("i" "u")))
     )
    )
    (t
     ; add additional element to answer by changing 'root' to
     ; start with 'as'
     (setq root (intern (concat "as" (substring rname 2))))
     (setq thisans (list pfxes root))
     (setq ans (cons thisans ans))
    )
   )
  )
  ans
 )
)
(defun inverse-aagama (s)
 (let (a~Nga ans j n s1 j1 s0vals dhaatus s-tok v uar s0 thisans u-string u)
  ; if s were a form conjugated in la~N, then it would
  ; be formed by adding the augment 'a' to the verbal base.
  ; if the root begins with a vowel, this 'a' would be added
  ; by forming vrddhi of the initial vowel. 
  ; s0 should be that to which 'a' is added
  (setq s-tok (car (ITRANS-parse-words-1 s)))
  (setq v (elt s-tok 0))
  (cond
   ((not (vowel-P v)) (setq uar nil)) ;this won't parse, s is not a la~N
   ((equal v 'a)  ; if it's a la~N, root starts with consonant
    (setq uar (list (substring s-tok 1)))
   )
   (t
    (setq uar (inverse-vrddhis s-tok)) ; when v has an inverse-vrddhi
   )
  )
  (setq ans nil)
  (while uar
   (setq u (car uar)) 
   (setq uar (cdr uar))
   ; u is a token array. Turn it into a string, u-string
   (setq u-string (symbol-name (sym-without-space u)))
   (setq ans (append-if-new ans u-string))
  )
  ans
 )
)

(defun explain-VERB-makeans
  (tense-sym dhaatu class pada person number Eng-def upasargas
	     &optional voice-sym)
 (let (key0 key def val pn verb cp)
  (if upasargas
   (setq verb (append upasargas (list dhaatu)))
   (setq verb dhaatu)
  )
 ; temporary (08-06-03) for comparison with prev versions
  (when (equal voice-sym 'PASSIVE)
    (setq class nil)
    (setq pada 'PASSIVE)
  )
  (if class
   (progn 
    (setq cp (sym-without-space (vector class pada)))
    (setq key0 (list tense-sym verb cp))
   )
   (setq key0 (list tense-sym verb pada))
  )
  ; 08-05-03. We want to include PASSIVE
  (if (and voice-sym (not (equal pada 'PASSIVE)))
   (setq key0 (append key0 (list voice-sym)))
  )
;  (fol-msg (format "Eng-def=%s\n" Eng-def))
  (setq def (mapcar 'solution Eng-def)) ;6-12-03
  (setq def (list (solution def)))
  (setq key (vector key0 def))
  (setq pn (sym-without-space (vector person number)))
  (setq val pn)
  (list key val)
 )
)

(defun explain-PRON (s-sym a~Nga san-info)
; (fol-msg (format "explain-PRON : %s %s\n" s-sym a~Nga))
 (let (ans a~Nga-info form-sym)
;  (setq a~Nga-info (sanget2 a~Nga '(PRON other-info)))
;  (setq a~Nga-info (plist-get2 san-info '(PRON other-info)))
  (setq a~Nga-info (plist-get2 san-info '(PRON)))
  (while a~Nga-info
   (let (ai Eng-def thisans subanta subanta-info si gender-sym)
    (setq ai (car a~Nga-info))
    (setq a~Nga-info (cdr a~Nga-info))
    ; ai = (subanta gender-sym form) 
    ; (aham M IRR-PRON)
    ; We proceed no further unless gender-sym = PRON , e.g. (tat PRON b)
    ; (a) form = ...-PRON, e.g. (tvam M IRR-PRON)
    (setq gender-sym (elt ai 1))
    (when (equal gender-sym 'PRON)
     (setq form-sym (elt ai 2))
     (setq subanta (elt ai 0)) ; the citation form for a~Nga
;     (setq subanta-info (sanget2 subanta '(subanta other-info)))
     ; subanta-info should contain an element whose form is
     ; (a~Nga PRON form-sym nil)
     (progn
      (setq Eng-def (sanget2 subanta '(subanta Eng-def)))
       (let (praatipadikas dtab dtabs  m i gender genders case number delt)
	(setq praatipadikas (list a~Nga))
	(setq genders '(M F N))
	(while (and genders praatipadikas)
	 (setq gender (car genders))
	 (setq genders (cdr genders))
	 (setq dtabs (explain-declension-pron subanta praatipadikas gender form-sym ))
	 (while dtabs
	  (setq dtab (car dtabs))
	  (setq dtabs (cdr dtabs))
	  (setq m (length dtab))
	  (setq i 0)
	  (while (< i m)
	   (setq delt (elt dtab i))
	   (when (or (and (symbolp delt) (equal s-sym delt))
		    (and (listp delt) (member s-sym delt)))
	    (setq case (elt (elt case-number-set i) 0))
	    (setq number (elt (elt case-number-set i) 1))
	    (setq thisans (explain-PRON-makeans
	     subanta gender form-sym case number Eng-def))
;	    (fol-msg (format "thisans=%s\n" thisans))
	    (setq ans (append-if-new ans thisans))
	   )
	   (setq i (1+ i))
          )
	 )
	)
       )
     )
    ); when
   )
  )
  ans
 )
)
(defun explain-PRON-makeans
 (subanta gender form case number Eng-def)
 (let (key0 key def val cn supform)
;  (setq supform (sym-without-space (vector form '-ADJ)))
;  (setq key0 (list subanta supform))
  (setq key0 (list subanta form))
  (setq def (list (solution Eng-def)))
  (setq key (vector key0 def))
  (setq cn (sym-without-space (vector case number)))
  (setq val (vector gender cn))
  (list key val)
 )
)

(defun explain-ADJ (s-sym a~Nga san-info)
 (let (ans a~Nga-info form-sym )
  (setq a~Nga-info (plist-get2 san-info '(ADJ)))
  (while a~Nga-info
   (let (ai Eng-def thisans subanta subanta-info si found praatipadikas)
    ; ai = (subanta gender-sym form) 
    ; (diirgha ADJ a)
    ; we proceed no further unless gender-sym = ADJ
    (setq ai (car a~Nga-info))
;    (fol-msg (format "explain-ADJ: a~Nga = %s, ai=%s\n" a~Nga ai))
    (setq a~Nga-info (cdr a~Nga-info))
    (when (equal (elt ai 1) 'ADJ) 
     (setq form-sym (elt ai 2))
     (setq subanta (elt ai 0)) ; the citation form for a~Nga
     (setq subanta-info (sanget2 subanta '(subanta other-info)))
;     (fol-msg (format "subanta-info=%s\n" subanta-info))
     ; subanta-info should contain an element whose form is
     ; (a~Nga ADJ form-sym nil)
     ; OR (6-22-03 for 'sat)
     ; (x ADJ form-sym nil), where x is a list containing 'a~Nga
     (setq found nil)
     (let (si cur test)
      (setq test (list a~Nga 'ADJ form-sym nil))
      (setq si subanta-info)
      (while (and si (not found))
       (setq cur (car si))
       (setq si (cdr si))
       (cond
	((equal test cur)
	 (setq found t)
	 (setq praatipadikas (list a~Nga))
	)
	((and (equal (cdr test) (cdr cur))
	      (listp (car cur))
	      (member a~Nga (car cur)))
	 (setq found t)
	 (setq praatipadikas (car cur))
        )
       )
      )
     )
     (when found
;      (member (list a~Nga 'ADJ form-sym nil) subanta-info)
      (setq Eng-def (sanget2 subanta '(subanta Eng-def)))
       (let ( dtab m i gender genders case number delt)
;	(setq praatipadikas (list a~Nga))
	(setq genders '(M F N))
	(while (and genders praatipadikas)
	 (setq gender (car genders))
	 (setq genders (cdr genders))
	 (setq dtab
	    (explain-declension-adj subanta praatipadikas gender form-sym ))
;	 (fol-msg (format "%s %s %s: %s\n" praatipadikas gender form-sym dtab))
	 (setq m (length dtab))
	 (setq i 0)
	 (while (< i m)
	  (setq delt (elt dtab i))
	  (when (or (and (symbolp delt) (equal s-sym delt))
		    (and (listp delt) (member s-sym delt)))
	   (setq case (elt (elt case-number-set i) 0))
	   (setq number (elt (elt case-number-set i) 1))
	   (setq thisans (explain-ADJ-makeans
	     subanta gender form-sym case number Eng-def))
	   (setq ans (append-if-new ans thisans))
	  )
	  (setq i (1+ i))
         )
	)
       )
     )
    ); when
   )
  )
  ans
 )
)

(defun explain-ADJ-makeans
 (subanta gender form case number Eng-def &optional degree)
 (let (key0 key def val cn supform)
;  (setq supform (sym-without-space (vector form '-ADJ)))
;  (setq key0 (list subanta supform))
  (setq key0 (list subanta form))
  (setq def (list (solution Eng-def)))
  (if degree
   (setq key (vector degree key0 def))
   (setq key (vector key0 def))
  )
  (setq cn (sym-without-space (vector case number)))
  (setq val (vector gender cn))
  (setq val (sym-without-space val))
  (list key val)
 )
)
(defun explain-PART-makeans
 (type upasargas dhaatu Eng-def gender case number
       &optional class pada voice-sym)
 (let (key0 key def val cn verb cp)
  (if upasargas
   (setq verb (append upasargas (list dhaatu)))
   (setq verb dhaatu)
  )
 ; temporary (08-06-03) for comparison with prev versions
  (when (equal voice-sym 'PASSIVE)
    (setq class nil)
    (setq pada 'PASSIVE)
  )
  (if class
   (progn 
    (setq cp (sym-without-space (vector class pada)))
    (setq key0 (list type verb cp))
   )
   (setq key0 (list type verb))
  )
;  (setq def (list (solution Eng-def)))
  (setq def (mapcar 'solution Eng-def)) ;6-12-03
  (setq def (list (solution def)))  
  (setq key (vector key0 def))
  (setq cn (sym-without-space (vector case number)))
  (setq val (vector gender cn))
  (setq val (sym-without-space val))
  (list key val)
 )
)
(defun explain-PART-IPP-makeans
 (type upasargas dhaatu Eng-def class)
 (let (key0 key def val cn verb cp)
  (if upasargas
   (setq verb (append upasargas (list dhaatu)))
   (setq verb dhaatu)
  )
  (if class
   (progn 
    (setq cp (sym-without-space (vector class)))
    (setq key0 (list type verb cp))
   )
   (setq key0 (list type verb))
  )
  (setq def (list (solution Eng-def)))
  (setq key (vector key0 def))
  (list key)
 )
)
(defun explain-PART-INF-makeans
 (type upasargas dhaatu Eng-def class)
 (explain-PART-IPP-makeans type upasargas dhaatu Eng-def class)
)

(defun remove-INDECL-upasarga (x-list)
 (let (ans x)
  (setq ans nil)
  (while x-list
   (setq x (car x-list))
   (setq x-list (cdr x-list))
   ; x has the form (xtype) (xdef)
   (if (not (and (listp x) (equal (solution (car x)) 'upasarga)))
    (setq ans (append ans (list x)))
   )
  )
  ans
 )
)
(defun extract-Subanta ()
 (let (all n i sym symvec)
  (setq all (sangetall 'Subanta))
  (while all
   (setq sym (car all))
   (setq all (cdr all))
   (setq symvec (car all))
   (setq all (cdr all))
   (setq n (length symvec))
   (setq i 0)
   (while (< i n)
    (let (x m j y)
     (setq x (extract-Subanta1 (elt symvec i) sym))
     (setq m (length x))
     (setq j 0)
     (while (< j m)
      (setq y (elt x j))
      (setq j (1+ j))
      (fol-msg (format ": %s : %s %s\n"
        y sym i))
     )
    )
    (setq i (1+ i))
   )
  )
  t
 )
)
(defun extract-Subanta1 (x sym)
 ;x is assumed to be an ITRANS  token array (an array of certain symbols)
 ;a vector, including x and variants of x, is returned
 (if (equal x []) (vector [])
 (let (ans i n n1 y c x1)
  (setq ans (list x))
  (setq n (length x))
  (setq n1 (1- n))
  (setq c (elt x n1))
  (setq x1 (substring x 0 n1))
  ;1. replace final 's with 'H
  (when (equal c 's)
   (setq y (vconcat x1 [H]))
   (setq ans (append ans (list y)))
  )
  ;2. replace non-last 'n with 'N
  (setq y (replace-n x))
  (when y
   (setq ans (append ans (list y)))
  )
  ;3. for verbal endings of class type 1, replace initial
  ;  'i or 'ii with 'e
  (let (sym-name)
   (setq sym-name (symbol-name sym))
   (when (string-match "-1" sym-name)
    (when (member (elt x 0) '(i ii))
     (setq y (vconcat x1 [e]))
     (setq ans (append ans (list y)))
    )
   )
  )
  (vconcat ans) ; return a vector
 ))
)
(defun replace-n (x)
 ;x is an array. Returns an array or nil
 (let (ans y z)
  (setq ans nil)
  (setq y (vconcat [r i] x))
  (setq z (sandhi-n-N y))
  (when z
   (setq ans (substring z 2))
;   (fol-msg (format "replace-n chk: %s -> %s\n" x z))
  )
  ans
 )
)
(defun inverse-vrddhis (tok)
 (let (ans ntok)
  (setq ans nil)
  (setq ntok (length tok))
  (mapcar 
   (lambda (v) 
    (let (vrddhi-tok match n thisans)
     (setq vrddhi-tok (sanget v 'vrddhi))
     (setq n (length vrddhi-tok))
     (setq match (and
      (<= n ntok)
      (equal vrddhi-tok (substring tok 0 n))
     ))
     (when match
      (setq thisans (vconcat (vector v) (substring tok n)))
      (setq ans (append ans (list thisans)))
     )
    )
   )
   vowel-set
  )
  ans
 )
)
(defun check-duals (w1 ans1 w2)
; some sandhis are invalidated by certain explanations,
; e.g., (Antoine 31):
; 'ii', 'uu', and 'e', when dual terminations, remain unchanged
; before vowels
 (let (xtype ans1a ans1b ex)
  (setq xtype (elt ans1 0))
  (and
   (member (sym-ends-with w1) '(ii uu e))
   (vowel-P (sym-begins-with w2))
   (or (equal xtype 'NOUN) (equal xtype 'ADJ))
   (setq ans1a (elt ans1 1)) ; list of explanations
   (setq ans1b nil) ; those of ans1a which are not duals
   (while ans1a
    (let (expl x)
     (setq expl (car ans1a))
     ; expl is like (lataa F aa 4 S creeper)
     ; we don't want the 4th element (like S) to be D
     (setq x (elt expl 4))
     (when (not (equal x 'D)) ; keep this one
      (if (not ans1b)
       (setq ans1b (list expl))
       (setq ans1b (append ans1b (list expl)))
      )
     )
    )
    (setq ans1a (cdr ans1a))
   )
   (setq ans1 ans1b)
   (setq ans1 (cons xtype (list ans1)))
  )
 )
 ans1
)
(defun init-explain-forms ()
 (let (explain-upasarga-forms-nonpfx explain-upasarga-forms-pfx
       explain-forms-nonpfx explain-forms-pfx)
  
   (setq explain-verb-forms
   '((VERB laT ACTIVE) (VERB laT-P PASSIVE)
     (VERB la~N ACTIVE) (VERB la~N-P PASSIVE)
     (VERB loT ACTIVE) (VERB loT-P PASSIVE)
     (VERB vidhili~N ACTIVE) (VERB vidhili~N-P PASSIVE)
     (VERB liT-r ACTIVE) (VERB liT-r-P PASSIVE)
     (VERB liT-p ACTIVE) (VERB liT-p-P PASSIVE)
     (VERB luT ACTIVE)  (VERB luT-P PASSIVE)
     (VERB lRiT ACTIVE) (VERB lRiT-P PASSIVE)
     (VERB lRi~N ACTIVE) (VERB lRi~N-P PASSIVE)
     (VERB lu~N1 ACTIVE) (VERB lu~N1-P PASSIVE)
     (VERB lu~N2 ACTIVE) (VERB lu~N2-P PASSIVE)
     (VERB lu~N3 ACTIVE) (VERB lu~N3-P PASSIVE)
     (VERB lu~N4 ACTIVE) (VERB lu~N4-P PASSIVE)
     (VERB lu~N5 ACTIVE) (VERB lu~N5-P PASSIVE)
     (VERB lu~N6 ACTIVE) (VERB lu~N6-P PASSIVE)
     (VERB lu~N7 ACTIVE) (VERB lu~N7-P PASSIVE)
    ))
 (setq explain-part-forms
  '((PART laT ACTIVE) ; PRESPART PRES-ACT
    (PART laT PASSIVE) ; PRESPART PRES-PASS
    (PART PERF PASSIVE) ; PPPART PERF-PASS
    (PART PERF ACTIVE)  ; PPPART PERF-ACT
    (PART POT PASSIVE)  ; POTPART POT-PASS
    (PART IPP  PASSIVE) ;* IPPART IPP explain-PART-IPP
    (PART RPERF ACTIVE) ; RPPART RPERF
    (PART PERPERF ACTIVE) ; PERPPART PERPERF
    (PART lRiT ACTIVE) ; FUTPART  FUT-ACT
    (PART lRiT PASSIVE) ; FUTPART  FUT-PASS
   ; the infinitive is not a participle, but the logic is similar
    (PART INF  ACTIVE) ; * INF INF explain-PART-INF
    )
 )
 (setq explain-upasarga-forms
   (append explain-verb-forms explain-part-forms))


 (setq explain-ADJ-forms
  '((ADJ)
   )
 )
 (setq explain-other-forms 
   '((NOUN)
     (INDECL)
     (PRON)
    )
 )
 (setq explain-forms
      (append explain-verb-forms
	      explain-part-forms
	      explain-ADJ-forms
	      explain-other-forms))
 (setq explain-forms-aagama nil)
 (setq explain-pr-forms '(VERB NOUN PART ADJ INDECL PRON))
 (setq save-explain-forms explain-forms)
 (setq save-explain-upasarga-forms explain-upasarga-forms)

 (setq explain-forms '(dhaatu subanta avyayapada))
 (setq explain-upasarga-forms '(dhaatu))
 (setq explain-dhaatu-plist '(
   laT (VERB ACTIVE)
   laT-P (VERB PASSIVE)
   la~N (VERB ACTIVE)
   la~N-P (VERB PASSIVE)
   loT (VERB ACTIVE)
   loT-P (VERB PASSIVE)
   vidhili~N (VERB ACTIVE)
   vidhili~N-P (VERB PASSIVE)
   liT-r (VERB ACTIVE)
   liT-r-P (VERB PASSIVE)
   liT-p (VERB ACTIVE)
   liT-p-P (VERB PASSIVE)
   luT (VERB ACTIVE)
   luT-P (VERB PASSIVE)
   lRiT (VERB ACTIVE)
   lRiT-P (VERB PASSIVE)
   lRi~N (VERB ACTIVE)
   lRi~N-P (VERB PASSIVE)
   lu~N1 (VERB ACTIVE)
   lu~N1-P (VERB PASSIVE)
   lu~N2 (VERB ACTIVE)
   lu~N2-P (VERB PASSIVE)
   lu~N3 (VERB ACTIVE)
   lu~N3-P (VERB PASSIVE)
   lu~N4 (VERB ACTIVE)
   lu~N4-P (VERB PASSIVE)
   lu~N5 (VERB ACTIVE)
   lu~N5-P (VERB PASSIVE)
   lu~N6 (VERB ACTIVE)
   lu~N6-P (VERB PASSIVE)
   lu~N7 (VERB ACTIVE)
   lu~N7-P (VERB PASSIVE)
   aashiirli~N (VERB ACTIVE)
   aashiirli~N-P (VERB PASSIVE)
   PRESPART (PART laT ACTIVE  PRES-ACT nil)
   PASSPART (PART laT PASSIVE PRES-PASS nil)
   PPPART (PART PERF PASSIVE PERF-PASS nil)
   PAPART (PART PERF ACTIVE PERF-ACT nil)
   POTPART (PART POT PASSIVE POT-PASS nil)
   IPPART (PART IPP PASSIVE IPP explain-PART-IPP)
   RPPART (PART RPERF ACTIVE RPERF nil)
   PERPPART (PART PERPERF ACTIVE PERPERF nil)
   FUTPART (PART lRiT ACTIVE FUT-ACT nil)
   FUTPPART (PART lRiT PASSIVE FUT-PASS nil)
   INF (PART INF ACTIVE INF explain-PART-INF)
  ))
 t
 )
)


(defun explain-declension-init (sym)
 (let ()
  (setq declension-buffer nil)
  (setq declension-buffer-key sym)
 )
)
(defun explain-declension-get (type key)
 "This was introduced to avoid multiple calls
  to get decelensions or conjugations.
  A simple test showed a reduction from 17 seconds to 5 seconds"
 (let (ans all x more tk tkx)
  (setq all declension-buffer)
  (setq tk (list type key))
;  (setq all nil) ; for debugging
  (setq more t)
  (while (and all more)
   (setq x (car all))
   (setq all (cdr all))
   (setq tkx (car x))
   (when (equal tkx tk)
    (setq ans (cadr x))
    (setq more nil)
   )
  )
  (when ans
;   (fol-msg (format "declension-get: %s %s\n" type key))
  )
  ans
 )
)
(defun explain-declension-put (type key ans)
 (let (tk rec)
;  (fol-msg (format "declension-put: %s %s\n" type key))
  (setq tk (list type key))
  (setq rec (list tk ans))
  (setq declension-buffer (append-if-new declension-buffer rec))
 )
)
(defun explain-declension-noun (subanta)
 (let (ans key type)
  (setq type 'noun)
  (setq key subanta)
  (setq ans (explain-declension-get type key))
  (when (not ans)
   (setq ans (declension-citation subanta))
   (when ans
    (explain-declension-put type key ans)
   )
  )
  ans
 )
)



(defun explain-declension-perperf-part (praatipadikas gender pada)
 ; periphrastic perfect participle (active)
 (let (ans key type )
;  (fol-msg (format "explain-declension-perperf-part INCOMPLETE: %s %s\n"
;	   praatipadikas gender))

  (setq type 'perperf-part-active)
  (setq key (list praatipadikas gender pada))
  (setq ans (explain-declension-get type key))
  (when (not ans)
   (setq ans (declension-perperf-part praatipadikas gender pada))
   (when ans
    (explain-declension-put type key ans)
   )
  )
  ans
 )
)


(defun explain-declension-pron (subanta praatipadikas gender form-sym)
 (let (ans key type)
  (setq type 'pron)
  (setq key (list subanta praatipadikas gender form-sym))
  (setq ans (explain-declension-get type key))
  (when (not ans)
   (setq ans (declension-pron subanta praatipadikas gender form-sym))
   (when ans
    (explain-declension-put type key ans)
   )
  )
  ans
 )
)
(defun explain-declension-adj (subanta praatipadikas gender form-sym)
 (let (ans key type)
  (setq type 'adj)
  (setq key (list subanta praatipadikas gender form-sym))
  (setq ans (explain-declension-get type key))
  (when (not ans)
   (setq ans (declension-adj subanta praatipadikas gender form-sym))
   (when ans
    (explain-declension-put type key ans)
   )
  )
  ans
 )
)
(defun explain-conjugation (a~Nga tense-sym classtmp padatmp dhaatu)
 (let (ans key type)
  (setq type 'conjugation)
  (setq key (list a~Nga tense-sym classtmp padatmp dhaatu))
;  (fol-msg (format "explain-conjugation: %s\n" key))
  (setq ans (explain-declension-get type key))
  (when (not ans)
   (setq ans (conjugation-tab a~Nga tense-sym classtmp padatmp dhaatu))
   (when ans
    (explain-declension-put type key ans)
   )
  )
  ans
 )
)


(defun Sx1 (s)
 "s is a string, representing an inflected word or 
  an indeclineable, coded in the SLP1 form."
 (let (ans pfx sfx j n n1 pfxdata thisans ans1 inflection)
  (setq n (length s))
  (setq n1 (1+ n))
  (setq j 1)
  (while (< j n1)
   (setq pfx (substring s 0 j))
   (setq sfx (substring s j))
;   (fol-msg (format "%s %s %s %s\n" j s pfx sfx))
   (setq j (1+ j))
   (setq pfxdata (Sx1-pfx pfx))
   (while pfxdata
    (let (data word parms tab dict)
     (setq data (car pfxdata))
     (setq pfxdata (cdr pfxdata))
     (setq word (elt data 0))
     (setq dict (elt data 1))
     (setq parms (elt data 2))
     (setq tab (elt data 3))
     (setq inflection (elt parms 0)) ; V, S, or I
     (cond
      ((equal tab 'B)
       (cond
	((equal inflection 'S)
	 (let (category subtype gender searchtype regexp genders)
	  (setq category (elt parms 1))
	  (setq subtype (elt parms 2))
	  (setq searchtype (format "%s-%s" category subtype))
	  (cond
	   ((equal category 'NOUN)
	    (setq gender (elt parms 3))
	    (if (and (member (list gender subtype)
			     '((M a) (N a) (F aa)
			       (M i) (F i) (N i)
			       (M u) (F u) (N u)
			       )
		      )
	         (SL-n-P pfx)
	        )
	     (setq searchtype (format "%s-N" searchtype))
	    )
	    (setq regexp (format "^%s %s %s " sfx searchtype gender))
	    (when nil ; dbg
	     (fol-msg (format "CHK: %s -> %s\n" parms regexp))
	    )
	    (setq ans1 (Sx1-endmatch regexp))
	    (setq ans (Sx1-updateans ans word parms ans1 dict))
	   )
	   ((equal category 'ADJ)
	    (setq gender (elt parms 3))
	    (if (member gender '(M F N))
	     (setq genders (list gender))
	     (setq genders '(M F N))
	    )
	    (if (and (member subtype '(a i u))
		     (SL-n-P pfx)
	         )
	      (setq searchtype (format "%s-N" searchtype))
	    )
	    (while genders
	     (setq gender (car genders))
	     (setq genders (cdr genders))
	     (setq regexp (format "^%s %s %s " sfx searchtype gender))
	     (setq ans1 (Sx1-endmatch regexp))
	     (when  nil ; dbg
	      (fol-msg (format "chk: %s -> %s\n" regexp ans1))
	     )
	     (setq ans (Sx1-updateans ans word
				      (append parms (list gender)) ans1 dict))
	    )
	   )
	  )
	 )
	)
	((equal inflection 'V)
	 (let (class voice conj regexp)
          (when nil ; dbg
	   (let (tmp)
	    (setq tmp (SL-n-P pfx))
            (fol-msg (format "chk: %s %s\n" pfx tmp))
	   )
	  )
	  (setq class (elt parms 1))
	  (setq voice (elt parms 2)) ; P, A, PV
	  (setq conj (elt parms 3)) ; PRE=present, etc
	  (cond
	   ((and (equal conj 'IPV)
		 (equal voice 'P)
		 (member class '(1 4 6 10))
		 (equal sfx "ARi")
	    )
	    (when (SL-n-P pfx)
	     (setq ans1 '(1S))
	     (setq ans (Sx1-updateans ans word parms ans1 dict))
	    )
	   )
	   (t
	    (setq regexp (format "^%s %s %s " sfx conj voice))
	    (setq ans1 (Sx1-endmatch regexp))
	    (setq ans (Sx1-updateans ans word parms ans1 dict))
	   )
	  )
	 )
	)
	((equal inflection 'I)
	 (when (equal s pfx) ; match on all of s required
;	  (setq ans (cons (list word parms) ans))
	  (setq ans (Sx1-updateans ans word parms ans1 dict))
	 )
	)
       )
      )
      ((arrayp tab)
;       (setq ans1 (Sx1-tabmatch s tab inflection))
       (setq ans1 (Sx1-tabmatch (concat "=" sfx) tab inflection))
       (setq ans (Sx1-updateans ans word parms ans1 dict))
      )
      ((numberp tab)
;       (fol-msg (format "chk2: %s %s %s\n" s tab inflection))
       (cond
	((not (equal s pfx))) ;
	((equal inflection 'S)
	 (let (category subtype gender cn)
	  (setq category (elt parms 1))
	  (setq subtype (elt parms 2))
	  (setq gender (elt parms 3))
	  (setq cn (sym-without-space (elt case-number-set tab)))
	  (setq ans1 (list cn))
	  (setq ans (Sx1-updateans ans word parms ans1 dict))
	 )
	)
	((equal inflection 'V)
	 (let (class voice conj regexp pn)
	  (setq class (elt parms 1))
	  (setq voice (elt parms 2)) ; P, A, PV
	  (setq conj (elt parms 3)) ; PRE=present, etc
	  (setq pn (sym-without-space (elt person-number-set tab)))
	  (setq ans1 (list pn))
	  (setq ans (Sx1-updateans ans word parms ans1 dict))
	 )
	)
	
       )
      )
      (t
       (fol-msg (format "Sx1 error: %s %s %s %s %s\n"
		       s pfx sfx data tab))
      )
     )
    )
   )
  )
  ans
 )
)
(defun Sx1-updateans (ans word parms ans1 dict)
 (let (thisans)
  (while ans1
    (setq thisans (car ans1))
    (setq ans1 (cdr ans1))
    (setq ans (cons (list word parms thisans dict) ans))
  )
  ans
 )
)
(defun Sx1-tabmatch (s tab inflection)
 "s is a string.
  tab is an array, each element of which
     is either a symbol or a list of symbols
  inflection is either
    'S' for subanta (a word declined), or
    'V' for verb (a word conjugated)
 "
; (fol-msg (format "tabmatch: %s %s %s\n" s tab inflection))
 (let (ans i n syms cn)
  (setq n (length tab))
  (setq i 0)
  (while (< i n)
   (setq syms (elt tab i))
   (if (not (listp syms)) (setq syms (list syms)))
   (mapcar 
    (lambda (sym2)
     (if (not (symbolp sym2))
      (fol-msg (format "not a symbol: %s\n" sym2))
     )
     (when (equal s (symbol-name sym2))
      (cond
       ((equal inflection 'S)
	(setq cn (sym-without-space (elt case-number-set i)))
	(setq ans (cons cn ans))
       )
       ((equal inflection 'V)
	(setq cn (sym-without-space (elt person-number-set i)))
	(setq ans (cons cn ans))
       )
       (t
       )
      )
     )
    )
    syms
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun Sx1-pfx (pfx)
 "pfx is a string.
 "
 (let (bufin regexp ans word parms tab filenames)
  (setq regexp (format "^%s " pfx))
  (setq filenames '(
   "MW-pco.txt"
   "MW-noun.txt"
   "MW-adj.txt"
   "MW-ind.txt"
   "conjtabs-MW.txt"
   "causalconjtabs-MW.txt"
   "participles-MW.txt"
  ))
  (while filenames
   (setq ans (Sx1-pfxa regexp (car filenames) ans))
   (setq filenames (cdr filenames))
  )  
  ans
 )
)
(defun Sx1-pfxa (regexp filename ans)
 "pfx is a string.
 "
 (let (word parms tab bufin dict wname shortword)
  (setq bufin
   (find-file-noselect (sangram-filename filename "construct") t)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (while (search-forward-regexp regexp nil t)
    (setq word (read (current-buffer)))
    (setq dict (read (current-buffer)))
    (setq parms (read (current-buffer)))
    (setq tab (read (current-buffer)))
    ; both 'word' and the elements of 'tab' (if it is an array) may
    ; have the form '=xxx'. We want to expand the symbol represented by
    ; '='
    (setq wname (symbol-name word))
    (when (equal (elt wname 0) ?=) ; initial char is '='
     (let (p pfx s1 s2 s)
      (setq p (point))
      (beginning-of-line)
      (setq pfx (read (current-buffer)))
      (setq s1 (symbol-name pfx))
      (setq s2 (substring wname 1))
      (setq s (concat s1 s2))
      (setq word (intern s)) ; so it is a symbol
      (goto-char p)
     )
    )
    (setq ans (cons (list word dict parms tab) ans))
   )
  )
  ans
 )
)
(defun Sx1-endmatch (regexp &optional bufin)
 "'sfx' and 'searchtype' are strings.
         
 "
 (let (ans i cn)
  (if (not bufin)
   (setq bufin
    (find-file-noselect (sangram-filename "endings.txt" "construct"))
   )
  )
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (while (search-forward-regexp regexp nil t)
    (setq i (read (current-buffer)))
    (setq cn (read (current-buffer)))
    (setq ans (cons cn ans))
   )
  )
  ans
 )
)
(defun SL-n-P (base)
 "base, a symbol or string coded in SLP1, is checked to see if
  it provides the context for changing a subsequent dental nasal 'n'
  to retroflex 'R'.
 "
 (let (x)
  (setq x (if (symbolp base) (symbol-name base) base))
  (if (sandhi-single (translate-SLP1-ITRANS (intern (concat x "ana"))))
   t
   nil
  )
 )
)
(defun SL-n-P0 (base)
 "base, a symbol or string coded in SLP1, is checked to see if
  it is spelled 'properly', as determined by 'sandhi-n-N'.
 "
 (let (x tok y z)
  (setq x (if (symbolp base)  base (intern base))) ; now x is symbol
  (setq y (translate-SLP1-ITRANS x)) ; y is a symbol
  (setq z (symbol-name y))
  (setq tok (car (ITRANS-parse-words-1 z))) ; a token sequence in ITRANS
  (if (sandhi-n-N tok)
   t
   nil
  )
 )
)
(defun old-Sx1-pfxa (regexp filename ans)
 "pfx is a string.
 "
 (let (word parms tab bufin dict)
  (setq bufin
   (find-file-noselect (sangram-filename filename "construct"))
  )
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (while (search-forward-regexp regexp nil t)
    (setq word (read (current-buffer)))
    (when (equal word '=)
     (let (p)
      (setq p (point))
      (beginning-of-line)
      (setq word (read (current-buffer)))
      (goto-char p)
     )
    )
    (setq dict (read (current-buffer)))
    (setq parms (read (current-buffer)))
    (setq tab (read (current-buffer)))
    (setq ans (cons (list word dict parms tab) ans))
   )
  )
  ans
 )
)
