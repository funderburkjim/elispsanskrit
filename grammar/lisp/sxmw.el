; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; sxmw.el  
; begun 09-19-05 by ejf
; Utilize the Monier-Williams dictionary of the
; Cologne  Digital Sanskrit Project.
; See file 'mwdoc.txt' for additional notes.
; These functions formerly in either 'explain.el' or 'mw.el'
; 10-10-2015  added check-buffer-grouping and check-group from
;   old/mw.el
(defun my-forward-sexp (bound begexp endexp)
 (let (ans p1 p2 n group-pairs pairs gans pair beg end b e)
  (setq group-pairs (list (list begexp endexp)))
  (save-excursion
   (setq gans (check-buffer-grouping (point) bound group-pairs))
   (when (elt gans 0)
    (setq pairs (elt gans 1))
;    (fol-msg (format "chk: pairs=%s\n" pairs))
    (when pairs
     (setq pairs (sort pairs
      (lambda (x y)
       (< (elt (elt x 0) 0) (elt (elt y 0) 0))
      )
     ))
;    (fol-msg (format "chk: sorted pairs=%s\n" pairs))
     (setq pair (elt pairs 0))
     (setq beg (elt pair 0))
     (setq end (elt pair 1))
     (setq b (elt beg 0))
     (setq b (- b (length begexp)))
     (setq e (elt end 0))
     (setq ans (list b e))
    )
   )
  )
  ans
 )
)
(defun mw (key &optional full noprflag)
 (let (ans ans1 key1)
  (with-temp-buffer
   (insert key)
   (goto-char 1)
   (while (search-forward-regexp "-" nil t)
    (replace-match "")
   )
   (goto-char 1)
   (setq key1 (current-line))
  )
  (setq ans1 (MW-key-find key1))
  (setq ans (mw-helper ans1 full noprflag))
  ans
 )
)
(defun mw-alt (key &optional full noprflag)
 (let (bufin keybuf ans ans1 anycase dir tabin keytab)
  (setq tabin "mw-rAma.txt")
  (setq keytab "mw-rAma-keys.txt")
  (setq dir "mbhlogs")
  (setq bufin (find-file-noselect (sangram-filename tabin dir)))
  (setq keybuf (find-file-noselect (sangram-filename keytab dir)))
  (setq ans1 (MW-key-find key anycase bufin keybuf))
;  (fol-msg (format "ans1=%s\n" ans1))
  (if (not ans1)
    ; use regular mw
    (setq ans1 (MW-key-find key anycase))
  )
  (setq ans (mw-helper ans1 full noprflag))
  ans
 )
)
(defun mw-helper (ans1 &optional full noprflag)
  ; ans1 is a list of pairs of strings
 (let (ans)
  (while ans1
   (let (tmp skey sdata sdata1)
    (setq tmp (car ans1))
    (setq ans1 (cdr ans1))
    (setq skey (elt tmp 0))
    (setq sdata (elt tmp 1))
    (setq ans (cons (format "%s" skey) ans))
    (if full
     (setq sdata1 sdata)
     (setq sdata1 (mw-help1-string sdata))
    )
    (setq ans (cons (format "%s" sdata1) ans))
   )
  )
  (setq ans (nreverse ans))
  (when (not noprflag)
   (mapcar (lambda (x) (fol-msg (format "%s\n" x))) ans)
   (setq ans (if ans t nil))
  )
  ans
 )
)
(defun mw-help1-string (sin)
 "The string 'sin' is assumed to be a record from the MW dictionary,
  or else coded like one.
  This returns a string where the various tagged elements in the
  record have been dealt with as desired.
 "
 (let (ans be p0 p1 more)
  (with-temp-buffer
   (insert sin)
   (goto-char 1)
   (setq more t)
   ; delete parenthesized regions
   (while (setq be (my-forward-sexp (point-max) "(" ")"))
    (delete-region (elt be 0) (elt be 1))
   )
   (goto-char 1)
   ; delete bracketed regions
   (while (setq be (my-forward-sexp (point-max) "[" "]"))
    (delete-region (elt be 0) (elt be 1))
   )
   (goto-char 1)
   ; handle tagged regions.
   (while (setq be (my-forward-sexp (point-max) "<" ">"))
    (setq p0 (elt be 0))
    (setq p1 (elt be 1))
    (goto-char p0)
;   (fol-msg (format "chk: %s\n" (buffer-substring p0 p1)))
    (if (search-forward-regexp "<\\([^=>]+\\)=\\([^>]+\\)>" p1 t)
     (let (key parm ans fsym fname)
      (setq key (buffer-substring (match-beginning 1) (match-end 1)))
      (setq parm (buffer-substring (match-beginning 2) (match-end 2)))
;    (fol-msg (format "key=%s\nparm=%s\n" key parm))
      (setq fname (format "mw-tag-%s" key))
      (setq fsym (intern-soft fname))
      (setq ans (fboundp fsym))
      (if ans
       (setq ans (funcall fsym parm))
      )
;      (fol-msg (format "chk1: %s\n" (buffer-substring p0 p1)))
      (delete-region p0 p1)
      (when ans
       ; replace the tagged item
       (insert ans)
      )
     )
     ; else (key/parm search fails)
     (progn
      (goto-char p0)
      (delete-region p0 p1)
     )
    )
   )
   (goto-char 1)
   (setq ans  (buffer-substring (point-min) (point-max)))
  )
  ans
 )
)
(defun mw-tag-AB (parm)
 "look up 'parm' in tags table. Return associated value."
 (let (ans MW-abbrev regexp)
  (setq MW-abbrev "abbrev.txt")
  (setq regexp (format "<AB=%s>" parm))
  (setq regexp (concat regexp " +=\\(.+\\)$"))
  (with-current-buffer (find-file-noselect (sangram-filename MW-abbrev MW-dir))
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp regexp nil t)
    (progn
     (setq ans (buffer-substring (match-beginning 1) (match-end 1)))
     (setq ans (upcase ans))
    )
    (setq ans (format "<AB=%s>" parm))
   )
  )
  ans
 )
)
(defun mw-tag-LEX (parm)
 "return <LEX=parm>"
 (let (ans)
  (setq ans (format "<LEX=%s>" parm))
  ans
 )
)
(defun mw-tag-KEY (parm)
 "return <KEY=parm>"
 (let (ans)
  (setq ans (format "<KEY=%s>" parm))
  ans
 )
)
(defun mw-tag-Q (parm)
 "return <Q=parm>"
 (let (ans)
  (setq ans (format "<Q=%s>" parm))
  ans
 )
)
(defun mw-tag-C (parm)
 "return the capitalized 'parm'"
 (let (ans)
  (setq ans (upcase parm))
  ans
 )
)
(defun mw-tag-BIO (parm)
 "parm is a list of comma (and/or space) separated words."
 (let (ans s)
  (setq ans "")
  (with-temp-buffer
   (insert parm)
   (goto-char 1)
   (while (search-forward-regexp "[^ ,]+" nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq ans (concat ans " " (upcase s)))
   )
  )
  ans
 )
)
(defun mw-tag-BOT (parm)
 "parm is a list of comma (and/or space) separated words."
 (mw-tag-BIO parm)
)
(defun mw-tag-V (parm)
 "parm is either 'P' (parasmEpada or active voice) or 
  'A' (Atmanepada or middle voice)"
 (cond
  ((equal parm "P") "ACTIVE VOICE")
  ((equal parm "A") "MIDDLE VOICE")
  (t "??UNKNOWN VOICE??")
 )
)
(defun sx2 (s) 
 "Alternate spelling of Sx2"
 (Sx2 s)
)
(defun Sx2 (s)
 "s is a string, representing an inflected word or 
  an indeclineable, coded in the SLP1 form."
 (let (ans pfx sfx j n n1 pfxdata thisans ans1 inflection)
  (setq n (length s))
  (setq n1 (1+ n))
  (setq j 1)
  (while (< j n1)
   (setq pfx (substring s 0 j))
   (setq sfx (substring s j))
   (when  nil ; dbg
    (fol-msg (format "Sx2: %s %s %s %s\n" j s pfx sfx))
   )
   (setq j (1+ j))
   (setq pfxdata (Sx2-pfx pfx))
   (while pfxdata
    (let (tab thisans)
     (setq tab (car pfxdata))
     (setq pfxdata (cdr pfxdata))
     (setq thisans (Sx2-forms s tab))
     (when  nil ;dbg
      (fol-msg (format "Sx2-forms: %s %s -> %s\n" s tab thisans))
     )
     (when thisans
      (setq ans (append ans thisans))
     )
    )
   )
  )
  ans
 )
)
(defun Sx2-forms (sin tab)
 (let (bufin regexp ans)
  (setq bufin
   (find-file-noselect (sangram-filename tab "forms") t)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   ; SLP1 format uses only letters
   (setq regexp (format "[^-a-zA-Z]%s[^-a-zA-Z]" sin))
   (when nil ;dbg
    (fol-msg (format "Sx2-forms regexp: '%s'\n" regexp))
   )
   (while (search-forward-regexp regexp nil t)
    (let (fs form subforms ans1 citation)
     (beginning-of-line)
     (when (equal (buffer-substring (point) (1+ (point))) ":")
      (setq fs (Sx2-forms-helper sin (current-line)))
      (when  nil ; dbg
       (fol-msg (format "Sx2-forms: %s \n" fs))
      )
      (when fs
       (setq citation (elt fs 0))
       (setq form (elt fs 1))
       (setq subforms (elt fs 2))
       (setq ans1 (concat citation ":" form ":" subforms))
       (setq ans (cons ans1 ans))
      )
     )
    )
    (forward-line)
   )
   (kill-buffer nil)
  )
  ans
 )
)
(defun Sx2-forms-helper (sin line)
 (let (ans words d-string data form n i datum formtype cn subforms citation
	   root)
     (setq words (gen-word-list line ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     
     (when (listp data)
      ; 01-09-2005. in forms/vc/gup, it was noticed that data has the
      ; form ([gopayati ...] [gopAyati ...]), i.e., a list of arrays.
      ; These need to be converted to one array.
      (let (new x)
       (setq new (car data))
       (setq data (cdr data))
       (while data
	(setq new (join-arrays new (car data)))
	(setq data (cdr data))
       )
       (setq data new)
      )
     )
     (when nil ; dbg
      (fol-msg (format "Sx2-forms-helper:\nd-string=\n%s\ndata=\n%s\n"
		      d-string data))
     )
     (setq form (elt words 0)) ; string
     (setq formtype (Sx2-extract-formtype form)) ; string, v,s,i
     (setq subforms nil)
     (setq n (length data))
     (setq i 0)
     (while (< i n)
      (setq datum (elt data i))
      (when (or (equal sin datum)
		(and (listp datum) (member sin datum))
	    )
       (cond
	((equal formtype "s")
	 (setq cn (sym-without-space (elt case-number-set i)))
	 (setq cn (symbol-name cn))
	)
	((equal formtype "v")
	 (setq cn (sym-without-space (elt person-number-set i)))
	 (setq cn (symbol-name cn))
	)
	(t
	 (setq cn "") ; empty for indeclineable
	)
       )
       (if subforms 
        (setq subforms (concat subforms "/" (downcase cn)))
	(setq subforms (downcase cn))
       )
      )
      (setq i (1+ i))
     )
  ; determine citation form
  (save-excursion
   (goto-char 1)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
   )
  )
  (cond
   ((not subforms)) ; do nothing
   ((equal formtype "v")
    (setq citation (if root root "?"))
   )
   ((equal formtype "s")
    (setq citation (elt words 1))
    (if root (setq citation (format "%s (%s)" citation root)))
   )
   ((equal formtype "i")
    (if root
     (progn
      (setq citation root)
     )
     (progn
      (setq citation sin)
     )
    )
   )
   (t (setq citation "FORMERR")
   )
  )
  (if subforms (setq ans (list citation form subforms)))
  ans
 )
)
(defun Sx2-updateans (ans word parms ans1 dict)
 (let (thisans)
  (while ans1
    (setq thisans (car ans1))
    (setq ans1 (cdr ans1))
    (setq ans (cons (list word parms thisans dict) ans))
  )
  ans
 )
)
(defun Sx2-tabmatch (s tab inflection)
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
(defun Sx2-pfx (pfx)
 "pfx is a string.
 "
 (let (ans filenames)
  (setq filenames '(
   "v-ptrs.txt"
   "vc-ptrs.txt"
   "s-ptrs.txt"
   "i-ptrs.txt"
  ))
  (while filenames
   (setq ans (Sx2-pfxa pfx (car filenames) ans))
   (setq filenames (cdr filenames))
  )  
  ans
 )
)
(defun Sx2-pfxa (pfx filename ans)
 "pfx is a string.
 "
 (let (bufin regexp)
  (setq bufin
   (find-file-noselect (sangram-filename filename "forms") t)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (setq regexp (format "^%s " pfx))
   (when (search-forward-regexp regexp nil t)
    (let (p0 more p1)
     (beginning-of-line)
     (setq p0 (point))
     (setq more t)
     (while more
      (end-of-line)
      (setq p1 (point))
      (beginning-of-line)
      (if (search-forward-regexp regexp p1 t)
       (let (tab)
	(setq tab (buffer-substring (point) p1))
	(setq ans (cons tab ans))
	(forward-line)
       )
       ;else stop
       (setq more nil)
      )
     )
    )
   )
  )
  ans
 )
)
(defun Sx2-endmatch (regexp &optional bufin)
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
(defun Sx2-extract-formtype (form)
 "form assumed to be a string"
 (let (ans words word1)
  (setq words (word-list form))
  (setq word1 (elt words 0)) ; 1st word
  (if (and (member word1 '("c"))
          (< 1 (length words)) (setq word1 (elt words 1)))
   (setq word1 (elt words 1))
  )
  (cond
   ((equal word1 "opt") (setq ans "v")) ; should use "pop"
   ((member word1
     '("pre" "ipf" "ipv" "pop" "prf" "fut" "pft" "aor" "con" "ben")
    )
    (setq ans "v")
   )
   ((member word1
     '("ppp" "pap" "prap" "noun" "prmp" "prpp" "gerundive" "adj" "pron" )
    )
    (setq ans "s")
   )
   ((member word1
     '("i" "ppfactn" "inf" "gerund" "abs" "adv")
    )
    (setq ans "i")
   )
   (t
    (fol-msg (format "Sx2-extract-formtype: unknown form '%s'\n" form))
    (setq ans "i")
   )
  )
  ans
 )
)

(defun sxmw (str &optional noprflag)
 (let (ans sxlist citations mwlist)
  (setq sxlist (Sx2 str))
  ; generate 'citations', the list of words, embedded in sxlist, which
  ; should be looked up in the MW dictionary
  (mapcar
   (lambda (sx)
    (let (words citation x)
     (setq words (gen-word-list sx ":"))
     (setq x (elt words 0))
     ; x will either be a citation or, for such words as
     ; participles, citation (root). We look up definitions
     ; for both parts
     (setq words (gen-word-list x "[ )(]"))
     (setq words (append words nil)) ; make a list of strings
     (while words
      (setq citation (car words))
      (setq words (cdr words))
      (when (and (not (member citation citations))
	         (< 0 (length citation))
	    )
       (setq citations (cons citation citations))
      )
     )
    )
   )
   sxlist
  )
  (setq citations (nreverse citations))
  ; mwlist has form of a list of pairs, each pair like (citation mw),
  ; where 'mw' is the value '(mw citation nil t); note that
  ; this 'mw' is itself a list of strings.
  ; 02-22-05: use mw-alt in place of mw. This allows a (fixed)
  ;  local dictionary to override the standard MW dictionary
  (mapcar
   (lambda (citation)
    (let (thisans newans thisans1 thisans2 found)
     (setq thisans (mw citation nil t)) ; a list of strings
;    (setq thisans (mw-alt citation nil t))   
     (setq found nil)
     (while thisans
      (setq thisans1 (car thisans))
      (setq thisans (cdr thisans))
      (setq thisans2 (car thisans))
      (setq thisans (cdr thisans))
      (let (x y z)
       (setq z mwlist)
       (while z
	(setq y (car z))
	(setq z (cdr z))
	(setq x (elt y 1))
	(when (member thisans1 x)
	 (setq thisans2 " <see above>")
	 (setq z nil)
	)
       )
      )
      (setq newans (cons thisans1 newans))
      (setq newans (cons thisans2 newans))
     )
     (setq newans (nreverse newans)) ; get in proper order
     ; add newans (as modified) to mwlist
      (setq newans (list citation newans))
      (setq mwlist (cons newans mwlist))
    )
   )
   citations
  )
  (setq mwlist (nreverse mwlist))
  (setq ans (list str sxlist mwlist))
  (when (not noprflag)
   (apply 'sxmwpr ans)
   (setq ans (if sxlist t nil))
  )
  ans
 )
)

(defun sxmwpr (str sxlist mwlist)
 (let (i n q)
  (setq q "\"")
  (fol-msg (format "* %s\n" str))
  (fol-msg (format "** (Sx2 %s%s%s)\n" q str q))
  (setq n (length sxlist))
  (setq i 0)
  (while (< i n)
   (fol-msg (format "   %s. %s\n" (1+ i) (elt sxlist i)))
   (setq i (1+ i))
  )

  (setq n (length mwlist))
  (setq i 0)
  (while (< i n)
   (let (mw citation defs m j def)
    (setq mw (elt mwlist i))
    (setq citation (elt mw 0))
    (setq defs (elt mw 1))
    (fol-msg (format "** (mw %s%s%s)\n" q citation q))
    (setq m (length defs))
    (setq j 0)
    (while (< j m)
     (setq def (elt defs j))
     (if (equal (mod j 2) 0)
      (fol-msg (format "   %s. %s\n" (1+ (/ j 2)) def)) ; key line of MW defn
      (fol-msg (format "     %s\n"  def)) ; data line of defn
     )
     (setq j (1+ j))
    )
   )
   (setq i (1+ i))
  )
  t
 )
)
(defun sxmw-Mbh-verse (verse)
 (let (words)
  (setq words (Mbh-verse verse))
  (fol-msg (format "* (sxmw-Mbh-verse %s)\n" verse))
  (fol-msg (format "  "))
  (mapcar 
   (lambda (x) (fol-msg (format "%s " x)))
   words
  )
  (fol-msg (format "\n"))
  (mapcar 'sxmw words)
  t
 )
)
(defun sxmw-Mbh-verses (verse1 verse2)
 (let (verse)
  (setq verse verse1)
  (while (<= verse verse2)
   (sxmw-Mbh-verse verse)
   (setq verse (1+ verse))
  )
  t
 )
)
; added from old/mw.el  Oct 9, 2015
(defun MW-init ()
 (MW-orig-init)
 (setq MW-dir "mw")
 (setq MW-mwindx "mwindx.txt")
 (setq MW-mwtot2 "mwtot2.txt")
 (setq SL-mwtot2 "SL-mwtot2.txt")
 (setq SL-mwtot3 "SL-mwtot3.txt")
 (setq SL-mwindx "SL-mwindx.txt")
)
(defun MW-orig-init ()
; (setq MW-orig-mwindx "C://SanDicts/extract/mwindx.txt")
; (setq MW-orig-mwtot2 "C://SanDicts/extract/mwtot2.txt")
 (setq MW-orig-mwindx "mwindx.txt")
 (setq MW-orig-mwtot2 "mwtot2.txt")
)
(defun MW-key-find (key &optional anycase bufin keybuf)
 (let (fkey ans regexp  MW-line s1 s2)
;  (setq bufin (SLtot3-buffer))
  (if (not bufin) (setq bufin (MW-buffer)))
  (if (not keybuf) (setq keybuf (MW-keys-buffer)))
  (if (symbolp key)
   (setq fkey (symbol-name key))
   (setq fkey key) ; assume string
  )
  (setq regexp (format "^%s +[-a-zA-Z]+ +\\([0-9]+\\)$" fkey))
  (save-excursion
   (with-current-buffer keybuf
    (goto-char 1)
    (if (not anycase) (setq case-fold-search nil))
    (while (search-forward-regexp regexp nil t)
     (setq MW-line
      (string-to-number
       (buffer-substring (match-beginning 1) (match-end 1)))
     )
     (save-excursion
      (with-current-buffer bufin
       (goto-line MW-line)
       (setq s1 (current-line))
       (forward-line)
       (setq s2 (current-line))
       (setq ans (cons (list s1 s2) ans))
      )
     )
    )
   )
  )
  (nreverse ans)
 )
)
(defun MW-buffer ()
 ; same as SLtot3-buffer (02-22-05)
 (find-file-noselect (sangram-filename SL-mwtot3 MW-dir))
)
(defun MW-keys-buffer ()
 (let (keytab keybuf)
  (setq keytab "SL-mwtot3-keys.txt")
  (setq keybuf (find-file-noselect (sangram-filename keytab MW-dir)))
  keybuf
 )
)
(defun check-buffer-grouping (c1 c2 group-pairs)
 "'group-pairs' assumed a list of pairs.
  Each pair is a list of two strings,
  the first being an open-grouping string, eg, left-paren,
  and the second being a close-grouping string, eg, right-paren.
  'c1' and 'c2' are character positions in the current-buffer.
  Within this region, we check that the grouping is consistent,
  in the sense that 
  a. open-grouping and close-grouping strings must occur in pairs,
     with the open-grouping preceding the close-grouping.
  b. groups may be consecutive or nested, but non-overlapping.

  Furthermore, the logic may give erroneous results if 
  some grouping string is a substring of another, e.g. '(' and '(*'.
 "
 (let (psave group-locs ans ok all-locs)
  (setq psave (point))
  ;1. for each grouping-pair, construct a list of opening-closing positions
  (let (p c pairs pair ends begs igroup)
   (setq pairs group-pairs)
   (setq igroup 0)
   (while pairs
    (setq pair (car pairs))
    (setq pairs (cdr pairs))
    (setq igroup (1+ igroup))
    ;1. get begs using 1st elt of pair
    (setq begs nil)
    (goto-char c1)
    (while (search-forward (elt pair 0) c2 t)
     (setq begs (cons (list (point) 'B igroup) begs))
    )
    (setq begs (nreverse begs)) 
    ;2. get ends using 2nd elt of pair
    (setq ends nil)
    (goto-char c1)
    (while (search-forward (elt pair 1) c2 t)
     (setq ends (cons (list (point) 'E igroup) ends))
    )
    (setq ends (nreverse ends))
    ;3. append (begs ends pair) to group-locs
    (setq group-locs (cons (list begs ends) group-locs ))
   )
  )
  (setq group-locs (nreverse group-locs))
  (when nil ; dbg
   (mapcar (lambda (x) (fol-msg (format "%s\n" x))) group-locs)
  )
  ;2. for each grouping, there must be the same # of begs as ends
  ; NOTE: This is unneeded, as done implicitly within check-group
  (setq ok t)
;   (mapcar
;    (lambda (x)
;     (when (not (equal (length (elt x 0)) (length (elt x 1))))
;      (setq ok nil)
;      (when nil ;dbg
;       (fol-msg (format "problem with group: %s\n" x))
;      )
;     )
;    )
;    group-locs
;   )
  ;3. get all locations into one array
  (setq all-locs
   (apply 'append
    (mapcar
     (lambda (x)
      (apply 'append x)
     )
     group-locs
    )
   )
  )
  (when nil ; dbg
   (fol-msg (format "all-locs=%s\n" all-locs))
  )
  ;4. Sort all-locs by char-pos (1st elt)
  (setq all-locs (sort all-locs
   (lambda (x y)
    (< (elt x 0) (elt y 0))
   )
  ))
  ;5. find grouping structure recursively
  (setq all-locs (vconcat all-locs))
  (setq ans (check-group all-locs))
  (when nil ; dbg
   (fol-msg (format "chk: sorted-locs=%s\n" all-locs))
   (fol-msg (format "chk: ans=%s\n" ans))
  )
  (goto-char psave) ; restore point
  ans
 )
)
(defun check-group (locs)
 " The first element of the answer is 't' or 'nil', as with
   check-string-grouping.
   The second element of the answer is a list, made up of
    pairs of matching elements of locs.
   The third element of the answer is a list
    which contains those elements of loc where an error occured;
    it is 'nil' when there are no errors.
    When non-nil, each element is a pair (x y), where
     x=nil when the close-grouping element y has no match
     y=nil when the open-grouping element x has no match
     x and y are non-nil when the open-grouping element x and
      the close-grouping element y are of different types. 
   locs: an array, each element having form (pos eb type),where
   pos = position (a number)
   eb = 'B' for beginning, 'E' for end
   type = a group type; lisp data type not important.  
 "
 (let (i n ok ansok stack x y anserr ans)
  ;1. check initial conditions
  (setq ok nil)
  (and
   (arrayp locs)
   (setq i 0)
   (setq n (length locs))
   (setq ok t)
   (while (< i n)
    (setq x (elt locs i))
    (if (equal (elt x 1) 'B)
     (setq stack (cons x stack))
     (if (not stack)
      (setq anserr (cons (list nil x) anserr))
      (progn
       (setq y (car stack))
       (setq stack (cdr stack))
       (if (equal (elt x 2) (elt y 2))
        (setq ansok (cons (list y x) ansok))
	(setq anserr (cons (list y x) anserr))
       )
      )
     )
    )
    (setq i (1+ i))
   )
  )
  (while stack
   (setq x (car stack))
   (setq stack (cdr stack))
   (setq anserr (cons (list x nil) anserr))
  )
  (if ok (setq ok (if anserr nil t)))
  (list ok ansok anserr)
 )
)
