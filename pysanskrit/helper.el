; help in formatting s-file-init-alt-helper for
; comparison with results of test_s_file_init_alt_helper
(defun s-file-init-alt-helper-py (key1 fg key2 &optional dbg)
 (let (x ans)
  (setq x (s-file-init-alt-helper key1 fg key2 dbg))
  (setq ans 
   (mapconcat 
    (lambda (y) (replace-regexp-in-string (format "\n") "" y)) x ";")
  )
  ans
 )
)
(defun s-file-init-alt-helper-py1 (inputlist &optional dbg)
 " inputlist is a list of cases, each case is a list (key1 form key2)
   or of form (key1 form)  (in which case key2 is set to key1)
 "
 (let (args ans key1 form key2)
  (defun doit (inputitem)
   ; convert to list of strings
   (setq args (mapcar (lambda (x) (format "%s" x)) inputitem))
   (setq key1 (elt args 0))
   (setq form (elt args 1))
   (if (= (length args) 2)
    (setq key2 key1)
    (setq key2 (elt args 2))
   )
   ; decline and output
   (setq ans (s-file-init-alt-helper-py key1 form key2 dbg))
   (insert (format "%s,%s,%s=%s\n" key1 form key2 ans))
  )
  (mapcar 'doit inputlist)
  t
 )
)
; generate declensions from elisp in form appropriate for test
(let (key ans keys key1 form key2)
 (setq keys '(yad etad kim tyad))
 (while keys
  (setq key (car keys))
  (setq keys (cdr keys))
  (setq key1 (format "%s" key))
  (setq form "pron")
  (setq key2 key1)
  (setq ans (s-file-init-alt-helper-py key1 form key2))
  (insert (format "%s,%s,%s=%s\n" key1 form key2 ans))
 )
)

;; generate placeholder 1cons functions
(let (endings ending)
 (setq endings '(l r c C j S s z h v t p k))
 (defun doit (e)
  (insert (format "def declension_general_1cons_%s(citation,gender,irregs,dbg=False):\n" e))
  (insert (format " \"\"\"\n \"\"\"\n"))
  (insert (format " raise NameError(\"declension_general_1cons_%s not implemented\")\n" e))
  (insert (format "\n"))
 )
 (mapcar 'doit endings)
)


;--------------------------------------------------------------
; Conjugation helpers 
;--------------------------------------------------------------
; Conjugation helpers 


(defun v-file-init-alt-helper-py (root class voice tense dtype  &optional dbg)
 " root is string spelling the root, in SLP1 transliteration
  class is a string representing a verb class (1,2,... 10)
  voice is string: 
   a (active=Parasmaipada),
   m (middle =atmanepada),
   p (passive)
  tense is string:
   pre = present tense,
   ipv = imperative tense,
   ipf = imperfect (past) tense,
   pop = present optative 'tense'
 "
 (let (x ans tenses)
  (setq tenses (list (intern tense)))
  (setq x (v-file-init-alt1-pre-helper root class voice tenses dtype dbg))
  (setq ans 
   (mapconcat 
    (lambda (y) (replace-regexp-in-string (format "\n") "" y)) 
     x 
     ";"
   )
   ;(format "%s\n" x)
  )
  ans
 )
)
(defun v-file-init-alt-helper-py1 (inputlist &optional dbg)
 " inputlist is a list of cases, each case is a list 
   (root class voice tense)

 "
 (let (args ans root class voice tense dtype argsout)
  (defun vdoit (inputitem)
   ; convert to list of strings
   (setq args (mapcar (lambda (x) (format "%s" x)) inputitem))
   (setq root (elt args 0))
   (setq class (elt args 1))
   (setq voice (elt args 2))
   (setq tense (elt args 3))
   (if (= (length args) 5)
    (setq dtype (elt args 4))
    (setq dtype nil)
   )
   ; conjugate and output
   (setq ans (v-file-init-alt-helper-py root class voice tense dtype dbg))
   ;(setq ans (apply 'v-file-init-alt-helper-py args))
   (setq argsout (mapconcat (lambda (x) (format "%s" x)) args ","))
   ;(insert (format "%s,%s,%s,%s,%s=%s\n" root class voice tense dtype ans))
   (insert (format "%s=%s\n" argsout ans))
  )
  (mapcar 'vdoit inputlist)
  t
 )
)
v-file-init-alt-helper-py1
(v-file-init-alt-helper-py1 '(
 (jYA 9 a aor5)
))
;--------------------------------------------------------------
; Conjugation helpers OLD VERSIONS
(defun v-file-init-alt-helper-py (root class voice tense &optional dbg)
 " root is string spelling the root, in SLP1 transliteration
  class is a string representing a verb class (1,2,... 10)
  voice is string: 
   a (active=Parasmaipada),
   m (middle =atmanepada),
   p (passive)
  tense is string:
   pre = present tense,
   ipv = imperative tense,
   ipf = imperfect (past) tense,
   pop = present optative 'tense'
 "
 (let (x ans tenses)
  (setq tenses (list (intern tense)))
  (setq x (v-file-init-alt1-pre-helper root class voice tenses dbg))
  (setq ans 
   (mapconcat 
    (lambda (y) (replace-regexp-in-string (format "\n") "" y)) 
     x 
     ";"
   )
   ;(format "%s\n" x)
  )
  ans
 )
)
(defun v-file-init-alt-helper-py1 (inputlist)
 " inputlist is a list of cases, each case is a list 
   (root class voice tense)

 "
 (let (args ans root class voice tense)
  (defun vdoit (inputitem)
   ; convert to list of strings
   (setq args (mapcar (lambda (x) (format "%s" x)) inputitem))
   (setq root (elt args 0))
   (setq class (elt args 1))
   (setq voice (elt args 2))
   (setq tense (elt args 3))
   ; conjugate and output
   (setq ans (v-file-init-alt-helper-py root class voice tense))
   ;(setq ans (apply 'v-file-init-alt-helper-py args))
   (insert (format "%s,%s,%s,%s=%s\n" root class voice tense ans))
  )
  (mapcar 'vdoit inputlist)
  t
 )
)
