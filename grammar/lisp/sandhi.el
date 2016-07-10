; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; sandhi.el  
; begun 09-15-02 by ejf
; All the code in itrans.el is assumed.
; All the code in gram1.el is assumed
; The particular spelling of the symbols representing
; the phonetic elements (the 'tokens') must be consistent with 
; that in itrans.el
(defun external-sandhi-doc ()
 "Deshpande p. 37.
  'Sandhi' refers to a process of combining adjoining sounds. This
  process takes place within a word, as well as when two words occur
  in a sequence.  The first is called internal sandhi and the second
  is called external sandhi.  We shall concentrate mostly on the 
  external sandhi rules.  The rules for external sandhi are OPTIONAL,
  but in actual usage they are almost always applied.
 "
)
(defun anusvaara-doc ()
 "Deshpande p. 38.
  1. Final 'm', when followed by a (word beginning with a) consonant, 
     is changed to 'anusvaara' (M). (The change is not easy to show
     in pronunciation.)
  2. Optionally, an 'anusvaara' is further changed to a nasal consonant,
     which is homorganic with the following consonant.
  NOTE: An 'anusvaara' does not occur before a vowel or at the end of a
     sentence.  Also note that it does not change to a homorganic nasal
     before the consonants 'r sh Sh s h'. Before these it remains an
     'anusvaara'. Change of 'anusvaara' before 'y l v' is less common.
     SUBNOTE: the symbol with a dot above a crescent is used in the text
     above the letters 'y l v' to indicate their nasalization.
  ejf note: example 'raamam karoti' -> raamaM karoti -> raama~Nkaroti'
   In the 1st variation, there are 2 words.
   In the 2nd variation, there is only 1 word.
   10-08-03: I have not implemented the 2nd (optional) variation.
 "
)
(defvar sandhi-pair-skiprefs nil)
(defun guna (sym)
 (if (simplevowel-P sym) (sanget sym 'guna) (vector sym))
)
(defun vrddhi (sym)
; (or (sanget sym 'vrddhi) (vector sym))
 (if (simplevowel-P sym) (sanget sym 'vrddhi) (vector sym))
)
(defun vrddhi1 (sym)
 (or (sanget sym 'vrddhi) (vector sym))
)
(defun lengthen-vowel (tok)
; By Kale (section 19) there is no long form of 'Li, hence
; the long form of 'Ri is used when a long form of 'Li is needed.
; Note that Apte has one word beginning with 'LI, in fact just the
; one letter, 'LI, meaning 'a mother', or 'Shiva'.
 (if (equal tok 'Li)
  'RI  
 (let (i x s l n ans)
  (setq s shortsimplevowel-set)
  (setq l longsimplevowel-set)
  (setq ans tok) ; in case no match, just return the arg
  (while s
   (setq x (car s))
   (if (equal tok x)
    (progn
     (setq ans (car l))
     (setq s nil) ; to end loop
    )
    ; otherwise
    (setq s (cdr s))
    (setq l (cdr l))
   )
  )
  ans
 )
)
)
(defun lengthen-diphthong (tok)
 (cond
  ((member tok '(e ai)) 'ai)
  ((member tok '(o au)) 'au)
  (t tok)
 )
)
(defun shorten-vowel (tok)
 (let (i x s l n ans)
  (setq l shortsimplevowel-set)
  (setq s longsimplevowel-set)
  (setq ans tok) ; in case no match, just return the arg
  (while s
   (setq x (car s))
   (if (equal tok x)
    (progn
     (setq ans (car l))
     (setq s nil) ; to end loop
    )
    ; otherwise
    (setq s (cdr s))
    (setq l (cdr l))
   )
  )
  ans
 )
)
(defun corresponding-letter (x set1 set2)
 (let (n i y z)
  (setq n (max (length set1) (length set2)))
  (setq i 0)
  (setq y nil)
  (while (< i n)
   (when (equal x (elt set1 i))
    (setq y (elt set2 i))
    (setq i n)
   )
   (setq i (1+ i))
  )
  y
 )
)
(defun sandhi-internal-diphthong-A (tokar)
 (let (ans v n x)
  (setq x (copy-sequence tokar))
  (setq n (length x))
  (setq v (if (< 0 n) (elt x (1- n))))
  (setq ans (cond
   ((eq v 'e)
    (aset x (1- n) 'a)
    (vconcat x [y])
   )
   ((eq v 'o)
    (aset x (1- n) 'a)
    (vconcat x [v])
   )
   ((eq v 'ai)
    (aset x (1- n) 'aa)
    (vconcat x [y])
   )
   ((eq v 'au)
    (aset x (1- n) 'aa)
    (vconcat x [v])
   )
   (t x)
  ))
  ans
 )
)
(defun sandhi-internal-A-simplevowel (tokar1 tokar2)
 (let (ans n1 n2 v1 v2 x1 v x2)
  (setq n1 (length tokar1))
  (setq n2 (length tokar2))
  (setq ans (cond
   ((or (= 0 n1) (= 0 n2)) (vconcat tokar1 tokar2))
   ((progn
     (setq v1 (elt tokar1 (1- n1))) ; last letter of tokar1
     (setq v2 (elt tokar2 0))       ; first letter of tokar2
     (or
      (not (member v1 '(a aa)))
      (not (simplevowel-P v2))
     )
    )
    (vconcat tokar1 tokar2)
   )
   (t
    (setq x1 (substring tokar1 0 (1- n1))) ; drops last char, v1, of tokar1
    (setq x2 (substring tokar2 (- (1- n2)))) ; drops 1st char, v2, of tokar2
    (setq v (guna v2)) ; a tokarr
    (vconcat x1 v x2)
   )
  ))
  ans
 )
)
(defun de-aspirate (a)
 "given an aspirate character symbol, return
  the corresponding deaspirated symbol"
 (let (ans s b)
  (setq ans a)
  (cond
   ((not (symbolp a)))
   ((progn ; this step fails
     (setq s (symbol-name a)) 
     nil
    )
   )
   ((/= (length s) 2))
;   ((and (fol-msg (format "chk 2: s=%s %c\n" s (elt s 1) )) nil))
   ((not (equal (elt s 1) ?h)))
;   ((and (fol-msg (format "chk 3: s=%s\n" s)) nil))
   ((equal a 'Sh)); skip this
   ((equal a 'ch)); skip this
   ((equal a 'Ch) (setq ans 'ch))
   (t
    (setq b (intern (substring s 0 1)))
    (if (equal b 'C)
     (setq ans 'c)
     (setq ans b)
    )
   )
  )
  ans
 )
)
(defun aspirate (a)
 "given an unaspirated character symbol, return
  the corresponding aspirated symbol"
 (let (ans s b)
  (setq ans a)
  (cond
   ((not (symbolp a)))
   ((progn ; this step fails
     (setq s (symbol-name a)) 
     nil
    )
   )
   ((equal a 'ch) (setq ans 'Ch))
   ((or (member a softnonaspirate-set) (member a hardnonaspirate-set))
    (setq ans (sym-without-space (vector a 'h)))
   )
  )
  ans
 )
)
(defun reduplicate (in &optional wparts)
 (let (tok ans ans1)
  (cond
   ((arrayp in)
    (setq tok in)
    (setq ans (reduplicate-join (reduplicative-pfx tok wparts) tok))
   )
   ((symbolp in)
    (setq tok (car (ITRANS-parse-words-1 (symbol-name in))))
    (setq ans1 (reduplicate-join (reduplicative-pfx tok wparts) tok))
    (when (arrayp ans1)
     (setq ans (sym-without-space ans1))
    )
   )
  )
  ans
 )
)
(defun reduplicative-pfx (tok &optional wparts)
 ;Antoine2#70. 
 ;Reduplication consists in repeating before a verbal root that
 ;initial portion of it which ends with its first vowel.
 ;Reduplication is subject to special rules:
 ;1. An initial aspirate loses its aspiration in reduplication
 ;  Note: In dealing with the perfect, one encounters reduplication
 ;  of roots starting with 'sh'.  Published examples show that in these
 ;  cases, the 'sh' does not lose its aspiration.
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
 ; (3) Antoine gives an example [s m Ri] -> [s a s m Ri].  My logic
 ;     provides [s i s m Ri]. 
 (let (partsx parts types ctok vtok c v ans)
  (if wparts
   (setq partsx wparts)
   (setq partsx (word-parts tok))
  )
  (setq parts (elt partsx 0))
  (setq types (elt partsx 1))
  (cond 
   ((equal (elt types 0) ?C)
    ;(member types '("CV" "CVC"))
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
    ; 8. 'Ri' and 'RI' become 'i' in reduplication
    (when (and (= 2 (length ctok))
	       (sibilant-P c)
	       (hard-P (elt ctok 1)))
     (setq c (elt ctok 1))
    )
    ; 1. initial aspirate loses aspiration
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
     ((member v '(Ri RI)) ; Ri, RI ->i
      (setq v 'i)
     )
    )
    (setq ans (vector c v))
   )
   ((equal (elt types 0) ?V)
    ;(member types '("V" "VC"))
    ; 8. 'Ri' and 'RI' become 'i' in reduplication
    ; For the conj-7 verb 'Ri', we return [i y]
    (setq vtok (elt parts 0)) ; initial or intermedicate vowel
    (setq v (elt vtok 0))
    (cond
     ((equal tok [Ri])
      (setq ans [i y])
     )
     ((member v '(Ri RI)) ; Ri, RI ->i
      (setq ans [i])
     )
     (t  (setq ans (vector v)))
    )
   )
   (t
    (fol-msg (format "reduplicate: unexpected form: %s %s %s\n"
		     tok parts types))
    (setq ans [])
   )
  )
  ans
 )
)
(defun reduplicate-join (base-tok sup) 
 ;07-01-03. based on 'declension-join
 (let (ans)
;  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ans (cond
   ((member sup '([s n i h] [s n u h]))
    (vconcat base-tok [Sh N] (substring sup 2))
   )
   ((solution (sandhi-pair base-tok sup 'internal 'join)))
   ((solution (sandhi-pair base-tok sup nil 'join)))
   ((arrayp base-tok) (vconcat base-tok sup))
   ((symbolp base-tok) (sym-without-space (vector base-tok sup)))
   (t
    (fol-msg (format "reduplication-join error. wrong types: %s %s\n"
		     base-tok sup))
    nil
   )
  ))
;  (sandhi-pair-skiprefs-set nil)
  (setq ans (or (sandhi-n-N ans) ans))
;  (fol-msg (format "base-tok=%s, sup=%s -> %s\n" base-tok sup ans))
  ans
 )
)
(defun Sandhi-append (v1 v2 w1 w2 action args)
 (let (sym prop val newargs)
  (setq sym 'Sandhi)
  (setq prop (intern (format "%s-%s" v1 v2)))
  (setq newargs (copy-sequence args))
  (setq val (list w1 w2 action newargs))
  (sanAppendElt sym prop val)
 )
)
(defun Sandhi-get (v1 v2)
 (let (sym prop val)
  (setq sym 'Sandhi)
  (setq prop (intern (format "%s-%s" v1 v2)))
  (sanget sym prop)
 )
)
(defun Sandhi-initAll ()
 (put 'Sandhi 'Sangram nil)
  (put 'Sandhi-Inverse-join 'Sangram nil)
  (put 'Sandhi-Inverse-nojoin 'Sangram nil)
  (put 'Sandhi-Inverse 'Sangram nil)
)
(defun Sandhi-initElt (prop)
 (sanput 'Sandhi prop nil)
)
(defun Sandhi-init ()
 (Sandhi-initAll)
 (sandhi-init-19)
 (sandhi-init-19a)
 (sandhi-init-20)
 (sandhi-init-21)
 (sandhi-init-22)
 (sandhi-init-23)
 (sandhi-init-24)
 (sandhi-init-24a)
 (sandhi-init-25)

 (sandhi-init-36)
 (sandhi-init-36a)

 (sandhi-visarga-init-1)
 (sandhi-visarga-init-2)
 (sandhi-final-r-init-1)
 (sandhi-final-m-init)
 (sandhi-s-Sh-init)
; consonant sandhis 
 (sandhi-cons72-4-init)
 (sandhi-cons72-6-init)
 (sandhi-cons87-1-init)
 (sandhi-cons87-2-init)
 (sandhi-cons88-1-init)
 (sandhi-cons88-1a-init)
 (sandhi-cons88-1b-init)
 (sandhi-cons88-2-init)
 (sandhi-cons88-3-init)
 (sandhi-cons88-3a-init)
 (sandhi-cons88-5a-init)
 (sandhi-cons88-5b-init)
 (sandhi-Kale33-init)
 (sandhi-Kale30-init) ; 06-25-03
 ;
 (Sandhi-Inverse-init)
 (Sandhi-Length-init)
)
(defun Sandhi-Inverse-init ()
 (let (all sympair vals symans procname)
  (setq procname "Sandhi-Inverse-init")
  (put 'Sandhi-Inverse-join 'Sangram nil)
  (put 'Sandhi-Inverse-nojoin 'Sangram nil)
  (put 'Sandhi-Inverse 'Sangram nil)
  (setq all (sangetall 'Sandhi))
  (while all
   (setq sympair (car all))
   (setq all (cdr all))
   (setq vals (car all))
   (setq all (cdr all))
   ; 1. get symans = [sym1 sym2] from sympair = sym1-sym2
   (setq symans (separate-symbol-pair sympair))

   ; 2. For each element of vals which is a join,
   ; constr
   (while (and symans vals)
    (let (val w1 w2 action args type ref condition)
     (setq val (car vals))
     (setq w1 (elt val 0)) ; tokarr
     (setq w2 (elt val 1)) ; tokarr
     (setq action (elt val 2))
     (setq args (elt val 3)) ; a plist
     (setq type (plist-get args 'type))
     (setq ref (plist-get args 'ref))
     (setq condition (plist-get args 'condition))
     (cond
      ((equal action 'join)
       (let (w symsource this)
	(setq w (vconcat w1 w2))
        (setq symsource (sym-without-space w))
	(setq this (list symans action type ref condition))
        (when (not (symbolp symsource))
	 (fol-msg (format "error (%s) sympair=%s, vals=%s\n"
			  symsource sympair vals ))
	 (setq all nil)
	)
	(sanAppendElt 'Sandhi-Inverse-join symsource this)
;	(sanAppendElt 'Sandhi-Inverse-join symsource symans)
       )
      )
      ((equal action 'nojoin)
       (let (w symsource sym1 sym2 this)
	(setq sym1 (sym-without-space w1))
	(setq sym2 (sym-without-space w2))
	(setq w (format "%s-%s" sym1 sym2))
	(setq symsource (intern w))
	(setq this (list symans action type ref condition))
	
	(sanAppendElt 'Sandhi-Inverse-nojoin symsource this)
;	(sanAppendElt 'Sandhi-Inverse-nojoin symsource symans)
       )
      )
      (t ; no match for action
       (fol-msg (format "%s (warning): unknown action (%s) for %s\n"
		 procname action sympair))
      )
     ); end cond
     ; put a record in 'Sandhi-Inverse
     (let (w symsource sym1 sym2 this)
      (setq sym1 (sym-without-space w1))
      (setq sym2 (sym-without-space w2))
      (setq w (format "%s-%s" sym1 sym2))
      (setq symsource (intern w))
      (setq this (list symans action type ref condition))
      (sanAppendElt 'Sandhi-Inverse symsource this)
     )
    ) ; end of let
    (setq vals (cdr vals))
   )
  )
  t
 )
)
(defun Sandhi-Length-init ()
 (let (all sympair  symans sym2beg len1 len2)
  (put 'Sandhi-Length 'Sangram nil)
  (setq all (sangetall 'Sandhi))
  (while all
   (setq sympair (car all))
   (setq all (cdr all))
;   (setq vals (car all)) unneeded
   (setq all (cdr all))
   ; 1. get symans = [sym1 sym2] from sympair = sym1-sym2
   (setq symans (separate-symbol-pair sympair))

   ; 2. get sym2beg = the symbol representing 1st token of sym2
   ;    len1 = length of tokens in sym1
   ;    len2 = length of tokens in sym2
   (let (tokar1 tokar2 sym1 sym2)
    (setq sym1 (elt symans 0))
    (setq sym2 (elt symans 1))
    (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name sym1))))
    (setq tokar2 (car (ITRANS-parse-words-1 (symbol-name sym2))))
    (setq sym2beg (elt tokar2 0))
    (setq len1 (length tokar1))
    (setq len2 (length tokar2))
   )
   ; update the value of 'Sandhi-Length to max of prev values and len1,2
   ; for key = sym2beg
   (let (val)
    (setq val (sanget 'Sandhi-Length sym2beg))
    (if (not val) (setq val [0 0]))
    (if (< (elt val 0) len1) (aset val 0 len1))
    (if (< (elt val 1) len2) (aset val 1 len2))
    (sanput 'Sandhi-Length sym2beg val)
   )
   
  )
  t
 )
)
(defun Sandhi-unload-old ()
 (let (all key val out nout nout2 i filename dirname)
  (setq filename "Sandhi.txt")
  (setq dirname "construct")
  (setq all (sangetall 'Sandhi)) ; a plist
  (setq nout2 (length all)) ; an even number
  (setq nout (/ nout2 2)) ; number of records
  (setq out (make-vector nout nil))
  (setq i 0)
  (while all
   (setq key (car all))
   (setq all (cdr all))
   (setq val (car all))
   (setq all (cdr all))
   (aset out i (list key val))
   (setq i (1+ i))
  )
  (write-table-file nout out filename dirname)
  t
 )
)
(defun Sandhi-unload ()
 (let (all key val out nout nout2 i filename dirname)
  (setq filename "Sandhi.txt")
  (setq dirname "construct")
  (setq all (sangetall 'Sandhi)) ; a plist
  (setq nout 1)
  (setq out all)
  (let (fileout bufout i x y z bufsave)
   (if (not dirname) (setq dirname "tables"))
   (setq fileout (sangram-filename filename dirname))
   (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
   (with-current-buffer bufout
    (erase-buffer)
    (let (keys key)
     (setq keys '(Sandhi Sandhi-Inverse-join
		  Sandhi-Inverse-nojoin Sandhi-Inverse Sandhi-Length))
     (while keys
      (setq key (car keys))
      (setq keys (cdr keys))
      (insert (format "%s %s\n" key (sangetall key)))
     )
    (save-buffer)
    )
   )
   (kill-buffer bufout)
  )
  t
 )
)
(defun Sandhi-load ()
 (let (tabname filename dirname buf key val more err)
  (setq tabname "Sandhi.txt")
  (setq dirname "construct")
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (while more
    (condition-case err
     (progn
      (setq key (read buf))
      (setq val (read buf))
      (put key 'Sangram val)
      (fol-msg (format "%s %s\n" key (length val)))
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
  t
 )
)
(defun Sandhi-load-init ()
 (Sandhi-initAll)
 (Sandhi-load)
; (Sandhi-Inverse-init)
; (Sandhi-Length-init)
)

(defun sandhi-init-default-plist (type ref &optional condition)
 (if condition
  (list 'type type
       'ref ref
       'condition condition)
  (list 'type type
       'ref ref)
 )
)
(defun sandhi-init-19 ()

 "Kale Section 19.
  If a simple vowel, short or long, be followed by a similar vowel,
  short or long, the substitute for them both is the similar
  long vowel"
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale19))
  (setq s1 simplevowel-set)
  (setq s2 simplevowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when (equal (lengthen-vowel v1) (lengthen-vowel v2))
     (let (w1 w2)
      (setq w1 (vector (lengthen-vowel v1)))
      (setq w2 (vector))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-19a ()
 ;
 "Kale Section 19a
  If 'Ri' or 'Li' be followed by a short 'Ri' or 'Li', then
  short 'Ri' or 'Li' is optionally substituted for both."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale19a))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 '(Ri Li))
  (setq s2 s1)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when t ; i.e. , always
     (let (w1 w2)
      (setq w1 (vector v2))
      (setq w2 (vector))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-20 ()
 "Kale Section 20
  When 'a' or 'aa' is followed by 'i', 'u', 'Ri', or 'Li', short or
  long, the 'guNa' letter corresponding to the latter takes the place
  of both."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale20))
  (setq s1 '(a aa))
  (setq s2 simplevowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when (not (member v2 s1))
     (let (w1 w2)
      (setq w1 (guna v2)) ; a tokarr
      (setq w2 (vector))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-21 ()
  "Kale Section 21
  When 'a' or 'aa' is followed by 'e' or 'ai', then 'ai' is substituted
  for both. When 'a' or 'aa' is followed by 'o' or 'au', then 'au' is 
  substituted for both. "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale21))
  (setq s1 '(a aa))
  (setq s2 diphthong-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2)
      (setq w1 (vector (lengthen-diphthong v2)))
      (setq w2 (vector))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-22()
  "Kale Section 22
  When 'i', 'u', 'Ri', or 'Li', short or long, is followed by a
  dissimilar vowel, then the corresponding semivowel 'y', 'v', 'r' or 'l'
  is substituted."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale22))
  (setq s1 simplevowel-set)
  (setq s2 vowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when (and (not (member v1 '(a aa)))
	       (not (equal (lengthen-vowel v1) (lengthen-vowel v2))))
     (let (w1 w2) 
      (setq w1 (vector (sanget v1 'semivowel))) ; a tokarr
      (setq w2 (vector v2))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-23()
  "Kale Section 23
  'i', 'u', 'Ri', or 'Li', short or long, at the end of a word followed
  by a dissimilar vowel except in a compound, are optionally not combined
  and when optionally not combined they are shortened if long."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale23))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 simplevowel-set)
  (setq s2 vowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when (and (not (member v1 '(a aa)))
	       (not (equal (lengthen-vowel v1) (lengthen-vowel v2))))
     (let (w1 w2) 
      (setq w1 (vector (shorten-vowel v1))) ; a tokarr
      (setq w2 (vector v2))
      (Sandhi-append v1 v2 w1 w2 'nojoin args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-24()
  "Kale Section 24
  'e', 'o', 'ai', and 'au', when followed by a vowel, are changed
  to 'ay', 'av', 'aay', and 'aav' respectively."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args args1)
  (setq args (sandhi-init-default-plist 'svara 'Kale24))
  (setq args1 (sandhi-init-default-plist 'svara 'Kale24))
  (setq args1 (plist-put args1 'condition 'internal))
  (setq s1 diphthong-set)
  (setq s2 vowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2 args0)
       (cond
        ((equal v1 'e)
	 (setq w1 [a y])
	 (setq args0 (if (equal v2 'a) args1 args))
        )
        ((equal v1 'o)
	 (setq w1 [a v])
	 (setq args0 (if (equal v2 'a) args1 args))
	)
        ((equal v1 'ai)
	 (setq w1 [aa y])
	 (setq args0 args)
        )
        ((equal v1 'au)
	 (setq w1 [aa v])
	 (setq args0 args)
	)
      )
      (setq w2 (vector v2))
      (Sandhi-append v1 v2 w1 w2 'join args0)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-24a()
  "Kale Section 24a
  'y' or 'v' at the end of a word and preceded by 'a' or 'aa' is
  optionally dropped when followed by a vowel or a soft consonant. Thus
  'e', 'o', 'ai', and 'au', when at the end of a word and followed by a vowel,
  are optionally changed to 'a', 'a', 'aa', and 'aa' respectively."

 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale24a))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 diphthong-set)
  (setq s2 vowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2)
       (cond
        ((and (equal v1 'e) (not (equal v2 'a)))
	 (setq w1 [a])
        )
        ((and (equal v1 'o) (not (equal v2 'a)))
	 (setq w1 [a])
	)
        ((equal v1 'ai)
	 (setq w1 [aa])
        )
        ((equal v1 'au)
	 (setq w1 [aa])
	)
      )
      (setq w2 (vector v2))
      (Sandhi-append v1 v2 w1 w2 'nojoin args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-25()
  "Kale Section 25
  When 'e' or 'o' at the end of a word is followed by 'a', the
  latter merges into the former and the avagraha sign is sometimes
  written in its place."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'svara 'Kale25))
  (setq s1 '(e o))
  (setq s2 '(a))
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2) 
      (setq w1 (vector v1))
      (setq w2 (vector 'AVAGRAHA))
      (Sandhi-append v1 v2 w1 w2 'join args)
     )
  )))
  't
 )
)
(defun sandhi-init-36 ()
 ;Kale Section 36 ('m' at end of a word changes to 'M')
 ;Also Antoine (I) 15.
 "'m' at the end of a word is changed into the anusvara 'M' when
  followed by a consonant."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'cons 'Kale36)) 
  (setq s1 '(m))
  (setq s2 consonant-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (when t
     (let (w1 w2)
      (setq w1 [M])
      (setq w2 (vector v2))
      (Sandhi-append v1 v2 w1 w2 'nojoin args)
     )
    )
   )
  )
  't
 )
)
(defun sandhi-init-36a ()
  "Kale Section 36a
  'm' and 'n', not at the end of a pada (i.e. word), are turned into an
  anusvaara ('M') when followed by a consonant except a nasal
  or a semivowel or 'h'.
  (02-10-03): I also allow this to be a joining sandhi when followed
  by 'h' this was designed to permit 'sam' upasarga, e.g. saMhRi
  (05-11-04): I also allow this to be a joining sandhi when followed
  by 'v' this was designed to permit 'sam' upasarga, e.g. saMvasati,
  (05-12-04): Allow 'Mn' to represent 'mn' so 'saMnipat' = 'sam ni pat'
  'm' at the end of a word is changed to anusvaara ('M')
  when followed by a consonant"
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2)
  (setq args (sandhi-init-default-plist 'cons 'Kale36a))
  (setq s1 '(m n))
  (setq s2 consonant-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w1 [M])
    (setq w2 (vector v2))
    (when (and (not (nasal-P v2))
	       (not (semivowel-P v2))
	       (not (equal v2 'h))
	       (not (equal v2 'v)) ; 05-11-04
          )
     (Sandhi-append v1 v2 w1 w2 'join args)
    )
    (when (equal v1 'm)
     (Sandhi-append v1 v2 w1 w2 'nojoin args)
    )
    (when (and (equal v1 'm) (equal v2 'h)) ; 02-10-03
      (Sandhi-append v1 v2 w1 w2 'join args)
    )
    (when (and (equal v1 'm) (equal v2 'v)) ; 05-11-04
      (Sandhi-append v1 v2 w1 w2 'join args)
    )
    (when (and (equal v1 'm) (equal v2 'n)) ; 05-12-04
      (Sandhi-append v1 v2 w1 w2 'join args)
    )
   )
  )
  't
 )
)
(defun sandhi-visarga-init-1 ()
  "Antoine(vol 1) Section 27
  The visarga sandhi rules depends on the vowel preceding the visarga H
  and the vowel or soft consonant following:
  Given any vowel + H + k kh p ph sh Sh s, then H is unchanged
  Given any vowel + H + ch or Ch, then H becomes sh
  Given any vowel + H + T or Th, then H becomes Sh
  Given any vowel + H + t or th, then H becomes s
  Given any vowel except 'a' 'aa' + H + vowel or soft cons., then 
   usually H becomes 'r', but there is an exception (ANTOINE 54)
   Given  v + H + f, where v is any vowel except 'a' or 'aa',
   and where f is a vowel or soft consonant;
   if 'f' is not 'r', then H becomes 'r'  and words are joined (ANTOINE 27)
   if 'f' = 'r', then 
    (a) v changes to its long form 
    (b) H is dropped
    (c) words are not joined

  Given 'aa' + H + vowel or soft cons., then H is dropped
  Given 'a' + H + soft cons., then 'aH' becomes 'o'
  Given 'a' + H + any vowel except 'a', then H is dropped
  Given 'a' + H + 'a', then 'aH' becomes 'o' and the following 'a' is elided."


 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'visarga 'Antoine27))
  (setq s1 vowel-set)
  (setq s2 (append vowel-set consonant-set))
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2 w1a w1b action sym1)
      (setq w2 v2)
      (setq w1a v1) 
      (setq action 'join)
      (cond
       ((member v2 '(k kh p ph sh Sh s))
	(setq w1b 'H) ; i.e., no change
	(setq action 'nojoin)
       )
       ((member v2 '(ch Ch)) (setq w1b 'sh))
       ((member v2 '(T Th)) (setq w1b 'Sh))
       ((member v2 '(t th)) (setq w1b 's))
       ((and (not (member v1 '(a aa)))
	     (or (vowel-P v2) (soft-P v2))
	     (not (equal v2 'r))
	     )
	(setq w1b 'r)
       )
       ((and (not (member v1 '(a aa)))
	     (or (vowel-P v2) (soft-P v2))
	     (equal v2 'r)
	     )
	(setq w1b nil) ; visarga dropped
	(setq w1a (lengthen-vowel v1))
	(setq action 'nojoin)
       )
       ((and (equal v1 'aa)
	     (or (vowel-P v2) (soft-P v2)))
        (setq w1b nil) ; visarga dropped
	(setq action 'nojoin)
       )
       ((and (equal v1 'a)
	     (soft-P v2))
	(setq w1a 'o)  
        (setq w1b nil) ; visarga dropped
	(setq action 'nojoin)
       )
       ((and (equal v1 'a)
	     (vowel-P v2)
	     (not (equal v2 'a)))
        (setq w1b nil) ; visarga dropped
	(setq action 'nojoin)
       )
       ((and (equal v1 'a)
	     (equal v2 'a))
	(setq w1a 'o)  
        (setq w1b nil) ; visarga dropped
	(setq w2 'AVAGRAHA)
       )
       (t
	;(fol-msg (format "sandhi skipping %sH-%s\n" v1 v2))
	(setq action nil))
      ); end cond
      (when action 
       (setq w1a (vector w1a))
       (setq w1b (if w1b (vector w1b) (vector)))
       (setq w1 (vconcat w1a w1b))
       (setq w2 (vector w2))
       (setq sym1 (sym-without-space (vector v1 'H)))
       (Sandhi-append sym1 v2 w1 w2 action args)
      )
     )
  )))
  't
 )
)
(defun sandhi-visarga-init-2 ()
  "Antoine(vol 1) Section 15 (optional)
  When final visarga 'H' is followed by a sibilant ('sh' 'Sh 's'), it
  is optionally changed to the sibilant."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'visarga 'Antoine15))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 vowel-set)
;  (setq s2 (append vowel-set consonant-set))
  (setq s2 '(sh Sh s))
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2 w1a w1b action sym1)
      (setq w2 v2)
      (setq w1a v1) 
      (setq action 'join)
      (setq w1b v2) ; change visarga to the following sibilant
      (setq w1a (vector w1a))
      (setq w1b (if w1b (vector w1b) (vector)))
      (setq w1 (vconcat w1a w1b))
      (setq w2 (vector w2))
      (setq sym1 (sym-without-space (vector v1 'H)))
      (Sandhi-append sym1 v2 w1 w2 action args)
     )
  )))
  't
 )
)
(defun sandhi-final-r-init-1 ()
  "Antoine(vol 1) Section 54
  Visarga standing for final 'r' follows the general rules of 
  visarga-sandhi (Antoine 27) with one exception:
  Visarga standing for final 'r', even when preceded by 'a' or 'aa', and
  followed by a vowel or a soft consonant, is changed to 'r'.
  The following tries to elaborate the actual cases; we are thinking
  of a word, like 'punar' or 'maatar' that ends in 'r'.

  Given any vowel + r + k kh p ph sh Sh s, then r becomes H
  Given any vowel + r + ch or Ch, then r becomes sh
  Given any vowel + r + T or Th, then r becomes Sh
  Given any vowel + r + t or th, then r becomes s
  Given any vowel  + r + vowel or soft cons. other than 'r', 
      then r is unchanged,
      AND the words are joined! e.g. punar + api -> punarapi
  Given any vowel v + r + r, then
    (a) v changes to its long form
    (b) first r is dropped
    (c) words are not joined      
  "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'final-r 'Antoine54))
  (setq s1 vowel-set)
  (setq s2 (append vowel-set consonant-set))
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2 w1a w1b action sym1 sym1b)
      (setq w2 v2)
      (setq w1a v1)
      (setq sym1b 'r)
      (cond
       ((member v2 '(k kh p ph sh Sh s))
	(setq w1b 'H) 
	(setq action 'nojoin)
       )
       ((member v2 '(ch Ch))
	(setq w1b 'sh)
        (setq action 'join)
       )
       ((member v2 '(T Th))
	(setq w1b 'Sh)
        (setq action 'join)
       )
       ((member v2 '(t th))
	(setq w1b 's)
        (setq action 'join)
       )
       ((equal v2 'r)
	(setq w1b nil) ; first 'r' dropped
	(setq w1a (lengthen-vowel v1))
	(setq action 'nojoin)	
       )
       ; so v2 != 'r
       ((or (vowel-P v2) (soft-P v2))
	(setq w1b 'r) ; this is tricky. Why not r?
        (setq action 'join)
;	(setq sym1b 'H)
       )
      ); end cond
      (when action 
       (setq w1a (vector w1a))
       (setq w1b (if w1b (vector w1b) (vector)))
       (setq w1 (vconcat w1a w1b))
       (setq w2 (vector w2))
       (setq sym1 (sym-without-space (vector v1 sym1b)))
       (Sandhi-append sym1 v2 w1 w2 action args)
      )
     )
  )))
  't
 )
)
(defun sandhi-final-m-init ()
 ; I cannot find a description of this, but it definitely
 ; appears in examples
 "Two words are joined when 
  the first ends in 'm' or 'n' and
  the second begins with a vowel."
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 'final-m 'AntoineXX))
  (setq s1 (list 'm 'n))
;  (setq s2 (append vowel-set consonant-set))
  (setq s2 vowel-set)
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t ; always
     (let (w1 w2 w1a w1b action sym1)
      (setq w2 v2)
      (setq w1 v1) 
      (setq action 'join)
      (setq w1 (vector w1))
      (setq w2 (vector w2))
      (Sandhi-append v1 v2 w1 w2 action args)
     )
  )))
  't
 )
)
(defun sandhi-s-Sh-init ()
  "Antoine(vol 1) Section 45
  When 's' is preceded by a vowel, except 'a' or 'aa', or by
  'k' or 'r', and is followed in the same word by 't', 'th',
  'm', 'y', 'v', or a vowel, then the 's' changes to 'Sh'. This
  also holds if there be an anusvaara or visarga between the
  preceding marker (vowel 'k' 'r') and the 's'.
  Addendum:
   If the following letter is 't', it is changed to 'T';
   If the following letter is 'th', it is changed to 'Th'
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args)
  (setq args (sandhi-init-default-plist 's 'Antoine45))
  (setq args (plist-put args 'condition 'internal))
  (setq s1 (append vowel-set '(k r)))
  (setq s2 (append vowel-set '(t th m y v)))
  (setq n1 (length s1)) (setq n2 (length s2))(setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when (not (member v1 '(a aa)))
     (let (w1 w2 action sym1 sym2)
      (setq action 'join)
      (setq w1 (vector v1))
      (cond
       ((equal v2 't) (setq w2 (vector 'Sh 'T)))
       ((equal v2 'th) (setq w2 (vector 'Sh 'Th)))
       (t (setq w2 (vector 'Sh v2)))
      )
      (setq sym1 (sym-without-space w1))
      (setq sym2 (sym-without-space (vector 's v2)))
      (Sandhi-append sym1 sym2 w1 w2 action args)
      ; allow intervening H
      (setq w1 (vector v1 'H))
      (setq sym1 (sym-without-space w1))
      (Sandhi-append sym1 sym2 w1 w2 action args)
      ; allow intervening M
      (setq w1 (vector v1 'M))
      (setq sym1 (sym-without-space w1))
      (Sandhi-append sym1 sym2 w1 w2 action args)
     )
  )))
  't
 )
)
(defun sandhi-cons72-4-init ()
  "Antoine(vol 1) Section 72 - rule 4
  (4) A final hard consonant becomes soft before a vowel or a soft consonant
  (5a) This rule does not apply to the final hard consonant of a verbal
       base or nominal stem followed by a termination or a case-ending
       beginning with a vowel or a semi-vowel.
  (5b) This rule does apply, however, to the final hard consonant of a verbal
       base or nominal stem followed by a termination or a case-ending
       beginning with a soft consonant other than a semi-vowel.
  Rule (4) and (5a) apply in both external and internal sandhi. For external
  sandhi, it appears optional whether the two words are joined or not. Thus,
  I enable both versions ('join' and 'nojoin')"
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args  ref1 ref2)
  (setq ref1 'Antoine72-4) ; 2nd letter a vowel or a semi-vowel
  (setq ref2 'Antoine72-5) ; 2nd letter a soft consonant but not a semi-vowel
  (setq args (sandhi-init-default-plist 'cons ref1))
;  (setq args (plist-put args 'condition 'internal))
  ; the first 10 elements of hard-set and soft-set correspond
  ; note resetting of n1
  (setq s1 hard-set)
  (setq s2 (append soft-set vowel-set))
  (setq n1 (length s1)) (setq n2 (length s2))
  (setq n1 10) 
  (setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (when t
     (let (w1 w2 action sym1 sym2)
      ; this may depend on whether internal or external sandhi
      (setq action 'join)
      ; 1- since we incremented already
      (setq w1 (vector (elt s2 (1- i1)))) 
      (setq w2 (vector v2))
      (if (or (vowel-P v2) (semivowel-P v2))
       (progn 
        (setq args (plist-put args 'ref ref1))
        (Sandhi-append v1 v2 w1 w2 action args)
        (Sandhi-append v1 v2 w1 w2 'nojoin args) ; 1-10-03
       )
       (progn
        (setq args (plist-put args 'ref ref2))
        (Sandhi-append v1 v2 w1 w2 action args)
        (Sandhi-append v1 v2 w1 w2 'nojoin args) ; 1-10-03
       )
      )
     )
  )))
  't
 )
)
(defun sandhi-cons72-6-init ()
  "Antoine(vol 1) Section 72 - rule 6
  (6) A soft consonant becomes hard before a hard consonant. 
  (7) This rule does not apply to the final soft aspirate of a
      verbal root followed by a termination beginning with 't' or 'th'.
      In that case, the final consonant of the root loses its aspiration
      and the 't' or 'th' is changed to 'dh.' e.g.,
      'labh' + 'ta' = 'labdha'
   Let 'cond1' denote the condition of a final soft aspirate followed
    by a 't' or 'th'.
   Let 'cond2' denote the other conditions 
   We use ref = Antoine72-6 for 'cond2'.
   We use ref = Antoine72-6a for 'cond1' when the soft consonant is 
    changed to a hard consonant.
   We use ref = Antoine72-7 for 'cond1' when the action of (7) is taken.
   Thus, to form the 'laT' etc for non-a conjugations, 
   we require that rules with ref = Antoine72-6a be skipped.
  This rule applies in both external sandhi and internal sandhi. For external
  sandhi, it appears optional whether the two words are joined or not. Thus,
  I enable both versions ('join' and 'nojoin')"
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args ref1 ref1a ref2)
  (setq ref1 'Antoine72-6)
  (setq ref1a 'Antoine72-6a)
  (setq ref2 'Antoine72-7)
  (setq args (sandhi-init-default-plist 'cons ref1))
;  (setq args (plist-put args 'condition 'internal))
  ; the first 10 elements of hard-set and soft-set correspond
  ; note resetting of n1
  (setq s1 soft-set)
  (setq s2 hard-set)
  (setq n1 (length s1)) (setq n2 (length s2))
  (setq n1 10) 
  (setq i1 0)
  (while (< i1 n1) (setq v1 (elt s1 i1)) (setq i1 (1+ i1)) (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2)) (setq i2 (1+ i2))
    (let (w1 w2 action sym1 sym2)
     (setq action 'join)
     (if (not (and (member v2 '(t th)) (softaspirate-P v1)))
      (progn
       (setq args (plist-put args 'ref ref1))
       ; 1- since we incremented already
       (setq w1 (vector (elt s2 (1- i1))))  ; hard corresponding to v1
       (setq w2 (vector v2))
       ;03-25-03 g + s -> k + s (so far). However,
       ;this normally is changed further to k + Sh
       (if (and (eq v1 'g) (eq v2 's))
	(Sandhi-append v1 v2 w1 [Sh] 'join args)
        (Sandhi-append v1 v2 w1 w2 action args)
       )
       (Sandhi-append v1 v2 w1 w2 'nojoin args)
      )
      (progn
       (setq args (plist-put args 'ref ref2))
       (setq w2 [dh])
       ; w1 is the soft nonaspirate corresponding to v1
       ; i.e., softaspirate-set = (gh jh Dh dh bh)
       ; corresponds to softnonaspirate-set = (g j D d b)
       (let (x i n)
	(setq n (length softaspirate-set))
	(setq i 0)
	(while (and (< i n) (not x))
	 (when (equal (elt softaspirate-set i) v1)
	  (setq x (elt softnonaspirate-set i))
	 )
	 (setq i (1+ i))
	)
	(when x
	 (setq w1 (vector x))
         (Sandhi-append v1 v2 w1 w2 action args)
         (Sandhi-append v1 v2 w1 w2 'nojoin args)
	)
       )
       (setq args (plist-put args 'ref ref1a))
       ; 1- since we incremented already
       (setq w1 (vector (elt s2 (1- i1))))  ; hard corresponding to v1
       (setq w2 (vector v2))
       (Sandhi-append v1 v2 w1 w2 action args)
       (Sandhi-append v1 v2 w1 w2 'nojoin args)
      )
     )
    )
   )
  )
  't
 )
)
(defun sandhi-cons87-1-init ()
  "Kale Section 39(a) and Antoine 87-1
  Final 'n' (and '~N' and 'N') at the end of a word and
  preceded by a short vowel and followed by a vowel is doubled"
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2 v1a w1a nasals nasal)
  (setq args (sandhi-init-default-plist 'cons 'Kale39a))
  (setq args (plist-put args 'condition 'external))
  (setq s1 shortsimplevowel-set)
  (setq s2 vowel-set)
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq nasals '(n ~N N))
    (while nasals
     (setq nasal (car nasals))
     (setq nasals (cdr nasals))
     (setq w1a (vector v1 nasal))
     (setq v1a (sym-without-space w1a))
     (setq w1 (vector v1 nasal nasal))
     (Sandhi-append v1a v2 w1 w2 'join args)
    )
   )
  )
  't
 )
)
(defun sandhi-cons87-2-init ()
  "Kale Section ?? and Antoine 87-2
  Final 'n' followed by 'ch' or 'Ch' is replaced by anusvaara and 'sh'.
  Final 'n' followed by 'T' or 'Th' is replaced by anusvaara and 'Sh'.
  Final 'n' followed by 't' or 'th' is replaced by anusvaara and 's'.
  This appears to be an external sandhi (for joining words)
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine87-2))
  (setq args (plist-put args 'condition 'external))
  (setq s1 [n])
  (setq s2 [ch Ch T Th t th])
  (setq s3 [[M sh] [M sh] [M Sh] [M Sh] [M s] [M s]])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq w1 (elt s3 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-1-init ()
  "Kale Section 28 and Antoine 88-1
  Any dental coming into contact (i.e., preceding or following) a
  palatal is changed to the corresponding palatal 
  (i.e., [t th d dh n] change respectively to [ch Ch j jh ~n],
   however rule 72-4 is implcitly applied first ,
   hard1+soft2->soft1+soft2 and soft1+hard2->hard1+hard2,
   so t + j -> d + j -> j + j).
  I use only the dentals [t th d dh] and exclude [n l s].
  I include only the palatals [ch Ch j jh] and exclude [~n y sh].
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-1))
  (setq s1 
   [ ; dental + palatal
    [[t ch] [ch ch]]
    [[t Ch] [ch Ch]]
    [[t j]  [j j]]
    [[t jh] [j jh]]
 
    [[th ch] [Ch ch]]
    [[th Ch] [Ch Ch]]
    [[th j]  [jh j]]
    [[th jh] [jh jh]]
 
    [[d ch] [ch ch]]
    [[d Ch] [ch Ch]]
    [[d j]  [j j]]
    [[d jh] [j jh]]
 
    [[dh ch] [Ch ch]]
    [[dh Ch] [Ch Ch]]
    [[dh j]  [jh j]]
    [[dh jh] [jh jh]]
; now palatal + dental
    [[ch t] [ch ch]]
    [[Ch t] [Ch ch]]
    [[j t] [ch ch]] ; ch t-> ch ch
    [[jh t] [Ch ch]] ; Ch t -> Ch ch
    [[ch th] [ch Ch]]
    [[Ch th] [Ch Ch]]
    [[j th] [ch Ch]] ; ch th -> ch Ch
    [[jh th] [Ch Ch]] ; Ch th -> Ch Ch
    [[ch d] [j j]]  ; j d -> j j
    [[Ch d] [jh j]] ; jh d -> jh j
    [[j d] [j j]]
    [[jh d] [jh j]]
    [[ch dh] [j jh]] ; j dh -> j jh
    [[Ch dh] [jh jh]] ; jh dh -> jh jh
    [[j dh] [j jh]] 
    [[jh dh] [jh jh]]
   ]
  )

  (setq n1 (length s1))
  (setq i1 0)
  (while (< i1 n1)
   (setq s2 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq v1 (elt (elt s2 0) 0))
   (setq v2 (elt (elt s2 0) 1))
   (setq w1 (vector (elt (elt s2 1) 0)))
   (setq w2 (vector (elt (elt s2 1) 1)))
   (Sandhi-append v1 v2 w1 w2 'join args)
  )
  't
 )
)
(defun sandhi-cons88-1a-init ()
  "Kale Section 28 and Antoine 88-1a
  The dental 'n' preceding or following one of the palatals [j jh] is
  changed to the palatal nasal '~n'.
 By another rule (36a above), it can also change to anusvara 'M'
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-1a))
  (setq s1 [n])
  (setq s2 [j jh])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq w1 [~n])
    (Sandhi-append v1 v2 w1 w2 'join args)
    (Sandhi-append v2 v1 w2 w1 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-1b-init ()
  "Kale Section 28 and Antoine 88-1b
  Any dental preceding the palatal 'sh' is changed to 'ch'.
  I use only the dentals [t th d dh] and exclude [n l s].
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-1b))
  (setq s1 [t th d dh])
  (setq s2 [sh])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq w1 [ch])
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-2-init ()
  "Kale Section 28 and Antoine 88-2
  Initial 'sh' preceded by any of the first four letters of a class
  is optionally changed to 'Ch'.
  I apply this only to one of the first 4 dentals, in which case
  the dental also is changed to 'ch', as in 88-1b.
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-2))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 [t th d dh])
  (setq s2 [sh])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 [Ch])
    (setq w1 [ch])
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-3-init ()
  "Kale Section 28 and Antoine 88-3
  Any dental coming into contact (i.e., preceding or following) a
  cerebral is changed to the corresponding cerebral (i.e. that cerebral).
  I use only the dentals [t th d dh] and exclude [n l s].
  I include only the cerebrals [T Th D Dh] and exclude [ N r Sh].
  Antoine says (88-4) that dental + 'Sh' is unchanged.
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-3))
  (setq s1 
   [ ; dental + palatal
    [[t T] [T T]]
    [[t Th] [T Th]]
    [[t D]  [D D]]
    [[t Dh] [D Dh]]
 
    [[th T] [Th T]]
    [[th Th] [Th Th]]
    [[th D]  [Dh D]]
    [[th Dh] [Dh Dh]]
 
    [[d T] [T T]]
    [[d Th] [T Th]]
    [[d D]  [D D]]
    [[d Dh] [D Dh]]
 
    [[dh T] [Th T]]
    [[dh Th] [Th Th]]
    [[dh D]  [Dh D]]
    [[dh Dh] [Dh Dh]]
; now palatal + dental
    [[T t] [T T]]
    [[Th t] [Th T]]
    [[D t] [T T]] ; T t-> T T
    [[Dh t] [Th T]] ; Th t -> Th T
    [[T th] [T Th]]
    [[Th th] [Th Th]]
    [[D th] [T Th]] ; T th -> T Th
    [[Dh th] [Th Th]] ; Th th -> Th Th
    [[T d] [D D]]  ; D d -> D D
    [[Th d] [Dh D]] ; Dh d -> Dh D
    [[D d] [D D]]
    [[Dh d] [Dh D]]
    [[T dh] [D Dh]] ; D dh -> D Dh
    [[Th dh] [Dh Dh]] ; Dh dh -> Dh Dh
    [[D dh] [D Dh]] 
    [[Dh dh] [Dh Dh]]
   ]
  )

  (setq n1 (length s1))
  (setq i1 0)
  (while (< i1 n1)
   (setq s2 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq v1 (elt (elt s2 0) 0))
   (setq v2 (elt (elt s2 0) 1))
   (setq w1 (vector (elt (elt s2 1) 0)))
   (setq w2 (vector (elt (elt s2 1) 1)))
   (Sandhi-append v1 v2 w1 w2 'join args)
  )
  't
 )
)
(defun sandhi-cons88-3a-init ()
  "Kale Section 28 and Antoine 88-3a
  Any dental following 'Sh' is changed to the corresponding cerebral.
  I use only the dentals [t th d dh] and exclude [n l s].
  whose corresponding cerebrals are [T Th D Dh].
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-3a))
  (setq s1 [Sh])
  (setq s2 [t th d dh])
  (setq s3 [T Th D Dh])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq w2 (vector (elt s3 i2)))
    (setq i2 (1+ i2))
    (setq w1 (vector v1))
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-5a-init ()
  "Kale Section 28 and Antoine 88-5a
  A dental among  [t th d dh] followed by  'l' is changed to 'l'.
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-5a))
  (setq s1 [t th d dh])
  (setq s2 [l])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq w1 w2)
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-cons88-5b-init ()
  "Kale Section 31 and Antoine 88-5b
  The dental 'n' followed by  'l' is changed to nasalized 'l'.
  I am unsure what is meant by 'nasalized l'. Both Antoine and Kale
  seem to used a symbol (dot over semicircle) above the devanagari
  for 'l' to represent this, but Antoine does not explain this.
  I will represent this (relatively rare, I think!) occurrence by [M l]
  (M = anusvara)
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2 ans args w1 w2  s3)
  (setq args (sandhi-init-default-plist 'cons 'Antoine88-5b))
  (setq s1 [n])
  (setq s2 [l])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq w1 (vconcat [M] w2))
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-Kale33-init ()
  "Kale Section 33
  'h' coming after any of the first four letters of a class is
  optionally changed to the soft aspirate (4th letter) of that class.
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2  v2a ans args w1 w2  s2a)
  (setq args (sandhi-init-default-plist 'cons 'Kale33))
  (setq args (plist-put args 'condition 'optional))
  (setq s1 [k kh g gh  ch Ch j jh  T Th D Dh   t th d dh   p ph b bh])
  (setq s2a [gh gh gh gh  jh jh jh jh  Dh Dh Dh Dh  dh dh dh dh  bh bh bh bh])
  (setq s2 [h])
  (setq n1 (length s1))
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq v2a (elt s2a i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2a))
    (setq w1 (vector v1))
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defun sandhi-Kale30-init ()
  "Kale Section 30  (06-25-03)
  If a consonant, except 'r' or 'h', coming at the end of a word,
  be followed by a nasal, the nasal of its class is optionally
  substituted for it.
  If the nasal belongs to a termination, the change is necessary:
   'vaak' + 'maya' -> va~Nmaya
  NOTES: (1) The rule is implemented as 'optional'.
  (2) The words are joined.
  (3) Only consonants in the five 'vargas' are counted; it is
      otherwise unclear what 'the nasal of its class' would be
 "
 (let (s1 s2 n1 n2 i1 i2 v1 v2  v1a ans args w1 w2  s1a)
  (setq args (sandhi-init-default-plist 'cons 'Kale30))
  (setq args (plist-put args 'condition 'optional))
;  (setq s1 consonant-set)
;  exclude '(y r l v h Sh sh s H M)' at end of 'consonant-set'
  (setq s1 [k kh g gh
	    ch Ch j jh
	    T Th D Dh
	    t th d dh
	    p ph b bh])
  (setq s2 nasal-set)
  (setq n1 (length s1)) ; so only the vargas are considered
  (setq s1a [~N ~N ~N ~N
	     ~n ~n ~n ~n
	     N N N N
	     n n n n
	     m m m m])
  (setq n2 (length s2))
  (setq i1 0)
  (while (< i1 n1)
   (setq v1 (elt s1 i1))
   (setq v1a (elt s1a i1))
   (setq i1 (1+ i1))
   (setq i2 0)
   (while (< i2 n2)
    (setq v2 (elt s2 i2))
    (setq i2 (1+ i2))
    (setq w2 (vector v2))
    (setq w1 (vector v1a))
    (Sandhi-append v1 v2 w1 w2 'join args)
   )
  )
  't
 )
)
(defvar sandhi-n-N-ifirst nil)
(defun sandhi-n-N-ifirst-set (ifirst)
 (setq sandhi-n-N-ifirst ifirst)
)
(defun sandhi-n-N (tokar-in)
  " Antoine 17
  When, in the same word, 'n' is preceded by 'Ri', 'RI', 'r', or 'Sh' and
  followed by a vowel or by 'n', 'm', 'y', or 'v', then it is changed to
  'N' even when there are letters between the preceding 'Ri' (etc) and 'n'
  provided these intervening letters be vowel, gutturals, labials, 
  'y', 'v', h', or 'M' (anusvaara)."
 (let (tokar x1 x2 y i i1 i2 n ok changed ifirst)
  (setq ifirst sandhi-n-N-ifirst)
  (if (not ifirst) (setq ifirst 0))
  (setq changed nil)
  (setq tokar (copy-sequence tokar-in)) 
  (setq n (length tokar))
  (setq i 0)
  (while (< i n)
   (setq x1 (elt tokar i))
   (setq i (1+ i))
   (when (member x1 '(Ri RI r Sh))
;    (fol-msg (format "found start: %s %s\n" x1 (1- i)))
    (setq i1 i)
    (setq i2 nil)
    (setq ok nil)
    (while (< i n)
     (setq x2 (elt tokar i))
     (setq i (1+ i))
     (when (and (equal x2 'n) (< i n))
      (let (x3)
       (setq x3 (elt tokar i))
       (when (or (vowel-P x3) (member x3 '(n m y v)))
        (setq i (1- i))
        (setq i2 i)
        (setq i n) ; break loop
       )
      )
     )
    )
    (setq i i1)
    (when i2 ; found a subsequent "n". Now check intervening letters
;     (fol-msg (format "found end: %s %s\n" x2 i2))
     (setq ok 't)
     (while (and ok (< i i2))
      (setq y (elt tokar i))
;      (fol-msg (format "%s %s:  %s\n" tokar y (member y '(y v h M))))
      (if (or (vowel-P y) (guttural-P y) (labial-P y)
	    (member y '(y v h M))
           )
       (setq i (1+ i))
       ; else ..
       (setq ok nil)
;       (fol-msg (format "intervening letter wrong: %s\n" y))
      )
     )
     (when ok
      (when (<= ifirst i2)
       ; make the change
       (setq changed 't)
       (aset tokar i2 'N)
       (setq i (1+ i2))
      )
     )
    )
   )
  )
  (if changed tokar)
 )
)
(defun separate-symbol-pair (sympair &optional pairsep-str)
   (let (s s1 s2 i sym1 sym2 symans)
    (if (not pairsep-str) (setq pairsep-str "-"))
    (setq s (symbol-name sympair))
    (setq i (string-match pairsep-str s))
    (cond
     (i
      (setq s1 (substring s 0 i))
      (setq s2 (substring s (1+ i)))
      (setq sym1 (intern s1))
      (setq sym2 (intern s2))
      (setq symans (vector sym1 sym2))
     )
     (t (setq symans nil)
     )
    )
   )
)
(defun sandhi-pair-skiprefs-set (new)
 (setq sandhi-pair-skiprefs new)
)
(defun sandhi-pair-main (tokar1 tokar2 n1 n2 v1 v2 x1 x2 condition action)
 (let (ans vals)
  ; vals is a list, each element, val, of which has form
  ; val = (list w1 w2 action args)
  ; where w1 and w2 are token arrays 
  (setq vals (Sandhi-get v1 v2))
;  (fol-msg (format "chk: %s %s %s\n" v1 v2 vals))
  (setq ans nil) ; default
  (while vals
   (let (val w1 w2 action-val args thisans condition-val type-val ref-val)
    (setq val (car vals))
    (setq w1 (elt val 0)) ; tokarr
    (setq w2 (elt val 1)) ; tokarr
    (setq action-val (elt val 2))
    (setq args (elt val 3)) ; a plist
    (setq type-val (plist-get args 'type))
    (setq ref-val (plist-get args 'ref))
    (setq condition-val (plist-get args 'condition))
    (setq thisans
     (cond
      ((member ref-val sandhi-pair-skiprefs) nil)
      ((not (equal action-val action)) nil)
      ((equal action 'join)
       (list (vconcat x1 w1 w2 x2))
      )
      ((equal action 'nojoin)
       (list (vconcat x1 w1) (vconcat w2 x2))
      )
      (t ; no match for action
       (fol-msg (format "sandhi-pair-main. action error %s\n" action))
       nil
      )
     )
    )
    (when thisans
     ; process condition, if present
     (if (not (equal condition-val condition))
       (setq thisans nil)
     )
    )
    (if thisans (setq ans (append ans (list thisans))))
   )
   (setq vals (cdr vals))
  )
  ans
 )
)

(defun sandhi-pair (t1 t2  condition action)
 (let (ans type tokar1 tokar2 n1 n2 tokans)
;  (if (not action) (setq action 'join))
  (setq type 
   (cond
    ((and (vectorp t1) (vectorp t2))
     (setq tokar1 t1)
     (setq tokar2 t2)
     'v)
    ((and (symbolp t1) (symbolp t2))
     (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name t1))))
     (setq tokar2 (car (ITRANS-parse-words-1 (symbol-name t2))))
     's)
    (t nil)
   )
  )
  (setq n1 (length tokar1))
  (setq n2 (length tokar2))
  (setq tokans nil)
   (if (and (< 0 n1) (< 0 n2))
    (let (lens len1 len2 v v1 v2 x1 x2 y1 y2 ilen1 ilen2 thisans i1 i2)
     (setq v (elt tokar2 0))
;     (fol-msg (format "chk: v=%s\n" v))
     (setq lens (sanget 'Sandhi-Length v))
     (setq len1 (elt lens 0))
     (setq len2 (elt lens 1))
     (setq ilen1 1)
     (while (and (<= ilen1 len1) (<= ilen1 n1))
      (setq i1  (- n1 ilen1))
      (setq ilen1 (1+ ilen1))
      (setq y1 (substring tokar1 i1))
      (setq x1 (substring tokar1 0 i1))
      (setq ilen2 1)
      (while (and (<= ilen2 len2) (<= ilen2 n2))
       (setq i2 ilen2)
       (setq ilen2 (1+ ilen2))
       (setq y2 (substring tokar2 i2))
       (setq x2 (substring tokar2 0 i2))
       (when t ;(and (<= 0 (length y1)) (<= 0 (length x2)))
        (setq v1 (sym-without-space y1))
        (setq v2 (sym-without-space x2))
        (setq thisans (sandhi-pair-main tokar1 tokar2 n1 n2 v1 v2 x1 y2
				       condition action))
;	(fol-msg (format "thisans=%s\n" thisans))
        (mapcar 
         (lambda (thisans1)
          (when (not (member thisans1 tokans))
	   (setq tokans (append tokans (list thisans1)))
	  )
	 )
	 thisans
        )
       )
      )
     )
    )
   )
;  (fol-msg (format "tokans=%s\n" tokans))
  (cond 
   ((not tokans) (setq ans nil))
   ((equal type 'v) (setq ans tokans))
   ((equal type 's) (setq ans (mapcar 'sym-without-space tokans)))
   (t (setq ans nil))
  )
  ans
 )
)
(defun sandhi-one-final-cons (tokar)
 "Antoine 72-1. No Sanskrit word can end with more than one
  consonant. A final compound consonant must be reduced to its
  first member. In particular, this applies to Nominative Singular
  ending 's' joined to nouns ending in consonants.
  Routine returns nil unless some change is made in tokar."
 (let (ans n0 n n1 n2 more x)
  (setq n0 (length tokar))
  (setq n n0)
;  (setq more 't)
  (setq more (< 1 n)) ; if n = 1 or 0, no changes
  (while more
   (setq n (1- n))
   (setq x (elt tokar n))
   (if (consonant-P x)
    (progn
     (if (not n2) (setq n2 n))
     (setq n1 n)
    )
    (setq more nil)
   )
  )
  (when nil
   (fol-msg (format "sandhi-one-final-cons: tokar=%s, n0=%s,n1=%s,n2=%s\n" tokar n0 n1 n2))
  )
  (cond
   ((not n2) (setq ans nil)) ; does not end in consonant
   ((= n1 n2) (setq ans nil)) ; ends a single consonant

   (t ; < n1 n2
    (if (equal (elt tokar n1) 'r) ; keep the initial 'r' and next cons
      (if (< (+ n1 2) n0) ; otherwise, no change
       (setq ans (substring tokar 0 (+ n1 2)))
      )
      ; keep the initial cons.
      (setq ans (substring tokar 0 (+ n1 1)))
    )
   )
  )
  ans
 )
)
(defun sandhi-legalise-final-cons (tokar)
 "Antoine 72.3. A Sanskrit word (i.e., a verb with its terminations
  or a nominal stem with its case endings) can end only with a vowel or
  with one of the eight following consonants: k T t p ~N n m H.
  All other final consonants must be reduced to one of these eight.
  1. 'h' and palatals are reduced to 'k' or 'T' :
      vaach -> vaak ; samraaj -> samraaT
  2. Cerebrals are reduced to 'T' : praavRiSh -> praavRiT
  3. Dentals are reduced to 't' : suhRid -> suhRit
  4. Labials are redueced to 'p' : kakubh -> kakup
  5. 's' and 'r' are reduced to H: kavis -> kaviH ; pitar -> pitaH
 "
 (let (ans pfx e dbg)
  (setq dbg nil)
  (setq ans (or (sandhi-one-final-cons tokar) tokar))
  (when dbg
   (fol-msg (format "sandhi_legalise_final_cons. base=%s, sandhi_one_final_cons=%s\n" tokar ans))
  )
  (setq pfx (substring ans 0 -1)) ; all but last
;  (fol-msg (format "pfx=%s\n" pfx))
  (setq e (elt (substring ans -1) 0)) ; last char
;  (fol-msg (format "e=%s\n" e))
  (cond
   ((not (consonant-P e))) ; no change needed
   ((member e '(k T t p ~N n m H))) ; no change needed
   ((member e '(s r)) (setq ans (vconcat pfx [H])))
   ((or (equal e 'h) (palatal-P e))
    (setq ans (list (vconcat pfx [k]) (vconcat pfx [T])))) ; only multiple ans
   ((guttural-P e) (setq ans (vconcat pfx [k])))
   ((cerebral-P e) (setq ans (vconcat pfx [T])))
   ((dental-P e) (setq ans (vconcat pfx [t])))
   ((labial-P e) (setq ans (vconcat pfx [p])))
  )
  (if (not (listp ans)) (setq ans (list ans)))
  ans
 )
)
(defun sandhi-single-main (tokar1 condition)
 (if (< 0 (length tokar1))
 (let (n1 v1 x1 tokans tok changed)
  (setq tok (sandhi-n-N tokar1))
  (if tok
    (setq changed 't)
    ;else
    (setq changed nil)    
    (setq tok (copy-sequence tokar1))
  )
  (let (tok1)
   (setq tok1 (sandhi-one-final-cons tok))
   (when tok1
    (setq changed 't)
    (setq tok tok1)
   )
  )
  (setq n1 (length tok))
  (setq v1 (elt tok (1- n1))) ; last letter of tok
  (setq x1 (substring tok 0 (- n1 1))) ; drops last char, v1, of tok
  (cond
   ((equal v1 's) (setq tokans (vconcat x1 [H])))
;   ((equal v1 'r) (setq tokans (vconcat x1 [H]))) ; removed 11-21-02
   (t (setq tokans (if changed tok nil)))
  )
  tokans
 ))
)
(defun sandhi-single (t1 &rest args)
 (if t1
 (let (ans type tokar1 n1 v1 x1 x2 tokans ok ok1
	   condition )
  (setq condition (elt args 0))
  (setq type 
   (cond
    ((vectorp t1)
     (setq tokar1 t1)
     'v)
    ((symbolp t1)
     (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name t1))))
     's)
    (t nil)
   )
  )
  (condition-case nil
   (setq tokans
	(sandhi-single-main tokar1 condition))
   (error
	(fol-msg (format "sandhi-single(error): %s \n"
			 tokar1))
	(setq tokans nil)
    )
  )		       
  (cond 
   ((equal type 'v) (setq ans tokans))
   ((equal type 's)
;    (setq ans (mapcar 'sym-without-space tokans))
    (setq ans (sym-without-space tokans))
   )
   (t (setq ans nil))
  )
  ans
 )
)
)
(defun sandhi-avagraha-separate (tokar)
; returns a sequence of token arrays, separating 
; instances of 'e.a' and 'o.a' into 'e a' and 'o a'.
 (let (n n1 n2 ans tok1 tok2 x y)
  (setq ans nil)
  (setq n (length tokar))
  (setq tok1 nil)
  (setq n1 0)
  (while (< n1 n)
   (setq x (elt tokar n1))
   (setq n1 (1+ n1))
   (setq y (if (= n1 n) nil (elt tokar n1)))
   (cond 
    ((equal n1 n)
     (setq tok1 (vconcat tok1 (list x)))
     (setq ans (vconcat ans (list tok1)))
    )
    ((and (or (equal x 'e) (equal x 'o)) (equal y 'AVAGRAHA))
     (setq tok1 (vconcat tok1 (list x)))
     (setq ans (vconcat ans (list tok1)))
     (setq tok1 (vector 'a))
     (setq n1 (1+ n1))
    )
    (t (setq tok1 (vconcat tok1 (list x)))
    )
   )
  )
  ans
 )
)
(defun sandhi-avagraha-separate-test (sym1)
 (let (tokar1 tokans symans)
  (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name sym1))))
  (setq tokans (sandhi-avagraha-separate tokar1))
;  (fol-msg (format "tokans=%s\n" tokans))
  (setq symans (mapcar 'sym-without-space tokans))
  (list symans)
 )
)

 
(defun sandhi-separate (symin)
; (03-15-03) To parse 'sharaddhimaH' properly, two steps are used:
; (1) sandhi-separate-main -> (... [sharad himaH] ...) 
; (2) sandhi-separate2 'sharad 'himaH  -> [sharat himaH]
; (03-31-03) To parse 'deveneti' into 'devena iti':
; While sandhi-separate-main was including this among its answers,
; 'sandhi-separate2' was changing it to 'devenaH iti'.
; The current solution in effect keeps both solutions,
; but only in case of consonant sandhi.
 (let (ans thisans ans1 x tokar1 tokar2)
  (setq ans (sandhi-separate-main symin))
  (setq ans1 ans)
  (while ans1
   (setq x (car ans1))
   (setq ans1 (cdr ans1))
   (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name (elt x 0)))))
   (setq tokar2 (car (ITRANS-parse-words-1 (symbol-name (elt x 1)))))
   (when (and (consonant-P (elt (substring tokar1 -1) 0))
	      (consonant-P (elt tokar2 0)))
    (setq thisans
      (solution (sandhi-separate2 (elt x 0) (elt x 1))))
    (if thisans
     (setq ans (append-if-new ans thisans)))
   )
  )
  ans
 )
)
(defun sandhi-separate-str (s1)
 (let (symin1 )
;  (setq symin1 (sym-without-space s1))
  (setq symin1 (intern-soft s1))
  (when symin1
   (sandhi-separate symin1)
  )
 )
)
(defun sandhi-separate-main (symin)
 (let (tokar n i more ans ians ibeg maxlen
       subtokar sym parses pfx sfx pfxtok sfxtok thisans chanaflag)

  (setq tokar (car (ITRANS-parse-words-1 (symbol-name symin))))
  (setq n (length tokar))
  (setq i (if (and (numberp ibeg) (<= 0 ibeg)) ibeg 0))
  (if (not (and (numberp maxlen) (<= 0 maxlen))) (setq maxlen 4))
  (setq more 't)
  (setq ans nil)
  (while (and more (< i n))
   (let (j iend)
    (setq j 1)
    (while (<= j maxlen)
     (setq iend (+ i j))
     (if (< n iend)
      (setq j maxlen)
       (setq subtokar (substring tokar i iend))
       (setq sym (sym-without-space subtokar))
       (setq parses (sanget 'Sandhi-Inverse-join sym))
;       (fol-msg (format "sym=%s, parses= %s\n" sym parses))
       ; handle case of 'aa
       (when (equal sym 'aa)
	(let (parse)
	 (setq parse (vector sym nil))
	 (setq parse (list parse))
	 (setq parses (append parses (list parse)))
	)
       )
       ; 06-26-03 : handle case of suffixes 'chid and 'chana
       ; Process this only if
       ; (a) parses is (otherwise) empty
       ; (b) tokar starts with 'k'
       ; (c) sym is either 'chi or 'chan
       (setq chanaflag nil)
       (when (and  (not parses)
		  (equal (elt tokar 0) 'k)
		  (member sym '(chi chan)))
	(let (parse)
	 (setq chanaflag t)
	 (setq parse (vector nil sym ))
	 (setq parse (list parse))
	 (setq parses (append parses (list parse)))
	)
       )
       (setq pfxtok (substring tokar 0 i))
       (setq sfxtok (substring tokar iend))
       (setq pfx (sym-without-space pfxtok))
       ; next is very ad-hoc. The idea is to change
       ; 'ki~n 'chana to 'kim 'chana
       (when (and chanaflag (equal pfx 'ki~n))
	(setq pfx 'kim)
	(setq pfxtok [k i m])
       )
       (setq sfx (sym-without-space sfxtok))
       (while parses
	(let (parse p1 p2 pfx1 sfx1 pfx1tok sfx1tok npfx1 nsfx1 symans
	      skipflag)
;          (fol-msg (format
; 		   "sandhi-separate. pfx=%s, sym=%s, sfx=%s parses=%s\n"
; 		   pfx sym sfx parses))
	 (setq parse (car parses))
	 (setq symans (elt parse 0))
	 (setq p1 (elt symans 0)) ; symbol or nil
	 (setq p2 (elt symans 1)) ; symbol or nil
;	 (setq pfx1tok (vconcat pfxtok (vector p1)))
	 ; 06-26-03
	 (setq pfx1tok (vconcat pfxtok (if p1 (vector p1) []) )) 

	 (setq sfx1tok (vconcat (if p2 (vector p2) []) sfxtok))
	 (setq npfx1 (length pfx1tok))
	 (setq nsfx1 (length sfx1tok))
	 (setq skipflag t)
	 (cond
	  ((and (< 1 npfx1)  ; exclude consecutive vowels (06-09-03)
		(vowel-P (elt pfx1tok (- npfx1 1)))
		(vowel-P (elt pfx1tok (- npfx1 2)))) )
	  ((and (< 1 nsfx1) ; exclude two vowels
		(vowel-P (elt sfx1tok 0))
		(vowel-P (elt sfx1tok 1))))
	  ((and (< 0 nsfx1)
		(equal (elt sfx1tok 0) 'M))) ; exclude initial anusvaara
	  ((and (= npfx1 1) ; exclude single cons.
	        (consonant-P (elt pfx1tok 0))))
	  ((and (< 1 npfx1)  ; exclude multiple consonants (AntoineI 72-1,2)
		(consonant-P (elt pfx1tok (- npfx1 1)))
		(consonant-P (elt pfx1tok (- npfx1 2)))
		(not (equal (elt pfx1tok (- npfx1 2)) 'r))) )
	  (t
	   (setq pfx1 (sym-without-space pfx1tok))
	   (setq sfx1 (sym-without-space sfx1tok))
;	   (fol-msg (format "    -> pfx1=%s sfx1=%s\n" pfx1 sfx1))
	   (setq thisans (vector pfx1 sfx1))
	   (setq ans (append ans (list thisans)))
	   (setq skipflag nil)
	  )
	 )
	)
	(setq parses (cdr parses))
       )
      
     )
     (setq j (1+ j))
    )
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun sandhi-separate2 (symin1 symin2 &optional optional)
 (let (tokar1 tokar2 n1 n2 i1 i2 more ans ians maxlen)
  (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name symin1))))
  (setq tokar2 (car (ITRANS-parse-words-1 (symbol-name symin2))))
  (setq n1 (length tokar1))
  (setq n2 (length tokar2))
  (setq more 't)
  (setq ans nil)
; check special sandhi rules. So far (11-29-02) just this one
  (cond
   ((equal symin1 'sa)
    (setq ans (list (vector 'saH symin2)))
    (setq more nil)
   )
   ((equal symin1 'eSha)
    (setq ans (list (vector 'eShaH symin2)))
    (setq more nil)
   )
   ; first word might end in 'eSha' e.g. 'yadyeSha' (02-15-03)
   ((and (<= 3 n1) (equal (substring tokar1 -3) [e Sh a]))
    (let (tmp symin1a)
     (setq tmp (vconcat tokar1 [H]))
     (setq symin1a (sym-without-space tmp))
     (setq ans (list (vector symin1a symin2)))
     (setq more nil)
    )
   )
   ((equal symin1 'bho)
    (setq ans (list (vector 'bhoH symin2)))
    (setq more nil)
   )
  )
  (setq maxlen 4)
  (setq i1 0)
  (while (and more (< i1 maxlen))
   (setq i2 0)
   (while (and more (< i2 maxlen))
    (let (j1 subtokar1 subtokar2 sym parses pfx sfx)
     (setq j1 (- n1 i1))
     (when (and (< 0 j1) (< i2 n2))
       (setq subtokar1 (substring tokar1 j1))
       (setq subtokar2 (substring tokar2 0 i2))
       (setq sym (intern (format "%s-%s"
	 (sym-without-space subtokar1) (sym-without-space subtokar2))))
       (setq parses (sanget 'Sandhi-Inverse-nojoin sym))
       (setq pfx (sym-without-space (substring tokar1 0 j1)))
       (setq sfx (sym-without-space (substring tokar2 i2)))
       (while parses
	(let (parse p1 p2 pfx1 sfx1 thisans checks ok symans)
	 (setq parse (car parses))
	 (setq symans (elt parse 0))
	 (setq p1 (elt symans 0)) ; symbol
	 (setq p2 (elt symans 1)) ; symbol or nil
;	 (setq p1 (elt parse 0)) ; symbol
;	 (setq p2 (elt parse 1)) ; symbol or nil
	 (setq pfx1 (sym-without-space (vector pfx p1)))
	 (setq sfx1 (sym-without-space
	   (if p2 (vector p2 sfx) (vector sfx))))
;	 (setq ok nil)
;         (setq checks (sandhi-pair pfx1 sfx1 optional 'nojoin))
	 (mapcar
	  (lambda (check)
	   (if (equal check (list symin1 symin2))
	    (setq ok t)
	   )
	  )
	  checks
	 )
	 (setq ok t)
	 (when ok
	  (setq pfx1 (praatar-modify pfx1))
	  (setq sfx1 (praatar-modify sfx1))
          (setq thisans (vector pfx1 sfx1))
	  (setq ans (append-if-new ans thisans))
;	  (setq ans (append ans (list thisans)))
	 )
	)
	(setq parses (cdr parses))
       )
     )
    )
    (setq i2 (1+ i2))
   )
   (setq i1 (1+ i1))
  )
  ans
 )
)
(defun sandhi-separate2-str (s1 s2 &optional optional)
 (let (symin1 symin2)
  (setq symin1 (sym-without-space s1))
  (setq symin2 (sym-without-space s2))
  (sandhi-separate2 symin1 symin2 optional)
 )
)
(defun sandhi-separate-final-M (symin1)
 (let (tokar1 nlast ans ans1 ans2)
  (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name symin1))))
  (setq nlast (1- (length tokar1)))
  (setq ans symin1)
  (when (equal (elt tokar1 nlast) 'M)
   (aset tokar1 nlast 'm)
   (setq ans (sym-without-space tokar1))
  )
  ans
 )
)
(defun sandhi-separate-final-r (symin1)
 (let (tokar1 nlast ans ans1 ans2)
  (setq tokar1 (car (ITRANS-parse-words-1 (symbol-name symin1))))
  (setq nlast (1- (length tokar1)))
  (setq symin1 (praatar-modify symin1)) ; 12-13-02 
  (setq ans (list symin1))
 
;   (when (equal (elt tokar1 nlast) 'H)
;    (aset tokar1 nlast 'r) ; word might be like 'punar'
;    (setq ans2 (sym-without-space tokar1))
;    (setq ans (list symin1 ans2))
;   )
  ans
 )
)
(defun sandhi-separate3-str (string &optional optional)
 (if (not (stringp string))
  (fol-msg (format "sandhi-separate3-str: string required : %s" string))
 (let (prevans ans syms nsyms isym sym)
  (setq syms (vconcat 
   (mapcar 'sandhi-separate-final-M
    (mapcar 'intern (word-list string)))))
;  (fol-msg (format "syms=%s\n" syms))
  (setq nsyms (length syms))
  (setq isym (1- nsyms))
  (setq ans (make-vector nsyms nil))
  (aset ans isym (sandhi-separate-final-r (elt syms isym)))
  (while (< 0 isym)
   (setq isym (1- isym))
   (let (w1 wans)
    (setq w1 (elt syms isym))
    (setq wans (apply 'append
     (mapcar 
      (lambda (w2) 
       (let (ans1 thisans)
	; ans1 is a list of vectors with 2 elements
	(setq ans1 (sandhi-separate2 w1 w2 optional))
;	(fol-msg (format "ans1=%s\n" ans1))
	; 1. check that w2 is the 2nd member of each parse
	(when ans1
	 (let (ok x2)
	  (setq ok 't)
	  (mapcar
	   (lambda (x)
	    (setq x2 (elt x 1))
	    (setq ok (and ok (equal w2 x2)))
	   )
	   ans1
	  )
	  (when (not ok)
	   (fol-msg (format
	     "sandhi-separate3-str: unexpected in word %s for %s\n"
	      isym string))
	  )
	 )
	)
	; 2. thisans is the list of first words in ans1
	; If the original 'w1 'w2 would join under Sandhi,
	; then 'w1' should not be part of thisans.
	(if (sandhi-pair w1 w2 nil 'join)
	  (setq thisans nil)
	  (setq thisans (list (praatar-modify w1)))
	)
	(mapcar
	 (lambda (x)
	  (let (x1)
	   (setq x1 (elt x 0))
	   (setq x1 (praatar-modify x1)) ; 12-13-02
	   (if (not (member x1 thisans))
	    (setq thisans (append thisans (list x1)))
	   )
;	   (fol-msg (format "x1=%s (%s), thisans=%s\n" (elt x 0) x1 thisans))
	  )
	 )
	 ans1
	)
	(if (not thisans) (setq thisans (list w1)))
	; 3. Make thisans the value of ans @ isym
	(aset ans isym thisans)
       )
      )
      (elt ans (1+ isym))
     )
    )) ; setq ans...
   )
  )
  (list syms ans)
; (append ans nil) ; return ans as a list
 )
 )
)

(defun examine-Sandhi-Inverse-nojoin ()
 (let (all sym vals)
 (setq all (sangetall 'Sandhi-Inverse-nojoin))
 (while all
  (setq sym (car all)) ; sym1-sym2
  (setq all (cdr all))
  (setq vals (car all)) ;([s1 s2]...)
  (setq all (cdr all))
  (let (val symkeys sympair ok)
   (setq symkeys (separate-symbol-pair sym))
   (setq ok t)
   (while vals
    (setq val (car vals))
    (setq vals (cdr vals))
    (when (not (equal (elt val 1) (elt symkeys 1)))
     (fol-msg (format "%s : found a non-match\n" sym))
     (setq ok nil)
    )
   )
   (if ok (fol-msg (format "%s is ok\n" sym)))
  )
 )
)
)
(defun gunate-final-vowel (tok &optional vrddhiP parts)
 (let (ans gunaP)
   ; when vrddhiP is nil (or absent) gunaP = t (so we gunate vowel)
   ; when vrddhiP is 't', gunaP = nil (so we vrddhate vowel)
  (if (not parts)
   (setq parts (word-parts tok))
  )
  (setq gunaP (not vrddhiP))
  (cond
    ((equal (substring (elt parts 1) -1) "V")
     (let (part1 p1 p2 v v0)
      (setq part1 (elt parts 0)) ; 
      (setq p1 (substring part1 0 -1)) ; all but last
      (setq p2 (substring part1 -1)) ; last
      (setq p2 (elt p2 0)) ; a 1-element array containing a vowel
      (setq v0 (elt p2 0)) ; the vowel
      (setq v (if gunaP (guna v0) (vrddhi v0)))
      (setq p2 (if (vectorp v) v (vector v)))
      (setq part1 (vconcat p1 p2))
      (setq ans (vconcat (flatten part1)))
     )
    )
    ((equal (substring (elt parts 1) -2) "VC")
     (let (part1 p1 p2 p3 v v0)
      (setq part1 (elt parts 0)) ; 
      (setq p1 (substring part1 0 -2)) ; all but last two
      (setq p2 (substring part1 -2 -1))
      (setq p2 (elt p2 0)) ; a 1-element array containing a vowel
      (setq v0 (elt p2 0)) ; the vowel
      (setq p3 (substring part1 -1)) ; 
      ; only gunate when v0 is a short simple vowel AND
      ; the final consonant is not compound
      (when (and (= (length (elt p3 0)) 1) 
	    (shortsimplevowel-P v0)) 
       (setq v (if gunaP (guna v0) (vrddhi v0)))
       (setq p2 (if (vectorp v) v (vector v)))
      )
      (setq part1 (vconcat p1 p2 p3))
      (setq ans (vconcat (flatten part1)))
      )
     )
    (t 
     (setq ans tok)
    )
   )
  ans
 )
)
