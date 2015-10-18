; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; gram3.el  
; 12-09-03 : changed to use citation forms as the 'base' forms,
;   e.g., cite 'ashvaH' as 'ashva'. Former function
;   'praatipadika-REGULAR' was changed to 'subanta-base'.
; begun 09-27-02 by ejf
; Code in the following may be used:
;   itrans.el, gram1.el, sandhi.el, gram2.el
; The particular spelling of the symbols representing
; the phonetic elements (the 'tokens') must be consistent with 
; that in itrans.el
(defun declension-citation1 (citation-sym praatipadika gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-citation1: %s %s %s %s %s\n"
     citation-sym praatipadika gender form irregs))
 )
 (setq praatipadika (solution praatipadika))
 (cond
  ((equal form 'ach)
   (declension-general-cons citation-sym praatipadika gender 'ach-ADJ irregs dbg)
  )
  ((equal form 'aach)
   (declension-general-cons citation-sym praatipadika gender 'aach-ADJ irregs dbg)
  )
  ((member form declension-cons-forms)
   (declension-general-cons citation-sym praatipadika gender form irregs dbg))
  ((and (member gender '(M F)) (member form '(uu1 ii1)))
   (declension-general-M-uu citation-sym praatipadika gender form irregs dbg))
  ((member (list gender form) '((M uu)))
   (cond
    ((symbolp praatipadika)
     (declension-general praatipadika gender form irregs dbg)
    )
    ((listp praatipadika)
     (setq praatipadika (car praatipadika))
     (declension-general praatipadika gender form irregs dbg)
    )
   )
  )
  (t (declension-general praatipadika gender form irregs dbg))
 )
)

(defun citation-pr (vec)
 (let (n i s j x)
  (setq n (length vec))
  (setq i 0)
  (setq j 0)
  (while (< i n)
   (setq x (substring vec i (+ i 3)))
   (setq i (+ i 3))
   (setq j (1+ j))
   (fol-msg (format "%s.  %s %s %s\n" j (elt x 0) (elt x 1) (elt x 2)))
  )
 )
)
(defun declension-citation-other1 (citation-sym gender-form gender form type &optional dbg)
 (let (ans Eng-def dtab base temp)
  (if (equal (elt gender-form 0) 'BASE)
   (progn
    (setq base (elt gender-form 1))
    (setq Eng-def (elt gender-form 2)) ; not used in this function
;    (fol-msg (format "chk1a: %s %s %s %s %s %s\n"
;		     Eng-def citation-sym type form base gender))
    (setq temp (construct-subanta1 citation-sym gender form))
;    (fol-msg (format "chk1b: %s\n" temp))
    (setq dtab (plist-get (elt temp 1) gender))
   )
   (progn
    (setq Eng-def (elt gender-form 0))
    (setq dtab (elt gender-form 1))
   )
  )
  (setq ans (list type gender form dtab))
;  (setq ans dtab)
  ans
 )
)
(defun declension-citation-other (citation-sym &optional dbg)
 (let (other-info1 other-info x x1 ans)
  (setq other-info1 (sanget2 citation-sym '(subanta)))
  (while other-info1
   (let (praatipadika x gender form iform genders x-new irregs gender-forms
		      this-info type gender-form) 
    (setq type (car other-info1)) ; NOUN / ADJ
    (setq other-info1 (cdr other-info1))
    (setq other-info (car other-info1)) ; a plist
    (setq other-info1 (cdr other-info1))
    (while other-info
     (setq form (car other-info)) ; a aa uu etc
     (setq other-info (cdr other-info))
     (setq gender-forms (car other-info))
     (setq other-info (cdr other-info))
     (while gender-forms
      (setq gender (car gender-forms))
      (setq gender-forms (cdr gender-forms))
      (setq gender-form (car gender-forms))
      (setq gender-forms (cdr gender-forms))
      (setq ans (append ans (list
       (declension-citation-other1
	citation-sym gender-form gender form type)))
      )
     )
    )
   )
  )
  ans
 )
)
(defun declension-citation (citation-sym &optional g printflag dbg)
 (let (ans  other-info gender form declension)
  (setq other-info (declension-citation-other citation-sym dbg))
;  (fol-msg (format "other-info=%s\n" other-info))
  (setq ans nil)
  (while other-info
   (let (praatipadika citation-tok praatipadika-tok irregs
	   declension x thisans gender-forms setq adjform )
    (setq x (car other-info))
    (setq other-info (cdr other-info))
    (setq praatipadika (elt x 0))
    (setq gender (elt x 1))
    (setq form (elt x 2))
    (setq irregs (elt x 3))
    (setq declension
     (cond
      ((and g (not (equal g gender))) nil)
;      (t (declension-citation1 citation-sym praatipadika gender form irregs))
      (t irregs)
     )
    )
    (when declension
     (setq thisans (list gender form declension))
     (setq ans (append-if-new ans thisans))
    )
   )
  )
  (when (and printflag ans)
   (fol-msg (format "declension-citation : %s\n" citation-sym))
   (mapcar
    (lambda (x)
     (setq gender (elt x 0))
     (setq form (elt x 1))
     (setq declension (elt x 2))
     (fol-msg (format " %s %s\n" gender form))
     (citation-pr declension)
     (fol-msg (format "\n"))
    )
    ans
   )
   (setq ans t)
  )
  ans
 )
)
(defvar declension-general-save nil)

(defun declension-general (praatipadika gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general: %s %s %s %s\n"
 		  praatipadika gender form irregs))
 )
 (let (ans sups)
  (setq sups (sup-get gender form)) ; an array
;  (fol-msg (format "chk0: %s %s %s\n" gender form sups))
  (cond 
   (sups
    (let (base-tok n i  sup irreg)
      (setq base-tok (car (ITRANS-parse-words-1 (symbol-name praatipadika))))
      (setq n (length sups))
      (setq ans (make-vector n nil))
      (setq i 0)
      (while (< i n)
       (setq sup (elt sups i))
       (setq irreg (elt irregs i))
;       (fol-msg (format "chk: %s %s %s %s\n" i base-tok sup irreg))
       (aset ans i
	(if irreg
	 irreg
	 (sym-without-space (declension-general-1 base-tok sup))
	)
       )
;       (fol-msg (format "chk: %s %s\n" i (elt ans i)))
       (setq i (1+ i))
      )
    )
   )
  )
  ans
 )
)
(defun declension-general-cons 
 (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-cons: %s\n"
   (list citation-sym praatipadikas gender form irregs)))
  )
  (let (ans procsym)
   (setq procsym
    (intern-soft (format "declension-general-%s" form)))
;   (fol-msg (format "procsym = %s\n" procsym))
   (when dbg
    (fol-msg (format "declension-general-cons calls: %s\n"
     (list procsym citation-sym praatipadikas gender form irregs dbg)
    ))
   )
   (setq ans
     (apply procsym (list  citation-sym praatipadikas gender form irregs dbg)))
   (setq declension-general-save (list
    (list praatipadikas gender form) ans))
   ans
  )
)
(defun declension-general-1cons
 (citation-sym praatipadikas gender form irregs &optional dbg)
 ; Note of 01-05-04: praatipadikas not used. It is computed here.
 ; citation-sym IS used.
 ; form is not used
 (when dbg
  (fol-msg (format "declension-general-1cons: %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
 (let (citation-tok last)
  (setq citation-tok (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0)) ; last char
  (cond
   ((equal last 'l) (declension-1cons-l citation-sym gender irregs dbg))
   ((equal last 'r) (declension-1cons-r citation-sym gender irregs dbg))
   ((equal last 'ch) (declension-1cons-ch citation-sym gender irregs dbg))
   ((equal last 'Ch) (declension-1cons-Ch citation-sym gender irregs dbg))
   ((equal last 'j) (declension-1cons-j citation-sym gender irregs dbg))
   ((equal last 'sh) (declension-1cons-sh citation-sym gender irregs dbg))
   ((equal last 's) (declension-1cons-s citation-sym gender irregs dbg))
   ((equal last 'Sh) (declension-1cons-Sh citation-sym gender irregs dbg))
   ((equal last 'h) (declension-1cons-h citation-sym gender irregs dbg))
   ((equal last 'v) (declension-1cons-v citation-sym gender irregs dbg))
   ((member last '(t th d dh))
    (declension-1cons-t citation-sym gender irregs dbg)
   )
   ((member last '(p ph b bh))
    (declension-1cons-p citation-sym gender irregs dbg)
   )
   ((member last '(k k  g gh))
    (declension-1cons-k citation-sym gender irregs dbg)
   )
   (t
    (fol-msg (format "declension-general-1cons: unexpected : %s %s\n"
		     citation-sym gender))
   )
  )
 )
)
(defun declension-1cons-finish (base-toks gender irregs &optional dbg)
 (let (ans sups n)
  (and
   ; 1
   (setq sups (sup-get gender '1cons)) ; an array
   (setq n (length sups))
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-1cons-ch (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-ch : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 (substring citation-tok 0 -1)) ; drop last char
  (setq tok2 citation-tok)
  (cond
   ((string-match
     (concat (make-regexp '(vrashch bhrasj
			    vRishch)) ; from SL. 
      "$"
     )
     (symbol-name citation-sym)
    )
    ; I think the sibilant disappears in front of consonants
    (setq tok2 (vconcat (substring tok1 0 -1) [T]))
   )
   ((string-match
     (concat (make-regexp '(vrashch bhrasj sRij mRij yaj raaj bhraaj
			    vRishch)) ; from SL. 
      "$"
     )
     (symbol-name citation-sym)
    )
    ; Kale 94(b) Pan VIII.2.36
    ; The ending 'sh' or 'Ch' of root-nouns and the final
    ; of 'vrashch bhrasj sRij mRij raaj and bhraaj'
    ; are changed to 'Sh' when followed 
    ; by any consonant except a nasal or a semi-vowel
    ; or by nothing.
    ; the 'Sh' is changed to 'T' or 'D' when at the end of a word,
    ; and to 'D' when followed by a soft consonant.
    ; The 'j' of 'parivraaj' is similarly changed to 'T' or 'D'.
    ; NOTE: this includes 'samraaj parivraaj'
    (setq tok2 (vconcat tok1 [T]))
   )
   (t
    ; Antoine1: 77(1) Nouns with stems ending in palatals
    ;  Final 'ch' is changed to 'k' in 1S and 7P
    ;                        to 'g' before 'bhyaam', 'bhiH' and 'bhyaH'
    ; Kale #94(a). 'ch' or 'j' is changed to 'k' when followed by
    ; a hard consonant or by nothing, and to 'g' when followed by
    ; a soft consonant. (Pan. VIII.2.30)
    (setq tok2 (vconcat tok1 [k]))
   )
  )
  (setq x citation-tok)
  (setq y tok2)
  (setq base-toks (vector
       y x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y x x
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-Ch (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-Ch : %s\n"
   (list citation-sym  gender irregs)))
 )

 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   ((vowel-P (elt (substring tok1 -2 -1) 0))
    (setq tok2 (vconcat (substring tok2 0 -1) [T]))
    (setq tok1 (vconcat (substring tok1 0 -1) [ch Ch]))
   )
   (t
    (setq tok2 (vconcat (substring tok2 0 -2) [T]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq base-toks (vector
       y x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y x x
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-j (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-j : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
    ;  Final 'j' is changed to 'k' or 'T' in 1S and 7P
    ;                       to 'g' or 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
  (cond
   ((string-match
     (concat (make-regexp '(bhrasj bhRisj))
      "$"
     )
     (symbol-name citation-sym)
    )
    ; also drop the sibilant
    (setq tok2 (vconcat (substring tok1 0 -2) [T]))
    ; acc. to SL: change 'sj' to 'jj' before vowel endings
    (setq tok1 (vconcat (substring tok1 0 -2) [j j]))
   )
   ((string-match
     (concat (make-regexp '(vrashch bhrasj sRij mRij yaj raaj bhraaj))
      "$"
     )
     (symbol-name citation-sym)
    )
    ; Kale 94(b) Pan VIII.2.36
    ; The ending 'sh' or 'Ch' of root-nouns and the final
    ; of 'vrashch bhrasj sRij mRij raaj and bhraaj'
    ; are changed to 'Sh' when followed 
    ; by any consonant except a nasal or a semi-vowel
    ; or by nothing.
    ; the 'Sh' is changed to 'T' or 'D' when at the end of a word,
    ; and to 'D' when followed by a soft consonant.
    ; The 'j' of 'parivraaj' is similarly changed to 'T' or 'D'.
    ; NOTE: this includes 'samraaj parivraaj'
    (setq tok2 (vconcat (substring tok1 0 -1) [T]))
   )
   ((string-match (concat (make-regexp '(sj)) "$")
     (symbol-name citation-sym)
    )
    (setq tok2 (vconcat (substring tok1 0 -2) [k]))
    ; acc. to SL: change 'sj' to 'jj' before vowel endings
    (setq tok1 (vconcat (substring tok1 0 -2) [j j]))
   )
   (t
    ; Kale #94(a). 'ch' or 'j' is changed to 'k' when followed by
    ; a hard consonant or by nothing, and to 'g' when followed by
    ; a soft consonant. (Pan. VIII.2.30)
    (setq tok2 (vconcat (substring tok1 0 -1) [k]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq base-toks (vector
       y x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y x x
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-sh (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-sh : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
    ;  Final 'sh' is changed to 'k' or 'T' in 1S and 7P
    ;                       to 'g' or 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
  (cond
   ((string-match
     (concat (make-regexp '(dish dRish spRish mRish))
      "$"
     )
     (symbol-name citation-sym)
    )
    ; Kale #94(c) p. 56.
    ; The 'sh' of the root-nouns 'dish dRish spRish mRish' is
    ; changed to 'k'. The 'sh' of 'nash' is optionally changed to
    ; 'T' or 'k'
    (setq tok2 (vconcat (substring tok1 0 -1) [k]))
   )
   ((string-match
     (concat (make-regexp '(nash))
      "$"
     )
     (symbol-name citation-sym)
    )
    (setq tok2 (list
      (vconcat (substring tok1 0 -1) [T])
      (vconcat (substring tok1 0 -1) [k])
    ) ) 
   )
   (t
    ; Kale #94(b). The ending 'sh' of root-nouns changes to 'Sh'
    ; when followed by any consonant except a nasal or a semi-vowel or
    ; by nothing. The 'Sh' changes to 'T' or 'D' when at the end of a
    ; word, and to 'D' when followed by a soft consonant.
    (setq tok2 (vconcat (substring tok1 0 -1) [T]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq y1 y)
  (when (string-match
     (concat (make-regexp '(dRish))
      "$"
     )
     (symbol-name citation-sym)
    )
    ; SL. Not sure why this is done
    (setq y1 (list y (vconcat (substring y 0 -1) [~N])))
  )
  (setq base-toks (vector
       y1 x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y1 x x
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-l (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-l : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 (substring citation-tok 0 -1)) ; drop last char
  (setq tok2 citation-tok)
   ; Kale#89a, p. 54
   ;After final 'l', the 's' of the loc. pl. is changed to 'Sh'
  (setq x citation-tok)
  (setq y tok2)
  (setq base-toks (vector
       y x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y x x
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  (aset ans 20 ; 7P
   (sym-without-space (vconcat x [Sh u]))
  )
  ans
 )
)
(defun declension-1cons-r (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-r : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z v y1 vowel)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 (substring citation-tok 0 -1)) ; drop last char
  (setq tok2 citation-tok)
    ; 77(5) Nouns with stems ending in 'r'
    ; Final 'r' is changed to 'H' (visarga) in 1S.
    ; A short vowel preceding 'r' is lengthened in 1S and
    ;  before 'bhyaam', 'bhiH' and 'bhyaH'
  (setq vowel (elt (substring citation-tok -2 -1) 0)) ; penultimate char
  (setq v (lengthen-vowel vowel))
  (setq tok2 (vconcat (substring citation-tok 0 -2) (vector v 'H)))
  (setq x citation-tok)
  (setq y tok2)
  (setq z (vconcat (substring x 0 -2) (vector v 'r)))
  (setq y1 (if (equal gender 'N) y x))
  (setq base-toks (vector
       y x x
       y1 x x
       x z z
       x z z
       x z z
       x x x
       x x z
       y x x
      ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-s (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-s : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       tok1 tok2 x y z v y1 vowel v2 last)
  (setq last 's)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
    ; 77(6) Nouns with stems ending in 's'
    ; Final 's' becomes visarga in 1S and before 'bhyaam', 'bhiH' and 'bhyaH',
    ; and that 'H' follows visarga sandhi rules in combining with the
    ; terminations,
    ; except that final 's' remains before terminations beginning with vowels,
    ; being changed to 'Sh' if preceding vowel is other than 'a' or 'aa'.
    ; M and F nouns in 'as' lengthen the 'a' in 1S.
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (setq vowel (elt (substring citation-tok -2 -1) 0)) ; penultimate char
  (if (and (equal vowel 'a) (member gender '(M F)))
   (progn
    (setq v (lengthen-vowel vowel))
    (setq tok2 (vconcat (substring tok1 0 -2) (vector v) [H]))
   )
   (setq tok2 (vconcat (substring tok1 0 -1) [H]))
  )
  (setq x tok1)
  (setq y tok2)
  (cond
   ((and (member gender '(M F))
	  (member vowel '(a aa))) ;12-29-03. Was '(a)'
     ; The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
     (let (z v)
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq base-toks (vector
       y x x
       x x x
       x z z
       x z z
       x z z
       x x x
       x x z
       x x x
      ))
     )
    )
   ((and (member gender '(N)))
     ; The neuter nouns in 'as', 'is', and 'us' lengthen the 'a', 'i',
     ; and 'u' and insert a nasal (anusvaara) in 1P 2P 8P
     ; The nouns in 'is' and 'us' become 'iSh' and 'uSh' before
     ; vowel endings (other than 1P 2P 8P)
     (let (z v w j last1)
      (setq v (lengthen-vowel vowel))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq last1
       (if (equal vowel 'a) last 'Sh)
      )
      (setq w (vconcat (substring x 0 -2) (vector v 'M last1)))
      (setq j (vconcat (substring x 0 -2) (vector vowel last1)))
      (setq base-toks (vector
       y j w
       y j w
       j z z
       j z z
       j z z
       j j j
       j j z
       x j w
      ))
     )
    )
   ((and (vowel-P vowel))
     ;There do not seem to be many M/F nouns ending in 's' and preceded
    ; by a non-a.  The irregular noun 'dos' (m. arm) (Antoine2-#78) is
    ; declined in part like a noun with one stem.
    ; The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
     (let (z v w j last1)
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq last1
       (if (equal vowel 'a) last 'Sh)
      )
      (setq j (vconcat (substring x 0 -2) (vector vowel last1)))
      (setq base-toks (vector
       y j j
       j j j
       j z z
       j z z
       j z z
       j j j
       j j z
       x j j
      ))
     )
    )
  (t
   (setq z (vconcat (substring x 0 -2) (vector v 'r)))
   (setq y1 (if (equal gender 'N) y x))
   (setq base-toks (vector
       y x x
       y1 x x
       x z z
       x z z
       x z z
       x x x
       x x z
       y x x
      ))
   )
  )
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-Sh (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-Sh : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1 y2)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   ((string-match
     (concat (make-regexp '(takSh gorakSh)) "$")
     (symbol-name citation-sym)
    )
    ; Kale #94(c) p. 56.
    ; The 'kSh' of 'takSh gorakSh' are optionally changed to 'T' or 'k'
    (setq tok2 (list
     (vconcat (substring tok1 0 -2) [T])
     (vconcat (substring tok1 0 -2) [k])
    ))
   )
   ((string-match
     (concat (make-regexp '(kSh)) "$")
     (symbol-name citation-sym)
    )
    ; Kale #94(c) p. 56.
    ; 'kSh' of such words as 'vipakSh' is changed to 'k'
    (setq tok2 (vconcat (substring tok1 0 -2) [k]))
   )
   ((string-match
     (concat (make-regexp '(dadhRiSh)) "$")
     (symbol-name citation-sym)
    )
    ; Kale #94(c) p. 56.
    ; The 'Sh' of 'daDhRiSh' (bold or impudent man) is changed to 'k'
    ; 'kSh' of such words as 'vipakSh' is changed to 'k'
    ; NOTE: SL has 'k' for masc., 'T' for neut.
    (if (equal gender 'N)
     (setq tok2 (vconcat (substring tok1 0 -1) [T]))
     (setq tok2 (vconcat (substring tok1 0 -1) [k]))
    )
   )
   (t
    ; Antoine1:77(2) Nouns with stems ending in cerebrals
    ;  Final 'Sh' is changed to 'T' in 1S and 7P
    ;                        to 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat (substring tok1 0 -1) [T]))
   )
  )
  (setq x citation-tok)
  (setq y tok2)
  (setq y1 (if (equal gender 'N) y x))
  (setq y2
   (if (equal gender 'N)
    (vconcat (substring x 0 -1) [M] (substring x -1))
    x)
  )
  (setq base-toks (vector
       y x y2
       y1 x y2
       x y y
       x y y
       x y y
       x x x
       x x y
       y x y2
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-h (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-h : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1 y2 x1 y3)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
    ;Antoine1 77(7) Nouns with stems ending in 'h'
    ; Usually, final 'h' becomes 'k' in 1S and 7P
    ;                   'g' before 'bhyaam', 'bhiH' and 'bhyaH'
    ; the appearance of 'dh' is explained by the following sandhi:
    ; When 'gh', 'dh', 'bh', or 'h' loses aspiration owing to combination
    ; with following consonants, the preceding consonant becomes aspirated,
    ; if possible
    ; In nouns ending with 'lih', final 'h' becomes 'T' in 1S and 7P,
    ; and final 'h' becomes 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    ; For the noun 'upaanah', the final 'h becomes 't' and 'd'
  (cond
   ((string-match "d[a-zA-Z]h$"
     (symbol-name citation-sym)
    )
    (setq tok2 (vconcat (substring tok2 0 -3)
	       [dh] (substring tok2 -2 -1) [k]))
   )
   ((string-match "g[a-zA-Z]h$"
     (symbol-name citation-sym)
    )
    (setq tok2 (vconcat (substring tok2 0 -3)
	       [gh] (substring tok2 -2 -1) [T]))
   )
   ((string-match
     (concat (make-regexp '(lih)) "$")
     (symbol-name citation-sym)
    )
    (setq tok2 (vconcat (substring tok2 0 -1) [T]))
   )
   ((string-match
     (concat (make-regexp '(upaanah)) "$")
     (symbol-name citation-sym)
    )
    (setq tok2 (vconcat (substring tok1 0 -1) [t]))
   )
   ((string-match
     (concat (make-regexp '(uShNih)) "$")
     (symbol-name citation-sym)
    )
    ; Kale#94(3), p.57 REF: Pan III.2.59
    (setq tok2 (vconcat (substring tok1 0 -1) [k]))
   )
   ((string-match
     (concat (make-regexp '(muh snuh snih)) "$")
     (symbol-name citation-sym)
    )
    ; Kale#95(a), p.57 
    (setq tok2 (list
     (vconcat (substring tok1 0 -1) [T])
     (vconcat (substring tok1 0 -1) [k])
    ))
   )
   ((string-match
     (concat (make-regexp '(druh)) "$")
     (symbol-name citation-sym)
    )
    ; Kale#95(a), p.57
    ; Not sure why 'd' is aspirated before consonant endings
    ; but Kale (p. 59 example) and SL show this.
    (setq tok2 (vconcat (substring tok2 0 -4) [dh r u h]))
    (setq tok2 (list
     (vconcat (substring tok2 0 -1) [T])
     (vconcat (substring tok2 0 -1) [k])
    ))
   )
   ((string-match
     (concat (make-regexp '(turaasaah turaaShaah)) "$")
     (symbol-name citation-sym)
    )
    ; Kale#98 p. 60.
    ; The 's' of 'turaasaah' (Indra) is changed to 'Sh' before the
    ; consonantal terminations (Pan VIII.3.56)
    ; Note: MW cites this is 'turaaShaah'
    (setq tok2 (vconcat (substring tok2 0 -3) [Sh aa T]))
   )
   (t
    (setq tok2 (vconcat (substring tok2 0 -1) [T]))
   )
  )
  (setq x tok1)
  (setq x1 x)
  (setq y tok2)
  (setq y1 (if (equal gender 'N) y x1))
  (setq y2
   (if (equal gender 'N)
    (vconcat (substring x 0 -1) [M] (substring x1 -1))
    x1)
  )
  (setq y3 y2)
  (when (string-match
     (concat (make-regexp '(vah vaah)) "$")
     (symbol-name citation-sym)
    )
    ; MW says 'shvetavah' is variant of 'shvetavaah'.
    ; Kale#100, p. 60. says:
    ; The 'vaa' of root-nouns ending in 'vaah' is changed to 'uu' before
    ; the vowel terminations beginning with 2P.
    ; REF: Pan VI.4.132, VI.1.108
    ; 'aa' and 'uu' combine into 'au' by Pan VI.1.89.
    (setq w (declension-general-1 (substring tok1 0 -3) [uu]))
    (if (member (substring tok1 -4 -3) '([a] [aa]))
      (setq w (vconcat (substring w 0 -1) [au])) ; very special sandhi
    )
    (setq tok1 (vconcat w [h]))
    (setq tok2 (vconcat (substring tok2 0 -2) [aa] [T]))
    (setq x tok1)
    (setq x1 (vconcat (substring tok2 0 -2) [aa] [h]))
    (setq y tok2)
    (setq y1 (if (equal gender 'N) y x1))
    (setq y2
     (if (equal gender 'N)
      (vconcat (substring x 0 -1) [M] (substring x1 -1))
      x1)
     )
    (setq y3 x)
  )

  (setq base-toks (vector
       y x1 y2
       y1 x1 y3
       x y y
       x y y
       x y y
       x x x
       x x y
       y x1 y2
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-v (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-v : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z v y1 vowel)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   ((string-match "div$" (symbol-name citation-sym))
    ; 'div' f. (sky) : Antoine2-#80. See irreg.el also
    ; This is declined like a noun with 1 consonant, except (in M/F)
    ; (a) 1S is 'dyauH'
    ; (a)' 8S is 'dyauH'
    ; (b) 2S is optionally 'dyaam'
    ; (c) before terminations beginning with a consonant it uses stem 'dyu'
    ;    (this is accomplished in algorithm, by providing two praatipadikas
    ;     ('div' and 'dyu')
    (setq tok2 (vconcat (substring tok2 0 -3) [d y u]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq z y)
  (setq y1 (if (equal gender 'N) y x))
  (setq base-toks (vector
       y x x
       y1 x x
       x z z
       x z z
       x z z
       x x x
       x x z
       y x x
      ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  (cond
   ((and (string-match "div$" (symbol-name citation-sym))
	 (member gender '(M F))
    )
    (let (tmp tmp1 tmpold)
     (setq tmp (vconcat (substring citation-tok 0 -3) [d y au H]))
     (setq tmp1 (sym-without-space tmp))
     (aset ans 0 tmp1)
     (aset ans 21 tmp1)
     (when (equal gender 'F)
      (setq tmp (vconcat (substring citation-tok 0 -3) [d y aa m]))
      (setq tmp1 (sym-without-space tmp))
      (setq tmpold (aref ans 3))
      (aset ans 3 (list tmpold tmp1))
     )
    )
   )
  )
  ans
 )
)
(defun declension-1cons-t (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-t : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1 y2)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   ((string-match
     (concat (make-regexp '(budh)) ; from SL. 
      "$"
     )
     (symbol-name citation-sym)
    )
    ; I think the sibilant disappears in front of consonants
    (setq tok2 (vconcat (substring tok1 0 -3) [bh u t]))
   )
   (t
    ; Antoine#1.77(3) Nouns with stems ending in dentals
    ;  Final dental other than 'n' is changed to 't' in 1S and 7P
    ;                       to 'd' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat (substring tok2 0 -1) [t]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq y1 (if (equal gender 'N) y x))
  (setq y2
   (if (equal gender 'N)
    (vconcat (substring x 0 -1) [n] (substring x -1))
    x)
  )
  (setq base-toks (vector
       y x y2
       y1 x y2
       x y y
       x y y
       x y y
       x x x
       x x y
       y x y2
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-p (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-p : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1 y2)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   (t
    ; Antoine#1. 77(4) Nouns with stems ending in labials
    ;  Final labial is changed to 'p' in 1S and 7P
    ;                       to 'b' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat (substring tok2 0 -1) [p]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq y1 (if (equal gender 'N) y x))
  (setq y2
   (if (equal gender 'N)
    (vconcat (substring x 0 -1) [m] (substring x -1))
    x)
  )
  (setq base-toks (vector
       y x y2
       y1 x y2
       x y y
       x y y
       x y y
       x x x
       x x y
       y x y2
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs))
  ans
 )
)
(defun declension-1cons-k (citation-sym gender irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-1cons-k : %s\n"
   (list citation-sym  gender irregs)))
 )
 (let (procname ans base-toks n  citation-tok
       last tok1 tok2 x y z w y1 y2)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 citation-tok)
  (setq tok2 citation-tok)
  (cond
   (t
    ; 
    (setq tok2 (vconcat (substring tok2 0 -1) [k]))
   )
  )
  (setq x tok1)
  (setq y tok2)
  (setq y1 (if (equal gender 'N) y x))
  (setq y2
   (if (equal gender 'N)
    (vconcat (substring x 0 -1) [M] (substring x -1))
    x)
  )
  (setq base-toks (vector
       y x y2
       y1 x y2
       x y y
       x y y
       x y y
       x x x
       x x y
       y x y2
  ))
  (setq ans (declension-1cons-finish base-toks gender irregs dbg))
  ans
 )
)

(defun declension-general-mat
  (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-mat : %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )

; note: citation-sym unused!
 (let (procname ans sups base-toks n praatipadika-tok wtok stok)
  (setq procname (format "declension-general-%s" form))
  (when nil ;dbg
   (fol-msg (format "%s : %s %s %s %s\n" procname
		    citation-sym praatipadikas gender form))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   (if (symbolp praatipadikas)
    (let (tok)
     (setq tok (car (ITRANS-parse-words-1 (symbol-name praatipadikas))))
     (setq praatipadika-tok tok)
     (setq wtok (vconcat tok [a t]))
     (if (equal praatipadikas 'mah)
      (setq stok (vconcat tok [aa n t])) ; mahat has strong stem mahaant
      (setq stok (vconcat tok [a n t])) ; dhiimat, etc, has strong = dhiimant
     )
    )
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname praatipadikas))
     nil ; so 'and' fails
    )
   )
   ; 3
   
   (cond 
    ((equal gender 'M)
     (let (nomsing )
      (setq nomsing (vconcat praatipadika-tok [aa n t]))
      (setq base-toks (vector
       nomsing stok stok
       stok stok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       stok stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let ()
      (setq base-toks (vector
       wtok wtok stok
       wtok wtok stok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok wtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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

(defun declension-general-mat-ADJ (c p g f i &optional dbg)
 (declension-general-mat c p g f i dbg)
)
(defun declension-general-vat (c p g f i &optional dbg)
 (declension-general-mat c p g f i dbg)
)
(defun declension-general-vat-ADJ (c p g f i &optional dbg)
 (declension-general-mat c p g f i dbg)
)
(defun declension-general-iiyas-ADJ
  (citation-sym praatipadikas gender form irregs &optional dbg)
; note: citation-sym unused!
 (let (procname ans sups base-toks n praatipadika-tok wtok stok vtok)
  (setq procname (format "declension-general-%s" form))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   (if (symbolp praatipadikas)
    (let (tok)
     (setq tok (car (ITRANS-parse-words-1 (symbol-name praatipadikas))))
     (setq praatipadika-tok tok)
     (setq wtok (vconcat tok [a H])) ; before weak cons endings
     (setq stok (vconcat tok [aa M s])) ; before all strong endings
     (setq vtok (vconcat tok [a s])) ; before weak vowel endings
    )
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname praatipadikas))
     nil ; so 'and' fails
    )
   )
   ; 3
   
   (cond 
    ((equal gender 'M)
     (let (nomsing vocsing)
      (setq nomsing (vconcat praatipadika-tok [aa n t]))
      (setq vocsing (vconcat praatipadika-tok [a n]))
      (setq base-toks (vector
       nomsing stok stok
       stok stok vtok
       vtok wtok wtok
       vtok wtok wtok
       vtok wtok wtok
       vtok vtok vtok
       vtok vtok wtok
       vocsing stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let ()
      (setq base-toks (vector
       wtok vtok stok
       wtok vtok stok

       vtok wtok wtok
       vtok wtok wtok
       vtok wtok wtok
       vtok vtok vtok
       vtok vtok wtok

       wtok vtok stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok vtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-in
  (citation-sym praatipadikas gender form irregs &optional dbg)
; note: citation-sym unused!
 (let (procname ans sups base-toks n praatipadika-tok wtok stok)
  (setq procname (format "declension-general-%s" form))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   (if (symbolp praatipadikas)
    (let (tok)
     (setq tok (car (ITRANS-parse-words-1 (symbol-name praatipadikas))))
     (setq praatipadika-tok tok)
     (setq wtok (vconcat tok [i]))
     (setq stok (vconcat tok [i n])) ; before ALL vowel case endings 
    )
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname praatipadikas))
     nil ; so 'and' fails
    )
   )
   ; 3
   
   (cond 
    ((equal gender 'M)
     (let (nomsing )
;      (setq nomsing (vconcat praatipadika-tok [ii]))
      (setq nomsing wtok)
      (setq base-toks (vector
       nomsing stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       stok stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let (xtok)
      (setq xtok (vconcat praatipadika-tok [ii n]))
      (setq base-toks (vector
       wtok stok xtok
       wtok stok xtok

       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok

       wtok stok xtok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok stok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-in-ADJ (c p g f i &optional dbg)
 (declension-general-in c p g f i dbg)
)
(defun declension-general-vas-ADJ
  (citation-sym praatipadikas gender &optional form irregs dbg)
; note: citation-sym IS used, praatipadikas is not used
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1)
  (if (not form) (setq form 'vas-ADJ))
  (setq procname (format "declension-general-%s" form))
;  (fol-msg (format "%s: %s %s %s %s\n"
;		   procname citation-sym praatipadikas gender form))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   citation-sym
   (if (symbolp citation-sym)
    t
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname citation-sym))
     nil ; so 'and' fails
    )
   )
   (setq ptok ; chakRivas  jagmivas vidvas
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
   (< 3 (length ptok))
   (equal (substring ptok  -3) [v a s])
   (setq ptok1 (substring ptok 0 -3)) ; [ch a k Ri] [j a g m i] [v i d]
   (let ()
    (setq stok (vconcat ptok1 [v aa M s]))
    (setq mtok (vconcat ptok1 [v a t]))
    (if (equal (substring ptok1 -1) [i])
     (setq wtok (vconcat (substring ptok1 0 -1) [u Sh])) ; [j a g m u Sh]
     (setq wtok (declension-join ptok1 [u Sh])) ; [ch a k r u Sh] [v i d u Sh]
     )
    )
   ; 3
   (cond 
    ((equal gender 'M)
     (let (1S 8S )
      (setq 1S (vconcat ptok1 [v aa n]))
      (setq 8S (vconcat ptok1 [v a n]))
;     (setq nomsing stok)
      (setq base-toks (vector
       1S stok stok
       stok stok wtok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok mtok
       8S stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let ()
      (setq base-toks (vector
       mtok wtok stok
       mtok wtok stok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok mtok
       mtok wtok stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok wtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-an-ADJ (c p g f i &optional dbg)
 (let (f1)
  (setq f1 'an) ; f = an-ADJ
  (declension-general-an c p g f1 i dbg)
 )
)
(defun declension-general-an
  (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-an : %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
; note: citation-sym IS used, praatipadikas is not used
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1 stok1)
  (setq procname (format "declension-general-%s" form))
  (when nil
   (fol-msg (format "%s: %s %s %s %s\n"
		   procname citation-sym praatipadikas gender form))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   citation-sym
   (if (symbolp citation-sym)
    t
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname citation-sym))
     nil ; so 'and' fails
    )
   )
   (setq ptok ; raajan vartman
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
   (< 2 (length ptok))
   (equal (substring ptok  -2) [a n])
   

   (setq ptok1 (substring ptok 0 -2)) ; [r aa j] [v a r t m]
   (setq stok (vconcat ptok1 [aa n])) ; [r aa j aa n] [v a r t m aa n]
   (setq stok1 stok)
   (progn
    ; Kale #111.
    (when (or (member citation-sym '(puuShan aryaman))
	      (equal (substring ptok -3) [h a n])
	  )
     (setq stok1 (vconcat ptok1 [a n]))
    )
    t ; for 'and'
   )
   (setq mtok (vconcat ptok1 [a])) ; [r aa j a] [v a r t m a]
   (setq wtok1 ptok) ;  [r aa j a n] [v a r t m a n]
   ; the usual weak stem drops the 'a' in 'an'
   ; In the masculine, sandhi may occur when
   ; the remaining dental 'n' combines with what precedes it, e.g.,
   ; [r aa j] + [n] -> [r aa j ~n]
   ; In the neuter, the [n] is joined without Sandhi. 
   ; Reason (03-16-03): Formerly, Sandhi was also applied in the neuter
   ; and in all regular neuter nouns in 'an', this means joining [... m] with
   ; [n] and there are no sandhi changes applicable.
   ; However, (Antoine, Vol II Chapter 5) two irregular neuter nouns
   ; (asthi and sakthi) in cases 3-7 before vowel terminations are
   ; declined as if they were 'asthan' and 'sakthan', and
   ; the sandhi combination [a s th] [n] is [a s dh n] , and
   ; for [s a k th] [n] is [s a k dh n]; However, this is incorrect 
   ; according to Antoine and Kale (Section 126).
   ; The similarly irregular 'akShi', which is declined before
   ; vowel endings like 'akShan', is also handled correctly as follows:
   ; The combination [a k Sh n] is joined to (say for case 3S) the 
   ; ending [aa], by the routine 'declension-general-1', which converts
   ; [a k Sh n aa] to the correct [a k Sh N aa]
   (setq wtok (or 
    (and (equal (substring ptok -3) [h a n])
	 (vconcat (substring ptok1 0 -1) [gh n])) ; Kale p. 76 example
    (and (equal gender 'M) (solution (sandhi-pair ptok1 [n] nil 'join)))
    (vconcat ptok1 [n])
   ))
   ; stems ending in 'man' and 'van' preceded by a consonant always
   ; form their weak stem in 'an' and not in 'n'
   (if (and
     (< 4 (length ptok))
     (member (substring ptok  -3) '([m a n] [v a n]))
     (consonant-P (elt (substring ptok -4 -3) 0))
    )
    (setq wtok wtok1)
    t ; (so encompassing 'and' does not fail)
   )
   ; 3
   (cond 
    ((equal gender 'M)
     (let (1S 8S 7S)
      (setq 1S ptok1) ; [r aa j] + ending [aa] -> [r aa j aa]
      (setq 8S ptok) ; [r aa j a n] + ending [] -> [r aa j a n]
      (if (equal wtok wtok1)
	(setq 7S wtok)
        (setq 7S (list wtok wtok1))
      )
      (setq base-toks (vector
       1S stok1 stok1
       stok1 stok1 wtok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       7S wtok mtok
       8S stok1 stok1
      ))
     )
    )
    ((equal gender 'N)
     (let (7S)
      (if (equal wtok wtok1)
	(setq 7S wtok)
        (setq 7S (list wtok wtok1))
      )
      (setq base-toks (vector
       mtok 7S stok
       mtok 7S stok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       7S wtok mtok
       mtok 7S stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok wtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok val)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
     (setq val
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
      )
     )
;       (fol-msg (format "%s %s %s %s -> %s\n" i sup irreg base-tok val))
     (aset ans i val)
     (setq i (1+ i))
    )
    t ; so and succeeds
   )
  )
  ans
 )
)
(defun declension-general-ach-ADJ
  (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-ach-ADJ : %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
; note: citation-sym IS used, praatipadikas is not used
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1)
  (setq procname (format "declension-general-%s" form))
  (when nil
   (fol-msg (format "%s: %s %s %s %s\n"
		   procname citation-sym praatipadikas gender form))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   citation-sym
   (if (symbolp citation-sym)
    t
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname citation-sym))
     nil ; so 'and' fails
    )
   )
   (setq ptok ; pratyach  anvach udach
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
   (<= 2 (length ptok)) ; 12-29-03
   (equal (substring ptok  -2) [a ch])
   (setq ptok1 (substring ptok 0 -2)) ; [p r a t y] [a n v] [u d]
   (setq stok (vconcat ptok1 [a ~n ch])) ; [p r a t y a ~n ch] etc
   ; note: Antoine says middle ends in [a ch]; these are endings
   ; preceding consonants; Kale asserts they are nouns with 1 stem
   ; ending in 'ch' in these spots, and this means we should change
   ; 'ch' to 'g' here.
   (setq mtok (vconcat ptok1 [a g])) ; [p r a t y a g] etc
   (cond
    ((equal citation-sym 'ach) ; 12-29-03
     (setq wtok ptok)
    )
    ((equal citation-sym 'tiryach)
     (setq wtok (vconcat (substring ptok1 0 -1) [a sh ch]))) ; [t i r a sh ch]
    ((equal (substring ptok1 -1) [v])
     (setq wtok (vconcat (substring ptok1 0 -1) [uu ch]))) ; [a n uu ch]
    ((equal (substring ptok1 -1) [y])
     (setq wtok (vconcat (substring ptok1 0 -1) [ii ch]))) ; [p r a t ii ch]
    (t
     (setq wtok (vconcat ptok1 [ii ch]))  ; [u d ii ch]
    )
   )
   ; 3
   (cond 
    ((equal gender 'M)
     (let (1S 8S 7P)
      (setq 1S (vconcat ptok1 [a])) ; [p r a t y a] + [~N]
      (setq 8S 1S)
      ; [p r a t y a k] + [s u] -> [p r a t y a k Sh u]
      (setq 7P (vconcat ptok1 [a k])) 
      (setq base-toks (vector
       1S stok stok
       stok stok wtok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok 7P
       8S stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let (1S 7P)
      (setq 1S (vconcat ptok1 [a])) ; [p r a t y a] + [k]
      (setq 7P (vconcat ptok1 [a k])) 
      (setq base-toks (vector
       1S wtok stok
       1S wtok stok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok 7P
       1S wtok stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok wtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-aach-ADJ
  (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-aach-ADJ : %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
; note: citation-sym IS used, praatipadikas is not used
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1)
  (setq procname (format "declension-general-%s" form))
;  (fol-msg (format "%s: %s %s %s %s\n"
;		   procname citation-sym praatipadikas gender form))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   citation-sym
   (if (symbolp citation-sym)
    t
    (progn
     (fol-msg (format "(%s) symbol required: %s\n" procname citation-sym))
     nil ; so 'and' fails
    )
   )
   (setq ptok ; [ p r aa ch]
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
   (< 2 (length ptok))
   (equal (substring ptok  -2) [aa ch])
   (setq ptok1 (substring ptok 0 -1)) ; [p r aa]
   (setq stok (vconcat ptok1 [~n ch])) ; [p r aa ~n ch]
   (setq wtok (vconcat ptok1 [ch])) ; [p r aa ch]
   ; note: Antoine says middle  = weak, thus ends in [ch]; these are endings
   ; preceding consonants; Kale asserts they are nouns with 1 stem
   ; ending in 'ch' in these spots, and this means we should change
   ; 'ch' to 'g' here.
   (setq mtok (vconcat ptok1 [g]))
   ; 3
   (cond 
    ((equal gender 'M)
     (let (1S 8S 7P)
      (setq 1S ptok1) ; [p r aa] + [~N]
      (setq 8S 1S)
      (setq 7P (vconcat ptok1 [k])) ; [p r aa k] + [s u] -> [p r aa k Sh u]
      (setq base-toks (vector
       1S stok stok
       stok stok wtok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok 7P
       8S stok stok
      ))
     )
    )
    ((equal gender 'N)
     (let (1S 7P)
      (setq 1S ptok1) ; [p r aa] + [k]
      (setq 7P (vconcat ptok1 [k])) 
      (setq base-toks (vector
       1S wtok stok
       1S wtok stok
       wtok mtok mtok
       wtok mtok mtok
       wtok mtok mtok
       wtok wtok wtok
       wtok wtok 7P
       1S wtok stok
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq base-tok wtok)
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-M-uu
  (citation-sym praatipadikas gender form irregs &optional dbg)
 (when dbg
  (fol-msg (format "declension-general-M-uu : %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
; note: citation-sym IS  used 
; praatipadikas is NOT used
; used for both form 'ii1' and form 'uu1'
; used for both gender=M/F
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1
	citation-tok len last sym1 sym2)
  (setq procname (format "declension-general-M-uu"))
  (when nil;dbg
    (fol-msg (format "%s: %s %s %s %s %s\n" procname
		    citation-sym praatipadikas gender form irregs))
  )

  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq len (length citation-tok))
  (setq last (elt (substring citation-tok -1) 0))
  (setq praatipadikas
     (subanta-base citation-sym gender form))
  (if (and (not (listp praatipadikas)) (symbolp praatipadikas))
    (setq praatipadikas (list praatipadikas praatipadikas))
  )
  (if (equal (length praatipadikas) 1)
   (setq praatipadikas (append praatipadikas praatipadikas))
  )
  (when (equal (length praatipadikas) 2)
   (setq sym1 (elt praatipadikas 0))
   (setq sym2 (elt praatipadikas 1))
   (setq sym2 (solution (sym-delete-last sym2)))
   (setq praatipadikas (list sym1 sym2))
  )
  (when nil;dbg
   (fol-msg (format "%s: %s %s %s %s %s\n" procname
		    citation-sym praatipadikas gender form irregs))
  )
  (when nil ;dbg
   (let (praatipadikas1)
    (setq praatipadikas1
	  (declension-general-M-uu-helper citation-sym gender form))
    (fol-msg (format "%s: %s %s\n" procname
		    citation-sym praatipadikas1 ))
   )
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   praatipadikas ; should not be nil
   (listp praatipadikas)
   (equal (length praatipadikas) 2)
   (setq ptok ; [s e n aa n]
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
   (if (equal last 'ii)
    (setq wtok (vconcat ptok [ii])) ; [s e n aa n ii] (for consonants)
    (setq wtok (vconcat ptok [uu])) ; 
   )
   (setq stok ; [s e n aa n y] (for vowels)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
   (progn
    (when (equal citation-sym 'khalapuu)
     (setq wtok ptok)
     (setq stok ptok)
    )
    t
   )
   ; 3
   (cond
    ((equal gender 'M)
     (let ( )
      (setq base-toks (vector
       wtok stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       wtok stok stok
      ))
     )
    )
    ((equal gender 'F)
     (let ( )
      (setq base-toks (vector
       wtok stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       wtok stok stok
      ))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
;   (progn (fol-msg (format "chk-ii: %s %s %s\n" ptok wtok stok)) t)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (when (and (equal i 18) (equal last 'ii)) ; locative singular
      (cond
       ((and (equal gender 'M)
	     (equal (substring citation-tok -2) [n ii])
	); Kale p.45, footnote
	(setq sup (vconcat (substring sup 0 -1) [aa m]))
       )
      )
     )
;      (if (and 
; 	      (equal last 'ii)
; ;	      (equal gender 'M)
; 	      (not (equal citation-sym 'nii)) 
; 	      (equal (substring stok -2) [i y]) ; sudhiiH
;          )
;       (setq sup [i])
;       )
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun declension-general-M-uu-helper
  (citation-sym gender form)
  ; Kale #77, p. 43. Root nouns in 'I' or 'U' m.f.n.
  ; (a) The ending 'i' or 'u', short or long, of nouns derived from
  ;    roots with the affix 'kvip' (o) and of the noun 'bhuu', is
  ;    changed to 'iy' or 'uv' before the vowel terminations (Pan. VI.4.77).
  ;    The feminine nouns of this description are optionally declined
  ;    like 'nadii' in the Da. Ab. Gen. and Loc. singulars, and Gen. plur.
  ; (b) BUT, if the ending 'i' or 'u' of a many-voweled noun having a
  ;    root at the end
  ;    - be not preceded by a radical conjunct consonant, or
  ;    - the root noun is preceded by 
  ;      -  a preposition termed 'gati' (i.e., as added to the root)
  ;      -  or a word governed by the root
  ;    then the ending 'i' or 'ii' is changed to 'y', and
  ;         the ending 'u' or 'uu' is changed to 'v'.
  ;   However, this does not apply to the noun 'sudhii' or
  ;     to nouns ending in 'bhuu'.

 (let (procname tok1 sym1 sym2 tok2 parts types len last tmp citation-tok
		praatipadikas)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq tmp (word-parts citation-tok))
  (setq parts (elt tmp 0))
  (setq types (elt tmp 1))
  (when nil
   (fol-msg (format "%s: %s %s\n" procname parts types))
  )
  (setq procname (format "declension-general-M-uu"))
  
  (setq len (length citation-tok))
  (setq last (elt (substring citation-tok -1) 0))

  (setq tok1 (substring citation-tok 0 -1)) ; remove [ii] or [uu]
  (setq sym1 (sym-without-space tok1))
  (cond
   ((equal last 'ii)
    (setq form 'ii)
    (setq praatipadikas sym1)
    (cond
     ((equal citation-sym 'sudhii) 
      (setq tok2 (vconcat tok1 [i y]))
      (setq sym2 (sym-without-space tok2))
      (setq praatipadikas (list sym1 sym2))
     )
     ((and (<= 3 (length types))
	   (member (substring citation-tok -3)
		      '([k r ii]))
	   )
       ; many-voweled noun whose root has conjunct consonant
      (setq form 'ii1)
      (setq tok2 (vconcat tok1 [i]))
      (setq sym2 (sym-without-space tok2))
      (setq praatipadikas (list sym1 sym2))
     )
     ((<= 3 (length types)) ; many-voweled noun 
      (setq form 'ii1)
      )
     )
    )
   ((equal last 'uu)
    (setq form 'uu)
    (setq praatipadikas sym1)
    (cond
     ((equal (substring citation-tok -2) [bh uu])
      )
     ((<= 3 (length types)) ; many-voweled noun (e.g. there is pfx
      (cond
       ((equal (substring citation-tok -3) [b r uu])
        ; root has conj cons
	(setq form 'uu1)
	(setq tok2 (vconcat tok1 [u]))
	(setq sym2 (sym-without-space tok2))
	(setq praatipadikas (list sym1 sym2))
       )
       (t ; root does not have conjunct consonant.
        (setq form 'uu1)
       )
      )
     )
    )
   )
  )
  praatipadikas
 )
)
(defun declension-general-1 (base-tok sup)
 (cond 
  ((listp sup)
   (mapcar (lambda (x) (declension-join base-tok x)) sup))
  ((listp base-tok)
   (mapcar (lambda (x) (declension-join x sup)) base-tok))
  (t (declension-join base-tok sup))
 )
)
(defun declension-join (base-tok sup)
 (let (ans len)
;  (fol-msg (format "base-tok=%s, sup=%s\n" base-tok sup))
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq ans (cond
   ((solution (sandhi-pair base-tok sup 'internal 'join)))
   ((solution (sandhi-pair base-tok sup nil 'join)))
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
   ((arrayp base-tok)  (vconcat base-tok sup))
   (t
    (fol-msg (format "declension-general-1 error. wrong types: %s %s\n"
		     base-tok sup))
    nil
   )
  ))
  (sandhi-pair-skiprefs-set nil)
  ; change 4-17-04. To (hopefully) inhibit unwanted 'n-N' changes.
  ; For instance, 'raghunandanaH' should not be changed to
  ; 'raghuNandanaH'; this is because of some inhibition of application
  ; of this sandhi rule across compound boundaries - the details are
  ; not clear but P. Scharf asserts.
  (setq len (length base-tok))
  (cond
   ((and (<= 7 len) (equal (substring base-tok -7) [sh a t r u gh n]))
    (sandhi-n-N-ifirst-set len)
   )
   (t  (sandhi-n-N-ifirst-set (1- len)))
  )
  (when nil; dbg
   (fol-msg (format "chkx: %s %s -> " base-tok ans))
  )
  (setq ans (or (sandhi-single ans) ans))
  (when nil ; dbg
   (fol-msg (format "%s\n" ans))
  )
  (sandhi-n-N-ifirst-set nil)
  ans
 )
)
(defun declension-pres-part (praatipadikas gender class pada)
 (if (equal pada 'P)
  (declension-pres-part-P praatipadikas gender class)
  (declension-pres-part-A praatipadikas gender class)
 )
)
(defun declension-general-at-ADJ 
 (citation-sym praatipadikas gender form irregs)
 (declension-pres-part-P praatipadikas gender nil)
)
(defun declension-pres-part-P (praatipadikas gender &optional class)
 ; NOTE: 'class' unused
 ; praatipadikas = (weak strong femtype) (nayat nayant S)
 ; femtype = S (use strong base)
 ;         = W (use weak base)
 ;         = W1 (use weak base)
 ;         = SW (use strong or weak base optionally)
 ; femtype also comes into play for the dual neuter
 (let (procname ans sups base-toks n praatipadika-tok wtok stok femtype alt)
  (setq procname (format "declension-pres-part-P"))
  (setq wtok (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
  (setq stok (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
  (setq femtype (elt praatipadikas 2))
  (setq alt (cond
       ((equal femtype 'S) stok)
       ((equal femtype 'W) wtok)
       ((equal femtype 'W1) wtok)
       ((equal femtype 'SW) (list wtok stok))
       (t ; should not happen
	(fol-msg (format "Error (%s): %s\n" procname praatipadikas))
	wtok
       )
   ))   
   (cond 
    ((equal gender 'M)
     (setq sups (sup-get gender 'normal)) ; an array
      (setq base-toks (vector
       stok stok stok
       stok stok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       stok stok stok
      )
     )
    )
    ((equal gender 'N)
     (let (stok1)
      (setq sups (sup-get gender 'normal)) ; an array
      (if (equal femtype 'W1)
       (setq stok1 (list stok wtok)) ; class 3
       (setq stok1 stok)
      )
      (setq base-toks (vector
       wtok alt stok1
       wtok alt stok1
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok wtok wtok
       wtok alt stok1
      ))
     )
    )
    ((equal gender 'F)
     (let (base-tok)
      (setq sups (sup-get gender 'ii)) ; an array
      (setq base-tok alt)
      (setq n (length sups))
      (setq base-toks (make-vector n base-tok))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   ; step 2 join base to endings
   (let (n i sup irreg base-tok)
    (setq n (length sups))
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
;     (setq irreg (elt irregs i))
     (setq irreg nil)
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
      )
     )
     (setq i (1+ i))
    )
    t ; so and succeeds
   )
  ans
 )
)
(defun declension-pres-part-A (praatipadikas gender &optional class)
 ; praatipadikas = (base) (vartamaan)
 ; for gender=M, the sups are sup-M-a
 ; for gender=N, the sups are sup-N-a
 ; for gender=F, the sups are sup-F-aa
 (let (procname praatipadika irregs ans)
  (setq procname (format "declension-pres-part-A"))
  (setq praatipadika (elt praatipadikas 0))
  (setq irregs nil)
  (setq ans 
   (cond 
    ((equal gender 'M) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'N) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'F) (declension-general praatipadika gender 'aa irregs))
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
  )
  ans
 )
)
(defun declension-perf-part-passive (praatipadikas gender)
 ; past partiticple passive (participle based on 'kta')
 ;praatipadikas = (base) e.g. (gat)  (for 'gam')
 ; for gender=M, the sups are sup-M-a
 ; for gender=N, the sups are sup-N-a
 ; for gender=F, the sups are sup-F-aa
 (let (procname praatipadika thisans ans irregs)
  (setq procname (format "declension-perf-part-passive"))
  (setq praatipadika (elt praatipadikas 0))
  (setq ans (cond 
    ((equal gender 'M) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'N) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'F) (declension-general praatipadika gender 'aa irregs))
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   ))
  ans
 )
)
(defun declension-pot-part-passive (praatipadikas gender)
 ; functionally idential to 'declension-perf-part-passive'
 ;praatipadikas = (base) e.g. (gantavy)  (for 'gam')
 ; for gender=M, the sups are sup-M-a
 ; for gender=N, the sups are sup-N-a
 ; for gender=F, the sups are sup-F-aa
 (let (procname praatipadika thisans ans irregs)
  (setq procname (format "declension-pot-part-passive"))
  
  (setq praatipadika (elt praatipadikas 0))
  (setq ans (cond 
    ((equal gender 'M) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'N) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'F) (declension-general praatipadika gender 'aa irregs))
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   ))
  ans
 )
)
(defun declension-perf-part-active (praatipadikas gender)
 ; past participle active (active participle based on 'kta')
 ;praatipadikas = (base) e.g. (gat)  (for 'gam')
 ; gat -> gatavat  decline like dhiimat
 ; Use declension-general-mat, which expects a base like 'gatav'
 (let (procname praatipadika thisans ans irregs)
  (setq procname (format "declension-perf-part-passive"))
  (let (tok)
   (setq tok
     (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))) )
   (setq praatipadika (sym-without-space (vconcat tok [a v])))
  )
;  (fol-msg (format "(%s) praatipadika=%s\n" praatipadika))
  (setq ans (declension-general-mat nil praatipadika gender 'mat irregs))
  ans
 )
)
(defun declension-rperf-part (pre-praatipadikas gender pada)
 ;reduplicative perfect participle (active)
 ; pre-praatipadikas is assumed to be a symbol
 (let (citation-sym ans)
   (setq citation-sym pre-praatipadikas) ; a symbol
   (cond
    ((equal pada 'P)
     (setq citation-sym (sym-concat citation-sym 'as))
     (setq ans (declension-general-vas-ADJ citation-sym nil gender))
    )
    ((equal pada 'A)
;     (fol-msg (format "chk: %s %s\n"  citation-sym gender))
     (setq ans 
      (cond
       ((equal gender 'M) (declension-general citation-sym gender 'a nil))
       ((equal gender 'N) (declension-general citation-sym gender 'a nil))
       ((equal gender 'F) (declension-general citation-sym gender 'aa nil))
      )
     )
    )
   )
 ans
 )
)
(defun declension-perperf-part (praatipadikas gender pada)
 ; periphrastic-perfect participle
 ; praatipadikas is a symbol
 (let (ans citation-sym pada1)
   (setq citation-sym praatipadikas) 
   (let (tok)
    (setq tok (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
    (if (equal (substring tok -1) [v])
     (setq pada1 'P)
     (setq pada1 'A)
    )
   )
   (cond
    ((equal pada1 'P)
     (setq citation-sym (sym-concat citation-sym 'as))
     (setq ans (declension-general-vas-ADJ citation-sym nil gender))
    )
    ((equal pada1 'A)
;     (fol-msg (format "chk: %s %s\n"  citation-sym gender))
     (setq ans 
      (cond
       ((equal gender 'M) (declension-general citation-sym gender 'a nil))
       ((equal gender 'N) (declension-general citation-sym gender 'a nil))
       ((equal gender 'F) (declension-general citation-sym gender 'aa nil))
      )
     )
    )
   )
 ans
 )
)
(defun declension-fut-part-active (pre-praatipadikas gender pada)
 (cond
  ((equal pada 'P)
   (declension-fut-part-active-P pre-praatipadikas gender)
  )
  ((equal pada 'A)
   (declension-fut-part-active-A pre-praatipadikas gender)
  )
 )
)
(defun declension-fut-part-active-P (pre-praatipadikas gender)
; future participle (active) parasmaipada
; 'pre-praatipadikas' is a list of symbols,
; each of which is used, with the gender, to create
; a declension table.
; The declension tables so formed are joined, element-wise,
; to form the answer (an array)
; Each symbol ends in 'y'; 
;  to get the decelension table in a particular gender for
;  the future participle from one of these symbols, say x,  ending in 'y':
;     (say, x = 'gamiShy)
;   (a) (setq p (pres-part-praatipadikas (sym-concat x 'ante) 6 'P x))
;     (p = '(gamiShyat gamiShyant SW))
;   (b) (setq dtab (declension-pres-part-P p gender)), e.g.
;      [gamiShyan gamiShyantau gamiShyantaH 
;       gamiShyantam gamiShyantau gamiShyataH 
;      ...
;      ]
 (let (ans dtab p x)
  (setq pre-praatipadikas (listify pre-praatipadikas))
  (while pre-praatipadikas
   (setq x (car pre-praatipadikas))
   (setq pre-praatipadikas (cdr pre-praatipadikas))
   (setq p (pres-part-praatipadikas (sym-concat x 'ante) 6 'P x))
   (setq dtab (declension-pres-part-P p gender))
   (when dtab
    (if ans
     (setq ans (join-arrays ans dtab))
     (setq ans dtab)
    )
   )
  )
  ans
 )
)
(defun declension-fut-part-active-A (praatipadikas gender)
; future participle (active), Atmanepada
; 'praatipadikas' is a list of symbols,
; each of which is used, with the gender, to create
; a declension table.
; Each symbol ends in 'aan' (or 'aaN'), the form
;   needed for the declension ending in 'a'.
; The declension tables so formed are joined, element-wise,
; to form the answer (an array)
 (let (ans dtab praatipadika irregs)
  (setq praatipadikas (listify praatipadikas))
  (while praatipadikas
   (setq praatipadika (car praatipadikas))
   (setq praatipadikas (cdr praatipadikas))
   (setq dtab (cond 
    ((equal gender 'M) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'N) (declension-general praatipadika gender 'a irregs))
    ((equal gender 'F) (declension-general praatipadika gender 'aa irregs))
    (t
;     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   ))
   (when dtab
    (if ans
     (setq ans (join-arrays ans dtab))
     (setq ans dtab)
    )
   )
  )
  ans
 )
)
(defun declension-fut-part-passive (praatipadikas gender)
; future participle (passive)
; The logic is exactly the same as for the active atmanepada.
 (declension-fut-part-active-A praatipadikas gender)
)
(defun declension-adj(citation-sym praatipadikas gender form-sym)
 ;praatipadikas = (base) e.g. (diirgh)  (for 'diirgha')
 ; the sups are sup-gender-form-ADJ (e.g., sup-M-a-ADJ)
;  (fol-msg (format "declension-adj: %s %s %s %s\n" 
;   citation-sym praatipadikas gender form-sym))
 (let (praatipadika thisans ans irregs supform)
;  (setq praatipadika (elt praatipadikas 0))
; 6-22-03: next is same as '(elt praatipadikas 0)' when
; praatipadikas is a singleton
  (setq praatipadika (solution praatipadikas))
  (setq supform (sym-without-space (vector form-sym '-ADJ)))
  (setq ans (cond
   ((member supform declension-cons-forms)
;    (setq citation-sym nil)
    (declension-general-cons citation-sym praatipadika gender supform irregs)
   )
   (t (declension-general praatipadika gender supform irregs))
  ))
  ans
 )
)
(defun declension-adj-new (citation-sym praatipadikas gender form-sym)
 ;praatipadikas = 
 ; the sups are sup-gender-form-PRON (e.g., sup-M-a-ADJ)
 (let (thisans ans supform citations citation )
  (setq supform (sym-without-space (vector form-sym '-ADJ)))
  (setq citations (declension-citation citation-sym))
  (while citations
   (setq citation (car citations))
   (setq citations (cdr citations))
   (when (and (equal (elt citation 0) gender)
	      (equal (elt citation 1) supform))
    (setq thisans (elt citation 2)) ; declension array
    (setq ans (append-if-new ans thisans))
   )
  )
  (solution ans)
 )
)
(defun declension-pron (citation-sym praatipadikas gender form-sym)
 ;praatipadikas = 
 ; the sups are sup-gender-form-PRON (e.g., sup-M-a-PRON)
 (when nil
  (fol-msg (format "%s: %s %s %s %s\n"
	    'declension-pron citation-sym praatipadikas gender form-sym))
 )
 (if (equal citation-sym 'bhavat)
  ; pronoun 'bhavat' is declined like adjective 'bhagavat'
  ; It has irregular form 'bhoH' in 8s (MW- bhos)
  (let (irreg)
   (when (equal gender 'M)
    (setq irreg (make-vector 24 nil))
    (aset irreg 21 '(bhavan bhoH))
   )
   (declension-general-vat citation-sym praatipadikas gender 'vat irreg)
  )
 (let (thisans ans supform citations citation )
  (setq supform (sym-without-space (vector form-sym '-PRON)))
  (setq citations (declension-citation citation-sym))
  (while citations
   (setq citation (car citations))
   (setq citations (cdr citations))
   (when (and (equal (elt citation 0) gender)
	      (equal (elt citation 1) supform))
    (setq thisans (elt citation 2)) ; declension array
    (setq ans (append-if-new ans thisans))
   )
  )
  ans
 )
 )
)

(defun subanta-base-1cons (citation-sym gender)
; 11-21-03. 
; This function is expanded to do what was formerly done in the
; 'subanta' input file.
 (let (citation-tok last tok2 tok1 ans)
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0))
  (setq tok1 (substring citation-tok 0 -1)) ; drop last char
  (setq tok2 citation-tok)
  (cond
   ((equal last 'ch)
    ; Antoine1: 77(1) Nouns with stems ending in palatals
    ;  Final 'ch' is changed to 'k' in 1S and 7P
    ;                        to 'g' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat tok1 [k]))
   )
   ((equal last 'j)
    ;  Final 'j' is changed to 'k' or 'T' in 1S and 7P
    ;                       to 'g' or 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    (cond
     ((member citation-sym '(vaNij bhiShaj Ritvij sraj asRij))
      (setq tok2 (vconcat tok1 [k]))
     )
     ((member citation-sym '(samraaj parivraaj))
      (setq tok2 (vconcat tok1 [T]))
     )
    )
   )
   ((equal last 'sh)
    ;  Final 'sh' is changed to 'k' or 'T' in 1S and 7P
    ;                        to 'g' or 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    (cond
     ((member citation-sym '(dish dRish))
      (setq tok2 (vconcat tok1 [k]))
     )
     ((member citation-sym '(vish))
      (setq tok2 (vconcat tok1 [T]))
     )
    )
   )
   ((equal last 'Sh)
    ; 77(2) Nouns with stems ending in cerebrals
    ;  Final 'Sh' is changed to 'T' in 1S and 7P
    ;                        to 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat tok1 [T]))
   )
   ((member last '(t th d dh))
    ; 77(3) Nouns with stems ending in dentals
    ;  Final dental other than 'n' is changed to 't' in 1S and 7P
    ;                       to 'd' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat tok1 [t]))
   )
   ((member last '(p ph b bh))
    ; 77(4) Nouns with stems ending in labials
    ;  Final labial is changed to 'p' in 1S and 7P
    ;                       to 'b' before 'bhyaam', 'bhiH' and 'bhyaH'
    (setq tok2 (vconcat tok1 [p]))
   )
   ((equal last 'r)
    ; 77(5) Nouns with stems ending in 'r'
    ; Final 'r' is changed to 'H' (visarga) in 1S.
    ; A short vowel preceding 'r' is lengthened in 1S and
    ;  before 'bhyaam', 'bhiH' and 'bhyaH'
    (let (v1 v2)
     (setq v1 (elt (substring tok1 -1) 0)) ; vowel
     (setq v2 (lengthen-vowel v1))
     (setq tok1 (vconcat (substring tok1 0 -1) (vector v2)))
     (setq tok2 (vconcat tok1 [H]))
    )
   )
   ((equal last 's)
    ; 77(6) Nouns with stems ending in 's'
    ; Final 's' becomes visarga in 1S and before 'bhyaam', 'bhiH' and 'bhyaH',
    ; and that 'H' follows visarga sandhi rules in combining with the
    ; terminations,
    ; except that final 's' remains before terminations beginning with vowels,
    ; being changed to 'Sh' if preceding vowel is other than 'a' or 'aa'.
    ; M and F nouns in 'as' lengthen the 'a' in 1S.
    (let (v1 v2)
     (setq v1 (elt (substring tok1 -1) 0))
     (when (and (equal v1 'a) (member gender '(M F)))
      (setq v2 (lengthen-vowel v1))
      (setq tok1 (vconcat (substring tok1 0 -1) (vector v2)))
     )
     (setq tok2 (vconcat tok1 [H]))
    )
   )
   ((equal last 'h)
    ; 77(7) Nouns with stems ending in 'h'
    ; Usually, final 'h' becomes 'k' in 1S and 7P
    ;                   'g' before 'bhyaam', 'bhiH' and 'bhyaH'
    ; the appearance of 'dh' is explained by the following sandhi:
    ; When 'gh', 'dh', 'bh', or 'h' loses aspiration owing to combination
    ; with following consonants, the preceding consonant becomes aspirated,
    ; if possible
    ; In nouns ending with 'lih', final 'h' becomes 'T' in 1S and 7P,
    ; and final 'h' becomes 'D' before 'bhyaam', 'bhiH' and 'bhyaH'
    ; For the noun 'upaanah', the final 'h becomes 't' and 'd'
    (cond
     ((equal citation-sym 'kaamaduh)
      (setq tok2 [k aa m a dh u k])
     )
     ((equal citation-sym 'madhulih)
      (setq tok2 [m a dh u l i T])
     )
     ((equal citation-sym 'upaanah)
      (setq tok2 [u p aa n a t])
     )
    )
   )
   ((equal citation-sym 'div)
    ; 'div' f. (sky) : Antoine2-#80
    ; This is declined like a noun with 1 consonant, except 
    ; (a) 1S is 'dyauH'
    ; (a)' 8S is 'dyauH'
    ; (b) 2S is optionally 'dyaam'
    ; (c) before terminations beginning with a consonant it uses stem 'dyu'
    ;    (this is accomplished in algorithm, by providing two praatipadikas
    ;     ('div' and 'dyu')
    (setq tok2 [d y u])
   )
   (t
    (fol-msg (format "subanta-base-1cons: unknown situation: %s\n"
		     citation-sym))
    (setq tok2 (vconcat tok1 (vector last)))
   )
  )
  (setq ans (list citation-sym (sym-without-space tok2)))
  ans
 )
)
(defun adjust-praatipadikas (old form)
 (cond
 ((member form '(1cons))
  (let (ans praatipadika tok newtok sym last)
   (while old
    (setq praatipadika (car old))
    (setq old (cdr old))
    (setq tok (car (ITRANS-parse-words-1 (symbol-name praatipadika))))
    (setq last (elt (substring tok -1) 0))
    (if (consonant-P last)
     (cond
      ((and (equal form '1cons) (equal last 's))
       (setq newtok (substring tok 0 -2)) ;all but last two
      )
      ((equal form '2cons) ; ok for 'matup' 'vatup'
       (setq newtok (substring tok 0 -2)) ;all but last two
      )
      (t ; otherwise
       (setq newtok (substring tok 0 -1)) ;all but last
      )
     )
     (setq newtok tok)
    )
    (setq sym (sym-without-space newtok))
    (setq ans (append-if-new ans sym))
   )
   ans
  )
 )
 (t ; no change  (not a consonant)
  old 
 )
 )
)
(defun init-avyayapada (&optional OBARRAY)
 (if (not OBARRAY) (setq OBARRAY obarray))
 (mapatoms 'init-avyayapada-helper-1  OBARRAY)
 't
)
(defun init-avyayapada-helper-1 (sym)
; sets to nil the 'avyayapada property of
; the 'Sangram plist of sym
 (when (sanget sym 'avyayapada)
  (sanput sym 'avyayapada nil)
 )
)

(defun old-declension-general-1cons
 (citation-sym praatipadikas gender form irregs)
 ; Note of 01-05-04: praatipadikas not used. It is computed here.
 ; citation-sym IS used.
 (when nil
  (fol-msg (format "1cons: %s\n"
   (list citation-sym praatipadikas gender form irregs)))
 )
 (let (procname ans sups base-toks n praatipadika-toks citation-tok
       last vowel)
  (setq praatipadikas (subanta-base-1cons citation-sym gender))
  (setq procname "declension-general-1cons")
  (setq citation-tok (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq last (elt (substring citation-tok -1) 0)) ; last char
  (setq vowel (elt (substring citation-tok -2 -1) 0)) ; penultimate char
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   (if (symbolp praatipadikas)
    (setq praatipadikas (list praatipadikas praatipadikas))
    t ; so 'and' succeeds
   )
   ; 3
   (if (listp praatipadikas)
    t  ; so 'and' succeeds
    (progn
     (fol-msg (format "(%s) list required: %s\n" procname praatipadikas))
     nil
    )
   )
   ; 4
   (setq praatipadika-toks
    (mapcar
     (lambda (praatipadika)
      (car (ITRANS-parse-words-1 (symbol-name praatipadika)))
     )
     praatipadikas
   ))
   (cond
    ((not (equal (length praatipadika-toks) 2))
     (fol-msg (format "Error (%s) form=%s, bases = %s %s %s\n" procname form
			praatipadikas praatipadika-toks))
     nil
    )
    ((equal last 'r)
     (let (x y z v)
      (setq x (elt praatipadika-toks 0)) ; same as citation-tok
      (setq y (elt praatipadika-toks 1))
      (setq v (lengthen-vowel vowel))
      (setq z (vconcat (substring x 0 -2) (vector v last)))
      (setq base-toks (vector
       y x x
       x x x
       x z z
       x z z
       x z z
       x x x
       x x z
       y x x
      ))
     )
    )
    ((and (equal last 's)
	  (member gender '(M F))
	  (member vowel '(a aa))) ;12-29-03. Was '(a)'
     ; The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
     (let (x y z v)
      (setq x (elt praatipadika-toks 0)) ; same as citation-tok
      (setq y (elt praatipadika-toks 1))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq base-toks (vector
       y x x
       x x x
       x z z
       x z z
       x z z
       x x x
       x x z
       x x x
      ))
     )
    )
    ((and (equal last 's) (member gender '(N)))
     ; The neuter nouns in 'as', 'is', and 'us' lengthen the 'a', 'i',
     ; and 'u' and insert a nasal (anusvaara) in 1P 2P 8P
     ; The nouns in 'is' and 'us' become 'iSh' and 'uSh' before
     ; vowel endings (other than 1P 2P 8P)
     (let (x y z v w j last1)
      (setq x (elt praatipadika-toks 0)) ; same as citation-tok
      (setq y (elt praatipadika-toks 1))
      (setq v (lengthen-vowel vowel))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq last1
       (if (equal vowel 'a) last 'Sh)
      )
      (setq w (vconcat (substring x 0 -2) (vector v 'M last1)))
      (setq j (vconcat (substring x 0 -2) (vector vowel last1)))
      (setq base-toks (vector
       y j w
       y j w
       j z z
       j z z
       j z z
       j j j
       j j z
       x j w
      ))
     )
    )
    ;There do not seem to be many M/F nouns ending in 's' and preceded
    ; by a non-a.  The irregular noun 'dos' (m. arm) (Antoine2-#78) is
    ; declined in part like a noun with one stem.
    ((and (equal last 's) (vowel-P vowel))
     ; The masculine and feminine nouns in 'as' lengthen the 'a' in 1S.
     (let (x y z v w j last1)
      (setq x (elt praatipadika-toks 0)) ; same as citation-tok
      (setq y (elt praatipadika-toks 1))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq z (vconcat (substring x 0 -1) (vector 'H)))
      (setq last1
       (if (equal vowel 'a) last 'Sh)
      )
      (setq j (vconcat (substring x 0 -2) (vector vowel last1)))
      (setq base-toks (vector
       y j j
       j j j
       j z z
       j z z
       j z z
       j j j
       j j z
       x j j
      ))
     )
    )
    ((and (equal last 't) (equal gender 'N))
     ; Neuter nouns in the nom., acc., and voc. plural insert a
     ; nasal before the case-ending 'i'.
     (let (x y z v w j last1)
      (setq x (elt praatipadika-toks 0)) ; same as citation-tok
      (setq y (elt praatipadika-toks 1))
      
      (setq w (vconcat (substring x 0 -1) (vector 'n last)))
      (setq base-toks (vector
       y x w
       x x w
       x y y
       x y y
       x y y
       x x x
       x x y
       y x w
      ))
     )
    )
    ((and (equal (length praatipadika-toks) 2))
     (let (x y)
      (setq x (elt praatipadika-toks 0))
      (setq y (elt praatipadika-toks 1))
      (setq base-toks (vector
       y x x
       x x x
       x y y
       x y y
       x y y
       x x x
       x x y
       y x x
      ))
     )
    )
    (t (fol-msg (format "Error (%s) form=%s, bases = %s\n" procname form
			praatipadikas))
     nil
    )
   )
   (progn 
    (when nil ;dbg
     (fol-msg (format "1cons: base=%s\nsups=%s\n" base-toks sups))
    )
    t
   )
   (let (i sup irreg base-tok)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun old-declension-general-F-ii1
  (citation-sym praatipadikas gender form irregs)
; used for forms ii1 uu1, ii2 (Antoine-II. Sections 24,25,26)
; note: citation-sym IS not used 
; and praatipadikas IS assumed an array (e.g. '(dh dhiy) for 'dhii)
; 'dh' + 'ii' is used before consonant endings
; 'dhiy' is used before vowel endings (the vowel ending used varies)
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1)
  (setq procname (format "declension-general-F-ii1"))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   (< 0 n)
   ; 2
   praatipadikas ; should not be nil
   (listp praatipadikas)
   (equal (length praatipadikas) 2)
   (setq ptok ; [dh] (for dhii)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
   (if (equal form 'uu1)
    (setq wtok (vconcat ptok [uu])) ; [bh uu] (for consonant endings)
    (setq wtok (vconcat ptok [ii])) ; [dh ii] (for consonant endings)
   )
   (setq stok ; [dh i y] (for vowel endings)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
   ; 3
   (cond
    ((equal gender 'F)
     (let (doublebase case6P case2S case2P case8S)
      ;1st ending uses strong, 2nd uses weak (i=17)
      (setq doublebase (list stok wtok))
      (setq case6P (if (member form '(ii1 uu1)) doublebase wtok))
      (setq case2S (if (member form '(ii2)) doublebase stok))
      (setq case2P case2S)
      (setq case8S (if (member form '(ii2)) (substring stok 0 -1) wtok))
      (setq base-toks (vector
       wtok stok stok
       case2S stok case2P
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok case6P
       stok stok wtok
       case8S stok stok
      ))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
;     (fol-msg (format "base-toks=%s\n" base-toks))
;     (fol-msg (format "sups=%s\n" sups))
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;     (fol-msg (format "%s %s %s\n" i sup base-tok))
     (aset ans i
      (if irreg
       irreg
       (if (or (not (listp base-tok)) (not (listp sup)))
        (sym-without-space (declension-general-1 base-tok sup))
	(list
	 (solution (sym-without-space
	  (declension-general-1 (elt base-tok 0) (elt sup 0))))
	 (solution (sym-without-space
	  (declension-general-1 (elt base-tok 1) (elt sup 1))))
	)
       )
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

(defun old-declension-general-F-ii2
  (citation-sym praatipadikas gender form irregs)
; used for ii2 (Antoine-II. Sections 26) (strii)
; note: citation-sym IS used 
; praatipadikas is NOT used
; form is NOT used (set to 'ii2)
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1)
  (setq procname (format "declension-general-F-ii2"))
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   (< 0 n)
   ; 2
   praatipadikas ; should not be nil
   (listp praatipadikas)
   (equal (length praatipadikas) 2)
   (setq ptok ; [dh] (for dhii)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
   (if (equal form 'uu1)
    (setq wtok (vconcat ptok [uu])) ; [bh uu] (for consonant endings)
    (setq wtok (vconcat ptok [ii])) ; [dh ii] (for consonant endings)
   )
   (setq stok ; [dh i y] (for vowel endings)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
   ; 3
   (cond
    ((equal gender 'F)
     (let (doublebase case6P case2S case2P case8S)
      ;1st ending uses strong, 2nd uses weak (i=17)
      (setq doublebase (list stok wtok))
      (setq case6P (if (member form '(ii1 uu1)) doublebase wtok))
      (setq case2S (if (member form '(ii2)) doublebase stok))
      (setq case2P case2S)
      (setq case8S (if (member form '(ii2)) (substring stok 0 -1) wtok))
      (setq base-toks (vector
       wtok stok stok
       case2S stok case2P
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok case6P
       stok stok wtok
       case8S stok stok
      ))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
;     (fol-msg (format "base-toks=%s\n" base-toks))
;     (fol-msg (format "sups=%s\n" sups))
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;     (fol-msg (format "%s %s %s\n" i sup base-tok))
     (aset ans i
      (if irreg
       irreg
       (if (or (not (listp base-tok)) (not (listp sup)))
        (sym-without-space (declension-general-1 base-tok sup))
	(list
	 (solution (sym-without-space
	  (declension-general-1 (elt base-tok 0) (elt sup 0))))
	 (solution (sym-without-space
	  (declension-general-1 (elt base-tok 1) (elt sup 1))))
	)
       )
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
(defun old-declension-general-M-ii
  (citation-sym praatipadikas gender form irregs)
; note: citation-sym IS  used (e.g. 'sudhiiH' yields the cons base 'sudhii'
; and praatipadikas IS assumed an array '(senaan senaany), 
; 'senaan' + 'ii' is used before consonant endings
; 'senaany' is used before vowel endings
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1)
  (setq procname (format "declension-general-M-ii"))
  (when t ;dbg
   (fol-msg (format "%s: %s %s %s %s %s\n" procname
		    citation-sym praatipadikas gender form irregs))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   praatipadikas ; should not be nil
   (listp praatipadikas)
   (equal (length praatipadikas) 2)
   (setq ptok ; [s e n aa n]
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
   (setq wtok (vconcat ptok [ii])) ; [s e n aa n ii] (for consonants)

   (setq stok ; [s e n aa n y] (for vowels)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
   ; 3
   (cond
    ((equal gender 'M)
     (let ( )
      (setq base-toks (vector
       wtok stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       wtok stok stok
      ))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
;   (progn (fol-msg (format "chk-ii: %s %s %s\n" ptok wtok stok)) t)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (if (and (equal i 18); locative singular
	      (equal gender 'M)
	      (not (equal citation-sym 'nii)) ; Kale p.45, footnote
	      (equal (substring stok -2) [i y]) ; sudhiiH
         )
      (setq sup [i])
     )
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
(defun old-declension-general-M-uu
  (citation-sym praatipadikas gender form irregs)
; note: citation-sym IS  used 
; praatipadikas is NOT used
; used for both form 'ii1' and form 'uu1'
; used for both gender=M/F
 (let (procname ans sups base-toks n ptok wtok stok mtok ptok1 wtok1
	citation-tok parts types len last)
  (setq procname (format "declension-general-M-uu"))
  
  (setq citation-tok
	 (car (ITRANS-parse-words-1 (symbol-name citation-sym))))
  (setq len (length citation-tok))
  (setq last (elt (substring citation-tok -1) 0))
  (let (tmp)
   (setq tmp (word-parts citation-tok))
   (setq parts (elt tmp 0))
   (setq types (elt tmp 1))
   (when nil
    (fol-msg (format "%s: %s %s\n" procname parts types))
   )
  )
  ; Kale #77, p. 43. Root nouns in 'I' or 'U' m.f.n.
  ; (a) The ending 'i' or 'u', short or long, of nouns derived from
  ;    roots with the affix 'kvip' (o) and of the noun 'bhuu', is
  ;    changed to 'iy' or 'uv' before the vowel terminations (Pan. VI.4.77).
  ;    The feminine nouns of this description are optionally declined
  ;    like 'nadii' in the Da. Ab. Gen. and Loc. singulars, and Gen. plur.
  ; (b) BUT, if the ending 'i' or 'u' of a many-voweled noun having a
  ;    root at the end
  ;    - be not preceded by a radical conjunct consonant, or
  ;    - the root noun is preceded by 
  ;      -  a preposition termed 'gati' (i.e., as added to the root)
  ;      -  or a word governed by the root
  ;    then the ending 'i' or 'ii' is changed to 'y', and
  ;         the ending 'u' or 'uu' is changed to 'v'.
  ;   However, this does not apply to the noun 'sudhii' or
  ;     to nouns ending in 'bhuu'.

   (let (tok1 sym1 sym2 tok2 )
     (setq tok1 (substring citation-tok 0 -1)) ; remove [ii] or [uu]
     (setq sym1 (sym-without-space tok1))
     (cond
      ((equal last 'ii)
       (setq form 'ii)
       (setq praatipadikas sym1)
       (cond
	((equal citation-sym 'sudhii) 
	   (setq tok2 (vconcat tok1 [i y]))
	   (setq sym2 (sym-without-space tok2))
	   (setq praatipadikas (list sym1 sym2))
	)
	((and (<= 3 (length types))
	      (member (substring citation-tok -3)
			   '([k r ii]))
	 )
	 ; many-voweled noun whose root has conjunct consonant
	 (setq form 'ii1)
	 (setq tok2 (vconcat tok1 [i]))
	 (setq sym2 (sym-without-space tok2))
	 (setq praatipadikas (list sym1 sym2))
	)
	((<= 3 (length types)) ; many-voweled noun 
	 (setq form 'ii1)
        )
       )
      )
      ((equal last 'uu)
       (setq form 'uu)
       (setq praatipadikas sym1)
       (cond
	((equal (substring citation-tok -2) [bh uu])
	)
	((<= 3 (length types)) ; many-voweled noun (e.g. there is pfx
	 (cond
	  ((equal (substring citation-tok -3) [b r uu])
	   ; root has conj cons
	   (setq form 'uu1)
	   (setq tok2 (vconcat tok1 [u]))
	   (setq sym2 (sym-without-space tok2))
	   (setq praatipadikas (list sym1 sym2))
	  )
	  (t ; root does not have conjunct consonant.
	   (setq form 'uu1)
	  )
	 )
        )
       )
      )
     )
    )
   
  (when nil;dbg
   (fol-msg (format "%s: %s %s %s %s %s\n" procname
		    citation-sym praatipadikas gender form irregs))
  )
  (if (and (not (listp praatipadikas)) (symbolp praatipadikas))
    (setq praatipadikas (list praatipadikas praatipadikas))
  )
  (if (equal (length praatipadikas) 1)
   (setq praatipadikas (append praatipadikas praatipadikas))
  )
  (and
   ; 1
   (setq sups (sup-get gender form)) ; an array
   (setq n (length sups))
   ; 2
   praatipadikas ; should not be nil
   (listp praatipadikas)
   (equal (length praatipadikas) 2)
   (setq ptok ; [s e n aa n]
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 0)))))
   (if (equal last 'ii)
    (setq wtok (vconcat ptok [ii])) ; [s e n aa n ii] (for consonants)
    (setq wtok (vconcat ptok [uu])) ; 
   )
   (setq stok ; [s e n aa n y] (for vowels)
	 (car (ITRANS-parse-words-1 (symbol-name (elt praatipadikas 1)))))
   ; 3
   (cond
    ((equal gender 'M)
     (let ( )
      (setq base-toks (vector
       wtok stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       wtok stok stok
      ))
     )
    )
    ((equal gender 'F)
     (let ( )
      (setq base-toks (vector
       wtok stok stok
       stok stok stok
       stok wtok wtok
       stok wtok wtok
       stok wtok wtok
       stok stok stok
       stok stok wtok
       wtok stok stok
      ))
     )
    )
    (t
     (fol-msg (format "Error (%s) gender: %s\n" procname gender))
     nil
    )
   )
   
   (let (i sup irreg base-tok)
;   (progn (fol-msg (format "chk-ii: %s %s %s\n" ptok wtok stok)) t)
    (setq ans (make-vector n nil))
    (setq i 0)
    (while (< i n)
     (setq sup (elt sups i))
     (when (and (equal i 18) (equal last 'ii)) ; locative singular
      (cond
       ((and (equal gender 'M)
	     (equal (substring citation-tok -2) [n ii])
	); Kale p.45, footnote
	(setq sup (vconcat (substring sup 0 -1) [aa m]))
       )
      )
     )
;      (if (and 
; 	      (equal last 'ii)
; ;	      (equal gender 'M)
; 	      (not (equal citation-sym 'nii)) 
; 	      (equal (substring stok -2) [i y]) ; sudhiiH
;          )
;       (setq sup [i])
;       )
     (setq irreg (elt irregs i))
     (setq base-tok (elt base-toks i))
;       (fol-msg (format "%s %s %s %s\n" i sup irreg base-tok))
     (aset ans i
      (if irreg
       irreg
       (sym-without-space (declension-general-1 base-tok sup))
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
