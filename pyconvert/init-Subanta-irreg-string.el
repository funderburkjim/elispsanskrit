; initialize elispsanskrit
; evaulate this buffer to define all the functions.
; Then, in scratch buffer, (convert-Subanta-irreg)


(defun convert-prop (x)
 "Assume x has form 'a-b'. Translate each of 'a' and 'b' from ITRANS to SLP1
  getting 'c' and 'd'
  Then return string 'c-d'
  This also works if there no '-'.
  This version also works if 'x' ends in a '-'.
 "
 (let (parts partsslp)
  (setq parts (split-string (format "%s" x) "-"))
  (if (string-match "-$" (format "%s" x))
   (setq partsslp (list (translate-string-ITRANS-SLP1 (nth 0 parts)) ""))
   (setq partsslp (translate-string-ITRANS-SLP1 parts))
  )
  (mapconcat (lambda (x) x) partsslp "-")
 )
)
(defun pystring (x)
 ;(format "'%s'" x)
 (format "%s" x)
)

(defun convert-val1 (y)
 " y is like [bh y aa m] (an array of ITRANS symbols
  which is converted to an SLP1 string 'ByAm'
  Sometimes there are alternates, so y may be a list '([i] [e])
  This is represented by i,e  (comma-separated), 
 "
 (let (z)
  (if (listp y)
   (progn
    (setq z (mapcar 'convert-val1 y)) ; w is a list of strings
    (mapconcat (lambda (x) x) z ",") ; join with comma
   )
   (progn ;  y assumed to be an ITRANS symbol
    (setq z (translate-ITRANS-SLP1 y)) ; array of symbols
    (pystring z)
   )
  ); end if
 )
)
(defun convert-val (x headword)
 " x is a property list. 
   The properties are declension category Elisp symbols (e.g. M-IRR-PRON)
   The values are declension arrays
   is an array of ITRANS character symbols
   Return a multiline string representation, one line per property:
   headword.property=<declension table>, a colon-separated string of 24 
   SLP1-coded declension table entries (use comma to separate optional forms)
 "
 (let (prop propval ans ans1 p v)
  (setq ans "")
  (while x
   (setq prop (car x))
   (setq x (cdr x))
   (setq propval (car x))
   (setq x (cdr x))
   (setq p (pystring prop))
   (setq v (mapconcat 'convert-val1 propval ":"))
   (setq ans1 (format "%s.%s=%s\n" pyprop p v))
   (setq ans (format "%s%s" ans ans1))
  )
  ans ; returned value
 )
)

(defun convert-Subanta-irreg (&optional m)
 (let (prop val pyprop pyval p name n pyname)
  (setq name 'Subanta-irreg)
  (setq pyname "Subanta_irreg")
  (if (not m)
   (setq m 1000000) ; a large number
  )
  ; initialize python multi-line string
  (insert (format "%s_string='''\n" pyname)) 
  (setq p (get name 'Sangram )) ; a property list
  (setq n 0)
  (while (and p (<= n m))
   (setq prop (car p))
   (setq p (cdr p))
   (setq val (car p))
   (setq p (cdr p))
   (setq pyprop prop)
   (setq pyval val)
   (setq pyprop (pystring (translate-ITRANS-SLP1 prop)))
   (setq pyval (convert-val val pyprop))
   ;(insert (format "%s=%s\n" pyprop pyval))
   (insert (format "%s" pyval))
   (setq n (1+ n))
  )
  (insert (format "'''\n"))
 )
)


(convert-Subanta-irreg)

