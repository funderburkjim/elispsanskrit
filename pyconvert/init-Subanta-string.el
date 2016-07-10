; initialize the elispsanskrit system 
; evaluate the buffer.
; Then, in scratch,
; (convert-Subanta)
; Output is a multi-line python string.
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
   (progn ;  y assumed to be an array of ITRANS symbols
    (setq z (translate-ITRANS-SLP1 y)) ; array of symbols
    (condition-case err
     (mapconcat 'symbol-name z "")
     (error
      (format "ERROR: %s" z)
     )
    )
   )
  ); end if
 )
)
(defun convert-val (x)
 " x is an array of  24 elements y1 y2 ...yn , each y is 
   is an array of ITRANS character symbols
   Return a string representation (colon-separated)
   z1:z2:...:zn
   where each zj is a conversion of yj into an SLP1 string
 "
 (mapconcat 'convert-val1 x ":")
)

(defun convert-Subanta (&optional m)
 (let (prop val pyprop pyval p name n pyname)
  (setq name 'Subanta)
  (setq pyname "Subanta")
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
   (setq pyprop (pystring prop))
   (setq pyval (convert-val val))
   (insert (format "%s=%s\n" pyprop pyval))
   (setq n (1+ n))
  )
  (insert (format "'''\n"))
 )
)
