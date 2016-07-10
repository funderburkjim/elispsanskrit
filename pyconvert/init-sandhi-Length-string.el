; initialize elispsanskrit
; evaulate this buffer to define all the functions.
; Then, in scratch buffer, (convert-sandhi-Length)


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
 " y is like ([a aa] join svara Kale19 nil)
 "
 (let (first second third fourth fifth z)
  (setq first (mapcar 'translate-ITRANS-SLP1 (nth 0 y))) 
  (setq first (pystring (mapconcat 'symbol-name first "-"))) 
  (setq second (pystring (nth 1 y))) ; join
  (setq third (pystring (nth 2 y))) ; svara
  (setq fourth (pystring (nth 3 y)))
  (if (nth 4 y)
   (setq fifth (pystring (nth 4 y)) )
   (setq firth "")
  )
  (setq z (list first second third fourth fifth))
  (mapconcat (lambda (x) x) z ",")
 )
)
(defun convert-val (x)
 " x is a list of elements integers.
   Return a string representation (colon-separated)
   z1:z2:...:zn
   where each zj is a conversion of yj into comma-separated values
   a-A,join,svara,Kale19,nil
 "
 (if (not (eq (length x) 2))
  (message "Wrong length: %s" x)
 )
 (mapconcat (lambda (y) (format "%s" y)) x ",")
)
(defun convert-sandhi-Length (&optional m)
 (let (prop val pyprop pyval p name n pyname)
  (setq name 'Sandhi-Length)
  (setq pyname "Sandhi_Length")
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
   (setq pyprop (convert-prop prop))
   (setq pyval (convert-val val))
   ;(setq pyprop prop)
   ;(setq pyval val)
   (insert (format "%s=%s\n" pyprop pyval))
   (setq n (1+ n))
  )
  (insert (format "'''\n"))
 )
)
