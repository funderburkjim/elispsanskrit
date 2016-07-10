; initialize the elispsanskrit system 
; evaluate the buffer.
; Then, in scratch,
; (convert-sandhi)
; Output is a multi-line python string.

(defun convert-prop (x)
 "Assume x has form 'a-b'. Translate each of 'a' and 'b' from ITRANS to SLP1
  getting 'c' and 'd'
  Then return string 'c-d'
 "
 (let (parts partsslp)
  (setq parts (split-string (format "%s" x) "-"))
  (setq partsslp (translate-string-ITRANS-SLP1 parts))
  (mapconcat (lambda (x) x) partsslp "-")
 )
)
(defun pystring (x)
 ;(format "'%s'" x)
 (format "%s" x)
)
(defun unused-array-to-list (a)
 (let (b n m)
  (setq b nil)
  (setq n 0)
  (setq m (length a))
  (while (< n m)
   (setq b (append b (list (elt n a))))
   (setq n (1+ n))
  )
  b
 )
)
(defun convert-val1 (y)
 " y is like ([aa] [] join (type svara ref Kale19))
 "
 (let (first second third fourth z)
  (setq first (mapcar 'translate-ITRANS-SLP1 (nth 0 y))) ; a list of symbols
  (setq first (pystring (mapconcat 'symbol-name first ""))) ; [aa]
  (setq second (mapcar 'translate-ITRANS-SLP1 (nth 1 y)))
  (setq second (pystring (mapconcat 'symbol-name second ""))) ; []
  (setq third (pystring (nth 2 y))) ; join
  (setq fourth (mapcar 'pystring (nth 3 y)))
  (setq z (append (list first second third) fourth))
  (mapconcat (lambda (x) x) z ",")
 )
)
(defun convert-val (x)
 " x is a list of elements y1 y2 ...yn , each y is like
     ([aa] [] join (type svara ref Kale19))
   Return a string representation (colon-separated)
   z1:z2:...:zn
   where each zj is a conversion of yj into comma-separated values
    'A','','join','type','svara','ref','Kale19 so
 "
 (mapconcat 'convert-val1 x ":")
)
(defun convert-sandhi (&rest m)
 (let (prop val pyprop pyval p name n)
  (setq name 'Sandhi)
  (if (not m)
   (setq m 1000000) ; a large number
  )
  ; initialize python multi-line string
  (insert (format "%s_string='''\n" name)) 
  (setq p (get name 'Sangram )) ; a property list
  (setq n 0)
  (while (and p (<= n m))
   (setq prop (car p))
   (setq p (cdr p))
   (setq val (car p))
   (setq p (cdr p))
   (setq pyprop (convert-prop prop))
   (setq pyval (convert-val val))
   (insert (format "%s=%s\n" pyprop pyval))
   (setq n (1+ n))
  )
  (insert (format "'''\n"))
 )
)

