; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
;;; dictionary12.el aid in constructing input files 
; 05-02-02
; uses functions in itrans-050402.el
; functions:  
;    sortdict sorts file in format of dictionary12.txt ,
;       in order of Sanskrit alphabet
;    word-list : returns words from a string (separated by spaces)
;    make-item1 : calls ITRANS-parse-word-string on the words in a 
;       string, and concatenates the translation.
;    make-item1 : prepares a line in dictionary12.txt for sorting

(defun sortdict (bufin)
 (let (sout colon lines)
  (setq sout "")
  (setq lines nil)
  (with-current-buffer bufin
   (goto-char 1)
   (let (s s1 s2 s3 s4 p1 p2 p3 e)
    (while (not (eobp))
     (setq p1 (point))
     (end-of-line)
     (setq s1 (buffer-substring p1 (point)))
     (setq e (make-item s1))
     (when e (setq lines (concat lines e "\n")))
     (when (not e) (fol-msg (format "make-item err: %s\n" s1)))
     (forward-line)
    )
   )
  )
;  (fol-msg (format "lines before sorting ...\n%s\n" lines))
;  (sort lines 'string<)
  (with-temp-buffer
   (insert lines)
   (sort-lines nil (point-min) (point-max))
   (goto-char 1)
   (setq lines nil)
   (let (s s1 s2 s3 s4 p1 p2 p3 e)
    (while (not (eobp))
     (setq p1 (point))
     (setq p2 (search-forward ":::" nil t))
     (when p2 (setq p1 p2))
     (end-of-line)
     (setq e (buffer-substring p1 (point)))
     (when e (setq lines (concat lines e "\n")))
     (forward-line)
    )
   )   
  )
  (fol-msg (format "lines after sorting ...\n%s\n" lines))
 )
)

(defun make-item1 (s)
 (let (ans w nw iw ans s1 s2 e1)
  (setq w (word-list s))
  (setq ans nil)
  (setq nw (length w))
  (setq iw 0)
  (while (< iw nw)
   (setq s1 (aref w iw))
   (setq e1 (ITRANS-parse-word-string s1))
   (setq ans (vconcat ans (car e1)))
   (setq iw (1+ iw))
  )
  ans
 )
)
(defun make-item (s)
 (let (s1 s2 s3 s4 s5 i1 i2 i3 sout j n x)
  (with-temp-buffer
   (setq sout nil)
   (insert s)
   (goto-char 1)
   (setq i1 (search-forward "##" nil t))
   (when (eq i1 3)
    (setq i2 (search-forward "##" nil t))
    (when i2
     (setq i2 (- i2 2))
     (setq s1 (buffer-substring i1 i2))
     (setq e (make-item1 s1))
     (setq s2 "")
     (setq n (length e))
     (setq j 0)
     (while (< j n)
      (setq x (eval (aref e j)))
      (setq s2 (format "%s %s" s2 x))
      (setq j (1+ j))
     )
     ; make prefix strings at least 10 chars long
     (while (< j 10)
       (setq s2 (format "%s 000" s2))
       (setq j (1+ j))
     )
     ; prefix the list of token codes, then ::: to s
     (setq sout (format "%s :::%s" s2 s))
    )
   )
  )
  sout
 )
)
