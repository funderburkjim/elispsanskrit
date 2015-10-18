; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; fileutil.el
; code moved from other modules, 08-28-03
(defun write-table-file (nout out filename &optional dirname)
 (let (fileout bufout i x y z bufsave)
  (if (not dirname) (setq dirname "tables"))
  (setq fileout (sangram-filename filename dirname))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (setq bufsave fol-msg-buffer)
  (setq fol-msg-buffer bufout)
  (setq i 0)
  (while (< i nout)
   (setq x (elt out i)) ; assumed to be a list or a vector
   (setq i (1+ i))
   (cond
    ((arrayp x) (setq x (append x nil))) ; turn into a list
    ((not (listp x)) (setq x (list x)))
   )
   (while x
    (setq y (car x))
    (setq x (cdr x))
    (if (not (listp y)) (setq y (list y)))
    (while y
     (setq z (car y))
     (setq y (cdr y))
     (fol-msg (format "%s" z))
     (when y (fol-msg " "))
    )
    (if x
     (fol-msg (format " : "))
     (fol-msg (format "\n"))
    )
   )
  )
  (with-current-buffer bufout
   (save-buffer)
  )
  (kill-buffer bufout)
  (setq fol-msg-buffer bufsave)
 )
)
(defun read-colon-file-table (tabname nfields &optional dirname)
 (let (filename)
  (if (not dirname) (setq dirname "tables"))
  (setq filename (sangram-filename tabname dirname))
  (read-colon-file filename nfields)
 )
)
(defun read-colon-file-validation (fname nfields)
 (let (filename)
  (setq filename (san-validation-filename fname))
  (read-colon-file filename nfields)
)
)
(defun read-colon-file (filename nfields)
 (let (buf ans)
  (setq buf (find-file-noselect filename 't)) ; 't suppresses warning
;  (fol-msg (format "filename=%s\n, buf=%s\n" filename buf))
  (setq ans (read-colon-buffer buf nfields))
 )
)
(defun read-colon-buffer (buf nfields &optional sep)
 (let (nline p1 p2 pmax xin x y ans xfields stemp more
	       ok)
  (if (not sep) (setq sep ':))
  (setq ans nil)
  (with-current-buffer buf
   (setq nline 0)
   (setq pmax (point-max))
   (setq p1 (point-min))
   (while (< p1 pmax)
;    (if (equal (mod nline 100) 0) (message (format "%s" nline)))
    (goto-char p1)
    (end-of-line)
    (setq p2 (point))
    (setq stemp (buffer-substring p1 p2))
    ; now xin contains the symbols in the line.
    ; e.g. with the line being
    ;   "a~nch : 1 P : REGULAR : worship ; a~nchati : Antoine "
    ; xin = (a~nch : 1 P : REGULAR : worship)
    (setq xin (read-colon-line stemp))
    (setq nline (1+ nline))

     ; 1. the colon serves as field separator.
     ;    Validate that the number of fields matches routine
     ;    input 'nfields' and construct xfields
    
    ; 
    (setq xfields (gather-fields xin sep))
     ; when xin
    (setq ok (or (= nfields 0) (= (length xfields) nfields)))
    (when (and (not ok) xin)
     (fol-msg (format "%s, not %s, fields needed %s: xin=%s\n"
		  nfields (length xfields) nline xin))
     )

    (if  (or ok (not xin))
      (setq p1 (1+ p2))
      (setq p1 pmax) ; to force loop exit
    )
    ; 2. install fields array into ans
    (when (and xin ok)
     (setq xfields (vconcat xfields)) ; turn into vector
;     (setq ans (append ans (list xfields)))
     (setq ans (cons xfields ans)) ; faster than appending
    )
   )
  )
;  (fol-msg (format "nline=%s\n" nline))
  (if nil (kill-buffer buf)) ; free up memory ( 'if t' for production)
  (setq ans (nreverse ans)) ; 
  ans
 )
)
(defun read-colon-line (stemp)
    ; now xin contains the symbols in the line.
    ; e.g. with the line being
    ;   "a~nch : 1 P : REGULAR : worship ; a~nchati : Antoine "
    ; xin = (a~nch : 1 P : REGULAR : worship)
 (let (x xin more)
  (save-excursion
    (with-temp-buffer
     (insert stemp)
     (goto-char 1)
     (setq xin nil)
     (setq more 't)
     (while more
      (condition-case nil
       (progn
        (setq x (read (current-buffer)))
        (setq xin (append xin (list x)))
       )
       (error (setq more nil))
      )
     )
    )
   )
  xin
 )
)
(defun read-colon-line-fields (stemp sep)
 (let (xin xfields)
  (setq xin (read-colon-line stemp))
  (setq xfields (gather-fields xin sep))
 )
)
(defun read-string-fields (s sep &optional keepsep)
 "Apply the regimen of 'read-buffer-fields' to the string 's'
 "
 (let (ans)
  (with-temp-buffer
   (insert s)
   (goto-char 1)
   (setq ans (read-buffer-fields  sep keepsep))
  )
  ans
 )
)
(defun read-buffer-fields (sep &optional keepsep)
 "Using the current line of the current buffer , and a string 'sep' used
  for matching, a list of strings is returned, 
  namely those subparts of 's' determined by the separator 'sep'.
  In a tab-delimited file, 'sep' would be the string '\t'.
  Note that 'sep' is treated as a regular 
  expression, so more complicated separation regimen are possible.
  When the optional flag 'keepsep' is true, the substrings matching 'sep'
  are included as separate elements in the answer; 
  otherwise, only the parts between the separators are returned.
  Note: the end of the line is counted as a final separator, but is
  not included in the answer, even if 'keepsep' is true.
 "
 (let (ans p2 p1 s s1 s2)
  (save-excursion

    (end-of-line)
    (setq p2 (point))
    (beginning-of-line)
    (setq p1 (point))
    (while (search-forward-regexp sep p2 t)
     ; current point is now just after the matching segment
     ; we must determine the string matching sep
     (setq s1 (buffer-substring p1 (match-beginning 0)))
     (setq ans (cons s1 ans))
     (when keepsep
      (setq s2 (buffer-substring (match-beginning 0) (match-end 0)))
      (setq ans (cons s2 ans))
     )
     (setq p1 (match-end 0))
    )
    ; get a final part due to end of line
    (setq s1 (buffer-substring p1 p2))
    (setq ans (cons s1 ans))
  )
  (nreverse ans)
 )
)
(defun read-expr-file-table (tabname nfields &optional dirname)
 (let (filename)
  (if (not dirname) (setq dirname "tables"))
  (setq filename (sangram-filename tabname dirname))
  (read-expr-file filename nfields)
 )
)
(defun read-expr-file (filename nfields)
 (let (buf ans)
  (setq buf (find-file-noselect filename 't)) ; 't suppresses warning
;  (fol-msg (format "filename=%s\n, buf=%s\n" filename buf))
  (setq ans (read-expr-buffer buf nfields))
 )
)
(defun read-expr-buffer (buf &optional nfields skips)
 (let (nline p1 p2 pmax xin x y ans xfields stemp more
	       ok expr)
  (setq ans nil)
  (with-current-buffer buf
   (setq nline 0)
   (setq p1 (point-min))
   (goto-char p1)
   (setq pmax (point-max))
   (while (< p1 pmax)
    (setq expr (read (current-buffer)))
    (if (not (member expr skips))
     (setq ans (cons expr ans))
    )
    (setq p1 (point))
    (fol-msg (format "point-min=%s, p1=%s\n" (point-min) p1))
    (setq p1 pmax)
   )
   (setq ans (nreverse ans))
  )
  (if nil (kill-buffer buf)) ; free up memory ( 'if t' for production)
  ans
 )
)

(defun gather-fields (xin separator)
; xin is assumed a list
; result is returned as a list
; (fol-msg (format "gather-fields: %s %s\n" xin separator))
 (when xin ; otherwise, nil is returned
 (let (xfields x y z)
  (setq xfields nil)
  (setq x xin)
  (setq z nil) ; the next field
  (while x
   (setq y (car x)) ; next field item or separator
   (setq x (cdr x))
   (cond
    ((equal y separator)
     (setq xfields (append xfields (list z)))
;     (setq xfields (vconcat xfields (vector z)))
     (setq z nil)
    )
    (t
     (setq z (append z (list y)))
    )
   )
  )
  ; the last element
;  (setq xfields (vconcat xfields (vector z)))
   (setq xfields (append xfields (list z)))
  xfields
 )
 )
)
(defun load-roots-seT ()
 (load-dhaatu-data "roots-seT.txt" 'seT-code t)
)
(defun load-dhaatu-data (tabname &optional subkey do-inversion)
 (let (nrec irec recs fields nfields out nout thisout thesubkey thekey)
 ; 1. get roots-seTPERF data
  (if subkey 
   (setq nfields 2)
   (setq nfields 3) ; subkey appears as 2nd field
  )
  (let ()
   (setq recs (read-colon-file-table tabname nfields))
  )
  (setq nrec (length recs))
  (setq irec 0)
  (while (< irec nrec)
   (setq fields (elt recs irec))
   (let (dhaatu upasargas val)
    ; 1. unpack 'fields' to other variables
    ; assume fields[0] is a list of length 1 whose member is a symbol
;    (fol-msg (format "fields=%s\n" fields))
    (let (field0) ; either (dhaatu) or (upasarga dhaatu)
      ; or (upasarga1 upasarga2 ... dhaatu)
     (setq field0 (elt fields 0))
     (cond
      ((= 1 (length field0))
       (setq dhaatu (elt field0 0))
       (setq upasargas nil)
      )
      (t
       (let (vec0 n0)
	(setq vec0 (vconcat field0))
	(setq n0 (length vec0))
	(setq upasargas (substring vec0 0 (1- n0)))
	(setq dhaatu (substring vec0 -1)) ; a vector
	(setq dhaatu (elt dhaatu 0)) ; the element, a symbol
	(setq upasargas (append upasargas nil)) ; a list
       )
      )
     )
    )
    (if (equal nfields 2)
     (progn
      (setq thesubkey subkey)
      (setq val (elt fields 1))
     )
     (progn
      (setq thesubkey (solution (elt fields 1)))
      (setq val (elt fields 2))
     )
    )
    (setq thekey (list 'dhaatu thesubkey))
     (sanput2 dhaatu thekey val)
;    (sanAppendElt2 dhaatu thekey val)
    (when do-inversion
;      (fol-msg (format "%s : %s\n" dhaatu val))
     (let (thisval)
      (if (not (listp val)) (setq val (list val)))
      (while val
       (setq thisval (car val))
       (setq val (cdr val))
       (sanAppendElt thisval thesubkey dhaatu)
      )
     )
    )
   )
   (setq irec (1+ irec))
  )
  t
 )
)

(defun fol-delete-files (dirin ending &optional verbose)
" delete files ending in a given string in a given directory.
  When 'ending' is the empty string, all files are deleted,
  except those ending in a period (this exception for Windows OS).
  When 'verbose' is non-nil, the name of each deleted file is written;
  Otherwise, only a summary of the number of files deleted is written.
"
 (let (ans dir files file n i s1)
  (setq dir (file-name-as-directory (fol-filename dirin)))
  (setq files (directory-files dir 't ending)) ; 't indicates fullnames
  (when nil ; dbg
   (fol-msg (format "fol-delete-files: %s\n" files))
  )
  (setq n 0)
  (while files
    (setq file (car files))
    (setq files (cdr files))
     ; the file names may contain 'ending' other than at end.
     ; do not delete such instances
    (when (not (equal (substring file -1) "."))
     (setq i (length file))
     (setq s1 (substring file (- i (length ending)) i))
     (when (string= ending s1)
      (when verbose (fol-msg (format "deleting file : %s\n" file)))
      (delete-file file)
      (setq n (1+ n))
     )
    )
  )
  (fol-msg (format "deleted %s files ending in %s from directory %s\n"
		   n ending dir))
  't
 )
)
(defun sangram-clean ()
 (let (dir dirs)
  (setq dirs '(
   lisp inputs construct explain validation tables
   forms forms/s forms/v forms/cv forms/i mw
  ))
  (while dirs
   (setq dir (car dirs))
   (setq dirs (cdr dirs))
;   (setq dir "grammar/lisp/")
   (fol-delete-files (format "grammar/%s" dir) "~")
  )
 )
)
(defun remove-duplicate-lines (intab indir &optional remove-orig)
 "Removes duplicate lines in file 'intab' in relative director 'indir.'
  Does not change order of remaining lines.
  If optional flag 'remove-orig' is true, then the a line with duplicates
  is also itself deleted from the file; only the lines in the original file
  without duplicates are retained.

  The changed file is saved to the original file, without a backup copy.
 "
 (let (filein bufin ans ndup found)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (setq ndup 0)
   (while (< (point) (point-max))
    (let (p1 s s1)
     (setq p1 (point))
     (setq s (current-line))
     (end-of-line)
     (setq found nil)
     (while (search-forward s nil t)
      (beginning-of-line)
      (kill-line 1)
      (setq ndup (1+ ndup))
      (setq found t)
     )
     (goto-char p1)
     (if (and found remove-orig)
      (progn
       (beginning-of-line)
       (kill-line 1)
       (setq ndup (1+ ndup))
      )
      (forward-line)
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
   (kill-buffer nil) ; kill current buffer
  )
  ndup
 )
)

(defun non-system-files (dir)
 "dir is a string representing a directory, e.g.,
  (setq dir1 (sangram-filename 's' 'forms'))
  (setq dir (file-name-as-directory dir1))
   has dir1 = 'c:/Sanskrit/grammar/forms/s'
   dir = 'c:/Sanskrit/grammar/forms/s/'
  The function returns the (short) names of files in the directory.
  Subdirectories are among those returned.
  The system files '.' and '..' are excluded.
 "
 (let (files file ans)
  (setq files (directory-files dir nil "")) ; nil indicates short names
  (while files
    (setq file (car files))
    (setq files (cdr files))
    (when (not (equal (substring file -1) ".")) ; skip system files
     (setq ans (cons file ans))
    )
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun dispLineLim (file)
 (with-current-buffer file
  (setq line-number-display-limit 50000000)
 )
 t
)

(defun fol-search-files (dirin regexp &optional verbose)
" Search each file in a given directory
"
 (let (ans dir files file n i s1 ending m)
  (setq dir (file-name-as-directory (fol-filename dirin)))
  (setq files (directory-files dir 't ending)) ; 't indicates fullnames
  (setq n 0)
  (setq m 0)
  (while files
    (setq file (car files))
    (setq files (cdr files))
     ; the file names may contain 'ending' other than at end.
     ; do not delete such instances
    (when (not (equal (substring file -1) "."))
     (setq m (1+ m))
     (let (buf ncount more)
      (setq buf (find-file-noselect file 't))
      (with-current-buffer buf
       (goto-char 1)
       (setq case-fold-search nil)
       (setq ncount 0)
       (setq more t)
       (while (and (search-forward-regexp regexp nil t) more)
	 (setq ncount (1+ ncount))
       )
       (when (> ncount 0)
        (fol-msg (format "%s  %s\n" file ncount))
        (setq n (1+ n))
       )
       (kill-buffer nil)
      )
     )
    )
  )
  (fol-msg (format "found regexp in %s of %s files from  %s\n"
		   n m dir))
  't
 )
)
