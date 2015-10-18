; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
;  start.el
(message "default-directory=%s" default-directory)
(defvar fol-default-directory  "..\\..\\" 
  "the 'home' directory for sanskrit, the 'parent' of 'grammar' directory")
(defvar san-tables-dir "grammar/tables/")
(defvar san-validation-dir "grammar/validation/")
(defvar san-explain-dir "grammar/explain/")
(defvar fol-ob nil "the 'obarray' for fol")
(defvar fol-read-err nil)
(defvar fol-msg-buffer "*scratch*")

(defun ejf-frames (&optional n)
 (let (i f)
  (if (not n) (setq n 3))
  (setq i 0)
  (if (< n 0) (setq n 0))
  (while (< i n)
   (setq i (1+ i))
   (setq f (make-frame-command))
;   (set-frame-height f 29)
   (set-frame-position f (* 20 i) (* 15 i))
  )
 )
)

(defun san-tables-filename (arg)
 (sangram-filename arg "tables")
)
(defun string-endswith-p (s ending)
" predicate checks that s and ending are strings
  and that the end of s equals ending"
 (when (and (stringp s) (stringp ending)
	    (<= (length ending) (length s)))
  (string= ending (substring s (- (length s) (length ending)) (length s))))
)

(defun fol-files-verify-dir (dirin &optional endingsin)
" return a list of all the files relevant to
  verification in a given directory. The default value
  of endingsin is the list of strings "

 (let (ans endings dir files file n ending tmp more)
  (setq endings
   (if endingsin endingsin '("")))
  (setq dir (file-name-as-directory (fol-filename dirin)))
  (setq files (directory-files dir)) ; 't indicates fullnames
  (setq n 0)
  (setq ans nil)
  (while files
    (setq file (car files))
    (setq files (cdr files))
    (setq tmp endings)
    (while tmp
     (setq ending (car tmp))
     (setq tmp (cdr tmp))
     (when (string-endswith-p file ending)
      (setq n (1+ n))
      (setq ans (cons file ans))
     )
    )
   
  )
 ans
 )
)

(defun san-search-dir (dir changes &optional all)
 "search for each string in 'changes', which is a list
  of strings. Search for these in all the files
  of the given directory which end in .el
  When a string is noted in a file, write the string and the
  file name.
  The searching is done with routine 'search-forward'.
  When optional flag 'all' is t, all instances are noted.
  Otherwise, just the first instance is noted.
 "
 (let (files file change buf dir1 endings savechanges)
 (setq dir1 (file-name-as-directory dir))
 (setq endings  '(".el"))
 (setq files (fol-files-verify-dir dir endings))
 (setq savechanges changes)
 (while files
  (setq file (car files))
  (setq file (concat dir1 file))
  (setq file (fol-filename file))
;  (fol-msg (format "fol-search-dir: %s\n" file))
  (setq buf (find-file-noselect file))
  (setq changes savechanges)
  (while changes
   (setq change (format "%s" (car changes)))
   (setq changes (cdr changes))
   (with-current-buffer buf
    (goto-char 1)
    (while (search-forward change nil t)
     (let (p1 p2)
      (beginning-of-line)
      (setq p1 (point))
      (end-of-line)
      (setq p2 (point))
      (fol-msg (format "%s :  %s\n" file change ))
      (when (not all)
       (goto-char (point-max)) ; force end of search
      )
     )
    )
   )
  )
  (kill-buffer buf)
  (setq files (cdr files))
 )
 )
)




(defun sanload ()
 "lisp code to load to start up sanskrit"
; (interactive)
 (my-load-library "grammar/lisp/start0")
 (my-load-library "grammar/lisp/inits")
 (my-load-library "grammar/lisp/itrans")
 (my-load-library "grammar/lisp/fileutil")
 ;(my-load-library "grammar/lisp/mw")
 (my-load-library "grammar/lisp/gram1")
 (my-load-library "grammar/lisp/endings")
 (my-load-library "grammar/lisp/sandhi")
 (my-load-library "grammar/lisp/gram2")
 (my-load-library "grammar/lisp/gram2-liT")
 (my-load-library "grammar/lisp/gram2-future")
 (my-load-library "grammar/lisp/aorist")
 (my-load-library "grammar/lisp/causal")
 (my-load-library "grammar/lisp/gram3")
 (my-load-library "grammar/lisp/kta")
 (my-load-library "grammar/lisp/irreg")
 (my-load-library "grammar/lisp/construct")
 (my-load-library "grammar/lisp/validation")
 (my-load-library "grammar/lisp/explain")
 (my-load-library "grammar/lisp/Mbh")
 (my-load-library "grammar/lisp/forms")
 (my-load-library "grammar/lisp/sxmw")
; (my-load-library "grammar/lisp/mw")
)
(defun my-byte-compile-file (s)
 (byte-compile-file (fol-filename s))
)
(defun sancompile ()
 "lisp code to recompile sanskrit"
 (interactive)
 (my-byte-compile-file "grammar/lisp/start0.el")
 (my-byte-compile-file "grammar/lisp/inits.el")
 (my-byte-compile-file "grammar/lisp/start.el")
 (my-byte-compile-file "grammar/lisp/itrans.el")
 (my-byte-compile-file "grammar/lisp/sxmw.el")
 ;(my-byte-compile-file "grammar/lisp/mw.el") ; removed Oct 8, 2015
 (my-byte-compile-file "grammar/lisp/fileutil.el")
 (my-byte-compile-file "grammar/lisp/gram1.el")
 (my-byte-compile-file "grammar/lisp/endings.el")
 (my-byte-compile-file "grammar/lisp/sandhi.el")
 (my-byte-compile-file "grammar/lisp/gram2.el")
 (my-byte-compile-file "grammar/lisp/gram2-liT.el")
 (my-byte-compile-file "grammar/lisp/gram2-future.el")
 (my-byte-compile-file "grammar/lisp/aorist.el")
 (my-byte-compile-file "grammar/lisp/causal.el")
 (my-byte-compile-file "grammar/lisp/gram3.el")
 (my-byte-compile-file "grammar/lisp/irreg.el")
 (my-byte-compile-file "grammar/lisp/kta.el")
 (my-byte-compile-file "grammar/lisp/construct.el")
 (my-byte-compile-file "grammar/lisp/validation.el")
 (my-byte-compile-file "grammar/lisp/explain.el")
 (my-byte-compile-file "grammar/lisp/Mbh.el")
 (my-byte-compile-file "grammar/lisp/forms.el")
)

(defun old-load-start0()
 (let (dir fname path)
  ; x is path to start0
  (setq fname "grammar/lisp/start0")
  ;(setq path (expand-file-name fname fol-default-directory))
  (setq path (expand-file-name fname))
  (message "load-start0: path=%s" path)
  (load-library path)
 )
)

(defun load-start0()
 (let (dir fname path)
  ; x is path to start0
  ;(setq fname "grammar/lisp/start0")
  (setq fname "start0")
  ;(setq path (expand-file-name fname fol-default-directory))
  (setq path (expand-file-name fname))
  (message "load-start0: path=%s" path)
  (load-library path)
 )
)
(defun sansession1 ()
  "startup activities for current sanskrit session"
 ;; (interactive)
 ;; (let (x)
 ;;  (setq x (read-string "Sanskrit home dir, (dflt=%s): " fol-default-directory))
 ;;  ; x is a string.
 ;;  (when (< 0 (length x))
 ;;   (setq fol-default-directory x)
 ;;  )
 ;; )
  (setq load-path (cons nil load-path)) ; add current directory to lisp load path
  (message "load-path=%s" load-path)
  (load-start0)
  (fol-msg (format "sanload...\n"))
  (sanload) 
  (fol-msg (format "init-sanskrit1...\n"))
  (init-sanskrit1)
  ;(fol-msg (format "ejf-frames...\n"))
  ;(ejf-frames 1)
  ;(find-file (fol-filename "grammar/lisp/mwdoc.txt"))
;  (outline-mode)
  ;(hide-sublevels 1)
  ;(find-file (fol-filename "grammar/lisp/start.el"))
;  (find-file (fol-filename "grammar/lisp/itrans.el"))
  ;(find-file (fol-filename "grammar/lisp/sxmw.el")) ; 10-10-2015 changed mw.el to sxmw.el
  ;(find-file (fol-filename "grammar/lisp/construct.el"))
  ;(find-file (fol-filename "grammar/lisp/explain.el"))

)

(sansession1)
(switch-to-buffer "*scratch*") ; switch to scratch buffer. NOT A STRING!
(message "current-buffer=%s" (current-buffer))
