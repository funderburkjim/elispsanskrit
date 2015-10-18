; inits.el
; 10-10-2015
(defun init-sanskrit1 (&optional options) ;XXX
 (fol-msg (format "init-sanskrit1 begins..."))
 (fol-msg (format "%s\n" (current-time-string)))
 (construct-init)
 (MW-init)
 (message "load constructions...")
; (load-construct-all)

  (init-gender-form-data) ; types of nominal endings

;   (message (format "initialize indeclineables"))
;   (init-avyayapada-data)

  ; initialization for 'explain'
  (init-explain-forms)
 (fol-msg (format "init-sanskrit1 ends..."))
 (fol-msg (format "%s\n" (current-time-string)))
)
(defun sangram-size () ;xxx
 (let (OBARRAY size val m num Xnum Xsize)
  (if (not OBARRAY) (setq OBARRAY obarray))
  (setq size 0)
  (setq num 0)
  (setq Xsize 0)
  (setq Xnum 0)
  (mapatoms 
   (lambda (sym)
    (setq val (sangetall sym))
    (setq m 0)
    (when val 
     (setq m (length (format "%s" val)))
     (setq size (+ size m))
     (setq num (1+ num))
    )
    (setq val (Xsangetall sym))
    (setq m 0)
    (when val 
     (setq m (length (format "%s" val)))
     (setq Xsize (+ Xsize m))
     (setq Xnum (1+ Xnum))
    )
   )
   OBARRAY
  )
  (list num size Xnum Xsize)
 )
)
(defun sangram-restore (&optional tabname dirname) ; xxx
 (init-Sangram)
 (let (filename buf key val more dhaatus subkey nrec err)
  (if (not dirname) (setq dirname "construct"))
  (if (not tabname) (setq tabname "sangram.txt"))
  (setq filename (sangram-filename tabname dirname))
  (setq buf (find-file-noselect filename nil)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char (point-min))
   (setq more t)
   (setq nrec 0)
   (while more
    (condition-case err
     (progn
      (setq key (read buf)) 
      (setq subkey (read buf)) 
      (setq val (read buf))
      (cond
       ((not (symbolp subkey))
	(fol-msg (format "sangram-load err: %s %s %s\n" (1+ nrec)
			 key subkey))
       )
       ((equal key 'S)
	(put subkey 'Sangram val)
       )
       ((equal key 'X)
	(put subkey 'XSangram val)
       )
       (t
	(fol-msg (format "sangram-load err: %s %s %s\n" (1+ nrec)
			 key subkey))
       )
      )
      (setq nrec (1+ nrec))
     )
     (error
      (setq more nil)
      (if (not (equal err '(end-of-file)))
       (fol-msg (format "error: %s %s\n" err (1+ nrec)))
      )
     )
    )
   )
   (kill-buffer nil)
  )

  (fol-msg (format "sangram-load : %s %s %s\n"
		   tabname dirname nrec))

  t
 )
)
(defun sangram-save (&optional outtab outdir)
 (let (OBARRAY size val m num Xnum Xsize fileout bufout)
  (if (not outdir) (setq outdir "construct"))
  (if (not outtab) (setq outtab "sangram.txt"))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (erase-buffer)
  )
  (if (not OBARRAY) (setq OBARRAY obarray))
  (setq size 0)
  (setq num 0)
  (setq Xsize 0)
  (setq Xnum 0)
  (mapatoms 
   (lambda (sym)
    (setq val (sangetall sym))
    (setq m 0)
    (when val 
     (setq m (length (format "%s" val)))
     (setq size (+ size m))
     (setq num (1+ num))
     (with-current-buffer bufout
      (insert (format "S %s %s\n" sym val))
     )
    )
    (setq val (Xsangetall sym))
    (setq m 0)
    (when val 
     (setq m (length (format "%s" val)))
     (setq Xsize (+ Xsize m))
     (setq Xnum (1+ Xnum))
     (with-current-buffer bufout
      (insert (format "X %s %s\n" sym val))
     )
    )
   )
   OBARRAY
  )
  (with-current-buffer bufout
   (save-buffer)
   (kill-buffer nil)
  )
  (list num size Xnum Xsize)
 )
)
