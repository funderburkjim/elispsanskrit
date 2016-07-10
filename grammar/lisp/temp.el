(defun temp ()
(defun fjoin (x)
 (if (listp x)
  (mapconcat (lambda (y) (format "%s" y)) x ",")
  (format "%s" x)
 )
)
(defun dhaatu-irreg-temp (dhaatu form Eng-def val)
 ;(sanput2 'dhaatu-irreg-temp (list dhaatu form) val)
 (let (tense class pada formparts newform)
  (setq formparts (split-string (format "%s" form) "-"))
  (setq tense (elt formparts 0))
  (setq class (elt formparts 1))
  (setq pada (elt formparts 2))
  (setq tense (translate-ITRANS-SLP1 (intern tense)))
  (setq pada (downcase pada))
  (setq newform (format "%s-%s-%s" tense class pada))
  (setq dhaatu (translate-ITRANS-SLP1 dhaatu))
  (setq val (translate-ITRANS-SLP1 val))
  (setq newval (mapconcat 'fjoin val ":"))
  (fol-msg (format "%s.%s=%s\n" dhaatu newform newval))
 )
 t
)

(dhaatu-irreg-temp 'kRi 'laT-8-P 'do
 [karoti kurutaH kurvanti karoShi kuruthaH kurutha karomi kurvaH kurmaH]
)
(dhaatu-irreg-temp 'kRi 'la~N-8-P 'do
 [akarot akurutaam akurvan akaroH akurutam akuruta akaravam akurva akurma]
)
(dhaatu-irreg-temp 'kRi 'loT-8-P 'do
 [karotu kurutaam kurvantu kuru kurutam kuruta karavaaNi karavaava karavaama]
)
(dhaatu-irreg-temp 'kRi 'vidhili~N-8-P 'do
 [kuryaat kuryaataam kuryuH kuryaaH kuryaatam kuryaata kuryaam kuryaava kuryaama]
)
(dhaatu-irreg-temp 'kRi 'laT-8-A 'do
 [kurute kurvaate kurvate kuruShe kurvaathe kurudhve kurve kurvahe kurmahe]
)
(dhaatu-irreg-temp 'kRi 'la~N-8-A 'do
 [akuruta akurvaataam akurvata akuruthaaH akurvaathaam akurudhvam akurvi akurvahi akurmahi]
)
(dhaatu-irreg-temp 'kRi 'loT-8-A 'do
[kurutam kurvaataam kurvataam kuruShva kurvaathaam kurudhvam karavai karavaavahai karavaamahai]
)
(dhaatu-irreg-temp 'kRi 'vidhili~N-8-A 'do
[kurviita kurviiyaataam kurviiran kurviithaaH kurviiyaathaam kurviidhvam kurviiya kurviivahi kurviimahi]
)
; as: as st san sth sv sm edh syaa syu
(dhaatu-irreg-temp 'as 'laT-2-P 'do
 [asti staH santi asi sthaH stha asmi svaH smaH]
)
(dhaatu-irreg-temp 'as 'la~N-2-P 'do
 [aasiit aastaam aasan aasiiH aastam aasta aasam aasva aasma]
)
(dhaatu-irreg-temp 'as 'loT-2-P 'do
 [astu staam santu edhi stam sta asaani asaava asaama]
)
(dhaatu-irreg-temp 'as 'vidhili~N-2-P 'do
 [syaat syaataam syuH syaaH syaatam syaata syaam syaava syaama]
)


; vid 2P (to know) Kale 436.
; laT: optionally takes the terminations of the Perfect
; la~N :  Note: Kale does not mention these as irregularities
;        3P is 'aviduH'  2S is 'aveH' or 'aved'
; loT : in addition to the normal imperative, there is
;  an optional form made by appending 'vidaam' to the imperative of 'kRi'
; vidhili~N : regular 
(dhaatu-irreg-temp 'vid 'laT-2-P 'know
 (let (x y z i n)
  (setq x (conjugation-tab-2 nil 'laT 2 'P 'vid))
  ; x = [vetti vittaH vidanti vetsi vitthaH vittha vedmi vidvaH vidmaH]
  (setq y [veda vidatuH viduH vettha vidathuH vida veda vidva vidma])
  (setq n (length x))
  (setq i 0)
  (setq z (make-vector n nil))
  (while (< i n)
   (aset z i (list (elt x i) (elt y i)))
   (setq i (1+ i))
  )
  z 
 )
)
(dhaatu-irreg-temp 'vid 'la~N-2-P 'know
 (let (x)
  (setq x (conjugation-tab-2 nil 'la~N 2 'P 'vid))
  ; x = [avet avittaam avidan (avet aved) avittam avitta avedam avidva avidma]
  (aset x 2 'avidan)
  (aset x 3 '(aveH aved))
  x
 )
)
(dhaatu-irreg-temp 'vid 'loT-2-P 'know
 (let (x y z i n)
  (setq x (conjugation-tab-2 nil 'loT 2 'P 'vid))
  ; x = [vettu vittaam vidantu viddhi vittam vitta vedaani vedaava vedaama]
  (setq y
	[vidaa~Nkarotu vidaa~Nkurutaam vidaa~Nkurvantu
	 vidaa~Nkuru vidaa~Nkurutam vidaa~Nkuruta 
	 vidaa~NkaravaaNi vidaa~Nkaravaava vidaa~Nkaravaama])
  (setq n (length x))
  (setq i 0)
  (setq z (make-vector n nil))
  (while (< i n)
   (aset z i (list (elt x i) (elt y i)))
   (setq i (1+ i))
  )
  z 
 )
)
(dhaatu-irreg-temp 'vid 'vidhili~N-2-P 'know
 (conjugation-tab-2 nil 'vidhili~N 2 'P 'vid)
 ; [vidyaat vidyaataam vidyuH 
 ;  vidyaaH vidyaatam vidyaata vidyaam vidyaava vidyaama]
)
(dhaatu-irreg-temp 'bruu 'laT-2-P 'speak
 ; 05-18-05.
 ;  the form 'prAha' occurs in mbh 3.264.028
 ; also must change 'conjugation-tab' in irreg.el
 [(braviiti aaha) (bruutaH aahatuH) (bruvanti aahuH)
  (braviiShi aattha) (bruuthaH aahathuH) bruutha 
  braviimi bruuvaH bruumaH] 
)
)

(defun v-file-init-alt1-aorvar (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write aorist varieties
  for each line to output outdir/outtab
  Use the the voice and class provided in 
  dcpforms-MW.txt
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
  Output is constructed by routine v-file-init-alt1-aorvar-helper.
 "
 (let (filein bufin bufout fileout outputs output nin nout
       words root class pada dict voice tenses)
  (setq nin 0)
  (setq nout 0)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout ; empty fileout, in case it already existed
   (erase-buffer)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line root class pada err)
     (setq outputs nil)
     (setq line (current-line))
     (setq nin (1+ nin))
     (when (and (<= n1 nin) (<= nin n2))
     (when dbg
      (fol-msg (format "%s line=%s\n" intab line)))
     (condition-case err
      (progn  
       ; as in v-file-init2a
       ;  skip initial and final parens in current line
       (setq line (substring line 1 -1))
       (setq words (gen-word-list line " ")) ; space for dcpforms-MW.txt
       (setq root (elt words 0))
       (setq class (elt words 1))
       (setq pada (elt words 2))
       (setq dict (string-trim (elt words 3))) ; unused
       ; as in v-file-init2a-helper. Change Pada to 'voice', per Scharf
       (setq voice (if (equal pada "P") "a" "m"))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq outputs (v-file-init-alt1-aorvar-helper root class voice dbg))
       (if (not outputs)
        (fol-msg (format "error@line: %s\n" line))
       )
      )
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
     )
     ; append output to bufout
     (when outputs
      (setq nout (1+ nout))
     )
     (with-current-buffer bufout
      (while outputs
       (setq output (car outputs)) 
       (setq outputs (cdr outputs))
       (insert output)
      )
     )
    )
    (forward-line)
   )
  )
  (kill-buffer bufin)
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  t
 )
)
(defun v-file-init-alt1-aorvar-helper (root class voice &optional dbg)
 "get aorist varieties
  Returns  list of strings, or nil
  root, class and voice are string
  root is spelled with SLP1 transliteration
 "
(let (err outline outlines)
  (when dbg 
   (fol-msg (format "v-file-init-alt1-aorvar-helper: %s\n"
		  (list root class voice tenses)))
  )
  (setq err nil)
  (condition-case err
   (let (aorvars dhaatu root1 class1 voice1 upasargas outline-pfx )
    (setq root1 (intern root))
    (if (equal voice "a")
     (setq voice1 'P)
     (setq voice1 'A)
    )
    (setq class1 (string-to-number class))
    (setq dhaatu (translate-SLP1-ITRANS root1))
    (setq upasargas nil)
     (setq aorvars (aorist-varieties dhaatu class1 voice1 nil))
     (when (not aorvars)
      (fol-msg (format "v-file-init-alt1-aorvar-helper: No aorist varieties for %s\n"
               (list root1 class1 voice1 )))
     )
     (if (not (listp aorvars)) (setq aorvars (list aorvars)))
     (setq outline-pfx (format ":%s %s %s%s" root "aorvars" class voice ))
      (setq outline (format "%s:%s\n" outline-pfx aorvars))
      (setq outlines (append outlines (list outline))) ; add to end of outlines
     
   
  )
  (error1
   (fol-msg (format "v-file-init-alt1-aorvar helper: error1(%s)\n" err))
   (fol-msg (format "inputs: %s\n" (list root class voice)))
  )
  )
  (if err nil outlines)
 )
)
