; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; itrans.el  
; begun 04-24-02 by ejf
; The following is the description of Itrans coding scheme that
; I use. It is based upon that describe in ITRANS.TXT in the
; ITRANS directory.  
; Following are the encodings of the letters of the Sanskrit alphabet,
; given in alphabetical order.
(defvar single-quote-sym (intern "'"))
(defun temp-vowels ()
; Vowels (dependent and independent):
; -------
; a       
; aa / A        
; i          
; ii / I        
; u       
; uu / U 
; RRi / R^i    
; RRI / R^I     
; LLi / L^i    
; LLI / L^I
; e      
; ai        
; o      
; au         
; aM      
; aH
)
(defun temp-Consonants ()
; Consonants:
; ----------- 
; k     
; kh     
; g     
; gh     
; ~N
; ch    
; Ch     
; j     
; jh     
; ~n
; T     
; Th     
; D     
; Dh     
; N
; t     
; th     
; d     
; dh     
; n
; p     
; ph     
; b     
; bh     
; m
; y     
; r      
; l     
; v
; sh    
; Sh     
; s     
; h    
; x / kSh     
; GY / j~n / dny    
; Anusvara:       .n / M / .m  (dot on top of previous consonant/vowel)
; Avagraha:       .a    (`S' like symbol basically to replace a after o)
; Halant:	.h    (to get half-form of the consonant - no vowel - virama)
; Visarga:        H     (visarga - looks like a colon character)
; Danda:          . Vertical line forming sentence or verse separator
; Space:          <space> space between 'words'
)
; The _TOK names and values are from
; testing/itransrc/parse1b/ItransCon.java
(defun word-list (s)
"Assume s is a string. Return an array
 of strings representing the 'words' of s.
 In this case, 'words' are contiguous 
 sequences of non-spaces. Thus, this is
 probably inappropriate if other
 white-space characters are present in 's'.
"
 (with-temp-buffer
  (insert s)
  (goto-char 1)
  (let (n p1 p2 w ans)
    (setq ans nil)
    (while (< (point) (point-max))
      (skip-chars-forward " ")
      (setq p1 (point))
      (skip-chars-forward "^ ")
      (setq p2 (point))
      (setq w (buffer-substring p1 p2))
      (setq ans (vconcat ans (vector w)))
      )
    ans
    )
  )
)


; (defvar DUMMY_TOK  257)
; (defvar A_TOK  258)
; (defvar AA_TOK  259)
; (defvar I_TOK  260)
; (defvar II_TOK  261)
; (defvar U_TOK  262)
; (defvar UU_TOK  263)
; (defvar RI_TOK  264)
; (defvar RII_TOK  265)
; (defvar LI_TOK  266)
; (defvar LII_TOK  267)
; (defvar AY_TOK  268)
; (defvar AAY_TOK  269)
; (defvar AI_TOK  270)
; (defvar O_TOK  271)
; (defvar OO_TOK  272)
; (defvar AU_TOK  273)
; (defvar AM_TOK  274)
; (defvar AHA_TOK  275)
; (defvar VISARGA_TOK 275   ; synonym for AHA_TOK
; (defvar ANUSVARA_TOK 274) ; synonym for AM_TOK
; (defvar HALF_TOK  276)
; (defvar IMPLICIT_TOK  277)
; (defvar CONJUNCT_TOK  278)

; (defvar KA_TOK  279)
; (defvar KHA_TOK  280)
; (defvar GA_TOK  281)
; (defvar GHA_TOK  282)
; (defvar NGA_TOK  283)
; (defvar CHA_TOK  284)
; (defvar CHHA_TOK  285)
; (defvar JA_TOK  286)
; (defvar JHA_TOK  287)
; (defvar JNA_TOK  288)
; (defvar TTA_TOK  289)
; (defvar TTHA_TOK  290)
; (defvar DDA_TOK  291)
; (defvar DDHA_TOK  292)
; (defvar NNA_TOK  293)
; (defvar TA_TOK  294)
; (defvar THA_TOK  295)
; (defvar DA_TOK  296)
; (defvar DHA_TOK  297)
; (defvar NA_TOK  298)
; (defvar PA_TOK  299)
; (defvar PHA_TOK  300)
; (defvar BA_TOK  301)
; (defvar BHA_TOK  302)
; (defvar MA_TOK  303)
; (defvar YA_TOK  304)
; (defvar YYA_TOK  305)
; (defvar RA_TOK  306)
; (defvar LA_TOK  307)
; (defvar VA_TOK  308)
; (defvar SHA_TOK  309)
; (defvar SHHA_TOK  310)
; (defvar SA_TOK  311)
; (defvar HA_TOK  312)

; (defvar LDA_TOK  313)
; (defvar KSHA_TOK  314)
; (defvar GYA_TOK  315)
; (defvar NNX_TOK  316)
; (defvar NYA_TOK  317)
; (defvar RRA_TOK  318)
; (defvar KADOT_TOK  319)
; (defvar KHADOT_TOK  320)
; (defvar GADOT_TOK  321)
; (defvar DDADOT_TOK  322)
; (defvar DDHADOT_TOK  323)

; (defvar JADOT_TOK  324)
; (defvar PHADOT_TOK  325)

; (defvar RA_HALF_TOK  326)
; ;   (defvar ANUSVARA_TOK  327)
; (defvar CHANDRA_TOK  328)
; (defvar CHANDRA_BN_TOK  329)
; (defvar VIRAAM_TOK  330)
; (defvar AVAGRAHA_TOK  331)
; (defvar SRI_TOK  332)
; (defvar AUM_TOK  333)

; (defvar BLANK_TOK  334)
; (defvar NEWLINE_TOK  335)
; (defvar NOLIG_TOK  336)
; (defvar COMMAND_TOK  337)
; (defvar ENDPREV_TOK  338)
; (defvar DANDA_TOK 338) ; synonym for ENDPREV_TOK
; (defvar IMARKER_TOK  339)


; (defvar ENDMARKER_TOK  352)
; (defvar SETIFM_TOK  353)
; (defvar SETFONT_TOK  354)
; (defvar ERR_TOK 999)
(defun gen-word-list (s &optional wsepin)
"Assume s is a string. Return an array
 of strings representing the 'words' of s.
 In this case, 'words' are contiguous 
 sequences of non-spaces. Thus, this is
 probably inappropriate if other
 white-space characters are present in 's'.
 'wsepin' is also a string.
"
 (with-temp-buffer
  (insert s)
  (goto-char 1)
  (if (not wsepin) (setq wsepin ""))
  (let (n p1 p2 w ans wsep wsep1)
    (setq wsep wsepin)
    (setq wsep1 (concat "^" wsep))
    (setq ans nil)
    (while (< (point) (point-max))
      (skip-chars-forward wsep)
      (setq p1 (point))
      (skip-chars-forward wsep1)
      (setq p2 (point))
      (setq w (buffer-substring p1 p2))
      (setq ans (vconcat ans (vector w)))
      )
    ans
    )
  )
)
(defun reg-word-list (s wordreg)
"Assume 's; is a string composed of words defined
 by the regular expression 'wordreg'.
 Return a list of the words.
"
 (with-temp-buffer
  (insert s)
  (goto-char 1)
  (let (ans s1 p)
   (setq ans nil)
   (while (search-forward-regexp wordreg nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq ans (cons s ans))
   )
;    (setq p (point))
;    (when (< p (point-max))
;     (setq s (buffer-substring p (point-max)))
;     (setq ans (cons s ans))
;    )
   (when ans
    (setq ans (nreverse ans))
    (setq ans (vconcat ans))
   )
   ans
  )
 )
)
(defvar ITRANS_PARSE nil)
(defvar NITRANS_PARSE 256) ;max ascii code expected
(defun parse_INIT_CAP ()
 "This function relates itrans encoding to the
  C-code symbols used by Chopde.
  It is not used within the lisp code, being replaced
  by parse_INIT.
 "
 (setq ITRANS_PARSE (make-vector NITRANS_PARSE nil))
 (aset ITRANS_PARSE ?a [ [ "aa" AA] [ "ai" AI] [ "au" AU] [ "a" A]])
 (aset ITRANS_PARSE ?A [ [ "A" AA]])
 (aset ITRANS_PARSE ?i [ [ "ii" II] [ "i" I]])
 (aset ITRANS_PARSE ?I [ [ "I" II]])
 (aset ITRANS_PARSE ?u [ [ "uu" UU] [ "u" U]])
 (aset ITRANS_PARSE ?U [ [ "U" UU]])
 (aset ITRANS_PARSE ?R [ [ "R^i" RI] [ "RRi" RI] 
		      [ "R^I" RII] [ "RRI" RII] ]) 
 (aset ITRANS_PARSE ?L [ [ "L^i" LI] [ "LLi" LI]
		      [ "L^I" LII] [ "LLI" LII]])
; (aset ITRANS_PARSE ?e [ [ "e" AY]])
 (aset ITRANS_PARSE ?e [ [ "e" E]])
 (aset ITRANS_PARSE ?o [ [ "o" O]])
 (aset ITRANS_PARSE ?H [ [ "H" VISARGA]]) ; same value as AHA
 (aset ITRANS_PARSE ?k [  [ "kh" KH] [ "k" K]])
  ; [ "ksh" KSH]
 (aset ITRANS_PARSE ?g [ [ "gh" GH] [ "g" G]])
 (aset ITRANS_PARSE ?~ [ [ "~N" NG] [ "~n" JN]])
 (aset ITRANS_PARSE ?c [ [ "chh" CHH] [ "ch" CH]])
 (aset ITRANS_PARSE ?C [ [ "Ch" CHH]])
 (aset ITRANS_PARSE ?j [ [ "jh" JH] [ "j" J]])
 (aset ITRANS_PARSE ?T [ [ "Th" TTH] [ "T" TT]])
 (aset ITRANS_PARSE ?D [ [ "Dh" DDH] [ "D" DD]])
 (aset ITRANS_PARSE ?N [ [ "N" NN]])
 (aset ITRANS_PARSE ?t [ [ "th" TH] [ "t" T]])
 (aset ITRANS_PARSE ?d [ [ "dh" DH] [ "d" D]])
 (aset ITRANS_PARSE ?n [ [ "n" N]])

 (aset ITRANS_PARSE ?p [ [ "ph" PH] [ "p" P]])
 (aset ITRANS_PARSE ?b [ [ "bh" BH] [ "b" B]])
 (aset ITRANS_PARSE ?m [ [ "m" M]])

 (aset ITRANS_PARSE ?y [ [ "y" Y]])
 (aset ITRANS_PARSE ?r [ [ "r" R]])
 (aset ITRANS_PARSE ?l [ [ "l" L]])
 (aset ITRANS_PARSE ?v [ [ "v" V]])

 (aset ITRANS_PARSE ?s [ [ "shh" SHH] [ "sh" SH] [ "s" S]])
 (aset ITRANS_PARSE ?S [ [ "Sh" SHH]])
 (aset ITRANS_PARSE ?h [ [ "h" H]])
;(aset ITRANS_PARSE ?x [ [ "x" KSH]])
 (aset ITRANS_PARSE ?x [ [ "x" K SHH]]) ; alternate to "kSh"
; (aset ITRANS_PARSE ?G [ [ "GY" GY])
 (aset ITRANS_PARSE ?G [ ["GY" J JN]])  ; alternate to "j~n"
 (aset ITRANS_PARSE ?M [ [ "M" ANUSVARA]])
 (aset ITRANS_PARSE ?. [ [ ".n" ANUSVARA] [ ".h" VIRAAM]
		      [ ".a" AVAGRAHA] [ "."  DANDA]])
 (aset ITRANS_PARSE ?| [ [ "|" DANDA]])
 't
)
(defun init-transliteration ()
 (init-ITRANS-ordering) ; for sorting
 (ITRANS-init)
 (init-HK-ordering)
 (HK-init)
 (HKMW-init)
 (SLP1-init)
 (AS-init)
 (SLP1-parsers-init)
 (AS-SLP1-parsers-init)
 t
)
(defun SLP1-init ()
 (setq SLP1-alphabet
 (vconcat
 [a A i I u U f F x X e E o O  M H
  k K g G N
  c C j J Y
  w W q Q R
  t T d D n
  p P b B m
  y r l v
  S z s h
 ]
 (vector single-quote-sym)
 ))
 (init-SLP1-ordering)
 (SLP1-PARSE-init)
 (SLP1-parsers-init)
)
(defun ITRANS-init ()
 "initialize ITRANS_PARSE for parsing strings to ITRANS symbols"
 (setq ITRANS-alphabet 
 [a aa i ii u uu Ri RI Li LI e ai o au M H
  k kh g gh ~N
  ch Ch j jh ~n
  T Th D Dh N
  t th d dh n
  p ph b bh m
  y r l v
  sh Sh s
  h
  AVAGRAHA
  ]
 )
 (setq ITRANS_PARSE (make-vector NITRANS_PARSE nil))
 (aset ITRANS_PARSE ?a [ [ "aa" aa] [ "ai" ai] [ "au" au] [ "a" a]])
 (aset ITRANS_PARSE ?A [ [ "A" aa]])
 (aset ITRANS_PARSE ?i [ [ "ii" ii] [ "i" i]])
 (aset ITRANS_PARSE ?I [ [ "I" ii]])
 (aset ITRANS_PARSE ?u [ [ "uu" uu] [ "u" u]])
 (aset ITRANS_PARSE ?U [ [ "U" uu]])
 (aset ITRANS_PARSE ?R [ [ "R^i" Ri] [ "RRi" Ri]  [ "Ri" Ri] 
		      [ "R^I" RI] [ "RRI" RI] [ "RI" RI ]])
 (aset ITRANS_PARSE ?L [ [ "L^i" Li] [ "LLi" Li] [ "Li" Li]
		      [ "L^I" LI] [ "LLI" LI] [ "LI" LI ]])
; (aset ITRANS_PARSE ?e [ [ "e" AY]])
 (aset ITRANS_PARSE ?e [ [ "e" e]])
 (aset ITRANS_PARSE ?o [ [ "o" o]])
 (aset ITRANS_PARSE ?H [ [ "H" H]]) ; same value as AHA
 (aset ITRANS_PARSE ?k [  [ "kh" kh] [ "k" k]])
  ; [ "ksh" KSH]
 (aset ITRANS_PARSE ?g [ [ "gh" gh] [ "g" g]])
 (aset ITRANS_PARSE ?~ [ [ "~N" ~N] [ "~n" ~n]])
 (aset ITRANS_PARSE ?c [ [ "chh" Ch] [ "ch" ch]])
 (aset ITRANS_PARSE ?C [ [ "Ch" Ch]])
 (aset ITRANS_PARSE ?j [ [ "jh" jh] [ "j" j]])
 (aset ITRANS_PARSE ?T [ [ "Th" Th] [ "T" T]])
 (aset ITRANS_PARSE ?D [ [ "Dh" Dh] [ "D" D]])
 (aset ITRANS_PARSE ?N [ [ "N" N]])
 (aset ITRANS_PARSE ?t [ [ "th" th] [ "t" t]])
 (aset ITRANS_PARSE ?d [ [ "dh" dh] [ "d" d]])
 (aset ITRANS_PARSE ?n [ [ "n" n]])

 (aset ITRANS_PARSE ?p [ [ "ph" ph] [ "p" p]])
 (aset ITRANS_PARSE ?b [ [ "bh" bh] [ "b" b]])
 (aset ITRANS_PARSE ?m [ [ "m" m]])

 (aset ITRANS_PARSE ?y [ [ "y" y]])
 (aset ITRANS_PARSE ?r [ [ "r" r]])
 (aset ITRANS_PARSE ?l [ [ "l" l]])
 (aset ITRANS_PARSE ?v [ [ "v" v]])

 (aset ITRANS_PARSE ?s [ [ "shh" Sh] [ "sh" sh] [ "s" s]])
 (aset ITRANS_PARSE ?S [ [ "Sh" Sh]])
 (aset ITRANS_PARSE ?h [ [ "h" h]])
;(aset ITRANS_PARSE ?x [ [ "x" KSH]])
 (aset ITRANS_PARSE ?x [ [ "x" k Sh]]) ; alternate to "kSh"
; (aset ITRANS_PARSE ?G [ [ "GY" GY])
 (aset ITRANS_PARSE ?G [ ["GY" j ~n]])  ; alternate to "j~n"
 (aset ITRANS_PARSE ?M [ [ "M" M]]) ; ANUSVARA
 (aset ITRANS_PARSE ?. [ [ ".n" M] ; ANUSVARA
		       [ ".h" VIRAAM] ; 
		      [ ".a" AVAGRAHA]  ; 
		      [ "."  DANDA ]]); 
 (aset ITRANS_PARSE ?| [[ "|" DANDA]])
 't
)
(defun any-parse-word-string (s transvec)
; single character tokens
; s is a string representing a word in  format indicated by 'transvec',
; function returns a list with two elements: (toks err)
; where toks is an array [ tok1 tok2 ... ] representing the tokens in the
; word
; and err is either nil (meaning no error)
; or something else (so there was an error)
 (let (ans toks err s1 c n i pd lpd j done p s1 n1 i1 nomatch k tok np ip 
       toklist ntransvec warning)
  (setq ntransvec (length transvec))
  (setq toklist nil)
  (setq err nil)
  (setq warning nil)
  (cond
   ((not (stringp s)) (setq err 1))
   ((= (setq n (length s)) 0)  (setq err 2))
   (t
    (setq i 0)
    ;(fol-msg (format "n=%s, s=%s\n" n s))
    (while (and (< i n) (not err))
     (setq c (aref s i)) ; c is an integer, assumed in range 0..ntransvec
     (if (< c ntransvec)
      (setq pd (aref transvec c)) ; the parsing data for this character
      (setq pd nil)
     )
     
     ;(fol-msg (format "c = %s, i=%s,  data = %s\n" c i pd))
     (cond
      ((not pd) (setq err 3)) ; unknown character
      (t
       (setq lpd (length pd))
       (setq j 0)
       (setq done nil)
       (while (and (< j lpd) (not done))
	(setq p (aref pd j))
	(setq s1 (aref p 0))
	(setq np (length p))
;	(setq tok (aref p 1))
	; logic needed for "GY" and "x"
	(setq tok (make-vector (1- np) nil))
	(setq ip 1)
	(while (< ip np)
	 (aset tok (1- ip) (aref p ip))
	 (setq ip (1+ ip))
        )
	(setq n1 (length s1))
	(setq j (1+ j)) ; prepare for rest of loop
	(setq k 0)
	(setq nomatch nil)
	(while (and (< k n1) (not nomatch))
	 (setq i1 (+ i k))
	 (if (<= n i1) (setq nomatch t) ; no chars left in s
	  (setq nomatch (not (= (aref s i1) (aref s1 k))))
	 )
	 (setq k (1+ k))
	)
	(when (not nomatch)
	  ; found a match. reset i , toklist, done
	  (setq done t)
	  (setq i (+ i n1))
	  (if (equal tok [NA])
	   (setq warning t) ; note [NA] is not expanded
	   (setq toklist (vconcat toklist tok))
	  )
	  ;(fol-msg (format "i=%s, toklist=%s\n" i toklist))
	)
       )
       (when (not done)
	 ; no match found - set error
	 (setq err 4)
       )
      )
     )
    )
   )
  )
  (list toklist err warning)
 )
)
(defun ITRANS-parse-word-string (s)
; single character tokens
; s is a string representing a word in ITRANS format,
; function returns a list with two elements: (toks err)
; where toks is an array [ tok1 tok2 ... ] representing the tokens in the
; word
; and err is either nil (meaning no error)
; or something else (so there was an error)
 (any-parse-word-string s ITRANS_PARSE)
)

(defun old-ITRANS-parse-word-string (s)
; single character tokens
; s is a string representing a word in ITRANS format,
; function returns a list with two elements: (toks err)
; where toks is an array [ tok1 tok2 ... ] representing the tokens in the
; word
; and err is either nil (meaning no error)
; or something else (so there was an error)
 (let (ans toks err s1 c n i pd lpd j done p s1 n1 i1 nomatch k tok np ip 
       toklist)
  (setq toklist nil)
  (setq err nil)
  (cond
   ((not (stringp s)) (setq err 1))
   ((= (setq n (length s)) 0)  (setq err 2))
   (t
    (setq i 0)
    ;(fol-msg (format "n=%s, s=%s\n" n s))
    (while (and (< i n) (not err))
     (setq c (aref s i)) ; c is an integer, assumed in range 0..NITRANS_PARSE
     (setq pd (aref ITRANS_PARSE c)) ; the parsing data for this character
     ;(fol-msg (format "c = %s, i=%s,  data = %s\n" c i pd))
     (cond
      ((not pd) (setq err 3)) ; unknown character
      (t
       (setq lpd (length pd))
       (setq j 0)
       (setq done nil)
       (while (and (< j lpd) (not done))
	(setq p (aref pd j))
	(setq s1 (aref p 0))
	(setq np (length p))
;	(setq tok (aref p 1))
	; logic needed for "GY" and "x"
	(setq tok (make-vector (1- np) nil))
	(setq ip 1)
	(while (< ip np)
	 (aset tok (1- ip) (aref p ip))
	 (setq ip (1+ ip))
        )
	(setq n1 (length s1))
	(setq j (1+ j)) ; prepare for rest of loop
	(setq k 0)
	(setq nomatch nil)
	(while (and (< k n1) (not nomatch))
	 (setq i1 (+ i k))
	 (if (<= n i1) (setq nomatch t) ; no chars left in s
	  (setq nomatch (not (= (aref s i1) (aref s1 k))))
	 )
	 (setq k (1+ k))
	)
	(when (not nomatch)
	  ; found a match. reset i , toklist, done
	  (setq done t)
	  (setq i (+ i n1))
	  (setq toklist (vconcat toklist tok))
	  ;(fol-msg (format "i=%s, toklist=%s\n" i toklist))
	)
       )
       (when (not done)
	 ; no match found - set error
	 (setq err 4)
       )
      )
     )
    )
   )
  )
  (list toklist err)
 )
)
(defun vector-droplast (match-val v)
 (let (n)
  (cond
   ((not (vectorp v)) v)
   ; hereafter v is a vector
   ((progn
     (setq n (length v))
     (= 0 n)
    )
    v
   )
   ; hereafter, v is a vector of positive length n
   ((equal (elt v (1- n)) match-val)
    (substring v 0 -1) ; drops last element of vector
   )
   ; otherwise
   (t v)
  )
 )
)
(defun vector-droplast-VIRAAM (v)
 (vector-droplast 'VIRAAM v)
)
(defun ITRANS-parse-words (s)
 (mapcar 'ITRANS-parse-word-string (word-list s))
)
(defun ITRANS-parse-words-1 (s)
 (let (ans1 ans)
  (setq ans1 (mapcar 'car (ITRANS-parse-words s)))
  (setq ans (mapcar 'vector-droplast-VIRAAM ans1))
  ans
 )
)
(defun ITRANS-parse-words-2 (s)
 (apply 'vconcat (ITRANS-parse-words-1 s))
)
(defun ITRANS-parse-words-3 (s)
 (append (ITRANS-parse-words-2 s) nil)
)

; predicates to sort in Sanskrit order
; (sort '(aap ad kamp cham) 'ITRANS-lt-sym) => (cham kamp ad aap)
; EMACS note: the list is sorted 'stably' (i.e., in place) by 'sort'
(defun ITRANS-lt-str (x y)
 (ITRANS-lt-tok (car (ITRANS-parse-words-1 x))
		(car (ITRANS-parse-words-1 y)))
)
(defun ITRANS-lt-sym (x y)
 (ITRANS-lt-str (symbol-name x) (symbol-name y))
)
(defun any-lt-tok (x1 x2 any-ordering)
 (let (ans i n1 n2 c1 c2 n maybe)
  (setq n1 (length x1))
  (setq n2 (length x2))
  (setq i 0)
  (setq n (if (<= n1 n2) n1 n2))
  (setq maybe t)
  (while (and maybe (< i n))
   (cond
    ((equal (elt x1 i) (elt x2 i))) ; no action maybe, maybe not
    ((any-lt-char (elt x1 i) (elt x2 i) any-ordering)
     (setq ans t)
     (setq maybe nil)
    )
    (t (setq ans nil)
       (setq maybe nil)
    )
   )
   (setq i (1+ i))
  )
  (when maybe
   (setq ans (<= n1 n2))
  )
  ans
 )
)
(defun any-lt-char (c1 c2 any-ordering)
 ; c1 and c2 are standard symbols representing Sanskrit characters
 ; in some alphabet, whose 'ordering' (e.g. ITRANS-ordering, SLP1-ordering)
 ; is given in any-ordering
 (let (n1 n2)
  (setq n1 (plist-get  any-ordering c1))
  (setq n2 (plist-get  any-ordering c2))
  (and n1 n2 (< n1 n2))
 )
)
(defun ITRANS-lt-tok (x1 x2)
 (any-lt-tok x1 x2 ITRANS-ordering)
)
(defun ITRANS-lt-char (c1 c2) 
 ; c1 and c2 are standard symbols representing Sanskrit characters
 ; in the ITRANS token string from (car (ITRANS-parse-words-1 x))
 (any-lt-char c1 c2 ITRANS-ordering)
) 
(defun old-ITRANS-lt-char (c1 c2)
 ; c1 and c2 are standard symbols representing Sanskrit characters
 ; in the ITRANS token string from (car (ITRANS-parse-words-1 x))
 (let (n1 n2)
  (setq n1 (plist-get  ITRANS-ordering c1))
  (setq n2 (plist-get  ITRANS-ordering c2))
  (and n1 n2 (< n1 n2))
 )
)

(defvar ITRANS-ordering nil)

(defvar ITRANS-alphabet nil)
(defun init-some-ordering (alphabet)
 (let (data n i x ans)
  (setq data alphabet)
  (setq n (length data))
  (setq ans nil)
  (setq i 0)
  (while (< i n)
   (setq x (elt data i))
   (setq ans (append ans (list x i)))
   (setq i (1+ i))
  )
  ans
 )
)
(defun init-ITRANS-ordering ()
 (setq ITRANS-ordering (init-some-ordering ITRANS-alphabet))
 t
)

(defvar SLP1-alphabet nil)

(defvar SLP1-ordering nil)
(defun ITRANS-SLP1-sym (sym)
 "convert a symbol in the ITRANS alphabet into the corresponding
  symbol in the SLP1 alphabet"
 (let (ans sym1 i)
  (setq i (plist-get ITRANS-ordering sym))
  (when i
   (setq ans (elt SLP1-alphabet i))
  )
  ans
 )
)
(defun init-SLP1-ordering ()
 (setq SLP1-ordering (init-some-ordering SLP1-alphabet))
 t
)

(defun SLP1-parse-words-1 (s)
 (let (ans1 ans)
  (setq ans1 (mapcar 'car (SLP1-parse-words s)))
  (setq ans (mapcar 'vector-droplast-VIRAAM ans1))
  ans
 )
)
(defun SLP1-parse-words (s)
 (mapcar 'SLP1-parse-word-string (word-list s))
)
(defun SLP1-parse-word-string (s)
; single character tokens
; s is a string representing a word in SLP1 format,
; function returns a list with two elements: (toks err)
; where toks is an array [ tok1 tok2 ... ] representing the tokens in the
; word
; and err is either nil (meaning no error)
; or something else (so there was an error)
 (any-parse-word-string s SLP1_PARSE)
)
(defun SLP1-lt-str (x y)
 (SLP1-lt-tok (car (SLP1-parse-words-1 x))
		(car (SLP1-parse-words-1 y)))
)
(defun SLP1-lt-sym (x y)
 (SLP1-lt-str (symbol-name x) (symbol-name y))
)
(defun SLP1-lt-tok (x1 x2)
 (any-lt-tok x1 x2 SLP1-ordering)
)
(defun SLP1-ITRANS-sym (sym)
 "convert a symbol in the SLP1 alphabet into the corresponding
  symbol in the ITRANS alphabet"
 (let (ans sym1 i)
  (setq i (plist-get SLP1-ordering sym))
  (when i
   (setq ans (elt ITRANS-alphabet i))
  )
  ans
 )
)

(defvar HK-alphabet nil)
(defvar HK-ordering nil)
(defun init-HK-ordering ()
 (setq HK-ordering (init-some-ordering HK-alphabet))
 t
)

(defun ITRANS-HK-sym (sym)
 "convert a symbol in the ITRANS alphabet into the corresponding
  symbol in the HK alphabet"
 (let (ans sym1 i)
  (setq i (plist-get ITRANS-ordering sym))
  (when i
   (setq ans (elt HK-alphabet i))
  )
  ans
 )
)
(defun HK-ITRANS-sym (sym)
 "convert a symbol in the HK alphabet into the corresponding
  symbol in the ITRANS alphabet"
 (let (ans sym1 i)
  (setq i (plist-get HK-ordering sym))
  (when i
   (setq ans (elt ITRANS-alphabet i))
  )
  ans
 )
)

(defvar HK_PARSE nil)
(defvar NHK_PARSE 256)

(defun HK-init ()
 "initialize HK-alphabet, HK_PARSE for parsing strings to HK symbols"
 (setq HK-alphabet
 (vconcat 
  [a A i I u U R RR lR lRR e ai o au M H
  k kh g gh G
  c ch j jh J
  T Th D Dh N
  t th d dh n
  p ph b bh m
  y r l v
  z S s
  h
  ]
  (vector single-quote-sym)
 ))
 (setq HK_PARSE (make-vector NHK_PARSE nil))
 (aset HK_PARSE ?a [["ai" ai] ["au" au] [ "a" a] ])
 (aset HK_PARSE ?A [ [ "A" A]])
 (aset HK_PARSE ?i [ [ "i" i]])
 (aset HK_PARSE ?I [ [ "I" I]])
 (aset HK_PARSE ?u [ [ "u" u]])
 (aset HK_PARSE ?U [ [ "U" U]])
 (aset HK_PARSE ?R [ [ "RR" RR] [ "R" R]])
 (aset HK_PARSE ?l [ [ "lRR" lRR] [ "lR" lR] [ "l" l] ])
 (aset HK_PARSE ?e [ [ "e" e]])
 (aset HK_PARSE ?o [ [ "o" o]])
 (aset HK_PARSE ?M [ [ "M" M]]) ; ANUSVARA
 (aset HK_PARSE ?H [ [ "H" H]]) ; visarga

 (aset HK_PARSE ?k [  [ "kh" kh] [ "k" k]])
 (aset HK_PARSE ?g [ [ "gh" gh] [ "g" g]])
 (aset HK_PARSE ?G [ [ "G" G] ])

 (aset HK_PARSE ?c [ [ "ch" ch] [ "c" c]])
 (aset HK_PARSE ?j [ [ "jh" jh] [ "j" j]])
 (aset HK_PARSE ?J [ ["J" J]])

 (aset HK_PARSE ?T [ [ "Th" Th] [ "T" T]])
 (aset HK_PARSE ?D [ [ "Dh" Dh] [ "D" D]])
 (aset HK_PARSE ?N [ [ "N" N]])

 (aset HK_PARSE ?t [ [ "th" th] [ "t" t]])
 (aset HK_PARSE ?d [ [ "dh" dh] [ "d" d]])
 (aset HK_PARSE ?n [ [ "n" n]])

 (aset HK_PARSE ?p [ [ "ph" ph] [ "p" p]])
 (aset HK_PARSE ?b [ [ "bh" bh] [ "b" b]])
 (aset HK_PARSE ?m [ [ "m" m]])

 (aset HK_PARSE ?y [ [ "y" y]])
 (aset HK_PARSE ?r [ [ "r" r]])
 (aset HK_PARSE ?v [ [ "v" v]])

 (aset HK_PARSE ?z [ [ "z" z]])
 (aset HK_PARSE ?s [ [ "s" s]])
 (aset HK_PARSE ?S [ [ "S" S]])
 (aset HK_PARSE ?h [ [ "h" h]])

; all the following characters are noticed, but are not
; viewed as legitimate
 (aset HK_PARSE ?- [ [ "-" NA]]) ; compound separator?
 (aset HK_PARSE ?4 [ [ "4" NA]]) ; accent
 (aset HK_PARSE ?7 [ [ "7" NA]]) ; 'blending of short and long vowel'
 (aset HK_PARSE ?9 [ [ "9" NA]]) ; similar to 7
 (aset HK_PARSE ?0 [ [ "0" NA]]) ; another vowel sandhi
 (aset HK_PARSE ?L [ [ "L" NA]]) ; a 'nasalized' 'l'. 52 instances in index
 (aset HK_PARSE ?@ [ [ "@" NA]]) ; 51 instances in index
 (aset HK_PARSE ?? (vector (vector (char-to-string 2291) 'NA))) ; unknown 7000 instances
 't
)

(defun parse-from-to-construct (parse1 alpha1 alpha2)
 "Given a parsing structure 'parse1' (like HK_PARSE or ITRANS_PARSE)
  and its associated alphabet 'alpha1' (like HK-alphabet or ITRANS-alphabet)
  and another alphabet 'alpha2', a structure 'parse2' is made.  When
  passed as the 2nd argument to 'any-parse-word-string', this translates
  a string written in 'alpha1' to the corresponding list of tokens
  using 'alpha2'.
 "
 (let (ans i n x y n1 i1 x1 y1 order1)
  (setq order1 (init-some-ordering alpha1))
  (setq n (length parse1))
  (setq ans (make-vector n nil))
  (setq i 0)
  (while (< i n)
   ; x is a vector of things like
   ;  x1 = [string sym1 ...] where sym1 is in alpha1
   ; We replace x with y, where y is 
   ; a vector each of whose elements is
   ;  y1 = [string sym2 ...] where 'sym2' is the element in 'alpha1'
   ; corresponding to 'sym1'.
   ; If there is no such element, then 'sym2' is the same as 'sym1'.
   (setq x (elt parse1 i))
   (setq n1 (length x)) 
   (setq i1 0)
   (setq y (make-vector n1 nil))
   (while (< i1 n1)
    (setq x1 (elt x i1))
    ; x1 = [string sym1 ...] sym1 in alpha1
    (let (s x2 j)
     (setq s (elt x1 0))
     ; x2 will be a list
     (setq x2
      (mapcar
       (lambda (sym1)
        (setq j (plist-get order1 sym1))
        (if j (elt alpha2 j) sym1)
       )
       (substring x1 1)
      )
     )
     (setq y1 (vconcat (cons s x2)))
    )
    (aset y i1 y1)
    (setq i1 (1+ i1))
   )
   (aset ans i y)
   (setq i (1+ i))
  )
  ans
 )
)

(defvar HKMW_PARSE nil)
(defvar NHKMW_PARSE 256)

(defun HKMW-init ()
 "initialize HKMW_PARSE for parsing strings to HKMW symbols"
 (setq HKMW_PARSE (make-vector NHKMW_PARSE nil))
 (aset HKMW_PARSE ?a [["ai" ai] ["au" au] [ "a" a] ])
 (aset HKMW_PARSE ?A [ [ "A" A]])
 (aset HKMW_PARSE ?i [ [ "i" i]])
 (aset HKMW_PARSE ?I [ [ "I" I]])
 (aset HKMW_PARSE ?u [ [ "u" u]])
 (aset HKMW_PARSE ?U [ [ "U" U]])
 (aset HKMW_PARSE ?R [ [ "RR" RR] [ "R" R]])
 (aset HKMW_PARSE ?l [ [ "lRR" lRR] [ "lR" lR] [ "l" l] ])
 (aset HKMW_PARSE ?e [ [ "e" e]])
 (aset HKMW_PARSE ?o [ [ "o" o]])
 (aset HKMW_PARSE ?M [ [ "M" M]]) ; ANUSVARA
 (aset HKMW_PARSE ?H [ [ "H" H]]) ; visarga

 (aset HKMW_PARSE ?k [  [ "kh" kh] [ "k" k]])
 (aset HKMW_PARSE ?g [ [ "gh" gh] [ "g" g]])
 (aset HKMW_PARSE ?G [ [ "G" G] ])

 (aset HKMW_PARSE ?c [ [ "ch" ch] [ "c" c]])
 (aset HKMW_PARSE ?j [ [ "jh" jh] [ "j" j]])
 (aset HKMW_PARSE ?J [ ["J" J]])

 (aset HKMW_PARSE ?T [ [ "Th" Th] [ "T" T]])
 (aset HKMW_PARSE ?D [ [ "Dh" Dh] [ "D" D]])
 (aset HKMW_PARSE ?N [ [ "N" N]])

 (aset HKMW_PARSE ?t [ [ "th" th] [ "t" t]])
 (aset HKMW_PARSE ?d [ [ "dh" dh] [ "d" d]])
 (aset HKMW_PARSE ?n [ [ "n" n]])

 (aset HKMW_PARSE ?p [ [ "ph" ph] [ "p" p]])
 (aset HKMW_PARSE ?b [ [ "bh" bh] [ "b" b]])
 (aset HKMW_PARSE ?m [ [ "m" m]])

 (aset HKMW_PARSE ?y [ [ "y" y]])
 (aset HKMW_PARSE ?r [ [ "r" r]])
 (aset HKMW_PARSE ?v [ [ "v" v]])

 (aset HKMW_PARSE ?z [ [ "z" z]])
 (aset HKMW_PARSE ?s [ [ "s" s]])
 (aset HKMW_PARSE ?S [ [ "S" S]])
 (aset HKMW_PARSE ?h [ [ "h" h]])

 ; '@' is used as a space (word separator). At this level of 
 ; parsing, it is ignored.
 (aset HKMW_PARSE ?@ [ [ "@" @]]) ; 51 instances in index. Space char.
 ; The single quote is mentioned in table on p.4 of CDSP as being
 ; part of the transliteration system for MW. It is perhaps not
 ; a standard symbol in Harvard-Kyoto scheme. I think it may be
 ; the 'avagraha' (symbol for missing initial 'a'), as this is also
 ; the use made for "'" in SLP
; (aset HKMW_PARSE ?' [ [ "'" ']]) 
 (aset HKMW_PARSE ?' (vector (vector "'" single-quote-sym)))
; all the following characters are noticed, but are
; skipped in the output
 (aset HKMW_PARSE ?- [ [ "-" NA]]) ; compound separator?
 (aset HKMW_PARSE ?4 [ [ "4" NA]]) ; accent
 (aset HKMW_PARSE ?7 [ [ "7" NA]]) ; 'blending of short and long vowel'
 (aset HKMW_PARSE ?8 [ [ "8" NA]]) ; 
 (aset HKMW_PARSE ?9 [ [ "9" NA]]) ; similar to 7
; (aset HKMW_PARSE ?' [ [ "'" NA]]) 
 (aset HKMW_PARSE ?0 [ [ "0" NA]]) ; another vowel sandhi
 (aset HKMW_PARSE ?L [ [ "L" NA]]) ; a 'nasalized' 'l'. 52 instances in index
 ; [d'243] = circled Sanskrit word expanded 7000 instances
 (aset HKMW_PARSE 243 (vector (vector (char-to-string 243) 'NA))) 
 't
)

(defvar SLP1_PARSE nil)
(defvar NSLP1_PARSE 256)
(defun SLP1-PARSE-init ()
 "initialize SLP1_PARSE for parsing strings to SLP1 symbols.
  All these are 1-letter codes"
 (setq SLP1_PARSE (make-vector NSLP1_PARSE nil))
; the following are in SLP1-alphabet order
 (aset SLP1_PARSE ?a [["a" a]])
 (aset SLP1_PARSE ?A [["A" A]])
 (aset SLP1_PARSE ?i [["i" i]])
 (aset SLP1_PARSE ?I [["I" I]])
 (aset SLP1_PARSE ?u [["u" u]])
 (aset SLP1_PARSE ?U [["U" U]])
 (aset SLP1_PARSE ?f [["f" f]])
 (aset SLP1_PARSE ?F [["F" F]])
 (aset SLP1_PARSE ?R [["R" R]])
 (aset SLP1_PARSE ?x [["x" x]])
 (aset SLP1_PARSE ?X [["X" X]])
 (aset SLP1_PARSE ?e [["e" e]])
 (aset SLP1_PARSE ?E [["E" E]])
 (aset SLP1_PARSE ?o [["o" o]])
 (aset SLP1_PARSE ?O [["O" O]])
 (aset SLP1_PARSE ?M [["M" M]])
 (aset SLP1_PARSE ?H [["H" H]])
 (aset SLP1_PARSE ?k [["k" k]])
 (aset SLP1_PARSE ?K [["K" K]])
 (aset SLP1_PARSE ?g [["g" g]])
 (aset SLP1_PARSE ?G [["G" G]])
 (aset SLP1_PARSE ?N [["N" N]])
 (aset SLP1_PARSE ?c [["c" c]])
 (aset SLP1_PARSE ?C [["C" C]])
 (aset SLP1_PARSE ?j [["j" j]])
 (aset SLP1_PARSE ?J [["J" J]])
 (aset SLP1_PARSE ?Y [["Y" Y]])
 (aset SLP1_PARSE ?w [["w" w]])
 (aset SLP1_PARSE ?W [["W" W]])
 (aset SLP1_PARSE ?q [["q" q]])
 (aset SLP1_PARSE ?Q [["Q" Q]])
 (aset SLP1_PARSE ?R [["R" R]])
 (aset SLP1_PARSE ?t [["t" t]])
 (aset SLP1_PARSE ?T [["T" T]])
 (aset SLP1_PARSE ?d [["d" d]])
 (aset SLP1_PARSE ?D [["D" D]])
 (aset SLP1_PARSE ?n [["n" n]])
 (aset SLP1_PARSE ?p [["p" p]])
 (aset SLP1_PARSE ?P [["P" P]])
 (aset SLP1_PARSE ?b [["b" b]])
 (aset SLP1_PARSE ?B [["B" B]])
 (aset SLP1_PARSE ?m [["m" m]])
 (aset SLP1_PARSE ?y [["y" y]])
 (aset SLP1_PARSE ?r [["r" r]])
 (aset SLP1_PARSE ?l [["l" l]])
 (aset SLP1_PARSE ?v [["v" v]])
 (aset SLP1_PARSE ?S [["S" S]])
 (aset SLP1_PARSE ?z [["z" z]])
 (aset SLP1_PARSE ?s [["s" s]])
 (aset SLP1_PARSE ?h [["h" h]])
 (aset SLP1_PARSE ?' (vector (vector "'" single-quote-sym)))
 (aset SLP1_PARSE ?. [["." DANDA]])
 t
)
(defvar SLP1-ITRANS-parser nil)
(defvar ITRANS-SLP1-parser nil)
(defun SLP1-parsers-init ()
 (setq SLP1-ITRANS-parser ; from SLP1 to ITRANS
  (parse-from-to-construct SLP1_PARSE SLP1-alphabet ITRANS-alphabet)
 )
 (setq ITRANS-SLP1-parser ; from ITRANS to SLP1
  (parse-from-to-construct ITRANS_PARSE ITRANS-alphabet SLP1-alphabet)
 )
 t
)

(defun translate-ITRANS-SLP1 (l)
 (mapcar-LE
  (lambda (x)
   (sym-without-space
    (car
     (any-parse-word-string (symbol-name x) ITRANS-SLP1-parser)
    )
   )
  )
  l
 )
)
(defun translate-SLP1-ITRANS (l)
 (mapcar-LE
  (lambda (x)
   (sym-without-space
    (car
     (any-parse-word-string (symbol-name x) SLP1-ITRANS-parser)
    )
   )
  )
  l
 )
)
(defun translate-string-ITRANS-SLP1 (l)
 (let (ans1 ans)
  (setq ans 
   (mapcar-LE
    (lambda (x)
    (mapconcat 'concat
     (mapcar 'symbol-name
      (let (y z)
       ; Example y = ([D a n a m] 4 nil)
       ; Example y = ([D a n a M j a y a] nil nil) D, etc are symbols
       (setq y (any-parse-word-string x ITRANS-SLP1-parser))
       (setq z (car y))
       (when (elt y 1) ; eg. 4 indicates an error. nil does not
        (setq z (vconcat z [- E R R]))
       )
       ;(fol-msg (format "z=%s\n" z))
       ;(setq zsave z)
      z
      )
     )
     ""
    )
   )
   l
   )
  )
  (if (equal ans "-ERR")
   (setq ans (format "%s-ERR" l))
  )
  ans
 )
)

(defun translate-string-ITRANS-SLP1-a (l)
 "l is a string assumed coded in 'ITRANS' 
  It is translated into SLP1 coding.
  However, hyphens, if present, are retained.
 "
 (let (ans s s1)
  (with-temp-buffer
   (insert (format "%s" l))
   (goto-char 1)
   (while (search-forward-regexp "[^-]+" nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq s1 (translate-string-ITRANS-SLP1 s))
    (replace-match s1 t t nil 0)
   )
   (goto-char 1)
   (setq ans (current-line))
  )
  ans
 )
)
(defun translate-string-ITRANS-SLP1-b (l)
 "l is a string assumed coded in 'ITRANS' 
  It is translated into SLP1 coding.
  However, hyphens and numbers, if present, are retained.
 "
 (let (ans s s1)
  (with-temp-buffer
   (insert (format "%s" l))
   (goto-char 1)
   (while (search-forward-regexp "[^0-9-]+" nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq s1 (translate-string-ITRANS-SLP1 s))
    (replace-match s1 t t nil 0)
   )
   (goto-char 1)
   (setq ans (current-line))
  )
  ans
 )
)
(defun translate-string-SLP1-ITRANS (l)
 (mapcar-LE
  (lambda (x)
    (mapconcat 'concat
     (mapcar 'symbol-name
      (let (y z)
       ; Example y = ([D a n a m] 4 nil)
       ; Example y = ([D a n a M j a y a] nil nil) D, etc are symbols
       (setq y (any-parse-word-string x SLP1-ITRANS-parser))
       (setq z (car y))
       (when (elt y 1) ; eg. 4 indicates an error. nil does not
        (setq z (vconcat z [- E R R]))
       )
       ;(fol-msg (format "z=%s\n" z))
       ;(setq zsave z)
      z
      )
     )
     ""
    )
  )
  l
 )
)

(defun translate-string-SLP1-ITRANS-a (l)
 "l is a string assumed coded in SLP1.
  It is translated into ITRANS coding.
  However, hyphens and spaces, if present, are retained.
 "
 (let (ans s s1)
  (with-temp-buffer
   (insert (format "%s" l))
   (goto-char 1)
   (while (search-forward-regexp "[^- ]+" nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq s1 (translate-string-SLP1-ITRANS s))
    (replace-match s1 t t nil 0)
   )
   (goto-char 1)
   (setq ans (current-line))
  )
  ans
 )
)
(defun translate-buffer-SLP1-ITRANS (buf)
 "translate a buffer from SLP1 to ITRANS.
  The SLP1 part is identified by %{...},
  and is changed to %<...>.
 "
 (let (s s1 savept p1 p2)
  (with-current-buffer buf
   (setq savept (point))
   (goto-char 1)
   (while (search-forward-regexp "%{\\([^}]+\\)}" nil t)
    (setq p1 (match-beginning 0))
    (setq p2 (match-end 0))
    (setq s (buffer-substring (match-beginning 1) (match-end 1)))
    (setq s1 (translate-string-SLP1-ITRANS-a s))
    (goto-char p1)
    (delete-char (- p2 p1))
    (insert (format "%%<%s>" s1))
   )
  (goto-char savept)
  )
  t
 )
)
(defun mapcar-LE (f l)
 "assume function 'f' applies to elements of a certain kind, this kind
  being neither of the list or vector type.
  Assume l is a list of such elements, a vector of such elements,
  or such an element.
  We apply f (recursively) to l.
 "
 (cond
  ((not l) nil)
  ((listp l)
   (mapcar (lambda (x) (mapcar-LE f x)) l)
  )
  ((vectorp l)
   (vconcat (mapcar (lambda (x) (mapcar-LE f x)) l))
  )
  (t ; (symbolp l)
   (funcall f l)
  )
 )
)
(defun make-regexp (x)
 ; x is assumed to be a (non-empty) list of symbols-or-strings.
 ; make a regular expression string representing these alternatives.
 ; e.g. if x = (abc def)
 ; return "\\(abc\\|def\\)"
 (let (ans y)
  (setq y (car x))
  (setq x (cdr x))
  (setq ans "")
  (setq ans (concat ans (format "%s" y)))
  (while x
   (setq y (car x))
   (setq x (cdr x))
   (setq ans (concat ans "\\|"))
   (setq ans (concat ans (format "%s" y)))
  )
  (setq ans (concat "\\(" ans "\\)"))
  ans
 )
)
(defun string-trim (s)
 "removes leading and trailing blanks, if any, from the string 's'.
 "
 (let ()
  (while (and (< 0 (length s)) (equal (elt s 0) ? ))
   (setq s (substring s 1))
  )
  (while (and (< 0 (length s)) (equal (substring s -1) " "))
   (setq s (substring s 0 -1))
  )
  s
 )
)
(defun string-delete-regexp (regexp string) 
 (with-temp-buffer
   (insert string)
   (delete-regexp regexp)
   (buffer-substring (point-min) (point-max))
 )
)

(defun delete-regexp (regexp)
 "Delete all occurences of the regular expression 'regexp' occurring
  in the current buffer.
 "
 (let (schg)
  (setq schg "")
  (save-excursion
   (goto-char 1)
   (while (search-forward-regexp regexp nil t)
    (replace-match schg t t nil 0)
   )
  )
 )
)

(defvar AS_PARSE nil)
(defvar NAS_PARSE 256)

(defun AS-init ()
 "initialize AS-alphabet, AS_PARSE for parsing strings to AS symbols.
  'AS' = 'Anglicized Sanskrit', used within Cologne Digital Sanskrit Project
  for Sanskrit words not embedded in %{}.
 "
 (setq AS-alphabet
 (vconcat 
  [a a1 i i1 u u1 r2 r21 lr2 lr21 e ai o au m2 h2
  k kh g gh n3
  c ch j jh n5
  t2 t2h d2 d2h n2
  t th d dh n
  p ph b bh m
  y r l v
  s3 sh s
  h
  ]
  (vector single-quote-sym)
 ))
 (setq AS_PARSE (make-vector NAS_PARSE nil))
 (aset AS_PARSE ?a [["a1" a1] ["a7" a1]["au" au] ["ai" ai] ["ai7" ai][ "a" a] ])
 (aset AS_PARSE ?A [ [ "A1" a1] ["Au" au] ["Ai" ai]  ["A7" a1] [ "A" a]])
 (aset AS_PARSE ?i [ [ "i1" i1] [ "i7" i1] [ "i" i]])
 (aset AS_PARSE ?I [ [ "I1" i1] [ "I1" I] [ "I" i]])
 (aset AS_PARSE ?u [ [ "u1" u1] [ "u7" u1] [ "u" u]])
 (aset AS_PARSE ?U [ [ "U1" u1] [ "U7" u1] [ "U" u]])
 (aset AS_PARSE ?R [[ "R2i" r2] [ "R2" r2] ["R" r]])
 (aset AS_PARSE ?l [ [ "lr21" lr21] ["lr2" lr2] ["l" l]])
 (aset AS_PARSE ?e [ [ "e7" e] [ "e" e] ])
 (aset AS_PARSE ?E [ [ "E" e]])
 (aset AS_PARSE ?o [ [ "o7" o] [ "o" o] ])
 (aset AS_PARSE ?O [ [ "O" o]])
 (aset AS_PARSE ?H [ [ "H" h]]) 

 (aset AS_PARSE ?k [  [ "kh" kh] [ "k" k]])
 (aset AS_PARSE ?K [  [ "Kh" kh] [ "K" k]])
 (aset AS_PARSE ?g [ [ "gh" gh] [ "g" g]])
 (aset AS_PARSE ?G [ [ "Gh" gh] [ "G" g]])

 (aset AS_PARSE ?c [ [ "ch" ch] [ "c" c]])
 (aset AS_PARSE ?C [ [ "Ch" ch] [ "C" c]])
 (aset AS_PARSE ?j [ [ "jh" jh] [ "j" j]])
 (aset AS_PARSE ?J [ [ "Jh" jh] [ "J" j]])

 (aset AS_PARSE ?T [ [ "T2h" t2h] [ "T2" t2] ["Th" th] ["T" t]])
 (aset AS_PARSE ?D [ ["D2h" d2h] ["D2" d2] [ "Dh" dh] [ "D" d]])
 (aset AS_PARSE ?N [ ["N2" n2] [ "N" n]])

 (aset AS_PARSE ?t [ ["t2h" t2h] ["t2" t2] [ "th" th] [ "t" t]])
 (aset AS_PARSE ?d [ ["d2h" d2h] ["d2" d2] [ "dh" dh] [ "d" d]])
 (aset AS_PARSE ?n [ ["n6" m2] ["n3" n3] ["n5" n5] ["n2" n2] [ "n" n]])

 (aset AS_PARSE ?p [ [ "ph" ph] [ "p" p]])
 (aset AS_PARSE ?P [ [ "Ph" ph] [ "P" p]])
 (aset AS_PARSE ?b [ [ "bh" bh] [ "b" b]])
 (aset AS_PARSE ?B [ [ "Bh" bh] [ "B" b]])
 (aset AS_PARSE ?m [ ["m2" m2] [ "m" m] ])
 (aset AS_PARSE ?M [ [ "M" m]]) 

 (aset AS_PARSE ?y [ [ "y" y]])
 (aset AS_PARSE ?Y [ [ "Y" y]])
 (aset AS_PARSE ?r [ [ "r21" r21] [ "r2i" r2][ "r2" r2] [ "r" r]])
 (aset AS_PARSE ?L [ [ "L" l]])
 (aset AS_PARSE ?v [ [ "v" v]])
 (aset AS_PARSE ?V [ [ "V" v]])

 (aset AS_PARSE ?s [ ["s3" s3] ["s4" s3] ["sh" sh] ["s2" sh]  [ "s" s]])
 (aset AS_PARSE ?S [ ["Sh" sh] ["S2" sh] ["S3" s3] [ "S" s] ])
 (aset AS_PARSE ?h [ [ "h2" H] [ "h" h]])

; all the following characters are noticed, but are not
; viewed as legitimate
 (aset AS_PARSE ?- [ [ "-" NA]]) ; compound separator?
; (aset AS_PARSE ?4 [ [ "4" NA]]) ; accent
; (aset AS_PARSE ?7 [ [ "7" NA]]) ; 'blending of short and long vowel'
; (aset AS_PARSE ?9 [ [ "9" NA]]) ; similar to 7
; (aset AS_PARSE ?0 [ [ "0" NA]]) ; another vowel sandhi
; (aset AS_PARSE ?@ [ [ "@" NA]]) ; 51 instances in index
; (aset AS_PARSE ?? (vector (vector (char-to-string 2291) 'NA))) ; unknown 7000 instances
 't
)
(defvar SLP1-AS-parser nil)
(defvar AS-SLP1-parser nil)
(defun AS-SLP1-parsers-init ()
 (setq SLP1-AS-parser ; from SLP1 to AS
  (parse-from-to-construct SLP1_PARSE SLP1-alphabet AS-alphabet)
 )
 (setq AS-SLP1-parser ; from AS to SLP1
  (parse-from-to-construct AS_PARSE AS-alphabet SLP1-alphabet)
 )
 t
)

(defun translate-AS-SLP1 (l)
 (mapcar-LE
  (lambda (x)
   (sym-without-space
    (let (y)
     (setq y (car (any-parse-word-string (symbol-name x) AS-SLP1-parser)))
     (when t ; dbg
      (fol-msg (format "translate-AS-SLP1: %s\n" y))
     )
     y
    )
   )
  )
  l
 )
)
(defun translate-string-AS-SLP1 (l)
 (mapcar-LE
  (lambda (x)
    (mapconcat 'concat
     (mapcar 'symbol-name
      (let (y z)
       ; Example y = ([D a n a m] 4 nil)
       ; Example y = ([D a n a M j a y a] nil nil) D, etc are symbols
       (setq y (any-parse-word-string x AS-SLP1-parser))
       (setq z (car y))
       (when (elt y 1) ; eg. 4 indicates an error. nil does not
        (setq z (vconcat z [- E R R]))
       )
       ;(fol-msg (format "z=%s\n" z))
       ;(setq zsave z)
      z
      )
     )
     ""
    )
  )
  l
 )
)

(defun translate-string-AS-SLP1-a (l)
 "l is a string assumed coded in 'AS' (Anglicized Sanskrit).
  It is translated into SLP1 coding.
  However, hyphens, if present, are retained.
 "
 (let (ans s s1)
  (with-temp-buffer
   (insert (format "%s" l))
   (goto-char 1)
   (while (search-forward-regexp "[^-]+" nil t)
    (setq s (buffer-substring (match-beginning 0) (match-end 0)))
    (setq s1 (translate-string-AS-SLP1 s))
    (replace-match s1 t t nil 0)
   )
   (goto-char 1)
   (setq ans (current-line))
  )
  ans
 )
)
(defun word-list-lines (txt)
 "txt is a string which may have multiple lines.
  Return a list, with an entry for each line being
  a list of strings representing the words in the line (using word-list).
 "
 (let (l  lines words)
  (with-temp-buffer
   (insert txt)
   (goto-char (point-min))
   (while (< (point) (point-max))
    (setq l (current-line))
    (forward-line)
    (setq words (word-list l))
    (setq words (append words nil)) ; from array to list
;    (fol-msg (format "words=%s\n" words))
    (setq lines (cons words lines ))
   )
  )
  (reverse lines)
 )
)
