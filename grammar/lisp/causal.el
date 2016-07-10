; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; causal.el  
; begun 08-17-03
; Based upon Kale, p.368 ff

(defun causal-doc-600 ()
 "Kale 600.
   Any root belonging to one of the Conjugational classes
   may have a causal form, which is conjugated like a root of
   the 10th class.
 "
)
(defun causal-doc-601 ()
 "Kale 601.
   The Causal of a root implies that a person or a thing causes
   or makes another person or thing to perform the action, or to
   be in the condition, denoted by the root.  It is also employed,
   sometimes, to convert an intransitive verb into a transitive one.
 "
)
(defun causal-doc-602 ()
 "Kale 602. 
   The Causal Base of a root is formed like that of a root of
   the 10th class.  In the case of roots of the 10th class,
   the causal form is identical with the primitive.  The
   Causal form takes either pada. For example: 
   'budh' has causal base 'bodhay': 'bodhayati, -te': he causes to know
   'kShubh' -> 'kShobhayati' : he shakes or agitates
   'gaN' -> 'gaNayati' : he causes to count
   'nii' -> 'naayayati' : he makes another lead or carry
   'kRi' -> 'kaarayati' : he causes to do
   'kRI' -> 'kaarayati' : he causes to scatter
   'kRIt' -> 'kiirtayati' : he causes to glorify
   Note 1 : the function 'dhaatu-a~Nga-10' in 'gram2.el' does this
            except for the roots in Section 400.
   Note 2 : The function 'class10-base' (in causal.el) includes
            the exceptions in Section 400. 
   Note 3 : They are many cases where this basic method is modified.
            This is detailed in the subsequent sections.
   Note 4 : Class 10 roots are handled by 'class10-base': this
     logic takes precedence over the other categories mentioned in
     subsequent sections regarding the causal.
 "
)
(defun causal-doc-603 ()
 "Kale 603.
  For several roots, the vowel takes its guna substitute:
   1. roots ending in 'am', except
    a. 'am' to go (class = 1) has causal base 'aamay'
       and the root 'am' to be ill (class = 10) has causal base 'aamay',
       since it is class 10
    b.  'kam' (to love)
    c. 'cham' (to eat)
    d. 'sham' when it means 'to see' (class = 10)
    e. 'yam' when it does not mean 'to eat' (Note: this is odd,
       I cannot find 'eat' as a definition for 'yam'.
   2. roots marked with an indicatory 'm' 
     NOTE: In some cases, the choice of 'a' or 'aa' form depends on
     the intended meaning of the causal.  The code as written notes
     such cases by call (indicatory-m1-P dhaatu). Thus, the distinction
     is lost by this code (the 'meaning' is unused).
   3. Six roots with short 'a' have an optional root lengthening
      when not used with a preposition; with a preposition, only
      the short-a form is used.
 "
)
(defun causal-doc-604 ()
 "Kale 604.
  Roots ending in 'aa' insert a 'p' before the [a y].
  Roots that often change to 'aa' (kale-459-P dhaatu class pada )
   do so here, and then insert a 'p' before the [a y].
  For the following roots, the vowel is gunated, then they also
   insert a 'p' before the [a y]
    'ri' (to go)
    'hrii' (to be ashamed)
    'rii'  (to go, to flow)
    'vlii' (to go , to choose)
  
 "
)
(defun causal-doc-605 ()
 "Kale 605.
  a. The following roots insert 'p' after changing their final vowel to 'aa':
   'mi' (throw) , 'mii' (destroy), 'dii' (perish), 'ji' (conquer),
   'krii' (buy)
  b. The following roots shorten their vowel (to 'a') and insert 'p' when
   not preceded by a preposition:
    'kShai' (wane) , 'shraa'(cook), 'shrai' ( cook)
    'j~naa' (to slay = causal meaning)
   NOTE: by #603, j~naa can have to forms (j~naapay, j~napay).
   Thus, the code excludes j~naa from this list
  c. The following roots have an optional form in which the vowel
     is shortened and 'p' added;  the other form changes the final
     vowel to 'aa' and adds 'p':
     'glai' (be weary), 'snaa' (bathe)
 "
)
(defun causal-doc-606 ()
 "Kale 606.
  a. The following insert 'y' instead of 'p' after changing the
     final vowel to 'aa'
     sho (pare, sharpen)  Cho (cut) so (finish)
     hve (call)  vye (cover) ve (weave)
     sai (waste away)
     paa (drink : class = 1)
    b. paa (protect : class = 2) inserts 'l'
     ve (to shake) inserts 'j' after changing vowel to 'aa'
    NOTE: 'shake' is not a meaning of 've' (acc. to Apte).
     Since 've' appears in both parts, I return both forms
 "
)
(defun causal-doc-607 ()
 "Kale 607.
  The roots 'jabh' , 'radh', 'rabh', and 'labh' 
  insert a nasal before their final consonant
 "
)
(defun causal-doc-608 ()
 "Kale 608. The following roots have two forms,
   the first is that constructed by 'class10-base',
   the second ends in [aa y] instead of [a y]
     gup vichCh dhuup paN pan Rit
 "
)
(defun causal-doc-609 ()
 "Kale 609.
   The following roots drop their final vowel before 'ay':
    diidhii vevii daridraa
 "
)
(defun causal-doc-610 ()
 "Kale 610.
   A number of roots form their Causal base anomalously.
   The function 'kale-610' has the details.
 "
)
(defun causal-doc-611 ()
 "Kale 601.
   
 "
)
(defun causal-base1a (dhaatu &optional class pada upasargas Eng-def)
 ; returns a list of symbols
 (mapcar 'sym-without-space 
   (causal-base dhaatu class pada upasargas Eng-def)
 )
)
(defun causal-base (dhaatu &optional class pada upasargas  Eng-def dbg)
 ; returns a list of token arrays
 ; 'pada' is unused.
 ; 'Eng-def' is used via 'class10-base'
 (let (tok b b1 ans)
  (when dbg
   (fol-msg (format "causal-base %s\n" (list dhaatu class pada upasargas  Eng-def)))
  )
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (cond
   ((setq ans (kale-610 dhaatu class upasargas ))) ;anomalous
   ((and (not upasargas) ; no preposition
         (member dhaatu '(vam nam van jval hval hmal))
    )
   ; Kale 603 (a)
    (setq b (list (vconcat (gunate-final-vowel tok) [a y])))
    (setq b1 (class10-base tok Eng-def)) ; lengthen
    (setq ans (append b b1))
   )
   ((member dhaatu '(mi mii dii ji krii))
    ; Kale 605a
    (setq ans (vconcat (substring tok 0 -1) [aa p a y]))
   )
   ((and (not upasargas) ; no preposition
	 (member dhaatu '(kShai shraa shrai)) ; j~naa
    )
    ; Kale 605b
    (setq ans (vconcat (substring tok 0 -1) [a p a y]))
   )
   ((and (not upasargas) ; no preposition
	 (member dhaatu '(glai snaa))
    )
    ; Kale 605c
    (setq b (vconcat (substring tok 0 -1) [a p a y]))
    (setq b1 (vconcat (substring tok 0 -1) [aa p a y]))
    (setq ans (list b1 b)) 
   )
   ((equal dhaatu 've)
    ; Kale 606a,b
    (setq b (vconcat (substring tok 0 -1) [aa y a y]))
    (setq b1 (vconcat (substring tok 0 -1) [aa j a y]))
    (setq ans (list b b1))
   )
   ((or (member dhaatu '(sho Cho so hve vye sai))
	(and (equal dhaatu 'paa) (equal class 1))
    )
    ; Kale 606a.
    (setq ans (vconcat (substring tok 0 -1) [aa y a y]))
   )
   ((and (equal dhaatu 'paa) (equal class 2))
    ; Kale 606b
    (setq ans (vconcat (substring tok 0 -1) [aa l a y]))
   )
   ((member dhaatu '(jabh radh rabh labh))
    ; Kale 607
    (if (equal dhaatu 'radh)
     (setq ans (vconcat (substring tok 0 2) [n] (substring tok -1) [a y]))
     (setq ans (vconcat (substring tok 0 2) [m] (substring tok -1) [a y]))
    )
   )
   ((member dhaatu '(gup vichCh dhuup paN pan Rit))
    ; Kale 608
    (setq b (class10-base tok)) ; a list with 1 elt
    (setq b (solution b))
    (setq b1 (vconcat (substring b 0 -2) [aa y]))
    (setq ans (list b b1))
   )
   ((causal-603-P tok dhaatu)
;    (fol-msg (format "check1: %s %s %s\n" dhaatu class pada))
    (setq b (vconcat (gunate-final-vowel tok) [a y]))
    (setq ans b)
   )
   ((indicatory-m1-P dhaatu)
    ; two forms. Kale 603
    (setq b (list (vconcat (gunate-final-vowel tok) [a y])))
    (setq b1 (class10-base tok Eng-def))
    (setq ans (append b b1))
   )
   ((member dhaatu '(diidhii vevii daridraa))
    ; Kale 609 : To get 'daridraa' right, this precedes next 604
    (setq ans (vconcat (substring tok 0 -1) [a y]))
   )
   ((or (kale-459-P dhaatu class) (equal (substring tok -1) [aa]))
    ; Kale 604.
    (setq ans (vconcat (substring tok 0 -1) [aa p a y]))
   )
   ((member dhaatu '(Ri hrii rii vlii))
    ; Kale 604
    (setq b (gunate-final-vowel tok))
    (setq ans (vconcat b [p a y]))
   )
   ((member dhaatu '(mi mii dii ji krii))
    ; Kale 605a
    (setq ans (vconcat (substring tok 0 -1) [aa p a y]))
   )
   ((equal class 10)
    (setq ans (class10-base tok Eng-def))
   )
   (t ; the 'general' case
    (setq ans (class10-base tok Eng-def))
    (let (ans1)
     (cond
      ((equal dhaatu 'pRI) ; Whitney
       (setq ans1 (list [p uu r a y]))
      )
     )
     (setq ans (append ans ans1))
    )
   )
  )
;  (fol-msg (format "ans=%s\n" ans))
  (if (not (listp ans)) (setq ans (list ans)))
  (setq b ans)
  (setq ans nil)
  (while b
   (setq b1 (car b))
   (setq b (cdr b))
   (setq b1 (causal-adjust b1 dhaatu class pada upasargas Eng-def))
   (setq ans (append ans (list b1)))
  )
  ans
 )
)

(defun causal-adjust (b dhaatu class pada upasargas Eng-def)
 ; b is a token string ending in [a y]. If the previous
 ; letter is 'aa' or 'a', a 'p' is inserted
 (let (ans)
  (if (member  (substring b -3 -2) '([aa] [a]))
   (setq ans (vconcat (substring b 0 -2) [p] (substring b -2)))
   (setq ans b)
  )
  ans
 )
)

(defun causal-603-P (tok dhaatu)
 (let ()
  (cond
   ((equal dhaatu 'am) nil) ; 603 not applicable . 'ans' returned nil
   ((equal dhaatu 'kam) nil)
   ((equal dhaatu 'cham) nil)
;   ((and (equal dhaatu 'sham) (equal class 10)) nil)
   ((equal dhaatu 'sham) nil)
   ((indicatory-m-P dhaatu) t)
   ((and (< 2 (length tok)) (equal (substring tok -2) [a m])) t)
  )
 )
)

(defun indicatory-m-P (dhaatu)
 (indicatory-m-P-1 dhaatu)
)
(defun indicatory-m1-P (dhaatu)
 (member dhaatu '(Chad j~naa sham yam))
)
(defun indicatory-m-P-1 (dhaatu)
 (let (data)
  (setq data '(
   ghaT
   vyath
   prath
   pras ; to spread 
   mrad ; to pound
   svad ; 1 A to destroy, to cut ?
   kSha~nj ; 1 A to go
   dakSh 
   krand
   kland ; 1 A
   tvar
   jvar
   gaD ; 1 P to distill
   heD ; to surround
   vaT
   bhaT ; to speak
   naT ; to dance
   stak ; 1 P to resist
   chak ; 1 P to shine
   kakh ; P to laugh
   rag ; P to doubt
   lag ; P to cling to
   hrag ; to cover
   lag ; to cover
   sag ; to cover
   stag ; to cover
   kag
   ak
   ag ; to move in a zigzag manner
   kaN
   raN ; P to go
   chaN
   shaN
   shraN ; P to give
   shrath
   knath ; ?
   krath
   klath ; all P and meaning 'to injure' , 'to kill'
   chan ; to kill
   van ; to honor
   jval ; to shine
   hval
   hmal ; to shake, to move
   smRi
   dRI ; 1 P to fear
   nRI ; to guide
   shraa ; to cook, to boil
   chal
   laD ; to sport, to loll the tongue
   mad ; to be poor, to reduce
   dhvan
   svan
   jan
   jRI
   knas ; ? to be crooked, to shine
   ra~nj
   ram
   gam
   phaN ; 1 P to go
   kram
  ))
  (member dhaatu data)
 )
)
(defun indicatory-m-P-2 (dhaatu class pada upasargas Eng-def)
  ; Chad ; to live or to be ('Chaadayati' in other senses)
 (let (def)
  (setq def (modify-Eng-def Eng-def))
  (or
   (and (equal dhaatu 'Chad) (member 'live def))
   (and (equal dhaatu 'j~naa) (member 'kill def))
  ) 
 )
)

(defun modify-Eng-def (Eng-def)
 (let (def)
  (if (not (listp Eng-def))
   (setq def (list Eng-def))
   (setq def Eng-def)
  )
  (setq def (flatten def))
  def
 )
)

(defun kale-610 (dhaatu &optional class upasargas)
 "Anomalous causals. The anomalous base(s) is returned,
 either as a token-array, or a list of token-arrays
 'nil' is returned when 'dhaatu', etc are not considered by this function
 "
 (let (dhaatus ans)
  (setq dhaatus '(
   i knuu knuuy kShmaay guuh chi jaagRi
   duSh dhuu prii bhaa
   bhrasj mRij ra~nj ruh laa lii vaa smi vii
   shad sidh sphaay sphur han
   pad pat
  ))
  (cond
   ((not (member dhaatu dhaatus)) nil)
   ((equal dhaatu 'i) '([aa p a y] [aa y a y])
     ; adhi-i adhi-aapay
     ; prati-i praty-aayay
     ; Kale shows [g a m a y] as default causal base ;
     ;  but since Whitney, MW do not, I do not
   )
   ((member dhaatu '(knuu knuuy)) [k n o p a y]) ; cause to sound
   ((equal dhaatu 'kShmaay) [k Sh m aa p a y]) ; cause to tremble
   ((equal dhaatu 'guuh) [g uu h a y]) ; cause to conceal
   ((equal dhaatu 'chi)
    '([ch aa p a y] [ch aa y a y] ; class = 5
      [ch a p a y] [ch a y a y])   ; class = 10
   )
   ((and (equal dhaatu 'chi) (equal class 5)) '([ch aa p a y] [ch aa y a y]))
   ((and (equal dhaatu 'chi) (equal class 10)) '([ch a p a y] [ch a y a y]))
   ((equal dhaatu 'duSh) '([d uu Sh a y] [d o Sh a y]))
     ; cause to sin, corrupt or make depraved
   ((equal dhaatu 'dhuu) [dh uu n a y]) ; cause to shake
   ((equal dhaatu 'prii) [p r ii N a y]) ; cause to please
   ((equal dhaatu 'bhaa) '([bh aa y a y] ; frightens with
			   [bh aa p a y] [bh ii Sh a y] ; inspires fear
			  ))
   ((equal dhaatu 'bhrasj) '([bh a r j a y] [bh r a j j a y])) ; cause to fry
   ((equal dhaatu 'mRij) [m aa r j a y]) ; cause to wipe
   ((equal dhaatu 'ra~nj)
     '([r a ~n j a y] ; dyes or paints, propitiates or satisfies
       [r a j a y] ; hunts deer
      ))
   ((equal dhaatu 'ruh) '([r o h a y] [r o p a y])) ; plant, cause to grow
   ((equal dhaatu 'laa) '([l aa l a y] [l aa p a y])) ; embrace, melt
   ((equal dhaatu 'li) '([l ii n a y] [l aa p a y])) ; embrace, melt
   ((equal dhaatu 'vaa) '([v aa p a y] ; cause to blow or move
			  [v aa j a y] ; shake
			 ))
   ((equal dhaatu 'smi) '([s m aa y a y] [s m aa p a y]))
   ((equal dhaatu 'vii) '([v aa p a y] [v aa y a y]))
   ((equal dhaatu 'shad) '([sh aa t a  y] ; cause to fall, cut down
			   [sh aa d a y] ; cause to go
			  ))
   ((equal dhaatu 'sidh) '([s aa dh a y] ; accomplish or prepare
			   [s e dh a y] ; makes perfect a sacred rite
			   ))
   ((equal dhaatu 'sphaay) [s ph aa v a y]) ; cause to swell
   ((equal dhaatu 'sphur) '([s ph o r a  y] [s ph aa r a y])) ; make tremble
   ((equal dhaatu 'han) [gh aa t a y]) ; cause to strike
   ((equal dhaatu 'pad) [p aa d a y]) ; cause to go
   ((equal dhaatu 'pat) '([p aa t a y] [p a t a y])) ; ref Whitney
  )
 )
)

(defun kale-400-P (dhaatu)
 (if (member dhaatu '(
    agh kath kShap gaN var dhvan mah rach ras rah 
    stan svar pat pad vaT karN chap shrath vyay spRih 
    mRig guN kuh sphuT sukh shaTh paT kal laj vas 
    puT shaTh paT kal laj vas puT aMs kRip))
  t
  nil
 )
)
