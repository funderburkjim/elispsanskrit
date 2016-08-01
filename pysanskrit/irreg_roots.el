; compile the function 'temp', and run it in scratch frame.
; This is adapted from irreg.el
; output is used for Dhatu_irreg_string in init.py

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

