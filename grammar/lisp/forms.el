; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; forms.el
; begun 05-28-04
; Uses functions in 'construct.el', to construct inflected forms.
; Verb forms are constructed within directory 'forms/v/'.
; One file within this directory contains the information for 
; one particular root.

; (fol-delete-files "grammar/forms/v" "") 
;(setq tmp (SL-conjtab 'gam 1 'a nil 'ipf nil))
;(maximal-prefix-helper (mapcar 'symbol-name (flatten tmp)) 5)
; 10-10-2015.  Moved defun regexp-lines and regexp-kill-lines functions here
;  from old/mw.el.
; 10-12-2015. s-file-init-alt, s-file-init-alt1
; 10-14-2015  v-file-init-alt1
; 10-14-2015 string-to-int => string-to-number (change obsolete function)
; 10-14-2015 v-file-init-alt1-pre, v-file-oinit-alt1-pre-helper
;            v-file-init-alt1-pre-p
; 10-15-2015 v-file-init-alt1-fut, v-file-init-alt1-prf
; 10-16-2015 v-file-init-alt1-ppfactn  (periphrastic perfect action noun)

(defun doc_v-root-make ()
; 1. v-root.txt
; A choice must be made as to the list of 'dhaatus', or verb roots, and
; this should be contained in its own file 'forms/v-root.txt'. The function
; 'v-root-make' constructs this file, which contains a list of roots in
; SLP1 transliteration, one per line.  At the present, this list is
; constructed from 'construct/dcpforms-MW.txt' (see 'construct-MW-all').
)
(defun doc_v-root-filenames-make ()
; 2. v-root-filenames.txt
; Verb forms for a particular root '%r' are, as a general rule,
; constructed within a file 'forms/v/%r' (eg. 'forms/v/gam').
; There are three reasons for modifying this scheme.
; 1. computer file-name limitations
;    One annoying computer detail is
;    the relation between file names and the case (of letters).
;    In Windows 98, different files require file names that differ in 
;    lower-case name (e.g., the sequence of letters, without regard to case,
;    must be different). Thus, the most obvious ploy, naming the file for
;    a root by its name (as coded into SLP) will not work; for instance,
;    'vid' and 'viD' would not be allowed as separate files.
; 2. different senses of roots.
;    This notion is highlighted in Whitney's 'Roots' book. A few roots
;    have multiple senses. For instance, 'vas' has three senses. Whitney
;    refers to these as '1 vas', '2 vas', '3 vas'.  These senses are
;    not separated by the present system conjugational class; for instance,
;    '1 vas' and '2 vas' both have forms in class 2 and 6.
; 3. multiple citation forms for a given root.
;    Several roots may be cited in more than one way. For instance, the
;    '1 vas' form has alternates 'uC' and 'uz' in Whitney's roots.
; A file records the association between a root name and the
;   file containing the data for the root; 
;   this file is 'forms/v-root-file.txt';
;   it is constructed by 'v-root-filenames-make', 
;   and the file name associated with
;   a root is accessed by 'v-root-filename-get'.  
; 'v-root-filenames.txt' is constructed from 'v-root.txt'.
)
(defun doc_v-file-init1 ()
; 3. construction of v/<root> files
; 3a. (v-file-init1) reads 'v-root-filenames.txt'. From each
;    line, it reads a root with associated file name, and calls
;    (v-file-init1-helper root file) to initialize the 
;     file associated with the root to have only the line 'root : <root>'
)
(defun doc_v-file-init1-file (tab)
; delete all lines in file 'tab' in directory 'forms/v'
; that start with ":". This leaves the 'root' and 'class' lines.
)
(defun doc_v-file-init2a (intab indir)
; 3b. (v-file-init2 intab indir)
;    This file puts class and 'voice' references into the
;    root files, based upon the input file 'intab' in directory 'indir'.
;    The model for 'intab' is one of the files
;    'dcpforms-mw-verb.txt', or 'dcpforms-mw-preverb.txt' in directory
;    'construct'.
;    A typical line is 
;        (BU 1 A <MW=vy-ati-BU,262293,1>) in 'preverb'
;        (heq 1 P <MW=heq,331535,2>) in 'verb'.
;    The line indicates that
;     - the root in question is 'BU' (SLP1 spelling).
;     - the conjugation class is 1
;     - the endings are 'A' = atmanepada.
;       In the output this is coded as 'm' (middle), while 
;       'P' = parasmaipada is coded 'a' (active); this is in accordance
;       with Scharf's notation.  Similarly, 'p' indicates passive endings.
;       So for a given conjugational class, the endings to be computed will
;       be either 'a m p' or 'a p' or 'm p'.
;     - <MW=vy-ati-BU,262293,1> indicates that this lexical form was 
;       associated with the prefixed verb whose entry in the Monier-Williams
;       dictionary was 'vy-ati-BU', at line number 262293, clause 1.
;     - <MW=heq,331535,2> indicates this lexical (class 1 active) was for
;       MW entry 'heq' at line 331535, clause 2.
;    From an input line is constructed a 'class' output line in the root file:
;     class : <class> <voice>
;    As mentioned, <voice> will be either 'a'  or 'm'.
)
(defun doc_v-file-init2b--incomplete ()
; 3b. (v-file-init2 intab indir)
;    This file puts class and 'voice' references into the
;    root files, based upon the input file 'intab' in directory 'indir'.
;    The model for 'intab' is one of the files
;    'dcpforms-mw-verb.txt', or 'dcpforms-mw-preverb.txt' in directory
;    'construct'.
;    A typical line is 
;        (BU 1 A <MW=vy-ati-BU,262293,1>) in 'preverb'
;        (heq 1 P <MW=heq,331535,2>) in 'verb'.
;    The line indicates that
;     - the root in question is 'BU' (SLP1 spelling).
;     - the conjugation class is 1
;     - the endings are 'A' = atmanepada.
;       In the output this is coded as 'm' (middle), while 
;       'P' = parasmaipada is coded 'a' (active); this is in accordance
;       with Scharf's notation.  Similarly, 'p' indicates passive endings.
;       So for a given conjugational class, the endings to be computed will
;       be either 'a m p' or 'a p' or 'm p'.
;     - <MW=vy-ati-BU,262293,1> indicates that this lexical form was 
;       associated with the prefixed verb whose entry in the Monier-Williams
;       dictionary was 'vy-ati-BU', at line number 262293, clause 1.
;     - <MW=heq,331535,2> indicates this lexical (class 1 active) was for
;       MW entry 'heq' at line 331535, clause 2.
;    From an input line is constructed a 'class' output line in the root file:
;     class : <class> <voice> <mw> <rootform>
;    As mentioned, <voice> will be either 'a'  or 'm'.
;    <mw> is just copied from the input record.
;    <rootform> is constructed from the root (or prefixed root) within the
;    <MW> record, according to function 'some-pfx-sandhi-at-hyphens'.
;    (This function is used in constructing the 'LXI' field within MW file.)
;     e.g., (some-pfx-sandhi-at-hyphens "vy-ati-BU") -> "vi-ati-BU";
;    an unhyphenated root is unchanged.
;    Also, a line for the passive form, namely
;     class : <class> p <mw> <rootform>
;    is constructed.  Note that the same passive line could occur if the root
;    has both active and middle endings and the same values for the other
;    fields; this duplication, however, is prohibited.
)
(defun doc_v-root-files ()
 "From the file v-root.txt the file v-root-MW-W.txt was created manually;
  it now has one or two fields in addition to the root.  An 'MW' indicates
  the root as coming from the Monier-Williams dictionary; so almost all
  have this indication.  A 'W' indicates the presence in the Whitney
  book 'The Roots, Verb-Forms, and Primary Derivatives of the Sanskrit
  Language'.  A few roots have only a 'W' indication.
  From this file, by filtering, were created 
   697 : 'v-root-W.txt' contains both MW and W flags. 
   884 'v-root-MW.txt' contains MW flag, but not W flag
    71 'v-root-W-only.txt' contains only W flag
    31 'v-root-W-alt.txt' contains only MW flag, but is 
        noted by Whitney as an alternate spelling of a root in 'v-root-W.txt'.
  At the moment 3 of the 1686 records in v-root-MW-W.txt are unaccounted for.
  The file 'v-root-W.txt' will be the object of most attention, as it 
  contains the most important roots.  
 "
)
(defun doc_v-file-init3 (action intab indir)
; 'intab' and 'indir' specify a file of root names.
; For each root so specified, the associated file 'tab' in forms/v/ is 
; determined by 'v-root-filename-get'. This file is then processed by
; a function determined by 'action', namely the function whose name is
; the concatenation of 'v-file-init3-' with the value of 'action'.
)
(defun doc_v-file-init3-pre (tab)
; For each line of the form 'class : <class> <voice>' (voice=a/m),
; insert 4 conjugation table lines for the tenses pre/ipf/ipv/pop.
; The line constructed has the form:
; :<tense> <class><voice>:<conjugation table>
; where the conjugation table is formatted as an Emacs array with 9 entries. 
; For a few verbs (e.g. gam with class=1) there are two present tense stems
; (e.g. gacC and gam); in these cases multiple lines with the same
; :<tense> <class><voice>: are constructed.
)
(defun doc_v-file-init3-pre-p (tab)
; For each line of the form 'class : <class> <voice>' (voice=a/m),
; insert 4 conjugation table lines for the tenses pre/ipf/ipv/pop
; with passive voice endings.
; Each line constructed has the form:
; :<tense> <class><pvoice>:<conjugation table>
; where the conjugation table is formatted as an Emacs array with 9 entries,
; and where <pvoice> = p.
; For roots with multiple classes, the passive base is normally the same,
; and in such a case, only the first class encountered contributes
; a conjugation table.  A notable exception is those verbs with more than
; one class, one of whose classes is class 10. The passive base is constructed
; differently for class 10 bases.  Note that Whitney apparently shows no
; class 10 examples, all such being considered the causative form.  In fact,
; from this observation it appears that it would be more elegant to follow
; Whitney in this regard, but I have not done this so far.
)
(defun doc_v-file-init3-prespart (tab)
 ; for each line in tab of form ':pre <class><voice>', construct 
 ; three lines, corresponding to the present participle in the three 
 ; genders. These have the form:
 ; :pr<voice>p <class> <gender>:<citation>:<dtab>
 ; where <gender> is one of m/f/n, and
 ;  <citation> is the form by which the participle is cited, and
 ; may appear as an entry in a dictionary like that of Monier-Williams.
 ; This citation form depends on <voice>:
 ; When <voice> = a, use m1s (e.g., 'nayat')
 ; When <voice> = m or p, then use m8s (e.g., gamamAna or gamyamaana) 
)
(defun doc_v-file-init3-ppfactn (tab)
; For each line of the form 'class : <class> <voice>' (voice=a/m),
; (a) test if the periphrastic perfect is applicable for the root in this
;     class, and,
; (b) if it is, construct the associated periphrastic perfect action noun,
;     ending in 'Am'.
; If there are any ppfactn's constructed, insert a single line containing
; them all. The format is
; :ppfactn:[<action nouns>]
;  
)
(defun doc_v-file-init3-inf (tab)
; For each line of the form 'class : <class> <voice>' (voice=a/m),
; try to construct an infinitive.
; If there are any infinitves constructed, insert a single line containing
; them all. The format is
; :inf:[<infinitives>]
;  
)
(defun doc_v-file-init3-abs (tab)
; For each line of the form 'class : <class> <voice>' (voice=a/m),
; try to construct the 'ktvaa' and 'tavya', what Antoine calls the
; indeclineable past participles for use in verbs without/with prefixes.
; These are normally (always?) used in so called 'absolute' constructions,
; hence the notation 'abs' (for 'absolutive')  used by Scharf.
; If there are any absolutives so constructed, insert a single line containing
; them all. The format is
; :abs:[<absolutives>]
;  
)
(defun doc_v-file-init3-prf (tab)
; Active and middle forms are constructed for the reduplicative perfect
; tense, if applicable.  Determination of applicability requires use
; of the classes present in the file.
; Each line constructed has the form:
; :<tense> <multi-class><voice>:<conjugation table>
; The term 'multi-class' has the form '#/#...', based on the classes 
; within the file for which the reduplicative perfect is applicable.
)
(defun doc_v-file-init3-ppp (tab)
; declension tables of past perfect participle (kta or ppp).
; Each line constructed has the form:
; :ppp <gender>:<citation>:<declension table>
; In most cases, there is only one ppp for a root. However,
; in some cases there is more than one form, sometimes depending
; on the class and/or voice. Declension tables in m/f/n are present
; for all variations, but the notation does not show 
; any reason for the multiple items.
; <citation> is like 'gata' 'kfta' (ends in 'a')
)
(defun doc_v-file-init3-pap (tab)
; declension tables of past active participle (based on 'ppp')
; Each line constructed has the form:
; :pap <gender>:<citation>:<declension table>
; any reason for the multiple items.
; <citation> is like 'gatavat' 'kftavat' 
)
(defun doc_v-file-init3-gerundive (tab)
; declension tables of gerundives (potential passive participles)
; Each line constructed has the form:
; :gerundive <gender>:<citation>:<declension table>
; In many cases there is more than one form, sometimes depending
; on the class and/or voice. Declension tables in m/f/n are present
; for all variations, but the notation does not show 
; any reason for the multiple items.
; <citation> is like 'kartavya'  (ends in 'a')
)
(defun doc_v-file-init3-fut  (tab)
; Construct the future (fut) and periphrastic future (pft) and
; conditional (con) and benedictive (ben)
; in both active and middle voices. The passive is believed to be
; the same as the middle voice.
; The class-voice of the underlying root is used only to distinguish
; class 10 and other classes; this is done as with perfect tense.
; The line constructed has the form:
; :<tense> <multi-class><voice>:<conjugation table>
; where the conjugation table is formatted as an Emacs array with 9 entries. 
)

(defun doc_v-file-init-aor (intab indir)
; examples:
;   (v-file-init-aor "v-root-aor.txt" "forms")
;   (v-file-init-aor "v-root-aor-temp.txt" "forms")
;   (form-file-init3-ptrs-helper "muc" "v")
;   (v-file-ptrs "v-ptrs.txt" "forms")
;   (SL-conjtab root1 class voice1 nil 'aor nil)
;   eg: (SL-conjtab 'Dfz 1 'a nil 'aor nil)
; read root and parameters from input file.
; Format of input line is:
; <root> <class> <voice> <aor#>
; where <aor#> is a number from 1 to 7 indicating the type of aorist.
; The aorist is handled this way because of low confidence in the
; general logic.
)
(defun doc_v-file-init-aor1 (tab class voice aornum)
; Construct some aorists, based on parameters
; output line is 
; :aor <aornum> <class><voice>:[table]
)
(defun doc_v-derived ()
 ; Kale (p. 367) mentions four types of 'derivative verbs':
 ; causals, desideratives, frequentatives, and nominal verbs.
 ; Any root may have a causal verb, which is essentially like a 
 ; a class 10 verb.  
)
(defun doc-emacs-array ()
; conjugation tables and declension tables are formatted as Emacs arrays,
; whose elements are either symbols or lists.
; The description of a 2-element array, [x y] will give the details.  
; The array is composed from those characters between
; the beginning left bracket and the right bracket.  In Lisp, the array
; elements can themselves be arrays, so a recursive algorithm is required to
; determine the end of an array. However, in this application, the entries
; are known to be non-arrays, so the end of the array is determined by ']'.
; In this example, the entries, 'x' and 'y', are known to be of two types.
; A simple type is simply a Sanskrit word in SLP format, e.g, a 
; regular expression [a-zA-Z]+.  A multiple type is of the form of an
; Emacs Lisp list of simple types, i.e., x = (x1 x2 x3), where the
; parentheses indicate the bounds of the multiple type, and the Sanskrit
; words x1 x2 x3 (separated by a space) indicate the elements.
; The order of the entries in the conjugation table is
; [3s 3d 3p 2s 2d 2p 1s 1d 1p]
)
(defun doc_v-file-init3-ptrs ()
; This contsructs, or reconstructs, a line 'ptrs : ' in all files
; in directory forms/v.
; in directory 'forms/v'. It is constructed as the 2nd line in the
; file. It lists those word fragments which may be used externally to
; identify an unknown word as one whose form appears in this file.
; 
)
(defun doc_v-file-ptrs (outtab &optional outdir)
; construct file 'outtab' using files in directory forms/v 
; by collecting all pointers. The structure of a line of 'outtab' is:
; <ptr> v/tab
; 
)
(defun doc_s-file-init3-ptrs ()
; performs action of (s-file-init3-ptrs-helper tab) for each
; 'tab' in the directory 'forms/s'.
)
(defun doc_s-file-init3-ptrs-helper (tab)
; This contsructs, or reconstructs, a line 'ptrs : ' in file 'tab'
; in directory 'forms/v'. 
; It lists those word fragments which may be used externally to
; identify an unknown word as one whose form appears in this file
; as a data item.  Data occur in lines beginning with a colon ':'.
; The colon is used to separate fields within the line. The
; last field is assumed to be an array (see 'doc-emacs-array'), and
; it is the words within this array which are used.
)
(defun doc_s-file-ptrs (outtab outdir)
; construct file 'outtab' in directory 'outdir'
; by collecting all pointers in files within directory 'forms/s'.
; The structure of a line of 'outtab' is:
; <ptr> s/tab
; 
)
(defun doc_form-file-ptrs-helper (tab subdir)
 ; Both 'tab' and 'subdir' are strings.
 ; Work in file 'tab' in directory 'forms/<subdir>'.
 ; In this file, search for a line beginning 'ptrs : '.
 ; Assume the rest of the line is an Emacs array [x1 x2 ... ]
 ; Return a list whose elements are the strings comprising this list.
)

(defun v-root-remake0 ()
 (v-root-make) ; 
 (v-root-filenames-make)
)
(defun v-root-remake ()
 (fol-msg (format "%s\n" (current-time-string)))
 (v-file-init1)
; (v-file-init2a "dcpforms-test.txt" "forms")
 (v-file-init2a "dcpforms-mw.txt" "construct")
 (v-file-init3 "pre" "v-root-W.txt" "forms")
 (v-file-init3 "pre-p" "v-root-W.txt" "forms")
 (v-file-init3 "prespart" "v-root-W.txt" "forms")
 (v-file-init3 "ppfactn" "v-root-W.txt" "forms")
 (v-file-init3 "inf" "v-root-W.txt" "forms")
 (v-file-init3 "abs" "v-root-W.txt" "forms")
 (v-file-init3 "prf" "v-root-W.txt" "forms")
 (v-file-init3 "ppp" "v-root-W.txt" "forms")
 (v-file-init3 "pap" "v-root-W.txt" "forms")
 (v-file-init3 "gerundive" "v-root-W.txt" "forms") ; takes a long time
 (v-file-init3 "fut" "v-root-W.txt" "forms")
 (v-file-init-aor "v-root-aor.txt" "forms")

 (v-file-init3-ptrs)
 (v-file-ptrs "v-ptrs.txt" "forms")
 (s-file-init3-ptrs)
 (s-file-ptrs "s-ptrs.txt" "forms")

 (i-file-init3-ptrs)
 (i-file-ptrs "i-ptrs.txt" "forms")

 (vc-file-init3-ptrs)
 (vc-file-ptrs "vc-ptrs.txt" "forms")

 (fol-msg (format "%s\n" (current-time-string)))

; (s-file-init "MW-noun.txt" "inputs")
; (s-file-init "MW-adj.txt" "inputs")
)
(defun vc-root-remake ()
 (fol-msg (format "%s\n" (current-time-string)))
 (vc-file-init1)
; (vc-file-init2a "dcpforms-test.txt" "forms")
; (vc-file-init2a "dcpforms-mw.txt" "construct")
 (vc-root-remake-file "v-root-W.txt")
; (vc-file-init3-ptrs)
; (vc-file-ptrs "vc-ptrs.txt" "forms")

 (fol-msg (format "%s\n" (current-time-string)))
)
(defun vc-root-remake-file (file)
 ; file format like "v-root-W.txt"  or "v-root-w-tmp.txt" in directory "forms"
 ; Example: (vc-root-remake-file "v-root-w-tmp.txt")
  (vc-file-init3 "prefut" file "forms")
  (vc-file-init3 "prespart" file "forms")
  (vc-file-init3 "ppfactn" file "forms")
  (vc-file-init3 "inf" file "forms")
  (vc-file-init3 "abs" file "forms")
; (vc-file-init3 "prf" file "forms")
  (vc-file-init3 "ppp" file "forms")
; (vc-file-init3 "pap" file "forms")
; (vc-file-init3 "gerundive" file "forms") ; takes a long time
; (vc-file-init-aor "v-root-aor.txt" "forms")
; (vc-file-init3-ptrs)
; (vc-file-ptrs "vc-ptrs.txt" "forms")
)
(defun forms-remake-extra ()
 (s-file-init "MW-s-extra.txt" "inputs")
 (i-file-init "MW-i-extra.txt" "inputs")
 (i-file-init "MW-i-extra1.txt" "inputs")
)
(defun v-root-remake1 (tab)
 "remakes (or initialzes) all data for one verb.
  'tab' is the name of the file in directory forms/v/."
 (fol-msg (format "%s\n" (current-time-string)))
 (v-file-init1-file tab)
 (v-file-init3-pre tab)
 (v-file-init3-pre-p tab)
 (v-file-init3-prespart tab)
 (v-file-init3-ppfactn tab)
 (v-file-init3-inf tab)
 (v-file-init3-abs tab)
 (v-file-init3-prf tab)
 (v-file-init3-ppp tab)
 (v-file-init3-pap tab)
 (v-file-init3-gerundive tab) ; takes a long time
 (v-file-init3-fut tab)
 (message "updating pointers in root files...")
 (form-file-init3-ptrs-helper tab "v") ; ptrs for this table
 (v-file-ptrs "v-ptrs.txt" "forms")

 (fol-msg (format "%s\n" (current-time-string)))
)
(defun vc-root-remake1 (root)
 "remakes (or initialzes) all causal data for one verb,
  'root' is a root name in SLP1 form.
  We implement this by creating a line in file 'v-root-w-tmp.txt' in
  directory 'forms' from 'root'. Then the command
  (vc-root-remake-file 'v-root-w-tmp.txt') is run.
  Finally, the pointers for this file are redone, and
  then integrated into 'vc-ptrs.txt'.
 "
 (fol-msg (format "%s\n" (current-time-string)))
 (vc-root-remake1-helper root "v-root-w-tmp.txt")
 (vc-root-remake-file "v-root-w-tmp.txt")
 (message "updating pointers in root files...")
 ; ptrs for this table
 (form-file-init3-ptrs-helper
  (v-root-filename-get root) ; table in forms/vc with this root
  "vc") 
 (vc-file-ptrs "vc-ptrs.txt" "forms")

 (fol-msg (format "%s\n" (current-time-string)))
)
(defun vc-root-remake1-helper (tab tmpfile)
 (let (dir file buf s)
  (setq dir "forms")
  (setq file (sangram-filename tmpfile dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf (erase-buffer))
  (with-current-buffer buf
   (goto-char 1)
   (insert (format "%s MW\n" tab))
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-root-make ()
 "Constructs file 'forms/v-root.txt'.
  Reads all lines in 'construct/dcpforms-MW.txt'; each line has form
  like (yup 4 P <MW=yup,219429,1>).  The root is the first item in
  the list.  Collects the distinct roots in a list, sorts the list,
  and writes this list to the output, one root per line,
  with the the code 'MW' as a second entry.
 "
 (let (intab indir outtab outdir fileout bufout filein bufin roots)
  (setq intab "dcpforms-MW.txt")
  (setq indir "construct")
  (setq outtab "v-root.txt")
  (setq outdir "forms")
  ; initialize output file to be empty
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't))
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (x)
     (setq x (read (current-buffer)))
     (forward-line)
     (setq x (symbol-name (car x)))
     (if (not (member x roots))
      (setq roots (cons x roots))
     )
    )
   )
  )
  ;
;  (setq roots (sort roots 'string<))
  (setq roots (sort roots 'SLP1-lt-str))
  (fol-msg (format "# roots = %s\n" (length roots)))
  (with-current-buffer bufout
   (while roots
    (insert (format "%s MW\n" (car roots)))
    (setq roots (cdr roots))
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufin)
  (kill-buffer bufout)
 )
)
(defun v-root-filenames-make ()
 ; construct 'v-root-filenames.txt' from 'v-root.txt'
 (let (intab indir outtab outdir fileout bufout filein bufin pl)
  (setq intab "v-root.txt")
  (setq indir "forms")
  (setq outtab "v-root-filenames.txt")
  (setq outdir "forms")
  ; initialize output file to be empty
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't))
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (x lx old)
     (setq x (read (current-buffer))) ; root symbol
     (forward-line)
     (setq lx (intern (downcase (symbol-name x)))) ; lower-case symbol
     (setq pl (plist-AppendElt pl lx x))
    )
   )
  )
  (with-current-buffer bufout
   (while pl
    (let (lx xlist x i)
     (setq lx (car pl))
     (setq pl (cdr pl))
     (setq xlist (car pl))
     (setq pl (cdr pl))
     (setq x (car xlist))
     (setq xlist (cdr xlist))
     (insert (format "%s %s\n" x lx))
     (setq i 0)
     (while xlist
      (setq x (car xlist))
      (setq xlist (cdr xlist))
      (setq i (1+ i))
      (insert (format "%s %s%s\n" x lx i))
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufin)
  (kill-buffer bufout)
 )
)
(defun v-root-filename-get (root)
 ; root is a symbol or string.
 ; Returns a string, the filename associated with root in file
 ; v-root-filenames.txt.
 (let (intab indir filein bufin regexp ans)
  (setq regexp (format "^%s " root))
  (setq intab "v-root-filenames.txt")
  (setq indir "forms")
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (setq case-fold-search nil)
   (when (search-forward-regexp regexp nil t)
    (let (p1)
     (setq p1 (point))
     (end-of-line)
     (setq ans (buffer-substring p1 (point)))
    )
   )
  )
  ans
 )
)
(defun v-file-init1 ()
 (let (intab indir filein bufin ans)
  (setq intab "v-root-filenames.txt")
  (setq indir "forms")
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root file words)
     (setq words (word-list (current-line)))
     (setq root (elt words 0))
     (setq file (elt words 1)) ; string
     (v-file-init1-helper root file)
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun v-file-init1-helper (root file)
 (let (outtab outdir fileout bufout)
  (setq outtab file)
  (setq outdir "forms/v")
  ; initialize output file to be empty
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  ; put 'root' line in file
  (with-current-buffer bufout
   (insert (format "root : %s\n" root))
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun v-file-init1-file (file)
 (let (outtab outdir fileout bufout)
  (setq outtab file)
  (setq outdir "forms/v")
  ; initialize output file to be empty
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout 
   (goto-char 1)
   (while (search-forward-regexp "^:" nil t)
    (beginning-of-line)
    (kill-line 1)
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun v-file-init2a (intab indir)
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root file words class pada dict)
     ; line is like (BU 1 A <MW=vy-ati-BU,262293,1>)
     ; but we ignore () and treat as 4 strings.
     (let (p1 p2 s)
      (forward-char) (setq p1 (point))
      (end-of-line) (backward-char) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq class (elt words 1))
     (setq pada (elt words 2))
;     (setq dict (elt words 3))
;     (fol-msg (format "%s\n" words))
     (v-file-init2a-helper root class pada)
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun v-file-init2a-helper (root class pada)
 (let (outtab outdir fileout bufout)
  (setq outtab (v-root-filename-get root))
  (setq outdir "forms/v")
  ; open output file
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (goto-char 1)
   (if (equal (buffer-substring 1 5) "root") ; be sure file is ok
    (let (outline voice )
     ; pada is "P" or "A"
     (setq voice (if (equal pada "P") "a" "m"))
     (setq outline (format "class : %s %s" class voice))
     (when (not (search-forward outline nil t))
      ; add new line at end of file
      (goto-char (point-max))
      (insert (format "%s\n" outline))
     )
    )
    (fol-msg (format "unknown root file: %s %s %s\n" root class pada))
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun v-file-init3 (action intab indir)
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (setq case-fold-search nil)
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root words tab)
     ; line assumed to have root (in SLP form) as first word
     (let (p1 p2 s)
      (setq p1 (point))
      (end-of-line) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq tab (v-root-filename-get root))
     (let (procname procsym)
      (setq procname (format "v-file-init3-%s" action))
      (setq procsym (intern procname))
      (funcall procsym tab)
     )
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun v-file-init3-pre (tab)
 ; conjugations 'pre ipf ipv pop' for specificied class-voice
 (let (dir file buf root cvs tenses)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather cvs
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq cv (list class voice))
     (setq cvs (append cvs (list cv)))
    )
   )
   ; loop thru cvs
   (while cvs
    (let (outline class voice tenses cv)
     (setq cv (car cvs))
     (setq cvs (cdr cvs))
     (setq class (elt cv 0))
     (setq voice (elt cv 1))
     (setq tenses '(pre ipf ipv pop))
     (while tenses
      (let (tense ctabs ctab root1 voice1 tense1 outline-pfx)
       (setq tense (car tenses))
       (setq tenses (cdr tenses))
       (setq root1 (intern root))
       (setq voice1 (intern voice))
       (setq ctabs (SL-conjtab root1 class voice1 nil tense nil))
       (if (not (listp ctabs)) (setq ctabs (list ctabs)))
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format "^:%s %s%s" tense class voice))
       (while (search-forward-regexp outline-pfx nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while ctabs
	(setq ctab (car ctabs))
	(setq ctabs (cdr ctabs))
	(setq outline (format ":%s %s%s:%s\n" tense class voice ctab))
	(insert outline)
	
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-pre-p (tab)
 ; passive in 4 present tenses
 ; With a few exceptions, the passive of the present, imperfect, etc., does
 ; not depend on the conjugational class or 'pada', but only on the root.
 ; This program computes a passive for every class-pada combination, but
 ; only inserts it into the file at the first occurrence, unless there is
 ; a difference.
 (let (dir file buf root ctabs-tenses-all cvs tenses)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather cvs
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq cv (list class voice))
     (setq cvs (append cvs (list cv)))
    )
   )
;   (fol-msg (format "cvs=%s\n" cvs))
   (while cvs
    (let (outline class voice cv ctabs-tenses)
     (setq cv (car cvs))
     (setq cvs (cdr cvs))
;     (fol-msg (format "cv=%s\n" cv))
     (setq class (elt cv 0))
;     (setq voice (elt cv 1))
     (setq voice "p") ; passive
     ; use 2 passes thru tenses
     ; In the first pass, add ctabs for each tense into list ctabs-tenses
     (setq tenses '(pre ipf ipv pop))
     (while tenses
      (let (tense ctabs ctab root1 voice1 tense1)
       (setq tense (car tenses))
       (setq tenses (cdr tenses))
       (setq root1 (intern root))
       (setq voice1 (intern voice))
       (setq ctabs (SL-conjtab root1 class voice1 nil tense nil))
       (if (not (listp ctabs)) (setq ctabs (list ctabs)))
       (setq ctabs-tenses (append ctabs-tenses (list ctabs)))
      )
     )
     ; if ctabs-tenses is not present in ctabs-tenses-all,
     ; make second pass through tenses
     (when (not (member ctabs-tenses ctabs-tenses-all))
      ; save new ctabs-tenses in ctabs-tenses-all
      (setq ctabs-tenses-all (cons ctabs-tenses ctabs-tenses-all))
      ; make 2nd pass thru tenses, adding data
      (setq tenses '(pre ipf ipv pop))
      (while tenses
       (let (tense ctabs ctab root1 voice1 tense1 outline-pfx)
        (setq tense (car tenses))
        (setq tenses (cdr tenses))
        (setq root1 (intern root))
        (setq voice1 (intern voice))
	; move thru ctabs-tenses in parallel to tenses
        (setq ctabs (car ctabs-tenses))
        (setq ctabs-tenses (cdr ctabs-tenses))
        ; delete old lines, if present
        (goto-char 1)
        (setq outline-pfx (format "^:%s %s%s" tense class voice))
        (while (search-forward-regexp outline-pfx nil t)
	 (beginning-of-line)
	 (kill-line 1)
        )
        ; go to end of file, and enter new lines there
        (goto-char (point-max))
        (while ctabs
	 (setq ctab (car ctabs))
	 (setq ctabs (cdr ctabs))
	 (setq outline (format ":%s %s%s:%s\n" tense class voice ctab))
	 (insert outline)
	)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-fut (tab)
 (let (dir file buf root cvs tenses multi-class class10 class0)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather cvs, for which reduplicative perfect is applicable.
   (setq multi-class nil)
   (setq class10 nil)
   (setq class0 nil)
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv dhaatu)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq cv class)
     (if (equal class 10)
      (setq class10 10)
      (if (not class0) (setq class0 class))
     )
     (when (and (not (member class cvs))
		(not (equal class 10))
	   )
      (if multi-class
       (setq multi-class (concat multi-class "/" (format "%s" class)))
       (setq multi-class (format "%s" class))
      )
      (setq cvs (append cvs (list cv)))
     )
    )
   )
   ; artificially set cvs
   (setq cvs nil)
   (if class0
    (setq cvs (cons (list class0 multi-class) cvs))
   )
   (if class10
    (setq cvs (cons (list class10 (format "%s" class10)) cvs))
   )
   (while cvs
    (let (outline class classid voices tense cv tenses)
     (setq cv (car cvs))
     (setq cvs (cdr cvs))
     (setq class (elt cv 0))
     (setq classid (elt cv 1))
     (setq tenses '(fut pft con ben))
     (while tenses
      (setq tense (car tenses))
      (setq tenses (cdr tenses))
      ; 'p' (passive) is same as 'm' (middle) for 'fut pft con ben'
      (setq voices '(a m))  
      (while voices
       (let (voice ctabs ctab root1 voice1 tense1 outline-pfx)
        (setq voice (car voices))
        (setq voices (cdr voices))
        (setq root1 (intern root))
	(setq voice1 voice)
	(setq ctabs (SL-conjtab root1 class voice1 nil tense nil))
	(if (not (listp ctabs)) (setq ctabs (list ctabs)))
        ; delete old lines, if present
        (goto-char 1)
	(setq outline-pfx (format ":%s %s%s:" tense classid voice))
	(while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	 (beginning-of-line)
	 (kill-line 1)
        )
        ; go to end of file, and enter new lines there
        (goto-char (point-max))
	(while ctabs
	 (setq ctab (car ctabs))
	 (setq ctabs (cdr ctabs))
	 (setq outline (format "%s%s\n" outline-pfx ctab))
	 (insert outline)
        )
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-prespart (tab &optional dir1)
 (let (dir file buf root cvs tenses savept)
  (setq join-array-method 1)
  (if (not dir1) (setq dir1 "v"))
  (setq dir (format "forms/%s" dir1))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
    ; allow class code to be eith a number , or 'c' (causal)
   (while (search-forward-regexp ":pre \\([0-9c]+\\)\\([amp]\\):" nil t)
    (let (class0 class voice ctab ctabelt declensions root1 voice1 citation)
     (setq savept (point))
     (setq class0 (buffer-substring (match-beginning 1) (match-end 1)))
     (if (member class0 '("c"))
      (setq class 10)
      (setq class (string-to-number class0))
     )
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
    
     (setq root1 (intern root))
     (setq voice1 (intern voice))
     (setq ctab (read (current-buffer)))
     ; ctab is in SLP1 format. We want to bypass reconstructing conjugation
     ; tables, since the only piece used is th3 3rd pers plur (elt ctab 2).
     (setq ctabelt (elt ctab 2))
     ; in at least one case (vid, class 2), ctabelt is a list at this
     ; point (e.g. (vidanti viduH).
     ; We pick out the first element of the list
     ; Obviously, if there are situations where the words begin differently,
     ; this process will miss the 2nd form.  But it is believed to be absent.
     (if (listp ctabelt) (setq ctabelt (car ctabelt)))
     (setq declensions (SL-prespart-declension root1 class voice1 ctabelt))
     (when declensions
      (let (dtabs dtab gender declension  outline-pfx outline)
       (setq citation "")
       (setq declension (car declensions))
       (setq gender (elt declension 0))
       (setq dtabs (elt declension 1))
       (if (not (listp dtabs)) (setq dtabs (list dtabs)))
       (when (equal gender 'm)
        ; construct citation. Assume 'm' is first in declensions, 
	; so citation is available for all genders
	(let (d0 d1 d2 c)
	 (while dtabs
	  (setq dtab (car dtabs))
	  (setq dtabs (cdr dtabs))
	  (setq d0 (elt dtab 0))
	  (if (equal voice1 'a)
	   (progn
	   ; e.g., d0 = gaman.  Change to gamat
	    (setq d1 (symbol-name d0))
	    (setq d2 (concat (substring d1 0 -1) "t"))
	    (setq citation (concat citation d2))
	   )
	   (progn
	   ; e.g., d0 = gamyamAnaH ->gamyamAna
	   ; or d0 = gamamAnaH ->  gamamAna
	    (setq d1 (symbol-name d0))
	    (setq d2 (substring d1 0 -1))
	    (setq citation (concat citation d2))
	   )
	  )
	 )
	)
       )
      )
     )
     ; construct output lines
     (while declensions
      (let (dtabs dtab gender declension  outline-pfx outline)
       (setq declension (car declensions))
       (setq declensions (cdr declensions))
       (setq gender (elt declension 0))
       (setq dtabs (elt declension 1))
       (if (not (listp dtabs)) (setq dtabs (list dtabs)))
       
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format ":pr%sp %s %s:%s:"
				 voice class0 gender citation))
       (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while dtabs
	(setq dtab (car dtabs))
	(setq dtabs (cdr dtabs))
	(setq outline (format "%s%s\n" outline-pfx dtab))
	(insert outline)
       )
      )
     )
    )
    ; proceed to next present tense
    (goto-char savept)
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-ppfactn (tab &optional dir1)
 (let (dir file buf root dhaatu actns dtype)
  (setq join-array-method 1)
  (cond
   ((equal dir1 "vc") (setq dtype 'c))
   (t (setq dir1 "v"))
  )
  (setq dir (format "forms/%s" dir1))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp
	"root : \\([a-zA-Z]+\\) *\\([a-zA-Z]+\\)?" nil t)
    (progn
     (setq root (buffer-substring (match-beginning 1) (match-end 1)))
     (if (and (match-beginning 2) (match-end 2))
      (setq dtype (buffer-substring (match-beginning 2) (match-end 2)))
      (setq dtype nil)
     )
    )
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather action-nouns, if any
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (class0 class voice voice1 actn root1 a )
     (setq class0 (buffer-substring (match-beginning 1) (match-end 1)))
     (setq class (string-to-number class0))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq voice1 (intern voice))
     (setq root1 (intern root))
     (setq actn (SL-ppfactn root1 class voice1 dtype))
     ; add elements not already present
     (if (not (listp actn)) (setq actn (list actn)))
     (while actn
      (setq a (car actn))
      (setq actn (cdr actn))
      (if (not (member a actns))
       (setq actns (cons a actns))
      )
     )
    )
   )
   ; if any actns are there, construct output line
   (when actns
    (let (outline outline-pfx id)
     (if dtype
      (setq id (format "%s ppfactn" dtype))
      (setq id "ppfactn")
     )
     (setq actns (flatten actns))
     (setq actns (vconcat actns)) ; make an array
     ; delete any lines previously constructed
     (goto-char 1)
     (setq outline-pfx (format ":%s:" id))
     (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
      (beginning-of-line)
      (kill-line 1)
     )
     ; go to end of file, and enter new lines there
     (goto-char (point-max))
     (setq outline (format "%s%s\n" outline-pfx actns))
     (insert outline)
    )
    (save-buffer 0) ; don't make duplicate
   )
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-inf (tab &optional dir1)
 (let (dir file buf root dhaatu actns dtype)
  (setq join-array-method 1)
  (cond
   ((equal dir1 "vc") (setq dtype 'c))
   (t (setq dir1 "v"))
  )
  (setq dir (format "forms/%s" dir1))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather action-nouns, if any
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (class voice voice1 actn root1 a)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq voice1 (intern voice))
     (setq root1 (intern root))
     (setq actn (SL-inf root1 class voice1 dtype))
     ; add elements not already present
     (if (not (listp actn)) (setq actn (list actn)))
     (while actn
      (setq a (car actn))
      (setq actn (cdr actn))
      (if (not (member a actns))
       (setq actns (cons a actns))
      )
     )
    )
   )
   ; if any actns are there, construct output line
   (when actns
    (let (outline outline-pfx id)
     (if dtype
      (setq id (format "%s inf" dtype))
      (setq id "inf")
     )
     (setq actns (flatten actns))
     (setq actns (vconcat actns)) ; make an array
     ; delete any lines previously constructed
     (goto-char 1)
     (setq outline-pfx (format ":%s:" id))
     (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
      (beginning-of-line)
      (kill-line 1)
     )
     ; go to end of file, and enter new lines there
     (goto-char (point-max))
     (setq outline (format "%s%s\n" outline-pfx actns))
     (insert outline)
    )
    (save-buffer 0) ; don't make duplicate
   )
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-abs (tab &optional dir1)
 (let (dir file buf root dhaatu actns dtype)
  (setq join-array-method 1)
  (cond
   ((equal dir1 "vc") (setq dtype 'c))
   (t (setq dir1 "v"))
  )
  (setq dir (format "forms/%s" dir1))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather action-nouns, if any
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (class voice voice1 actn root1 a)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq voice1 (intern voice))
     (setq root1 (intern root))
     (setq actn (SL-abs root1 class voice1 dtype))
     ; add elements not already present
     (if (not (listp actn)) (setq actn (list actn)))
     (while actn
      (setq a (car actn))
      (setq actn (cdr actn))
      (if (not (member a actns))
       (setq actns (cons a actns))
      )
     )
    )
   )
   ; if any actns are there, construct output line
   (when actns
    (let (outline outline-pfx id)
     (setq id "abs")
     (setq actns (flatten actns))
     (setq actns (vconcat actns)) ; make an array
     ; delete any lines previously constructed
     (goto-char 1)
     (setq outline-pfx (format ":%s:" id))
     (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
      (beginning-of-line)
      (kill-line 1)
     )
     (if dtype
      (setq id (format "%s abs" dtype))
      (setq id "abs")
     )
     (setq outline-pfx (format ":%s:" id))
     (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
      (beginning-of-line)
      (kill-line 1)
     )

     ; go to end of file, and enter new lines there
     (goto-char (point-max))
     (setq outline (format "%s%s\n" outline-pfx actns))
     (insert outline)
    )
    (save-buffer 0) ; don't make duplicate
   )
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-prf (tab)
 (let (dir file buf root cvs tenses multi-class)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather cvs, for which reduplicative perfect is applicable.
   (setq multi-class "")
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv dhaatu)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq cv class)
     (setq dhaatu (translate-SLP1-ITRANS (intern root)))
     (when (and (not (member class cvs))
		(reduplicative-liT-P dhaatu class)
	   )
      (if cvs
       (setq multi-class (concat multi-class "/" (format "%s" class)))
       (setq multi-class (format "%s" class))
      )
      (setq cvs (append cvs (list cv)))
     )
    )
   )
   ; use first class mentioned to construct perfect
   (when cvs
    (let (outline class voices tense cv)
     (setq cv (car cvs))
     (setq cvs (cdr cvs))
     (setq class cv)
     (setq tense 'prf)
     (setq voices '(a m))
     (while voices
      (let (voice ctabs ctab root1 voice1 tense1 outline-pfx)
       (setq voice (car voices))
       (setq voices (cdr voices))
       (setq root1 (intern root))
       (setq voice1 voice)
       (setq ctabs (SL-conjtab root1 class voice1 nil tense nil))
       (if (not (listp ctabs)) (setq ctabs (list ctabs)))
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format ":%s %s%s:" tense multi-class voice))
       (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while ctabs
	(setq ctab (car ctabs))
	(setq ctabs (cdr ctabs))
	(setq outline (format "%s%s\n" outline-pfx ctab))
	(insert outline)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-ppp (tab &optional dir1)
 (let (dir file buf root cvs ppps dtype)
  (setq join-array-method 1)
  (cond
   ((equal dir1 "vc") (setq dtype 'c))
   (t (setq dir1 "v"))
  )
  (setq dir (format "forms/%s" dir1))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; calculate ppps, list containing all values of 'ppp' found by
   ; calucating for each class-voice mentioned in file.
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv root1 ppps1 ppp voice1)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq root1 (intern root))
     (setq voice1 (intern voice))
     (setq ppps1 (SL-ppp root1 class voice1 dtype)) ; a list
     (while ppps1
      (setq ppp (car ppps1))
      (setq ppps1 (cdr ppps1))
      (if (not (member ppp ppps)) (setq ppps (append ppps (list ppp))))
     )
    )
   )
   ; delete any lines previously constructed
   (let (outline-pfx id)
    (if dtype
     (setq id (format "%s ppp" dtype))
     (setq id "ppp")
    )
    (setq outline-pfx (format ":%s" id))
    (goto-char 1)
    (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
     (beginning-of-line)
     (kill-line 1)
    )
   )
   ; loop thru ppps
   (while ppps
    (let (outline ppp declensions)
     (setq ppp (car ppps))
     (setq ppps (cdr ppps))
     ; ppp is a symbol, ending in 'a'.
     (setq declensions (SL-ppp-declension ppp))
     (while declensions
      (let (dtabs dtab gender declension  outline-pfx outline id)
       (setq declension (car declensions))
       (setq declensions (cdr declensions))
       (setq gender (elt declension 0))
       (setq dtabs (elt declension 1))
       (if (not (listp dtabs)) (setq dtabs (list dtabs)))
       ;(setq outline-pfx (format ":ppp %s:%s:" gender ppp))
       (if dtype
	(setq id (format "%s ppp" dtype))
        (setq id "ppp")
       )
       (setq outline-pfx (format ":%s %s:%s:" id gender ppp))
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while dtabs
	(setq dtab (car dtabs))
	(setq dtabs (cdr dtabs))
	(setq outline (format "%s%s\n" outline-pfx dtab))
	(insert outline)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-pap (tab)
 (let (dir file buf root cvs ppps)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; calculate ppps, list containing all values of 'ppp' found by
   ; calucating for each class-voice mentioned in file.
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv root1 ppps1 ppp voice1)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq root1 (intern root))
     (setq voice1 (intern voice))
     (setq ppps1 (SL-ppp root1 class voice1)) ; a list
     (while ppps1
      (setq ppp (car ppps1))
      (setq ppps1 (cdr ppps1))
      (if (not (member ppp ppps)) (setq ppps (append ppps (list ppp))))
     )
    )
   )
   ; loop thru ppps
   (while ppps
    (let (outline ppp declensions pap)
     (setq ppp (car ppps))
     (setq ppps (cdr ppps))
     ; ppp is a symbol, ending in 'a'.
     ; citation form is got by concatenating appending 'vat' to ppp
     ; e.g. 'gata' -> 'gatavat'
     (setq pap (sym-concat ppp 'vat))
     (setq declensions (SL-pap-declension ppp))
     (while declensions
      (let (dtabs dtab gender declension  outline-pfx outline)
       (setq declension (car declensions))
       (setq declensions (cdr declensions))
       (setq gender (elt declension 0))
       (setq dtabs (elt declension 1))
       (if (not (listp dtabs)) (setq dtabs (list dtabs)))
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format ":pap %s:%s:" gender pap))
       (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while dtabs
	(setq dtab (car dtabs))
	(setq dtabs (cdr dtabs))
	(setq outline (format "%s%s\n" outline-pfx dtab))
	(insert outline)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-gerundive (tab)
 (let (dir file buf root cvs gerundives)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; calculate gerundives, list containing all values of 'gerundive' found by
   ; calucating for each class-voice mentioned in file.
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv root1 gerundives1 gerundive voice1)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq voice (buffer-substring (match-beginning 2) (match-end 2)))
     (setq root1 (intern root))
     (setq voice1 (intern voice))
     (setq gerundives1 (SL-gerundive root1 class voice1)) ; a list
     (while gerundives1
      (setq gerundive (car gerundives1))
      (setq gerundives1 (cdr gerundives1))
      (if (not (member gerundive gerundives))
       (setq gerundives (append gerundives (list gerundive)))
      )
     )
    )
   )
   ; loop thru gerundives
   (while gerundives
    (let (outline gerundive declensions)
     (setq gerundive (car gerundives))
     (setq gerundives (cdr gerundives))
     ; gerundive is a symbol, ending in 'a'.
     (setq declensions (SL-gerundive-declension gerundive))
     (while declensions
      (let (dtabs dtab gender declension  outline-pfx outline)
       (setq declension (car declensions))
       (setq declensions (cdr declensions))
       (setq gender (elt declension 0))
       (setq dtabs (elt declension 1))
       (if (not (listp dtabs)) (setq dtabs (list dtabs)))
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format ":gerundive %s:%s:" gender gerundive))
       (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while dtabs
	(setq dtab (car dtabs))
	(setq dtabs (cdr dtabs))
	(setq outline (format "%s%s\n" outline-pfx dtab))
	(insert outline)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init-aor (intab indir)
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root class voice aornum words tab)
     ; line format : <root> <class> <voice> <aornum>
     (let (p1 p2 s)
      (setq p1 (point))
      (end-of-line) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq class (string-to-number (elt words 1)))
     (setq voice (intern (elt words 2)))
     (setq aornum (string-to-number (elt words 3)))
     (setq tab (v-root-filename-get root))
     (v-file-init-aor1 tab class voice aornum)
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun v-file-init-aor1 (tab class voice aornum)
 "tab is a string, class and aornum are numbers, voice is a symbol"
 ; (SL-conjtab root1 class voice1 nil 'aor nil)
 (let (dir file buf root cvs tenses multi-class)
  (setq join-array-method 1)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   ; use first class mentioned to construct perfect
   (when root
    (let (outline tense)
     (setq tense 'aor)
     (when t
      (let (alltabs ctabs ctab root1 voice1 outline-pfx)
       (setq root1 (intern root))
       (setq voice1 voice)
       ; alltabs is a list of 7 items, for aornum=1...7
       (setq alltabs (SL-conjtab root1 class voice1 nil tense nil))
       (fol-msg (format "root1=%s, class=%s, voice1=%s, tense=%s\n"
			root1 class voice1 tense))
       (setq ctabs (elt alltabs (- aornum 1)))
       (if (not (listp ctabs)) (setq ctabs (list ctabs)))
       ; delete old lines, if present
       (goto-char 1)
       (setq outline-pfx (format ":%s %s %s%s:" tense aornum class voice))
       (fol-msg (format "chk: %s: alltabs=%s\n" outline-pfx alltabs))
       (while (search-forward-regexp (format "^%s" outline-pfx) nil t)
	(beginning-of-line)
	(kill-line 1)
       )
       ; go to end of file, and enter new lines there
       (goto-char (point-max))
       (while ctabs
	(setq ctab (car ctabs))
	(setq ctabs (cdr ctabs))
	(setq outline (format "%s%s\n" outline-pfx ctab))
	(insert outline)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun v-file-init3-ptrs ()
 (form-file-init3-ptrs "v")
)
(defun v-file-ptrs (outtab outdir)
 (form-file-ptrs outtab outdir "v")
)
(defun form-file-ptrs (outtab outdir subdir)
 (let (filein bufin ans fileout bufout ans dir files file n i s1)
  ; prepare bufout
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  ; 
  (setq dir (file-name-as-directory (sangram-filename subdir "forms")))
  (setq files (non-system-files dir))
  (fol-msg (format "# files = %s\n" (length files)))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (let (ptrs ptr tab ltab lptr)
    (setq tab file)
    (setq ltab (length tab)) ; string length
    (setq ptrs (form-file-ptrs-helper tab subdir))
    (if (arrayp ptrs) (setq ptrs (append ptrs nil))) ; make ptrs a list
    (while ptrs
     (setq ptr (car ptrs))
     (setq ptrs (cdr ptrs))
     (with-current-buffer bufout
	(insert (format "%s %s/%s\n" ptr subdir tab))
     )
    )
   )
  )
  (with-current-buffer bufout
   (sort-lines nil (point-min) (point-max))
   (save-buffer 0)
  )
  (kill-buffer bufout)
  t
 )
)
(defun form-file-ptrs-helper (tab subdir)
 (let (dir file buf root ptrs)
  (setq dir (concat "forms/" subdir)) ; e.g., subdir="v"
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (when (search-forward-regexp "^ptrs : " nil t)
    (let (words d-string data data1 cur-ptrs ptr)
     (setq words (gen-word-list (current-line) ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     (setq ptrs data)
    )
   )
  )
  (kill-buffer buf)
  ptrs
 )
)
(defun parse-dict-ref (dict)
 "dict is a string with form <d=e,l,i>.  
  This function returns the list of substrings (d e l i)
 "
 (let (d e l i words ans)
  (setq words (gen-word-list dict "<=,>"))
  (if (<= 4 (length words)) (setq ans (substring words 0 4)))
  ans
 )
)
(defun v-file-init3-helper (root)
 (let (outtab outdir fileout bufout)
  (setq outtab (v-root-filename-get root))
  (setq outdir "forms/v")
  ; open output file
  (setq fileout (sangram-filename outtab outdir))
  (v-file-init3-helpera fileout)
 )
)
(defun v-file-init3-helpera (fileout)
 (let (bufout)
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  ; put 'root' line in file
  (with-current-buffer bufout
   (goto-char 1)
   (when (equal (buffer-substring 1 5) "root")  ; be sure file is ok
     nil
;    (fol-msg (format "unknown root file: %s %s %s %s\n" root class pada dict))
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun parse-string-to-array (sin)
 "'sin' is a string such as [xx yy (zz ww) uu]. We return a lisp object of
  the same form, whose entries are Lisp strings.
 "
 (let (ans symbol-data)
  (setq symbol-data (car (read-from-string sin)))
  (setq ans (mapcar-LE 'symbol-name symbol-data))
  ans
 )
)

(defun s-work1a (intab outtab &optional indir)
 "intab is like 'MW-noun.txt' in directory 'inputs'.
  We select records from 'intab' and write to 'outtab' in director 'indir'.
  The selection is described in terms of two sample records
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'  (is not selected)
  'ajirIya : S adj : <MW=ajirIya,2839,1>' (is selected).
  The MW record has a hyphenated word, indicating a compound, in the
  unselected record. There is an absence of hyphen in the selected record.
  Thus the selected records are simple in this sense.
 "
 (let (filein bufin fileout bufout outdir)
  (if (not indir) (setq indir "inputs"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (setq outdir indir)
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     (setq line (current-line))
     (setq words (gen-word-list line ":"))
     (setq subanta (elt words 0))
     (setq fg (elt (elt words 1) 1)) ; form or gender
     (setq dict (elt words 2))
     ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
     (setq words (reg-word-list dict "[^ <=,>]+"))
     (setq words1 (elt words 1)) ; "gaRi-mat"
     (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;     (fol-msg (format "%s -> %s -> %s -> %s\n" dict words words1 subwords))
     (setq suffix (elt (substring subwords -1) 0)) ; "mat"
     (when (equal (length subwords) 1)
      (with-current-buffer bufout
       (insert (format "%s\n" line))
      )
     )
    )
    (forward-line)
   )
  )
  (with-current-buffer bufout (save-buffer 0))
;  (kill-buffer bufin)
;  (kill-buffer bufout) 
  t
 )
)

(defun s-work1b (intab intab1 outtab &optional indir)
 "intab is as in s-work1, e.g., 'MW-noun.txt'; 'indir' is similar.
  intab1 is as created by s-work1 (e.g., 'MW-noun-simple.txt').
  Here we examine the records unselected by s-work1, e.g.,
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'  (is not selected)
  We take the suffix (e.g. 'mat') and search for it in 'intab1'.
  If NOT found, we write the record to 'outtab'.  
 "
 (let (filein bufin fileout bufout outdir filein1 bufin1 nchk)
  (setq nchk 0)
  (if (not indir) (setq indir "inputs"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (setq filein1 (sangram-filename intab1 indir))
  (setq bufin1 (find-file-noselect filein1 't)) ; 't suppresses warning
  (setq outdir indir)
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (while (search-forward-regexp "-" nil t)
    (let (line subanta fg dict words words1 subwords suffix)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     (setq line (current-line))
     (setq words (gen-word-list line ":"))
     (setq subanta (elt words 0))
     (setq fg (elt (elt words 1) 1)) ; form or gender
     (setq dict (elt words 2))
     ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
     (setq words (reg-word-list dict "[^ <=,>]+"))
     (setq words1 (elt words 1)) ; "gaRi-mat"
     (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;     (fol-msg (format "%s -> %s -> %s -> %s\n" dict words words1 subwords))
     (setq suffix (elt (substring subwords -1) 0)) ; "mat"
     (when (not (equal (length subwords) 1))
      (with-current-buffer bufin1
       (goto-char 1)
       (when (not (search-forward-regexp (format "^%s " suffix) nil t))
	(with-current-buffer bufout
         (insert (format "%s\n" line))
        )
	(setq nchk (1+ nchk))
	(message (format "%s %s" nchk dict))
       )
      )
     )
    )
    (forward-line)
    (when nil
     (setq nchk (1+ nchk))
     (when (equal (mod nchk 100) 0)
      (message (format "%s" nchk))
     )
    )
   )
  )
  (with-current-buffer bufout (save-buffer 0))
;  (kill-buffer bufin)
;  (kill-buffer bufout) 
  t
 )
)

(defun s-work2a (intab outtab npfx &optional indir)
 "intab is like 'MW-noun.txt' in directory 'inputs'.
  Two sample records are:
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'
  'ajirIya : S adj : <MW=ajirIya,2839,1>'
  It is assumed the records are alphabetized (acc. to Sanskrit alphabet).
  The program keeps track of strings of identical starting substrings
  of length given by 'npfx', and writes these, along with the number of
  occurrences, to the output file. 'identical' means without regard to case,
  e.g., 'hO' is converted to 'ho' before comparing.
 "
 (let (filein bufin fileout bufout outdir)
  (if (not indir) (setq indir "inputs"))
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (setq outdir indir)
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (let (prev nprev cur ntot)
    (setq prev (downcase (buffer-substring (point) (+ (point) npfx))))
    (setq nprev 0)
    (setq ntot 0)
    (while (< (point) (point-max))
     (setq cur (downcase (buffer-substring (point) (+ (point) npfx))))
     (if (equal cur prev)
      (setq nprev (1+ nprev))
      (progn
       (with-current-buffer bufout
	(insert (format "%s %s\n" prev nprev))
       )
       (setq ntot (1+ ntot))
       (message (format "%s %s" ntot prev))
       (setq prev cur)
       (setq nprev 1)
      )
     )
     (forward-line)
    )
    ; do last one
    (with-current-buffer bufout
      (insert (format "%s %s\n" prev nprev))
     )
    (setq ntot (1+ ntot))
   )
  )
  (with-current-buffer bufout (save-buffer 0))
;  (kill-buffer bufin)
;  (kill-buffer bufout) 
  t
 )
)

(defun s-file-init (intab indir &optional outdir)
"intab is like 'MW-noun.txt' in directory 'inputs'.
  Two sample records are:
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'
  'ajirIya : S adj : <MW=ajirIya,2839,1>'
  The output is written to file forms/s/<tab>, where
  <tab> is the 1st two characters of the citation form, in lower-case.
  e.g., 'ga' or 'aj'.
  Output is constructed by routine s-file-init-helper.
  Output files are modified within directory 'outdir' (default is
   'forms/s'), which is a string.
 "
 (let (filein bufin ans dbg)
  (setq dbg t)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix tab npfx err)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     ; data for this line will be written to file forms/s/tab
     (setq npfx 2)
     (setq tab (downcase (buffer-substring (point) (+ (point) npfx))))
     (setq line (current-line))
     (when dbg
      (fol-msg (format "%s line=%s\n" intab line)))
     (condition-case err
      (progn
       (setq words (gen-word-list line ":"))
       (setq subanta (string-trim (elt words 0)))
       (setq fg (elt (reg-word-list (elt words 1) "[^ ]+") 1))
       (setq dict (string-trim (elt words 2)))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq words (reg-word-list dict "[^ <=,>]+"))
       (setq words1 (elt words 1)) ; "gaRi-mat"
;      (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;       (fol-msg (format "entering : %s\n" line))
       (if (not (s-file-init-helper tab subanta fg words1 outdir))
        (fol-msg (format "error@line: %s\n" line))
       )
      )
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
    )
    (forward-line)
   )
  )
  (kill-buffer bufin)
  t
 )
)
(defun s-file-init-alt (intab indir outtab outdir  &optional dbg)
" This is almost identical to s-file-init, EXCEPT, that all output
  goes to one file, namely outdir/outtab.
  intab is like 'MW-noun.txt' in directory 'inputs'.
  Two sample records are:
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'
  'ajirIya : S adj : <MW=ajirIya,2839,1>'
  Output is constructed by routine s-file-init-helper.
 "
 (let (filein bufin ans fileout outputs output nin nout bufout)
  (setq nin 0)
  (setq nout 0)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (if (not outdir) (setq outdir "forms/s"))
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout ; empty fileout, in case it already existed
   (erase-buffer)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix tab npfx err)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     ; data for this line will be written to file forms/s/tab
     (setq outputs nil)
     ;(setq tab (downcase (buffer-substring (point) (+ (point) npfx))))
     (setq line (current-line))
     (setq nin (1+ nin))
     (when dbg
      (fol-msg (format "%s line=%s\n" intab line)))
     (condition-case err
      (progn
       (setq words (gen-word-list line ":"))
       (setq subanta (string-trim (elt words 0)))
       (setq fg (elt (reg-word-list (elt words 1) "[^ ]+") 1))
       (setq dict (string-trim (elt words 2)))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq words (reg-word-list dict "[^ <=,>]+"))
       (setq words1 (elt words 1)) ; "gaRi-mat"
;      (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;       (fol-msg (format "entering : %s\n" line))
       (setq outputs (s-file-init-alt-helper subanta fg words1 dbg))
       (if (not outputs)
        (fol-msg (format "error@line: %s\n" line))
       )
      )
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
     ; append output to bufout
     (when outputs
      (setq nout (1+ nout))
      (when (= (mod nout 1000) 0)
       (with-current-buffer bufout (save-buffer 0))
       (with-current-buffer bufout ; empty fileout, in case it already existed
        (kill-buffer)
       )
       ; reopen fileout in bufout
       (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
       ; move to bottom
       (with-current-buffer bufout 
        (goto-char (point-max))
       )

     )
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
(defun s-file-init-alt1 (intab indir outtab outdir n1 n2 &optional dbg)
" Because of persistent problems with Windows10 file permissions, I added
  n1,n2 to be able to do a segment of cases in intab.   
 This is almost identical to s-file-init, EXCEPT, that all output
  goes to one file, namely outdir/outtab.
  intab is like 'MW-noun.txt' in directory 'inputs'.
  Two sample records are:
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'
  'ajirIya : S adj : <MW=ajirIya,2839,1>'
  Output is constructed by routine s-file-init-helper.
 "
 (let (filein bufin ans fileout outputs output nin nout bufout)
  (setq nin 0)
  (setq nout 0)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (if (not outdir) (setq outdir "forms/s"))
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout ; empty fileout, in case it already existed
   (erase-buffer)
  )
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix tab npfx err)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     ; data for this line will be written to file forms/s/tab
     (setq outputs nil)
     ;(setq tab (downcase (buffer-substring (point) (+ (point) npfx))))
     (setq line (current-line))
     (fol-msg (format "line=%s\n" line))
     (setq nin (1+ nin))
     (when (and (<= n1 nin) (<= nin n2))
     (when dbg
      (fol-msg (format "%s line=%s\n" intab line)))
     (condition-case err
      (progn
       (setq words (gen-word-list line ":"))
       (setq subanta (string-trim (elt words 0)))
       (setq fg (elt (reg-word-list (elt words 1) "[^ ]+") 1))
       (setq dict (string-trim (elt words 2)))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq words (reg-word-list dict "[^ <=,>]+"))
       (setq words1 (elt words 1)) ; "gaRi-mat"
;      (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;       (fol-msg (format "entering : %s\n" line))
       (setq outputs (s-file-init-alt-helper subanta fg words1 dbg))
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
(defun s-file-init-alt-helper (subanta fg mw-word &optional dbg)
 " Returns  list of strings, or nil
  'subanta' is the citation, in string form.
  'fg' is the form or gender of the item, a string.
  'mw-word' the string as cited in MW, e.g. 'gaNi-mat'.
  An array of strings, 'subwords', is formed from 'mw-word',
   e.g. ['gaNi' 'mat']
  A declension is formed using (*) the last word of 'subwords' and 'fg',
  using routine 'SL-construct-subanta1', which returns a list of the
  form 
  '((noun u) (m [dtab]))'
  '((adj a) (m [dtab] f [dtab] n [dtab]))'
  The output is of form:
  :noun u m:<citation>:<mw-word>:[dtab]
  :adj a m:<citation>:<mw-word>:[dtab]
  :adj a f:<citation>:<mw-word>:[dtab]
  :adj a n:<citation>:<mw-word>:[dtab]
  Returns either a string (like above) or nil if there was a problem
  (*) 09-26-04. Usually, the subwords combine to give the subanta;
     e.g., when subanta = 'gaNimat' and mw-word = 'gaNi-mat'.
    However, sometimes this is not the case; for example,
    subanta = 'SreyaskAmatA' and mw-word = 'Sreyas-kAma'
    It is not known why there are such situations; until today,
    it was not known that there even were such situations.  The
    program is modified to (a) notice such instances, and
    (b) to use the entire citation in such instances.
 "
 (let (subwords ans lword err outline outlines)
  (when dbg 
   (fol-msg (format "s-file-init-alt-helper: %s\n"
		  (list subanta fg mw-word)))
  )
  (setq err nil)
  (condition-case err
  (let (forms form gender tmp
	s-type s-form ans1 g dtab dtab1 outline-pfx altsubanta)
   (setq subwords (gen-word-list mw-word "-"))
   (setq altsubanta (apply 'concat (append subwords nil)))
   (when (not (equal subanta altsubanta)) 
    (fol-msg (format "s-file-init-alt-helper WARNING: %s != %s (mw-word=%s)\n"
		     subanta altsubanta mw-word))
    (setq subwords (vector subanta))
   )
   (setq lword (elt (substring subwords -1) 0)) ; string
   ; Feb 28, 2016. Argument 'subanta' in next seems wrong. 
   ; Shouldn't it be 'lword' ?
   (setq forms (s-file-init-genform  subanta fg))
   (when dbg
    (fol-msg (format "forms: %s %s -> %s\n" subanta fg forms)))
   (while forms
    (setq outline nil)
    (setq tmp (car forms))
    (setq forms (cdr forms))
    (setq form (car tmp))
    (setq gender (cadr tmp))
    (when dbg
     (fol-msg (format
	       "s-file-init-helper SL-construct-subanta1: %s %s %s ->%s\n"
		     lword gender form ans))
    )
    (setq ans (SL-construct-subanta1 (intern lword) gender form dbg))
    (when dbg
     (fol-msg (format
	       "s-file-init-helper SL-construct-subanta1: %s %s %s ->%s\n"
		     lword gender form ans))
    )
    (when ans
     (setq s-type (elt (elt ans 0) 0)) ; adj
     (setq s-form (elt (elt ans 0) 1)) ; a
     (setq ans1 (elt ans 1)) ; [m dtab ...]
     (while ans1
      (setq g (car ans1))
      (setq ans1 (cdr ans1))
      (setq dtab (car ans1))
      (setq ans1 (cdr ans1))
      (when dtab
       (setq dtab1 (s-file-init-helper1 subwords dtab))
       (when dbg
        (fol-msg (format " dtab=%s\n dtab1=%s\n" dtab dtab1))
       )
       (setq outline-pfx (format ":%s %s %s:%s:" s-type s-form g subanta))
       (setq outline
	   (format "%s%s:%s\n" outline-pfx  mw-word dtab1))
       (setq outlines (append outlines (list outline))) ; add to end of outlines
      )
      (when (not dtab)
       (fol-msg (format "Warning %s %s %s: %s %s\n"
	 subanta fg mw-word (elt ans 0) g))
      )
     )
    )
   )
  )
  (error1
   (fol-msg (format "error1(%s)\n" err))
   (fol-msg (format "inputs: %s\n" (list subanta fg mw-word)))
  )
  )
  (if err nil outlines)
 )
)
(defun s-file-init-helper (outtab subanta fg mw-word &optional outdir dbg)
 "'outtab' is a string, specifying a file in directory 'forms/s'
  'subanta' is the citation, in string form.
  'fg' is the form or gender of the item, a string.
  'mw-word' the string as cited in MW, e.g. 'gaNi-mat'.
  An array of strings, 'subwords', is formed from 'mw-word',
   e.g. ['gaNi' 'mat']
  A declension is formed using (*) the last word of 'subwords' and 'fg',
  using routine 'SL-construct-subanta1', which returns a list of the
  form 
  '((noun u) (m [dtab]))'
  '((adj a) (m [dtab] f [dtab] n [dtab]))'
  The output is of form:
  :noun u m:<citation>:<mw-word>:[dtab]
  :adj a m:<citation>:<mw-word>:[dtab]
  :adj a f:<citation>:<mw-word>:[dtab]
  :adj a n:<citation>:<mw-word>:[dtab]

  (*) 09-26-04. Usually, the subwords combine to give the subanta;
     e.g., when subanta = 'gaNimat' and mw-word = 'gaNi-mat'.
    However, sometimes this is not the case; for example,
    subanta = 'SreyaskAmatA' and mw-word = 'Sreyas-kAma'
    It is not known why there are such situations; until today,
    it was not known that there even were such situations.  The
    program is modified to (a) notice such instances, and
    (b) to use the entire citation in such instances.
 "
 (let (fileout bufout subwords ans lword err)
  (when dbg 
   (fol-msg (format "s-file-init-helper: %s\n"
		  (list outtab subanta fg mw-word)))
  )
  (if (not outdir) (setq outdir "forms/s"))
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (setq err nil)
  (condition-case err
  (let (forms form gender tmp
	s-type s-form ans1 g dtab dtab1 outline-pfx outline altsubanta)
   (setq subwords (gen-word-list mw-word "-"))
   (setq altsubanta (apply 'concat (append subwords nil)))
   (when (not (equal subanta altsubanta))
    (fol-msg (format "s-file-init-helper WARNING: %s != %s (mw-word=%s)\n"
		     subanta altsubanta mw-word))
    (setq subwords (vector subanta))
   )
   (setq lword (elt (substring subwords -1) 0)) ; string
   (setq forms (s-file-init-genform  subanta fg))
   (when dbg
    (fol-msg (format "forms: %s %s -> %s\n" subanta fg forms)))
   (while forms
    (setq tmp (car forms))
    (setq forms (cdr forms))
    (setq form (car tmp))
    (setq gender (cadr tmp))
    (when dbg
     (fol-msg (format
	       "s-file-init-helper SL-construct-subanta1: %s %s %s ->%s\n"
		     lword gender form ans))
    )
    (setq ans (SL-construct-subanta1 (intern lword) gender form))
    (when dbg
     (fol-msg (format
	       "s-file-init-helper SL-construct-subanta1: %s %s %s ->%s\n"
		     lword gender form ans))
    )
    (when ans
     (setq s-type (elt (elt ans 0) 0)) ; adj
     (setq s-form (elt (elt ans 0) 1)) ; a
     (setq ans1 (elt ans 1)) ; [m dtab ...]
     (while ans1
      (setq g (car ans1))
      (setq ans1 (cdr ans1))
      (setq dtab (car ans1))
      (setq ans1 (cdr ans1))
      (when dtab
       (setq dtab1 (s-file-init-helper1 subwords dtab))
       (when dbg
        (fol-msg (format " dtab=%s\n dtab1=%s\n" dtab dtab1))
       )
       (with-current-buffer bufout
        (setq outline-pfx (format ":%s %s %s:%s:" s-type s-form g subanta))
        (goto-char 1)
	(setq case-fold-search nil)
        (when (search-forward-regexp (format "^%s" outline-pfx) nil t)
	 (beginning-of-line)
	 (kill-line 1)
        )
        (goto-char (point-max))
        (setq outline
	   (format "%s%s:%s\n" outline-pfx  mw-word dtab1))
        (insert outline)
	(when nil ; dbg
         (fol-msg outline)
	)
       )
      )
      (when (not dtab)
       (fol-msg (format "Warning2 %s %s %s: %s %s\n"
	 subanta fg mw-word (elt ans 0) g))
      )
     )
    )
   )
  )
  (error1
   (fol-msg (format "error1(%s)\n" err))
   (fol-msg (format "fileout=%s\n" fileout))
   (fol-msg (format "inputs: %s\n" (list outtab subanta fg mw-word)))
  )
  )
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  (if err nil t)
 )
)
(defun s-file-init-helper1 (subwords dtab)
 (let (pfx ans subwords1)
  (setq subwords1 (substring subwords 0 -1)) ; remove last word
  (setq subwords1 (append subwords1 nil)) ; make a list
  (if subwords1
   (setq pfx (apply 'concat subwords1)) ; form prefix
   (setq pfx "")
  )
  (setq ans ; concatenate prefix to each dtab entry
   (mapcar-LE
    (lambda (x)
     (concat pfx (symbol-name x))
    )
    dtab
   )
  )
  (setq ans (vconcat ans)) ; array form
  ans
 )
)

(defun s-file-init-genform (subanta fg)
 "subanta and fg are strings, subanta in SLP format,
  fg is a subanta gender or type-specifier.
  Returned is a list of pairs, each of the form (<f> <g>), where
  <g> and <f> are Elisp symbols,
  <g> is a gender (cap M/F/N), and 
  <f> is a subanta type (a,u,vat, etc).
  Each such pair is appropriate for calls:
   (SL-construct-subanta1 <subanta> <g> <f>)
 "
 (let (ans tmp1 isubanta lexinfo forms form gender tmp ans formx)
 ;(fol-msg (format "s-file-init-genform: %s\n" (list subanta fg)))
   (setq tmp1 fg) ; a string
   (setq isubanta (translate-SLP1-ITRANS (intern subanta)))
   (setq lexinfo (list 'S (intern tmp1)))
   (setq forms (convert-subanta-lexinfo isubanta lexinfo))
   (setq ans nil)
   (while forms
    (setq formx (car forms))
    (setq forms (cdr forms))
    (setq form (elt formx 0))
    (setq gender (elt formx 1))
    (cond
     ((member form '(manC manV)) (setq form 'an))
     ((equal gender 'ADJt) (setq gender 'ADJ))
     ((equal form 'at) (setq form 'mat))
    )
    (setq form (list form gender))
    (setq ans (cons form ans))
   )
   (setq ans (nreverse ans))
   ans
 )
)
(defun form-file-init3-ptrs-helper (tab subdir)
 (let (dir file buf root ptrs maxpfxlen)
  (setq maxpfxlen (length tab)) ; = 2
  (setq dir (format "forms/%s" subdir))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (while (search-forward-regexp "^:" nil t)
    (let (words d-string data data1 cur-ptrs ptr)
     (setq words (gen-word-list (current-line) ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     (setq data1 (flatten data))
     (setq cur-ptrs (maximal-prefix-helper data1 maxpfxlen))
     (if (> (* 2 (length cur-ptrs)) (length data1))
       (setq cur-ptrs data1)
     )
     (when nil ; dbg
      (fol-msg (format "data = %s\n" data))
      (fol-msg (format "cur-ptrs = %s\n" cur-ptrs))
     )
     (while cur-ptrs
      (setq ptr (car cur-ptrs))
      (setq cur-ptrs (cdr cur-ptrs))
      (if (not (member ptr ptrs))
       (setq ptrs (cons ptr ptrs))
      )
     )
    )
   )
   ; now, insert
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (when ptrs
    (setq ptrs (maximal-prefix-helper ptrs maxpfxlen))
    (setq ptrs (sort ptrs 'string<))
    (when nil ; dbg
     (fol-msg (format "ptrs : %s\n" ptrs))
    )
    (goto-char 1)
    (if (not (search-forward-regexp "^ptrs : " nil t))
     (let (p)
      (goto-char 1)
      (insert (format "ptrs : "))
      (setq p (point))
      (insert (format "\n"))
      (goto-char p)
     )
     (progn
      (kill-line)
     )
    )
    (setq ptrs (vconcat ptrs nil)) ; turn into array
    (insert (format "%s" ptrs))
    (save-buffer 0) ; don't make duplicate
   )
  )
  (kill-buffer buf)
  t
 )
)
(defun form-file-init3-ptrs (subdir)
 (let (ans dir files file n i s1)
  (setq dir (file-name-as-directory (sangram-filename subdir "forms")))
  (setq files (non-system-files dir))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (form-file-init3-ptrs-helper file subdir)
  )
 )
)
(defun s-file-init3-ptrs ()
 (form-file-init3-ptrs "s")
)
(defun s-file-ptrs (outtab outdir)
  (form-file-ptrs outtab outdir "s")
)
(defun subtler-s-file-ptrs (outtab outdir)
 (let (filein bufin ans fileout bufout ans dir files file n i s1)
  ; prepare bufout
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  ; 
  (setq dir (file-name-as-directory (sangram-filename "s" "forms")))
  (setq files (non-system-files dir))
  (fol-msg (format "# files = %s\n" (length files)))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (let (ptrs ptr tab ltab lptr)
    (setq tab file)
    (setq ltab (length tab)) ; string length
    (setq ptrs (form-file-ptrs-helper tab "s"))
    (if (arrayp ptrs) (setq ptrs (append ptrs nil))) ; make ptrs a list
    ; make tab itself a pointer into tab.
    (setq ptrs (cons tab ptrs))
    (while ptrs
     (setq ptr (car ptrs))
     (setq ptrs (cdr ptrs))
     (setq lptr (length ptr)) ; string length
     (if (and (< ltab lptr)
	      (equal (substring ptr 0 ltab) tab)
	 )
      ; do nothing
      t
      ; tab is not a proper substring of ptr. Write a line
      ; not this will occur with ptr=tab as '(< ltab lptr)' is false.
      (with-current-buffer bufout
	(insert (format "%s s/%s\n" ptr tab))
      )
     )
    )
   )
  )
  (with-current-buffer bufout
   (sort-lines nil (point-min) (point-max))
   (save-buffer 0)
  )
  (kill-buffer bufout)
  t
 )
)

(defun i-file-init (intab indir)
"intab is like 'MW-ind.txt' in directory 'inputs'.
  Two sample records are:
  'aMSAMsi : I I : <MW=aMSAMsi,49,1>'
  'akzaraSas : I I : <MW=a-kzara-Sas,781,1>'
  The output is written to file forms/i/<tab>, where
  <tab> is the 1st two characters of the citation form, in lower-case.
  e.g., 'am'
  Output is constructed by routine i-file-init-helper.
 "
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix tab npfx err)
     ; line assumed to have form like in inputs/MW-ind.txt
     ; data for this line will be written to file forms/i/tabs
     (setq npfx 2)
     (setq tab (downcase (buffer-substring (point) (+ (point) npfx))))
     (setq line (current-line))
;     (fol-msg (format "line=%s\n" line))
     (condition-case err
      (progn
       (setq words (gen-word-list line ":"))
       (setq subanta (string-trim (elt words 0)))
       (setq fg (elt (reg-word-list (elt words 1) "[^ ]+") 1))
       (setq dict (string-trim (elt words 2)))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq words (reg-word-list dict "[^ <=,>]+"))
       (setq words1 (elt words 1)) ; "gaRi-mat"
;      (setq subwords (gen-word-list words1 "-")) ; ["gaRi" "mat"]
;       (fol-msg (format "entering : %s\n" line))
       (if (not (i-file-init-helper tab subanta fg words1))
        (fol-msg (format "error@line: %s\n" line))
       )
      )
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
    )
    (forward-line)
   )
  )
  (kill-buffer bufin)
  t
 )
)
(defun i-file-init-helper (outtab subanta fg mw-word)
 "'outtab' is a string, specifying a file in directory 'forms/i'
  'subanta' is the citation, in string form.
  'fg' is the form of the item, a string (it is not interesting)
  'mw-word' the string as cited in MW, e.g. 'gaNi-mat'.

  The output is of form (note 'adv' is a constant).
  Many indeclineables are adverbs.  This may be changed manually.
  :adv:<citation>:<mw-word>:[dtab],
  where dtab = citation, for indeclineables. This repetition leaves
  the form so it can be processed similarly to the data for verbs and nouns.
 "
; (fol-msg (format "chk: %s\n" (list outtab subanta fg mw-word)))
 (let (fileout bufout outdir subwords ans lword)
  (setq outdir "forms/i")
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (let (forms form gender tmp
	s-type s-form ans1 g dtab dtab1 outline-pfx outline)
   (setq subwords (gen-word-list mw-word "-"))
   (setq lword (elt (substring subwords -1) 0)) ; string
   (setq forms (s-file-init-genform  subanta fg))
;   (fol-msg (format "forms: %s %s -> %s\n" isubanta lexinfo forms))
   (setq s-type "adv")
   (setq dtab1 (vector subanta))
   (with-current-buffer bufout
    (setq outline-pfx (format ":%s:%s:" s-type subanta))
    (goto-char 1)
    (setq case-fold-search nil)
    (when (search-forward-regexp (format "^%s" outline-pfx) nil t)
     (beginning-of-line)
     (kill-line 1)
    )
    (goto-char (point-max))
    (setq outline
     (format "%s%s:%s\n" outline-pfx  mw-word dtab1)
    )
    (insert outline)
   )
  )
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  t
 )
)
(defun i-file-init3-ptrs ()
 (form-file-init3-ptrs "i")
)
(defun i-file-ptrs (outtab outdir)
 (form-file-ptrs outtab outdir "i")
)
(defun chk-s-file-init (intab indir)
"intab is like 'MW-noun.txt' in directory 'inputs'.
  Two sample records are:
  'gaRimat : S m : <MW=gaRi-mat,83017,1>'
  'ajirIya : S adj : <MW=ajirIya,2839,1>'
  The output is written to file forms/s/<tab>, where
  <tab> is the 1st two characters of the citation form, in lower-case.
  e.g., 'ga' or 'aj'.
  Output is constructed by routine chk-s-file-init-helper.
 "
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line subanta fg dict words words1 subwords suffix tab npfx err)
     ; line assumed to have form like in inputs/MW-noun.txt
     ; gaRimat : S m : <MW=gaRi-mat,83017,1>
     ; data for this line will be written to file forms/s/tabs
     (setq npfx 2)
     (setq tab (downcase (buffer-substring (point) (+ (point) npfx))))
     (setq line (current-line))
;     (fol-msg (format "line=%s\n" line))
     (condition-case err
      (progn
       (setq words (gen-word-list line ":"))
       (setq subanta (string-trim (elt words 0)))
       (setq fg (elt (reg-word-list (elt words 1) "[^ ]+") 1))
       (setq dict (string-trim (elt words 2)))
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq words (reg-word-list dict "[^ <=,>]+"))
       (setq words1 (elt words 1)) ; "gaRi-mat"
       (when (not (chk-s-file-init-helper tab subanta fg words1))
        (fol-msg (format "%s\n" line))
        (message line)
       )
      )
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
    )
    (forward-line)
   )
  )
  (kill-buffer bufin)
  t
 )
)
(defun chk-s-file-init-helper (outtab subanta fg mw-word)
 "'outtab' is a string, specifying a file in directory 'forms/s'
  'subanta' is the citation, in string form.
  'fg' is the form or gender of the item, a string.
  'mw-word' the string as cited in MW, e.g. 'gaNi-mat'.
  An array of strings, 'subwords', is formed from 'mw-word',
   e.g. ['gaNi' 'mat']
  A declension is formed using the last word of 'subwords' and 'fg',
  using routine 'SL-construct-subanta1', which returns a list of the
  form 
  '((noun u) (m [dtab]))'
  '((adj a) (m [dtab] f [dtab] n [dtab]))'
  The output is of form:
  :noun u m:<citation>:<mw-word>:[dtab]
  :adj a m:<citation>:<mw-word>:[dtab]
  :adj a f:<citation>:<mw-word>:[dtab]
  :adj a n:<citation>:<mw-word>:[dtab]
 "
; (fol-msg (format "chk: %s\n" (list outtab subanta fg mw-word)))
 (let (fileout bufout outdir subwords ans regexp   )
  (setq outdir "forms/s")
  (setq fileout (sangram-filename outtab outdir))
;  (fol-msg (format "fileout=%s\n" fileout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (setq case-fold-search nil)
   (goto-char 1)
   (setq regexp (format ":%s:%s:" subanta mw-word))
   (if (search-forward-regexp regexp nil t)
    (setq ans t)
    (setq ans nil)
   )
  )
  (kill-buffer bufout)
  ans
 )
)

(defun work-praps (&optional tabout)
 "Construct output file by examining the root files in forms/v.
  From each such, extract a list of strings, each of which
  has form:
   <prap> <class> <root>
  Where <prap> is the citation form of a present active participle
   (e.g., 'gacCat'), 
  <class> is a conjugation class (e.g. '1')
  <root> is the root (e.g. 'gam')
  Each member of the list of strings is inserted as a line in the
  output file.
 "
 (let (ans dir files file  fileout dirout bufout thisans)
  (setq dirout "forms")
  (if (not tabout) (setq tabout "work-praps.txt"))
  (setq fileout (sangram-filename tabout dirout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (setq dir (file-name-as-directory (sangram-filename "v" "forms")))
  (setq files (non-system-files dir))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (setq thisans (work-praps-helper file)) ; a list of strings
   (with-current-buffer bufout
    (mapcar
     (lambda (s) (insert (format "%s\n" s)))
     thisans
    )
   )
  )
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)
  )
  t
 )
)
(defun work-praps-helper (tab)
 (let (ans dir file buf root ptrs maxpfxlen root)
  (setq maxpfxlen (length tab)) ; = 2
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (progn
     (fol-msg (format "malformed root file: %s\n" tab))
     (setq root (format "%s??" tab))
    )
   )
   (goto-char 1)
   (while (search-forward-regexp "^:prap" nil t)
    (let (words work data data1 pc ptr class)
     ; line = :prap 1 m:harat:[haran ...]
     (setq words (gen-word-list (current-line) ":"));
     ; words= ["prap 1 m" "harat" "[haran ...]"]
     (setq ptr (elt words 1)) ; "harat"
     (setq work (gen-word-list (elt words 0) " ")) ; ["prap" "1" "m"]
     (setq class (elt work 1)) ; "1"
     (setq pc (list ptr class))
     (if (not (member pc ptrs))
       (setq ptrs (cons pc ptrs))
      )
     
    )
   )
  )
  (kill-buffer buf)
  (setq ptrs (nreverse ptrs))
  (setq ans (mapcar 
   (lambda (x)
    (let (prap class)
     (setq prap (elt x 0))
     (setq class (elt x 1))
     (format "%s %s %s" prap class root)
    )
   )
   ptrs
  ))
  ans
 )
)
(defun work-praps1 (&optional tabin tabout)
 "Process lines of file like that constructed by 'work-praps',
  i.e. assume each line of 'tab' has form:
   <prap> <class> <root>.
  Actually, only <prap> is used.
  For example, suppose <prap> = 'gacCat'.
  We look in file 'inputs/MW-adj.txt' for a line beginning with
   'gacCat : S adj :';
  If such a line is not found, 
   we write a message to 'tabout' and process no further.
  If such a line is found, we process further:
   We look in file 'forms/s/ga' (first 2 letters of <prap>) for
   lines of the form:
    ':noun 1cons <g>:gacCat' where <g> can be one of 'mfn'.
   Any such line found is deleted.
   An appropriate message is written to 'tabout'
 "
 (let (ans dir fileout dirout bufout thisans filein bufin)
  (setq dirout "forms")
  (if (not tabout) (setq tabout "work-praps1.txt"))
  (setq fileout (sangram-filename tabout dirout))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (if (not tabin) (setq tabin "work-praps.txt"))
  (setq filein (sangram-filename tabin dirout))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
;  (fol-msg (format "bufin=%s\n" bufin))
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (line words prap)
     (setq line (current-line))
     (setq words (gen-word-list line " "))
     (setq prap (elt words 0))
;     (fol-msg (format "CHK: %s\n" prap))
     (if (regexp-lines (sangram-buffer "MW-adj.txt" "inputs")
		       (format "^%s : S adj" prap))
      ; remove certain lines from 'forms/s/<pr>'
      (let (stab sdir sfile sbuf nkill)
       (setq stab (substring prap 0 2)) ; 1st 2 letters
       (setq stab (downcase stab))
       (setq sdir "forms/s")
       (setq sbuf (sangram-buffer stab sdir))
       (setq nkill (regexp-kill-lines sbuf
          (format "^:noun 1cons [mfn]:%s:" prap)))
       (with-current-buffer sbuf (save-buffer 0) (kill-buffer nil))
       (with-current-buffer bufout
	(if (< 0 nkill)
         (insert (format "%s KILLED %s lines in %s\n" prap nkill stab))
	 (insert (format "%s FOUND no lines in %s\n" prap stab))
	)
       )
      )
      ; no lines to examine in 'forms/s/<pr>. Write output message
      (with-current-buffer bufout
       (insert (format "%s NOT-MW-adj\n" prap))
      )
     )
    )
    (forward-line)
   )
  )
  (with-current-buffer bufout
   (save-buffer 0)
   (kill-buffer nil)
  )
  t
 )
)

(defun vc-file-init1 ()
 (let (intab indir filein bufin ans)
  (setq intab "v-root-filenames.txt")
  (setq indir "forms")
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root file words)
     (setq words (word-list (current-line)))
     (setq root (elt words 0))
     (setq file (elt words 1)) ; string
     (vc-file-init1-helper root file)
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun vc-file-init1-helper (root file)
 (let (outtab outdir fileout bufout)
  (setq outtab file)
  (setq outdir "forms/vc")
  ; initialize output file to be empty
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  ; put 'root' line in file
  (with-current-buffer bufout
   (insert (format "root : %s c\n" root)) ; 'c' is for 'causal'
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun vc-file-init2a (intab indir)
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root file words class pada dict)
     ; line is like (BU 1 A <MW=vy-ati-BU,262293,1>)
     ; but we ignore () and treat as 4 strings.
     (let (p1 p2 s)
      (forward-char) (setq p1 (point))
      (end-of-line) (backward-char) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq class (elt words 1))
     (setq pada (elt words 2))
;     (setq dict (elt words 3))
;     (fol-msg (format "%s\n" words))
     ; for causal, skip class 10, which would be same in
     ; causal and as class 10
     (if (not (equal class "10"))
      (vc-file-init2a-helper root class pada)
     )
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun vc-file-init2a-helper (root class pada)
; (fol-msg (format "chk: %s %s %s\n" root class pada))
 (let (outtab outdir fileout bufout)
  (setq outtab (v-root-filename-get root))
  (setq outdir "forms/vc")
  ; open output file
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout
   (goto-char 1)
   (if (equal (buffer-substring 1 5) "root") ; be sure file is ok
    (let (outline voice )
     ; pada is "P" or "A"
     (setq voice (if (equal pada "P") "a" "m"))
     (setq outline (format "class : %s %s" class voice))
     (when (not (search-forward outline nil t))
      ; add new line at end of file
      (goto-char (point-max))
      (insert (format "%s\n" outline))
     )
    )
    (fol-msg (format "unknown root file: %s %s %s\n" root class pada))
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer bufout)
  t
 )
)
(defun vc-file-init3 (action intab indir)
 (let (filein bufin ans)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (setq case-fold-search nil)
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root words tab)
     ; line assumed to have root (in SLP form) as first word
     (let (p1 p2 s)
      (setq p1 (point))
      (end-of-line) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq tab (v-root-filename-get root))
     (let (procname procsym)
      (setq procname (format "vc-file-init3-%s" action))
      (setq procsym (intern procname))
;      (fol-msg (format "chk: %s %s\n" procsym tab))
      (funcall procsym tab)
     )
    )
    (forward-line)
   )
  )
  ans
 )
)
(defun vc-file-init3-prefut (tab)
 "The causal conjugation table is formed in the
  active, middle and passive voices for the following tenses:
  pre ipf ipv pop
  fut pft con ben
 "
 (let (dir file buf root cvs dtype all-tvs tvs)
  (setq all-tvs '((pre a) (pre m) (pre p)
		  (ipf a) (ipf m) (ipf p)
		  (ipv a) (ipv m) (ipv p)
		  (pop a) (pop m) (pop p)
		  (fut a) (fut m) (fut p)
		  (pft a) (pft m) (pft p) 
		  (con a) (con m) (con p)
		  (ben a) (ben m) (ben p)
	         ))
  (setq dtype 'c) ; symbol for causal
  (setq join-array-method 1)
  (setq dir (format "forms/v%s" dtype))
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (setq case-fold-search nil)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   ; gather all classes
   (while (search-forward-regexp "class : \\([0-9]+\\) \\([am]\\)" nil t)
    (forward-line)
    (let (outline class voice cv)
     (setq class (string-to-number
		  (buffer-substring (match-beginning 1) (match-end 1))))
     (setq cvs (append-if-new cvs class))
    )
   )
;   (fol-msg (format "cvs=%s\n" cvs))
   ; remove all conjugations for the tenses
   (setq tvs all-tvs)
   (while tvs
    (let (tv tense voice ctabs ctab root1 outline-pfx)
     (setq tv (car tvs))
     (setq tvs (cdr tvs))
     (setq tense (elt tv 0))
     (setq voice (elt tv 1))
     (setq root1 (intern root))
     (goto-char 1)
     (setq outline-pfx (format "^:%s %s%s:" tense dtype voice))
      (while (search-forward-regexp outline-pfx nil t)
       (beginning-of-line)
       (kill-line 1)
      )
     )
    )
   ; loop thru cvs
;   (fol-msg (format "begin cvs loop...\n"))
   (while cvs
    (let (outline class voice cv)
     (setq cv (car cvs))
     (setq cvs (cdr cvs))
     (setq class cv)
     (setq tvs all-tvs)
     (while tvs
      (let (tv tense ctabs ctab root1 voice1 tense1 outline)
       (setq tv (car tvs))
       (setq tvs (cdr tvs))
       (setq tense (elt tv 0))
       (setq voice (elt tv 1))
       (setq root1 (intern root))
;       (fol-msg (format "before... %s %s %s %s %s %s\n"
;			root1 class voice nil tense dtype))
       (setq ctabs (SL-conjtab root1 class voice nil tense dtype))
;       (fol-msg (format "after... %s\n" ctabs))
       (if (not (listp ctabs)) (setq ctabs (list ctabs)))
       (while ctabs
	(setq ctab (car ctabs))
	(setq ctabs (cdr ctabs))
	(setq outline (format ":%s %s%s:%s\n" tense dtype voice ctab))
	(goto-char (point-min))
	(when (not (search-forward outline nil t))
	 (goto-char (point-max))
	 (insert outline)
	)
       )
      )
     )
    )
   )
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun vc-file-init3-prespart (tab)
 "present participles for causal verbs. 
 "
 (v-file-init3-prespart tab "vc")
)
(defun vc-file-init3-inf (tab)
 "infinitive for causal verbs. 
 "
; (fol-msg (format "begin tab=%s\n" tab))
 (v-file-init3-inf tab "vc")
)
(defun vc-file-init3-ppfactn (tab)
 "periphrastic perfect action nouns for causal verbs. 
 "
; (fol-msg (format "vc-file-init3-ppfactn: NOT FUNCTIONAL\n"))
 (v-file-init3-ppfactn tab "vc")
)
(defun vc-file-init3-abs (tab)
 "absolutives (gerunds) for causal verbs. 
 "
; (fol-msg (format "begin tab=%s\n" tab))
 (v-file-init3-abs tab "vc")
)
(defun vc-file-init3-ppp (tab)
 "perfect passive participles for causal verbs. 
 "
; (fol-msg (format "begin tab=%s\n" tab))
 (v-file-init3-ppp tab "vc")
)
(defun vc-file-init3-ptrs ()
 (form-file-init3-ptrs "vc")
)
(defun vc-file-ptrs (outtab outdir)
 (form-file-ptrs outtab outdir "vc")
)

(defun old-s-file-init3-ptrs-helper (tab)
 (let (dir file buf root ptrs maxpfxlen)
  (setq maxpfxlen (length tab)) ; = 2
  (setq dir "forms/s")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (while (search-forward-regexp "^:" nil t)
    (let (words d-string data data1 cur-ptrs ptr)
     (setq words (gen-word-list (current-line) ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     (setq data1 (flatten data))
     (setq cur-ptrs (maximal-prefix-helper data1 maxpfxlen))
     (if (> (* 2 (length cur-ptrs)) (length data1))
       (setq cur-ptrs data1)
     )
     (when nil ; dbg
      (fol-msg (format "data = %s\n" data))
      (fol-msg (format "cur-ptrs = %s\n" cur-ptrs))
     )
     (while cur-ptrs
      (setq ptr (car cur-ptrs))
      (setq cur-ptrs (cdr cur-ptrs))
      (if (not (member ptr ptrs))
       (setq ptrs (cons ptr ptrs))
      )
     )
    )
   )
   ; now, insert
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (setq ptrs (maximal-prefix-helper ptrs maxpfxlen))
   (setq ptrs (sort ptrs 'string<))
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (goto-char 1)
   (if (not (search-forward-regexp "^ptrs : " nil t))
    (let (p)
     (goto-char 1)
     (insert (format "ptrs : "))
     (setq p (point))
     (insert (format "\n"))
     (goto-char p)
    )
    (progn
     (kill-line)
    )
   )
   (setq ptrs (vconcat ptrs nil)) ; turn into array
   (insert (format "%s" ptrs))
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun old-i-file-init3-ptrs-helper (tab)
 (let (dir file buf root ptrs)
  (setq dir "forms/i")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (while (search-forward-regexp "^:" nil t)
    (let (words d-string data data1 cur-ptrs ptr)
     (setq words (gen-word-list (current-line) ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     (setq data1 (flatten data))
     (setq cur-ptrs (maximal-prefix-helper data1 3))
     (if (> (* 2 (length cur-ptrs)) (length data1))
       (setq cur-ptrs data1)
     )
     (when nil ; dbg
      (fol-msg (format "data = %s\n" data))
      (fol-msg (format "cur-ptrs = %s\n" cur-ptrs))
     )
     (while cur-ptrs
      (setq ptr (car cur-ptrs))
      (setq cur-ptrs (cdr cur-ptrs))
      (if (not (member ptr ptrs))
       (setq ptrs (cons ptr ptrs))
      )
     )
    )
   )
   ; now, insert
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (setq ptrs (maximal-prefix-helper ptrs 3))
   (setq ptrs (sort ptrs 'string<))
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (goto-char 1)
   (if (not (search-forward-regexp "^ptrs : " nil t))
    (let (p)
     (goto-char 1)
     (insert (format "ptrs : "))
     (setq p (point))
     (insert (format "\n"))
     (goto-char p)
    )
    (progn
     (kill-line)
    )
   )
   (setq ptrs (vconcat ptrs nil)) ; turn into array
   (insert (format "%s" ptrs))
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun old-v-file-init3-ptrs (tab)
 (let (dir file buf root ptrs)
  (setq dir "forms/v")
  (setq file (sangram-filename tab dir))
  (setq buf (find-file-noselect file 't)) ; 't suppresses warning
  (with-current-buffer buf
   (goto-char 1)
   (if (search-forward-regexp "root : \\([a-zA-Z]+\\)" nil t)
    (setq root (buffer-substring (match-beginning 1) (match-end 1)))
    (fol-msg (format "malformed root file: %s\n" tab))
   )
   (goto-char 1)
   (if (not root) (goto-char (point-max)))
   (while (search-forward-regexp "^:" nil t)
    (let (words d-string data data1 cur-ptrs ptr)
     (setq words (gen-word-list (current-line) ":")); array of strings
     (setq d-string (elt (substring words -1) 0)) ; string representing data
     (setq data (parse-string-to-array d-string))
     (setq data1 (flatten data))
     (setq cur-ptrs (maximal-prefix-helper data1 3))
     (if (> (* 2 (length cur-ptrs)) (length data1))
       (setq cur-ptrs data1)
     )
     (when nil ; dbg
      (fol-msg (format "data = %s\n" data))
      (fol-msg (format "cur-ptrs = %s\n" cur-ptrs))
     )
     (while cur-ptrs
      (setq ptr (car cur-ptrs))
      (setq cur-ptrs (cdr cur-ptrs))
      (if (not (member ptr ptrs))
       (setq ptrs (cons ptr ptrs))
      )
     )
    )
   )
   ; now, insert
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (setq ptrs (maximal-prefix-helper ptrs 3))
   (setq ptrs (sort ptrs 'string<))
   (when nil ; dbg
    (fol-msg (format "ptrs : %s\n" ptrs))
   )
   (goto-char 1)
   (if (not (search-forward-regexp "^ptrs : " nil t))
    (let (p)
     (goto-char 1)
     (forward-line)
     (insert (format "ptrs : "))
     (setq p (point))
     (insert (format "\n"))
     (goto-char p)
    )
    (progn
     (kill-line)
    )
   )
   (setq ptrs (vconcat ptrs nil)) ; turn into array
   (insert (format "%s" ptrs))
   (save-buffer 0) ; don't make duplicate
  )
  (kill-buffer buf)
  t
 )
)
(defun old-i-file-ptrs (outtab outdir)
 (let (filein bufin ans fileout bufout ans dir files file n i s1)
  ; prepare bufout
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  ; 
  (setq dir (file-name-as-directory (sangram-filename "i" "forms")))
  (setq files (non-system-files dir))
  (fol-msg (format "# files = %s\n" (length files)))
  (while files
   (setq file (car files))
   (setq files (cdr files))
   (message file)
   (let (ptrs ptr tab ltab lptr)
    (setq tab file)
    (setq ltab (length tab)) ; string length
    (setq ptrs (form-file-ptrs-helper tab "i"))
    (if (arrayp ptrs) (setq ptrs (append ptrs nil))) ; make ptrs a list
    (while ptrs
     (setq ptr (car ptrs))
     (setq ptrs (cdr ptrs))
     (with-current-buffer bufout
	(insert (format "%s i/%s\n" ptr tab))
     )
    )
   )
  )
  (with-current-buffer bufout
   (sort-lines nil (point-min) (point-max))
   (save-buffer 0)
  )
  (kill-buffer bufout)
  t
 )
)
(defun old-v-file-ptrs (intabs indir outtab &optional outdir)
 (let (filein bufin ans tmptab)
  (if (not (listp intabs)) (setq intabs (list intabs)))
  (setq tmptab "v-file-ptrs-temp.txt")
  (setq filein (sangram-filename tmptab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  (with-current-buffer bufin
   (erase-buffer)
   (mapcar
    (lambda (intab)
     (insert-file-contents (sangram-filename intab indir))
    )
    intabs
   )
   (save-buffer 0)
  )
  (setq ans (old-v-file-ptrs-main  tmptab indir outtab))
  ans
 )
)
(defun old-v-file-ptrs-main (intab indir outtab &optional outdir)
 (let (filein bufin ans fileout bufout)
  (setq filein (sangram-filename intab indir))
  (setq bufin (find-file-noselect filein 't)) ; 't suppresses warning
  ; initialize output file to be empty
  (if (not outdir) (setq outdir indir))
  (setq fileout (sangram-filename outtab outdir))
  (setq bufout (find-file-noselect fileout 't)) ; 't suppresses warning
  (with-current-buffer bufout (erase-buffer))
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let (root words tab)
     ; line assumed to have root (in SLP form) as first word
     (let (p1 p2 s)
      (setq p1 (point))
      (end-of-line) (setq p2 (point))
      (setq s (buffer-substring p1 p2))
      (setq words (word-list s))
     )
     (setq root (elt words 0))
     (setq tab (v-root-filename-get root))
     (let (ptrs ptr)
      (setq ptrs (form-file-ptrs-helper tab "v"))
      (if (arrayp ptrs) (setq ptrs (append ptrs nil))) ; make ptrs a list
      (while ptrs
       (setq ptr (car ptrs))
       (setq ptrs (cdr ptrs))
       (with-current-buffer bufout
	(insert (format "%s v/%s\n" ptr tab))
       )
      )
     )
    )
    (forward-line)
   )
  )
  (with-current-buffer bufout
   (sort-lines nil (point-min) (point-max))
   (save-buffer 0)
  )
  (kill-buffer bufin)
  (kill-buffer bufout)
  t
 )
)

(defun regexp-lines (bufin REGEXP-gen)
 "Returns all lines matching a given regular expression.
 "
 (let (ncount more ans nlines line)
  (with-current-buffer bufin
   (goto-char 1)
;   (fol-msg (format "max=%s\n" (point-max)))
   (setq case-fold-search nil)
   (setq ncount 0)
   (setq more t)
   (when (not REGEXP-gen) (setq more nil))
   (while (and more
	       (search-forward-regexp REGEXP-gen nil t)
          )
    (setq ncount (1+ ncount))
    (when (equal (mod ncount 100) 0) (message (format "%s" ncount)))
    (setq line (current-line))
    (setq ans (cons line ans))
    (forward-line)
    (setq more (< (point) (point-max)))
   )
  )
  (setq ans (nreverse ans))
  ans
 )
)
(defun regexp-kill-lines (bufin REGEXP-gen &optional bufout)
 "Kills all lines matching a given regular expression.
  Returns # of lines deleted.
  If optional argument 'bufout' is present, the deleted lines
  are dumped into that buffer.
 "
 (let (ncount more ans nlines line)
  (with-current-buffer bufin
   (goto-char 1)
;   (fol-msg (format "max=%s\n" (point-max)))
   (setq case-fold-search nil)
   (setq ncount 0)
   (setq more t)
   (when (not REGEXP-gen) (setq more nil))
   (while (and more
	       (search-forward-regexp REGEXP-gen nil t)
          )
    (setq ncount (1+ ncount))
    (beginning-of-line)
    (when bufout
     (let (s)
      (setq s (current-line))
      (with-current-buffer bufout
	(insert (format "%s\n" s))
      )
     )
    )
    (kill-line 1)
   )
  )
  (setq ans ncount)
  ans
 )
)

(defun v-file-init-alt1-pre (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write conjugation
  information for each line to output outdir/outtab
  Use the 4 present tenses, and the voice and class provided in 
  dcpforms-MW.txt
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
  Output is constructed by routine v-file-init-alt1-pre-helper.
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
       (setq tenses '(pre ipf ipv pop))
       (setq outputs (v-file-init-alt1-pre-helper root class voice tenses dbg))
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
(defun v-file-init-alt1-pre-helper (root class voice tenses &optional dtypein dbg)
 "get conjugations per 'tenses'
  Returns  list of strings, or nil
  root, class and voice are string
  tenses is a list of symbols
  June 24, 2016. added 'dtype' as optional. Unless nil, dtype should have
  value 'c', meaning causal.
 "
(let (err outline outlines)
  (when dbg 
   (fol-msg (format "v-file-init-alt1-pre-helper: %s\n"
		  (list root class voice tenses)))
  )
  (setq err nil)
  (condition-case err
   (let (tense ctabs ctab root1 class1 voice1 tense1 outline-pfx dtype)
    (setq root1 (intern root))
    (setq voice1 (intern voice))
    (setq class1 (string-to-number class))
    ;(fol-msg (format "chk1\n"))
    (if (string= dtypein "c")
     (setq dtype 'c) ; symbol
     (setq dtype nil)
    )
    ;(fol-msg (format "chk2\n"))
    (while tenses
     (setq tense (car tenses))
     (setq tenses (cdr tenses))
     (setq ctabs (SL-conjtab root1 class1 voice1 nil tense dtype dbg))
     ; Could ctabs be nil? If so, need a warning
     (when (not ctabs)
      (fol-msg (format "v-file-init-alt1-pre-helper: No conjugation for %s\n"
               (list root1 class1 voice1 tense dtype)))
     )
     (if (not (listp ctabs)) (setq ctabs (list ctabs)))
     (if (equal dtype 'c)
      (setq dtype " c")
      (setq dtype "")
     )
     (setq outline-pfx (format ":%s %s %s%s%s" root tense class voice dtype))
     (while ctabs
      (setq ctab (car ctabs))
      (setq ctabs (cdr ctabs))
      (setq outline (format "%s:%s\n" outline-pfx ctab))
      (setq outlines (append outlines (list outline))) ; add to end of outlines
     )
   )
  )
  (error1
   (fol-msg (format "v-file-init-alt-helper: error1(%s)\n" err))
   (fol-msg (format "inputs: %s\n" (list root class voice tenses)))
  )
  )
  (if err nil outlines)
 )
)
(defun v-file-init-alt1-pre-p (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write conjugation
  information for each line to output outdir/outtab
  Use the 4 present tenses, and the voice and class provided in 
  dcpforms-MW.txt
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
  Output is constructed by routine v-file-init-alt1-pre-helper, and the
  this output is slightly altered before insertion into outtab (dummy class
  removed).  Also, since the passive depends only on the root (and not on
  the class or pada), logic of this function discards duplicate records
  (i.e., only one set of tables for a give root, even if that root
  appears in multiple class-pada combinations in 'intab'.
 "
 (let (filein bufin bufout fileout outputs output nin nout
       words root class pada dict voice tenses roots)
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
       (setq root (elt words 0)) ; root is a string
       (setq class (elt words 1))
       (setq pada (elt words 2))
       (setq dict (string-trim (elt words 3))) ; unused
       ; as in v-file-init2a-helper. Change Pada to 'voice', per Scharf
       (setq voice (if (equal pada "P") "a" "m")) ; reset below
       ; next words -> ; ["MW" "gaRi-mat" "83017" "1"]
       (setq tenses '(pre ipf ipv pop))
       (when (not (member root roots))
        ; avoid duplicates
        ;(fol-msg (format "found new root: '%s'\n" root))
        ; reset voice to passive
        (setq voice "p")
        ; reset class to 0 - it is not used for passive
        (setq class "0")
        (setq outputs (v-file-init-alt1-pre-helper root class voice tenses dbg))
        (setq roots (cons root roots))
        (if (not outputs)
         (fol-msg (format "error@line: %s\n" line))
        )
       ) ; when not member
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
       ; change '0p:' to 'p'  (the '0' is dummy class)
       (setq output (replace-regexp-in-string "0p:" "p:" output))
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

(defun v-file-init-alt1-fut (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write conjugation
  information for each line to output outdir/outtab
  Use the 4 future tenses (fut pft con ben) 
and the voice and class provided in 
  dcpforms-MW.txt
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
  Output is constructed by routine v-file-init-alt1-pre-helper, and the
  this output is slightly altered before insertion into outtab (dummy class
  removed).  Also, since the passive depends only on the root (and not on
  the class or pada), logic of this function discards duplicate records
  (i.e., only one set of tables for a give root, even if that root
  appears in multiple class-pada combinations in 'intab'.

  The class-voice combinations to use for a given root are derived in a
  complex way by function v-file-init3-fut.  We mimic that function's
  behavior here; the grammatical reason for this is unclear at
  present (10-15-2015).
 "
 (let (filein bufin bufout fileout outputs output nin nout line
       words root class pada dict voice tenses roots plist root1 classes)
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
  ; preliminary step reads all of bufin, and a constructs a property list,
  ; whose keys are the roots (as symbols). For each root, the associated
  ; property  is a list of the classes
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let ()
     (setq line (current-line))
     (setq line (substring line 1 -1))
     (setq words (gen-word-list line " ")) ; space for dcpforms-MW.txt
     (setq root (elt words 0)) ; root is a string
     (setq class (elt words 1))
     ;(setq pada (elt words 2))
     (when (not (member root roots))
      (setq roots (append roots (list root)))
     )
     (setq root1 (intern root)) ; symbol. Used as plist 'keys
     (setq classes (plist-get plist root1))
     (when (not (member class classes))
      (setq classes (append classes (list class)))
      (setq plist (plist-put plist root1 classes))
     )
     ; (setq dict (string-trim (elt words 3))) ; unused
    )
    (forward-line)
   )
  )
  ; end of construction of plist
  ; now loop through 'roots'
  (while roots
   (setq root (car roots))
   (setq roots (cdr roots))
   (setq nin (1+ nin))
   (when (and (<= n1 nin) (<= nin n2))
    (let (multi-class class10 class0 cvs cv voices classid)
     (setq outputs nil)
     ; determine multi-class, class10, class0 
     ; cvs is only a temporary variable for this section
     (setq multi-class nil)
     (setq class10 nil)
     (setq class0 nil)
     (setq root1 (intern root))
     (setq classes (plist-get plist root1))
     (while classes
      (setq class (car classes)) ; a string
      (setq classes (cdr classes))
      (if (equal class "10")
       (setq class10 class)
       (if (not class0) (setq class0 class))
      )
      (when (and (not (member class cvs))
	 	 (not (equal class "10"))
	    )
       (if multi-class
        (setq multi-class (concat multi-class "/" (format "%s" class)))
        (setq multi-class (format "%s" class))
       )
       (setq cvs (append cvs (list class)))
      )
     )
     ; reset cvs
     ; artificially set cvs
     (setq cvs nil)
     (if class0
      (setq cvs (cons (list class0 multi-class) cvs))
     )
     (if class10
      (setq cvs (cons (list class10 (format "%s" class10)) cvs))
     )
     (when dbg
      (setq classes (plist-get plist root1))
      (fol-msg (format "dbg:%s : %s  => %s\n" root classes cvs)) 
     )
     (condition-case err
      (while cvs
       (setq cv (car cvs))
       (setq cvs (cdr cvs))
       (setq class (elt cv 0))
       (setq classid (elt cv 1))
       (setq tenses '(fut pft con ben))
       ; 'p' (passive) is same as 'm' (middle) for 'fut pft con ben'
       (setq voices '("a" "m"))
       (while voices
        (setq voice (car voices))
        (setq voices (cdr voices))
        (let (outputs1)
         ; note classid is not used, except for format of output in helper
         (setq outputs1 (v-file-init-alt1-pre-helper root classid voice tenses dbg))
         (if (not outputs1)
          (fol-msg (format "error %s\n" (list root class voice)))
         )
         (while outputs1
          (setq output (car outputs1))
	  (setq outputs1 (cdr outputs1))
          (setq outputs (append outputs (list output)))
         )
        )
       ) ; while voices
      ) ; while cvs
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
     ; append output to bufout
     (with-current-buffer bufout
      (while outputs
       (setq output (car outputs)) 
       (setq outputs (cdr outputs))
       ; change '0p:' to 'p'  (the '0' is dummy class)
       ;(setq output (replace-regexp-in-string "0p:" "p:" output))
       (insert output)
      )
     )
    )
   )
  )
  (kill-buffer bufin)
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  t
 )
)
(defun v-file-init-alt1-prf (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write conjugation
  information for each line to output outdir/outtab
  Use the perfect tense, in ??
  Use the voice and class provided in  dcpforms-MW.txt
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)
  Output is constructed by routine v-file-init-alt1-pre-helper, and the
  this output is slightly altered before insertion into outtab (dummy class
  removed).  Also, since the passive depends only on the root (and not on
  the class or pada), logic of this function discards duplicate records
  (i.e., only one set of tables for a give root, even if that root
  appears in multiple class-pada combinations in 'intab'.

  The class-voice combinations to use for a given root are derived in a
  complex way by function v-file-init3-prf.  We mimic that function's
  behavior here; the grammatical reason for this is unclear at
  present (10-15-2015).
 "
 (let (filein bufin bufout fileout outputs output nin nout line
       words root class pada dict voice tenses roots plist root1 classes)
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
  ; preliminary step reads all of bufin, and a constructs a property list,
  ; whose keys are the roots (as symbols). For each root, the associated
  ; property  is a list of the classes
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let ()
     (setq line (current-line))
     (setq line (substring line 1 -1))
     (setq words (gen-word-list line " ")) ; space for dcpforms-MW.txt
     (setq root (elt words 0)) ; root is a string
     (setq class (elt words 1))
     ;(setq pada (elt words 2))
     (when (not (member root roots))
      (setq roots (append roots (list root)))
     )
     (setq root1 (intern root)) ; symbol. Used as plist 'keys
     (setq classes (plist-get plist root1))
     (when (not (member class classes))
      (setq classes (append classes (list class)))
      (setq plist (plist-put plist root1 classes))
     )
     ; (setq dict (string-trim (elt words 3))) ; unused
    )
    (forward-line)
   )
  )
  ; end of construction of plist
  ; now loop through 'roots'
  (while roots
   (setq root (car roots))
   (setq roots (cdr roots))
   (setq nin (1+ nin))
   (when (and (<= n1 nin) (<= nin n2))
    (let (multi-class class10 class0 cvs cv voices classid root2)
     (setq outputs nil)
     ; determine multi-class, class10, class0 
     ; cvs is only a temporary variable for this section
     (setq multi-class "")
     (setq root1 (intern root))
     ; Must convert root1 to ITRANS form reduplicative-liT-P to work 
     (setq root2 (translate-SLP1-ITRANS root1))
     (setq classes (plist-get plist root1))
     (while classes
      (setq class (car classes)) ; a string
      (setq classes (cdr classes))
      (setq class (string-to-number class))
      (when (and (not (member class cvs))
		(reduplicative-liT-P root2 class)
	     )
       (if cvs
        (setq multi-class (concat multi-class "/" (format "%s" class)))
        (setq multi-class (format "%s" class))
       )
       (setq cvs (append cvs (list class)))
      )
     )
     ; use first class mentioned in cvs to construct perfect
     (when (not cvs)
      (setq classes (plist-get plist root1))
      (fol-msg (format "%s has no prf. Classes=%s\n" root classes))
     )
     (condition-case err
      (when cvs ; use first class
       (setq class (car cvs))
       ; v-file-init-alt1-pre-helper expects class as string
       (setq class (format "%s" class)) 
       (setq tenses '(prf))
       (setq voices '("a" "m"))
       (while voices
        (setq voice (car voices))
        (setq voices (cdr voices))
        (let (outputs1)
         ; note classid is not used, except for format of output in helper
         (setq outputs1 (v-file-init-alt1-pre-helper root class voice tenses dbg))
         (if (not outputs1)
          (fol-msg (format "error %s\n" (list root class voice)))
         )
         (while outputs1
          (setq output (car outputs1))
	  (setq outputs1 (cdr outputs1))
	  ; change labeling
          (let (old new)
	   (setq old (format " %s%s:" class voice))
	   (setq new (format " %s%s:" multi-class voice))
           (setq output (replace-regexp-in-string old new output))
          )
          (setq outputs (append outputs (list output)))
         )
        )
       ) ; while voices
      ) ; when cvs
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
     ; append output to bufout
     (with-current-buffer bufout
      (while outputs
       (setq output (car outputs)) 
       (setq outputs (cdr outputs))
       ; change '0p:' to 'p'  (the '0' is dummy class)
       ;(setq output (replace-regexp-in-string "0p:" "p:" output))
       (insert output)
      )
     )
    )
   )
  )
  (kill-buffer bufin)
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  t
 )
)

(defun v-file-init-alt1-ppfactn (intab indir outtab outdir n1 n2 &optional dbg)
" Read from file like construct/dcpforms-MW.txt, and write 
  information for each line to output outdir/outtab
 
  Use the class provided in  dcpforms-MW.txt
  to determine if a periphrastic perfect is computable.
  Sample records are:
  (Ap 5 P <MW=Ap,35737,1>)
  (As 2 A <MW=As,39445,1>)
  (BA 2 A <MW=vy-ati-BA,262287,1>)

 "
 (let (filein bufin bufout fileout outputs output nin nout line
       words root class pada dict voice tenses roots plist root1 classes)
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
  ; preliminary step reads all of bufin, and a constructs a property list,
  ; whose keys are the roots (as symbols). For each root, the associated
  ; property  is a list of the class and voice.  (NOTE: it is not clear 
  ; whether the voice (a=active, m=middle) is actually used.)
  (with-current-buffer bufin
   (goto-char 1)
   (while (< (point) (point-max))
    (let ()
     (setq line (current-line))
     (setq line (substring line 1 -1))
     (setq words (gen-word-list line " ")) ; space for dcpforms-MW.txt
     (setq root (elt words 0)) ; root is a string
     (setq class (elt words 1))
     (setq pada (elt words 2))
     (setq voice (if (equal pada "P") "a" "m"))
     (when (not (member root roots))
      (setq roots (append roots (list root)))
     )
     (setq root1 (intern root)) ; symbol. Used as plist 'keys
     (setq classes (plist-get plist root1))
     (setq class (list class voice)) ; note misuse - store class voice
     (when (not (member class classes))
      (setq classes (append classes (list class)))
      (setq plist (plist-put plist root1 classes))
     )
     ; (setq dict (string-trim (elt words 3))) ; unused
    )
    (forward-line)
   )
  )
  ; end of construction of plist
  ; now loop through 'roots'
  (while roots
   (setq root (car roots))
   (setq roots (cdr roots))
   (setq nin (1+ nin))
   (when (and (<= n1 nin) (<= nin n2))
    (let (actns cv dtype)
     (setq outputs nil)
     (setq dtype nil) ; apparently, can get ppfactn for dtype="c" also (causal)
     ; determine multi-class, class10, class0 
     ; cvs is only a temporary variable for this section
     (setq root1 (intern root))
     (setq classes (plist-get plist root1))
     ; update actns
     (while classes
      (setq cv (car classes)) ; a string
      (setq classes (cdr classes))
      ; unpack class, voice
      (setq class (elt cv 0))
      (setq voice (elt cv 1))
      (let (class1 voice1 actn a)
       (setq class1 (string-to-number class))
       (setq voice1 (intern voice))
       (setq actn (SL-ppfactn root1 class1 voice1 nil))
       ;(fol-msg (format "chk: %s %s %s -> %s\n" root1 class voice1 actn))
       ; apparently, actn can be either a string (symbol?) or a list of such
       (if (not (listp actn)) (setq actn (list actn)))
       (while actn
        (setq a (car actn))
        (setq actn (cdr actn))
        (if (not (member a actns))
         (setq actns (cons a actns))
        )
       )
      )
     )
     (when (not actns)
      (setq classes (plist-get plist root1))
      (fol-msg (format "%s has no ppfactn. Classes=%s\n" root classes))
     )
     (condition-case err
      (when actns
       ; turn acts into a vector
       (setq actns (flatten actns))
       (setq actns (vconcat actns))
       (setq output (format ":%s %s:%s\n" root "ppfactn" actns))
       (setq outputs (list output))
      ) ; when actns
      (error
       (fol-msg (format "error(%s)\n" err))
       (fol-msg (format "error in file-init @ line= '%s'\n" line))
      )
     )
     ; append output to bufout
     (with-current-buffer bufout
      (while outputs
       (setq output (car outputs)) 
       (setq outputs (cdr outputs))
       ; change '0p:' to 'p'  (the '0' is dummy class)
       ;(setq output (replace-regexp-in-string "0p:" "p:" output))
       (insert output)
      )
     )
    )
   )
  )
  (kill-buffer bufin)
  (with-current-buffer bufout (save-buffer 0))
  (kill-buffer bufout)
  t
 )
)
; July 10, 2016
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