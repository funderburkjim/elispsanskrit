; -*- mode:Emacs-Lisp; mode:outline-minor;   -*- 
; aorist.el  
; begun 08-13-03
; Based upon Kale, p.332 ff
(defun aorist-doc ()
 "Kale 529.
 There are seven varieties or forms of the Aorist. The augment
 'a' is prefixed to the root as in the Imperfect.
 "
)

(defvar aorist-tok nil)
(defvar aorist-id nil)
(defun aorist1-doc ()
 "Kale 530.
   The terminations of the first variety are the same as those
   of the imperfect, except that the 3P terminations is 'us'.
  Kale 531.
   The radical 'aa' is dropped before the 3P ending 'us'.
  Kale 532.
   'i', 'sthaa', 'daa', 'dhaa', and roots assuming the form of
   'daa' and 'dhaa' (see Kale 459), 'paa 1P (to drink)', and 
   'bhuu' take this variety.
  Kale 533.
   The roots 'ghraa', 'dhe', 'sho', 'so', and 'Cho' belong to the
   first variety optionally. They optionally take the 6th form.
   'dhe' also takes the 3rd form.
  Kale 534.
   'bhuu' takes 'an' instead of 'us' as the 3P ending; it changes
   its vowel to 'uuv' before the vowel terminations:
   abhuuvam, abhuuva, abhuuma (1st pers)
   abhuut, abhuutaam, abhuuvan (3rd pers)
  Kale 535.
   'gaa' is substituted for 'i' in the Aorist:
    1s pers: agaam, agaava, agaama
    1st pers, with 'adhi': adhyagam, adhyagaava, adhyagaama
  Kale 536.
   The 1st variety is exclusively parasmaipadi.
   The roots 'daa', 'dhaa', and 'sthaa' take the 4th variety in atmanepada.
   'bhuu' takes the 5th variety in the atmanepada.
   'i' with 'adhi' takes the 4th variety.
 "
)
(defun aorist2-doc ()
 "Kale 537.
   In this variety, 'a' is added on to the root, and then the
   terminations of the Imperfect of the first group of conjugational
   tenses are added. (See tables lu~N2-1-P, lu~N2-1-A)
  Kale 538.
   The preceding 'a' is dropped before endings 'am', 'an', 'anta',
   and lengthened before 'va' and 'ma'. 
   The radical vowel does not undergo guna or vrddhi strengthening,
   except for
     - final 'Ri'
     - final 'RI'
     - the 'Ri' of 'dRish'
  Kale 539. 
   This variety is parasmaipadi with a few exceptions:
    - 'Ri' with 'sam'
    - 'khyaa', 'vach'
    - 'as' (class 4) with a preposition
   The roots 'lip', 'sich', and 'hve' take this form in both P and A;
   they also take the 4th form in the 'A'.
  Kale 540.
   The penultimate nasal of a root is dropped:
    bhraMsh -> abhrashat
    skand -> askadat
  Kale 541.
   Some other alterations are made to particular roots:
    'as' : 'asth'  3S = aasthat
    'khyaa' : 'khya'  3S = akhyat
    'pat' : 'papt' 
    'vach' : 'voch' 3S = avochat
    'shaas' : 'siSh'  3S = ashiShat
    'shvi' : 'shva'
    'hve' : 'hva'
  Kale 542.
   A list of roots necessarily belongs to the 2nd variety
   (see aorist-2-542-P)
  Kale 543.
   A list of roots belong optionally to the 2nd variety.
   They also belong to the 4th variety if they are 'aniT' or
   the 5th variety if they are 'seT' (see aorist-2-543-P)
  Kale 544.
   A list of 25 roots, which are all atmanepada, are conjugated
   in the parasmaipada when they belong to this variety.
   In the atmanepada, they belong to the 4th or 5th variety 
   according as they are 'aniT' or 'seT' (see aorist-2-544-P)
 Notes.
  1. lip, sich, and hve also take the 4th variety in A
 "
)
(defun aorist3-doc ()
 "Kale 545.
   The terminations are the same is in the 2nd variety.
  Kale 546.
   Roots taking the 3rd variety:
   - roots of the 10th class
   - causals (recognized by class = 10)
   - some derivatives (not implemented)
   - 'kam'
   - the roots 'shri', 'dru', and 'sru' when expressing the agent
   - the roots 'dhe' and 'shvi' take the 3rd variety optionally.
  Kale 547.
   The root is first reduplicated and then the augment 'a' and the
   terminations are added as in the 2nd variety.
   By example, the base of 'dhe' is 'dadh'
  Kale 548. Roots of 10th class and causals
   a. 1. The root is modified as in accordance with present-tense of
      verbs of the 10th class
      2. he 'ay' of the base is dropped
      3. vowels are shortened:
         aa -> a, ii -> i, uu -> u, RI -> Ri, LI -> Li
         e and ai -> i, o and au -> u
  b1. 'i' is substituted for the 'a' of the reduplicative syllable
     if the syllable following it be short and not prosodially long.
  b2. If 'i' is the vowel of the reduplicative syllable (whether by
      virtue of (b1) or otherwise),  then that 'i' is lengthened
       when both of the following conditions hold:
       (1) it is not followed by a long syllable, and
       (2) it is not followed by a conjunct consonant.
  NOTE:  These alterations occur in function 'aorist3-alter10'.
  c. Roots having a penultimate 'Ri' or 'RI' optionally preserve it,
     the long 'RI' being changed to the short one.
  Kale 549. Bases with initial vowels
  a. If a root begins with a vowel and ends in a single consonant,
     - the consonant is reduplicated (NOTE: 'reduplicate-cons')
     - 'i' is added to it in reduplicate syllable
     NOTE: 'as' -> aasisat (rather than 'aasiShat')
  b. If a root begins with a vowel and ends in a conjunct consonant,
    whose first member is a nasal or 'd' or 'r', then
    the second member of the conjunct consonant is reduplicated.
    NOTE 1: In either case, the last letter of the root is the
    one reduplicated.
  a'. If a root begins with a vowel and ends in a conjunct consonant
    other than that of the type in b, it is treated as case (a).
    Example : aTT -> aaTTiTat
  c. The roots uun a~Nk a~Ng andh aMs arTh, and some others,
     substitute 'a' for 'i' in the reduplicative syllable.

 Kale 550.
  Roots ending in 'u' or 'uu' substitute 'u' (changeable to 'uu' like 'i')
  for 'i' in the reduplicative syllable, provided
  it be not (immediately) followed by 
   a labial consonant or a semivowel or j,
   followed by 'a' or 'aa'

 Kale 551.
  Several roots (bhraaj bhaas bhaSh diip jiiv miil piiD ...)
  shorten their penultimate optionally.
 Kale 552.
  In several roots the 'a' of the reduplicative syllable is not
  changed to 'i'; in two others, it is optionally changed to 'i'
 Kale 553.
  The root 'hve' takes samprasaaraNa; 
  The root 'shvi' takes samprasarraNa optionally
 Kale 554.
  The roots given under #400 (re 10th conj) preserve their
  vowel unchanged; i.e., do not substitute 'i' in the reduplicative
  syllable.
  Note : Not all roots listed in Kale-400 follow this. 'aMs' is one such
  Note 2: Kale gives 'suuch' as an example here, but it is not mentioned
  in #400.
 Kale 555. Several roots (shaas ,...) do not shorten their penultimate.
  Note: To match examples, I had to add some irregularities
 "
)
(defun aorist6-doc ()
 "Kale 557.
   The 6th variety is only parasmaipada
  Kale 558.
   Roots ending in 'aa' (including those that change their final to 'aa')
   are conjugated in the 6th variety (See 559)
   Also taking the 6th variety are:
   - 'yam' in the parasmaipada
   - 'ram' in the parasmaipada (i.e., with 'vi' and 'aa')
   - 'pari' (?? this is a prefix??)
   - 'nam'
   - 'yam' in atmanepada (e.g. with 'upa' or 'ud') takes 4th variety
   - 'ram' in the atmanepada takes 4th variety
 Kale 559. 
   Roots ending in 'aa' which are restricted to varieties 1-3 do not
   take the 6th form. 
   From the first aorist, probably permitted are only 
     'ghraa dhe sho so Cho' (See Kale 533)
     'mi mii lii' (See Kale 559 p. 345)
 
 "  
)
(defun aorist7-doc ()
 "
 Kale 560. This is both P and A. terminations given (all start with 's')
 Kale 561. 
  - 'aniT' roots ending in 'sh Sh s h' and
    having 'i u Ri Li' for their penultimate take the 7th variety
  - dRish, however, takes 4th form
 Kale 562.
  'mRish spRish and kRiSh' 1P 6 PA optionally belong to the 7th variety.
 Kale 563.
  The roots 'duh dih lih guh' in 'A' 
    optionally drop the initial 'sa' or 'saa' of the endings of
    1D, 3S, 2S, 2P
 "
)
(defun aorist4-doc ()
 "Kale 564. There are both P and A forms
 Kale 565.
  a1. aniT roots not belonging to any of the preceding varieties
    (i.e. 1,2,3,6,7) take this form 
  a2. aniT roots that optionally take any of the preceding varieties
    take this form.
    NOTE: aniT roots in variety '6' generally don't take this variety
  a3. weT roots optionally belong to this variety
  Exceptions:
   1. 'stu' and 'su' when P belong to 5th variety
   2. Roots ending in 'Ri' preceded by a conjunct consonant may take 4th or 5th
   3. 'a~nj' and 'dhuu' Par. take 5th only;
      'dhuu' A takes 4th or 5th
   4. Of 'seT' roots, 'vRi' and those ending in 'RI' when A take 4th or 5th.
      'snu' and 'kram' when 'A' belong to 4th.
 
  Kale 566.
   a. In the parasmaipada, the radical vowel takes its vrddhi substitute
   b. In the atmanepada, 
     - guna is substituted for final 'i ii u uu'
     - final 'Ri' is unchanged
     - penultimate vowel is unchaged
     - final 'RI' is changed to 'iir uur' in accordance with #394,
       NOTE: 394 is written in another context. In this context,
       we use 'uur' when a labial or 'v' precedes 'RI';
       we use 'iir' otherwise.
        'stRI' -> astiirShTa, vRI -> avuurShTa
    c. The penultimate 'Ri' is optionally changed to 'ra':
       kRiSh -> akaarkShiit or akraakShiit
  Kale 567.
    After a short vowel, and after a consonant, except a nasal or a semivowel,
    the 's' of terminations beginning with 'sta' and 'stha' is dropped.
  IRREGULAR AORIST OF THE 4th FORM
  Kale 568.
    'daa', 'dhaa', and roots assuming the form of 'daa' and 'dhaa' (Vide
    #459) and 'sthaa' substitute 'i' for their final vowel in the A.
    This 'i' does not take its guna substitute. In the P, these roots
    take the 1st variety (see #532).
  Kale 569.
   'han' (with 'aa' A) drops its nasal before the terminations.
   It takes the 5th form optionally both in the P and A, in which case
   'vagh' is substituted for it.
  Kale 570.
   a. gam optionally drops its nasal with the A terminations.
   b. yam with 'upa' (to marry) optionally drops nasal with A terminations
   c. yam when it means 'to give out' (as the faults of others) necessarily
      drops its nasal with A terminations. NOTE: This is all but with '(upa)'
  Kale 571.
   a. the 3S of 'pad' (4 A) is 'apaadi'
   b. 'budh' (4 A) takes the termination 'i' optionally in the 3S,
      before which the penultimate 'u' takes guna. e.g. 'abodhi'
  From example p. 352.
  See Kale 486.
    In the case of 'i' with 'adhi', 'gaa' is optionally substituted for 'i'
    in the conditional and the Aorist.
    In the case of the aorist, this 'gaa' is treated like 'gii'
  From example p. 352. 'mii' (A) is conjugated like 'maa'
 "
)
(defun aorist5-doc ()
 "Kale 572. 
   The terminations are obtained by prefixing the augment 'i' to the
   terminations of the 4th variety. dropping the 's' in the case of
   the 2S and 3S (P). 
   NOTE: These are reflected in 'lu~N5-1-P-endings', 'lu~N5-1-A-endings'
  Kale 573.
   All roots not restricted to any of the preceding varieties 
   (1,2,3,4,6,7) take the 5th variety.  Consequently, it is peculiar
   to 'seT' roots.
   NOTE 1: This is unclear in application. For instance, I could not
   see why 'budh' accepts 5th variety.
   NOTE 2: This is Antoine's 3rd for (p. 155, # 221). His statement of
   the criterion is:
     Roots ending in consonants or in other vowels than 'aa' take this form.
   I partially use this.
 Kale 574.
 a. In the Parasmaipada, the vowel is strengthened by vrddhi in the cases:
    root ends in a vowel
    root has penultimate 'a' and root ends in 'r' or 'l'
    root is 'dah' or 'vraj'
 b. In the Parasmaipada, the vowel is strenghthened by guna in the cases:
    root has a penultimate short vowel (except those mentioned above)
 c. In the Parasmaipada, vrddhi is optionally substituted in the case:
    root has a penultimate 'a' (not prosodially long - root ends in simple
      consonant) and root has an initial consonant and root does not
      end in 'r' or 'l'
 d. In the Parasmaipada, , The vrddhi substitute is NOT used in any
    of the following cases (rather, the guna substitute is used)
    - root ends in 'h m y'
    - root is 'kShaN', 'shvas', 'shvi', 'kaT' (cover, surround)
      'chaT' (break, pierce), 'chat', 'chad' (ask, beg),
      'path' (go, move), 'math' (churn), 'lag'(stick, cling to),
      'has', 'hlas' (sound, be diminished)
 e. In the Atmanepada, the radical vowel takes its guna substitute.
   NOTE: Antoine2 p 155 says: 
    A final vowel and short medial vowel take guna in the atmanepada
    The function 'gunate-final-vowel' takes these circumstances into
    account, so it is used. 'aorist-gunate-final-vowel' does not
    so it is unused here.
 Kale 575.
  The roots 'diip jan puur taay vyaay' optionally substitute 'i' for
   the 3S Atmanepada ending 'iShTa'.
 Kale 576.
  Roots of the 8th class ending in 'N' or 'n' have an optional form
  in the 2S and 3S Atmanepada: they drop the nasal and substitute
  'thaaH' for the ending 'iShTaaH' (2S) and 'ta' for ending 'iShTa'.
  The root 'san', further,  lengthens its vowel in 2S and 3S A.
 Kale 577.
  The vowel of 'uurNu' takes vrddhi optionally in the P (i.e., it
  optionally takes guna substitute). Also, it optionally remains unchanged
  before 'i' (P and A). Thus, it has 3 forms in P and 2 forms in A
 Kale 578.
  the 'aa' of 'daridraa' is optionally dropped in the aorist,
  consequently it takes the 6th and 5th forms ('aa' is dropped in 5th form)
 "
)
(defun aorist-passive-doc ()
 "Kale 597(a)
   The Passive of the Aorist of roots belong to the
   4th, 5th, and 7th varieties is made up by appending
   the A terminations to the base.
  Kale 597(b)
   The Passive of the Aorist of roots belong to the
   1st, 2nd, 3rd, and 6th varieties take the
   4th, 5th, or 7th varieties in the Passive, in acccordance
   with the general rules.
  Kale 597(c)
   The 3S of the Aorist Passive of all roots is formed by
   adding 'i'.
  Kale 597(c-1)
   Before this 'i',
   - the penultimate (prosodially) short vowel takes its guna substitute
   - the penultimate 'a' takes its vrddhi substitute, in general
   - however, the root 'jan' is unchanged
   - roots which are 'seT' and end in 'am' are unchanged, in general.
     However, the following 'seT' roots ending in 'am' do take vrddhi:
     'cham' with prefix 'aa', 'kram', and 'vam'
   - the final vowel takes its vrddhi substitute
  Kale 597(c-2)
   Roots ending in 'aa', original or substituted (i.e. roots ending
   in 'e ai o') insert 'y' before this 'i'.
  Kale 597(c-3)
   The roots 'radh jabh rabh' insert a nasal before their final
   consonant; thus, in this situation, they are prosodially long
   so their penultimate 'a' cannot take guna or vrddhi substitute;
   i.e., their vowel is unchanged.
  Kale 597(c-4)
   'labh' with a preposition inserts a nasal, and vowel is unchanged
   'labh' without a preposition has two forms, one with an inserted
     nasal and no vowel change, one with vowel-vrddhi and no nasal.
  Kale 597(c-5)
   'bha~nj' (to break) forms 'abha~nji' or 'abhaaji'
   'sham' in the sense of 'to observe' (10 A) forms 'ashami' and 'ashaami'
  Kale 597(c-6)
   'sRij' takes vrddhi ('asaarji')
   'guh' lengthens its vowel ('aguuhi')
  Kale 597(c-7)
   'i' (to go) has 'agaayi'
   'i' with preposition 'adhi' has 'adhyaayi' or 'adhyagaayi'
  Kale 597(d)
   The roots at #461 (e.g. gup dhuup vichCh paN pan kam)
   will have two forms, one formed according to above logic,
   the other with the 'conjugational base'
  Kale 597(e)
   The optional forms of Section 596(b) (see below) hold good in the
   Passive aorist except in the 3S. 
   The optional forms must be made up by appending the A terminations
   of the 5th variety as the roots necessarily take 'i'.
  Kale 598: Roots of the 10th class
  Kale 598(a)
   The 'ay' (i.e., the 'aya' with the final 'a' dropped) is optionally
   dropped in the General Tenses, except the Perfect.
   The Aorist forms, except that of the 3S, are made up by adding the
   terminations of the 5th form.
  Kale 598(b)
   Roots which do not lengthen their penultimate 'a' (see also #603)
   lenghten it optionally in the general tenses of the Passive, except
   in the Perfect, when 'ay' is dropped.
  Kale 598(c)
   The 3S of the Passive Aorist is formed by dropping 'ay' necessarily
   and adding 'i'.
  N.B. The Passive forms of roots of the 10th class do not differ from
   the Passive forms of Causals.
 "
)
(defun aorist-Deshpande-doc ()
 "Deshpande #37.
  SUMMARY :
  7 : 'a' + root + 'sa' + final termination : few roots ending in 'sh' and 'h'
  4 : 'a' + root + 's' + final termination : roots ending in cons or non-aa
  5 : 'a' + root + 'iSh' + final termination : roots ending in cons or non-aa
  6 : 'a' + root + 'siSh' + final termination : (P) roots ending in 'aa'
  2 : 'a' + root + 'a' + final termination : 
  1 : 'a' + root  + final termination : (P)
  3 : 'a' + reduplicated-root + 'a' + final termination : conj. 10

  There are seven different varieties, all of which take the past tense
  augment 'a', like the past imperfect. These seven varieties may be
  divided between two general classes, i.e. sigmatic aorist and
  non-sigmatic aorist.  The sigmatic aorist has an infix ''s' and the
  non-sigmatic aorist does not.
 SIGMATIC AORIST VARIETIES
  There are four varieties of the sigmatic aorist, depending on whether
  the form shows 'sa s iSh siSh'
  ** 'sa' variety (7th variety)
   This variety has the infix 'sa' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'sa' + final termination.
   Only a few roots ending in 'sh' and 'h' have this variety.
   Examples : 
    dish 6 P:
      adikShat adikShataam adikShan
      adikShaH adikShatam adikShata 
      adikSham adikShaava adikShaama
    duh 2 P : (NOTE: As shown, Kale has optional forms in 3S 2S 2P 1D)
      (adhukShata adugdha) adhukShaataam adhukShanta 
      (adhukShathaaH adugdhaaH) adhukShaathaam (adhukShadhvam adhugdhvam) 
       adhukShi (adhukShaavahi aduhvahi) adhukShaamahi
  ** 's' variety (4th variety)
   This variety has the infix 's' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 's' + final termination.
   This variety is generally used for several roots ending in consonsants
   or in vowels other than 'aa'.
   Examples :
    kRi 8 P:
       akaarShiit akaarShTaam akaarShuH 
       akaarShiiH akaarShTam akaarShTa 
       akaarSham akaarShva akaarShma
    shap 1 A:
       ashapta ashapsaataam ashapsata 
       ashapthaaH ashapsaathaam ashapdhvam (NOTE: D=ashabdhvam)
       ashapsi ashapsvahi ashapsmahi
  ** 'iSh' variety (5th variety)
   This variety has the infix 'iSh' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'iSh' + final termination.
   This variety is used for several roots ending in consonsants
   or in vowels other than 'aa'.   
   Examples :
    budh 1 P:
       abodhiit abodhiShTaam abodhiShuH 
       abodhiiH abodhiShTam abodhiShTa 
       abodhiSham abodhiShva abodhiShma
    shii 2 A:
       ashayiShTa ashayiShaataam ashayiShata 
       ashayiShThaaH ashayiShaathaam ashayidhvam (D=ashayiDhvam)
       ashayiShi ashayiShvahi ashayiShmahi
  ** 'siSh' variety (6th variety)
   This variety has the infix 'siSh' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'siSh' + final termination.
   Only a few roots ending in 'aa' have this variety, and there
   are no middle forms.   
   Examples :
    yaa 2 P:
       ayaasiit ayaasiShTaam ayaasiShuH 
       ayaasiiH ayaasiShTam ayaasiShTa 
       ayaasiSham ayaasiShva ayaasiShma

 NON-SIGMATIC AORIST VARIETIES
  These varieties do not have any kind of 's' infix.
  ** 'a' infix variety (2nd variety)
   This variety has the infix 'a' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root + 'a' + final termination.
   Examples :
    gam 1 P:
       agamat agamataam agaman 
       agamaH agamatam agamata 
       agamam agamaava agamaama
    vach 2 A: 
    (NOTE: is only 2P in conjugational tenses; but (Apte) is 'A' in
      non-conjugational tenses)
       avochata avochetaam avochanta 
       avochathaaH avochethaam avochadhvam 
       avoche avochaavahi avochaamahi
  ** zero infix or root aorist variety (1st variety)
   This variety has no infix  between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + root  + final termination.
   This variety is found only in the active (Parasmaipada)
   Examples :
    daa 3 P:
       adaat adaataam aduH 
       adaaH adaatam adaata 
       adaam adaava adaama
    bhuu 1 P:
       abhuut abhuutaam abhuuvan 
       abhuuH abhuutam abhuuta 
       abhuuvam abhuuva abhuuma
  ** reduplicated aorist variety (3rd variety)
   In this variety, the root undergoes reduplication and 
    there is an infix 'a' between the root and the final
   terminations, thus yielding the sequence of elements:
    'a' + reduplicated-root + 'a' + final termination.
   This variety is found mostly for 10 conjugation verbs and
   secondary verbs such as causatives.

   Examples :
    chur 10 P:
       achuuchurat achuuchurataam achuuchuran 
       achuuchuraH achuuchuratam achuuchurata 
       achuuchuram achuuchuraava achuuchuraama
    chur 10 A:
       achuuchurata achuuchuretaam achuuchuranta 
       achuuchurathaaH achuuchurethaam achuuchuradhvam 
       achuuchure achuuchuraavahi achuuchuraamahi
   NOTE: THE CAUSATIVE EXAMPLE is given on p. 316. I could
    not duplicate it.  By modifying it, the example of 'chur'
    above was seen to agree with the algorithm.
    much 6 P (causative):
       amuumuchat amuumuchataam amuumuchan 
       amuumuchaH amuumuchatam amuumuchata 
       amuumucham amuumuchaava amuumuchaama
    much 6 A (causative):
       amuumuchata amuumuchetaam amuumuchanta 
       amuumuchathaaH amuumuchethaam amuumuchadhvam 
       amuumuche amuumuchaavahi amuumuchaamahi

 "
)
(defvar aorist-voice nil)
(defvar aorist-passive-P nil)
(defvar aorist-sym nil) 
(defvar aorist-pada nil)

(defun aorist-varieties (dhaatu class pada upasargas &optional dbg)
 ; returns a list of numbers from 1 to 7, indicating
 ; which of the seven varieties of aorist apply.
 (let (ans  i n fname)
  (cond
   ((symbolp dhaatu)
    (setq aorist-tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   )
   ((and (listp dhaatu) dhaatu (symbolp (car dhaatu)))
    (setq aorist-tok
	  (car (ITRANS-parse-words-1 (symbol-name (car dhaatu)))))
   )
   (t (setq aorist-tok 'NA))
  )
  (setq aorist-pada pada)
  (setq n 7)
  (setq i 1)
  (while (< i 8)
   (setq fname (format "aorist-%s-P" i))
   (if (funcall (intern-soft fname) dhaatu class pada upasargas)
    (setq ans (append ans (list i)))
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun aorist-1-P (dhaatu class pada upasargas)
 ;Kale 532, 533
 (let ()
  (cond 
   ((not (equal pada 'P)) nil)
   ; I think next 3 cases take variety 1 necessarily
   ((member dhaatu '(i sthaa daa dhaa bhuu)) t)
   ((and (equal dhaatu 'paa) (equal class 1)) t) ; to drink
   ((kale-459-P dhaatu class pada aorist-tok) t)
   ; these are stated to optionally  take variety 1.
   ; they also may take variety 6.
   ; 'dhe' also takes variety 3.
   ((member dhaatu '(ghraa dhe sho so Cho)) t) 
  )
 )
)
(defun kale-459-P (dhaatu class &optional pada tok)
 ; roots treated as roots ending in 'aa'
 (let (lc)
  (if (not tok) (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu)))))
  (setq lc (elt (substring tok -1) 0)) ; assumes tok not empty
  (cond
   ((member lc '(e ai o)) t)
   ((member (list dhaatu class)
     ; when before a termination causing guna or vrddhi
     '((mi 5)
       (mii 9)
       (dii 4)
       (lii 9) (lii 4) ;optional 
    ))
    t
   )
  )
 )
)
(defun aorist-2-P (dhaatu class pada upasargas)
 ; Kale 539,
 (let ()
  (cond
   ((or (and (equal dhaatu 'Ri) (equal upasargas '(sam)))
	(and (equal dhaatu 'khyaa) upasargas)
	(and (equal dhaatu 'vach) upasargas)
        (and (equal dhaatu 'as) (equal class 4) upasargas) ; 'as (4) with prep'
	(equal dhaatu 'lip)
	(equal dhaatu 'sich)
	(equal dhaatu 'hve)
    )
    t; Kale 539
   )
   ((aorist-2-542-P dhaatu class pada upasargas) t)
   ((aorist-2-543-P dhaatu class pada upasargas) t)
   ((aorist-2-544-P dhaatu class pada upasargas) t)
  )
 )
)
(defun aorist-2-542-P (dhaatu class pada upasargas)
 (let (data found data1 dhaatu1 class1 pada1 upasargas1)
  ; The table is an abbreviated list of
  ; elements of form (dhaatu1 class1 pada1 upasargas1)
  ; Incomplete records are interpreted as follows:
  ; 'upasargas1' missing means a match with any 'upasargas'
  ; 'pada1' missing means that pada must be 'P'
  ; 'class1' missing means match with any 'class'. In this case,
  ; the element of 'data' is actually a symbol.
  ; Sometimes definitions are given by Kale on p.336; these
  ; are shown below, but do not enter the algorithm
  (setq data '(
   (khyaa 2 P) (khyaa 2 A (sam))
   (Ri 3 P) (Ri 3 A (sam))
   (sRi 1 P)
   (hve 1 P)
   (vach 2 P) ; also that substituted for 'bruu' P and A
   (vach 2 A)
   (sich 6 P) (sich 6 A)
   (lip 6 P) (lip 6 A)
   (as 4 P) (as 4 A (pari))
  ; From table p. 336
   (shak 5 P) (shak 5 A) (shak 4 P) (shak 4 A) ; (shak 4 A) also takes 4th,5th
   (uch 4 P) ; to collect
   much
   (luT 4 P) ; to wallow
   pat
   (klid 4 P) ; to be wet
   (kShvid 4 P)
   mad
   (mid 1 A) (mid 4 P) ; be unctuous, melt
   (vid 6 P) (vid 6 A) ; 'vid 6 A' takes 4th or 5th variety
   (shad 1) ; perish , decay
   sad
   svid
   (Ridh 4) (Ridh 5) ; prosper
   krudh
   kShudh
   (gRidh 4) ; covet
   (radh 4) ; hurt
   shudh
   sidh
   aap
   kup
   (gup 4) ; be confused
   (Dip 4) ; throw
   yup
   rup
   (lup 4 P) (lup 4 A) (lup 6 P) (lup 6 A)
   sRip
   kShubh
   tubh ; kill
   (nabh 4) ; kill
   lubh
   klam
   kSham
   gam
   tam
   dam
   bhram
   sham
   shram
   (sam 1) ; be confused or agitated
   (kRish 4) ; be thin
   nash
   (bhRish 4) ; fall
   bhraMsh
   (vRish 4) ; choose
   tuSh
   (tRiSh 4) ; be thirsty
   (duSh 4) ; be spoiled
   piSh
   puSh
   (pluSh 4) ; burn
   (riSh 4) ; injure, kill
   (ruSh 4) ; be angry or vexed
   (viSh 3 P) (viSh 3 A) ; spread through (viSh 3 A) takes 7th
   trup ; note: Kale p.337 has 'truSh', but this root not found
   (vyuSh 4) ; divide
   shiSh
   (shuSh 4) ; be dry
   hRiSh
   (kus 4) ; embrace
   (ghas 1) ; eat
   (jas 4) ; release
   (tas 4) ; fade-away
   (das 4) ; decay, perish
   (bas 4) ; be-straight
   (bis 4) ; go , direct
   (byus 4) ; throw
   (mas 4) ; weigh, change-form
   (mus 4) ; cut
   (yas 4) ; strive
   (vas 4) ; same as (bas 4)
   (vis 4) ; same as (bis 4)
   bus
   vus
   shaas
   druh
   muh
   snih 
   snuh
  ))
  (while (and data (not found))
   (setq data1 (car data))
   (setq data (cdr data))
   (setq found
    (cond
     ((symbolp data1) (and (equal data1 dhaatu) (equal pada 'P)))
     ((not (listp data1)) nil)
     ((equal (length data1) 2)
      (and (equal data1 (list dhaatu class)) (equal pada 'P))
     )
     ((equal (length data1) 3)
      (equal data1 (list dhaatu class pada))
     )
     ((equal data1 (list dhaatu class pada upasargas)))
    )
   )
  )
  found
 )
)
(defun aorist-2-543-P (dhaatu class pada upasargas)
 (let (data found data1 dhaatu1 class1 pada1 upasargas1)
  ; The table is an abbreviated list of
  ; elements of form (dhaatu1 class1 pada1 upasargas1)
  ; Incomplete records are interpreted as follows:
  ; 'upasargas1' missing means a match with any 'upasargas'
  ; 'pada1' missing means that pada must be 'P'
  ; 'class1' missing means match with any 'class'. In this case,
  ; the element of 'data' is actually a symbol.
  ; Sometimes definitions are given by Kale on p.336; these
  ; are shown below, but do not enter the algorithm
  (setq data '(
   shvi
   jRI
   gruch
   gluch
   glu~nch
   mruch
   mluch
   rich
   vich
   shuch
   nij
   yuj
   vij
   sphuT
   chut
   chyut
   jyut
   shchut
   shchyut
   kShud
   Chid
   ChRid
   tRid
   bund
   bhid
   rud
   skand
   budh
   rudh
   tRip
   dRip
   stambh
   dRish
   shliSh
   ghuSh
   uh
   tuh
   duh
   bRih
  ))
  (while (and data (not found))
   (setq data1 (car data))
   (setq data (cdr data))
   (setq found
    (cond
     ((symbolp data1) (and (equal data1 dhaatu) (equal pada 'P)))
     ((not (listp data1)) nil)
     ((equal (length data1) 2)
      (and (equal data1 (list dhaatu class)) (equal pada 'P))
     )
     ((equal (length data1) 3)
      (equal data1 (list dhaatu class pada))
     )
     ((equal data1 (list dhaatu class pada upasargas)))
    )
   )
  )
  found
 )
)
(defun aorist-2-544-P (dhaatu class pada upasargas)
 (if (member (list dhaatu class pada) '( 
  (ruch 1 A)
  (ghuT 1 A)
  (ruT 1 A)
  (luT 1 A)
  (luTh 1 A)
  (dyut 1 A)
  (vRit 1 A)
  (shvit 1 A)
  (kShvid 1 A)
  (bhid 7 A)
  (syand 1 A)
  (svid 1 A)
  (vRidh 1 A)
  (shRidh 1 A)
  (kLip 1 A)
  (kShubh 1 A)
  (tubh 1 A)
  ;(nubh 1 A) ; This root not found??
  (shubh 1 A)
  (sraMbh 1 A)
  (bhrash 1 A)
  (bhraMsh 1 A)
  (dhvaMs 1 A)
  (bhraMs 1 A)
  (sraMs 1 A)
  )
 )
 t)
)
(defun aorist-3-P (dhaatu class pada upasargas)
 (let ()
  (cond
   ((equal class 10) t) ; Kale 546
   ((equal dhaatu 'kam) t) ; Kale 546
   ((member dhaatu '(shri dru sru)) t) ; Kale 546 
   ((member dhaatu '(dhe shvi)) t) ; Kale 546, 533
   ((equal class 11) t) ; I use class '11' for the causal aorist
  )
 )
)
(defun aorist-6-P (dhaatu class pada upasargas)
 (let (tok lc)
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0)) ; assumes tok not empty
  (cond
   ((not (equal pada 'P)) nil) ; 6th form is only parasmaipada
   ((member class '(10 11)) nil) ; 10th and 11th (causal) take 3rd variety
   ; next three classes take variety 1 only 
   ((member dhaatu '(i sthaa daa dhaa bhuu)) nil)
   ((and (equal dhaatu 'paa) (equal class 1)) nil) ; to drink
   ((member dhaatu '(ghraa dhe sho so Cho)) t); Kale 533
   ((member dhaatu '(yam ram nam)) t) ; Kale 558
   ((member dhaatu '(mi mii lii)) t) ; Kale 559
   ((member dhaatu '(daridraa)) t) ; Kale 559 
   ((kale-459-P dhaatu class pada aorist-tok) nil)
   ((aorist-2-542-P dhaatu class pada upasargas) nil) ; 2 necessarily 
   ((equal lc 'aa) t)
  )
 )
)
(defun aorist-7-P (dhaatu class pada upasargas)
 (let (ans seT-code tok lc pc)
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
  (setq seT-code (solution seT-code))
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0)) ; last
  (setq pc (and (< 1 (length tok)) (elt (substring tok -2 -1) 0))) ;penultimate
  (cond
   ((equal (list dhaatu class pada) '(viSh 3 A)) t)
   ((and (member seT-code '(aniT veT))
	 (member lc '(sh Sh s h))
	 (member pc '(i u Ri Li))) t) ; Kale 561
   ((member dhaatu '(mRish spRish kRiSh)) t) ; Kale 562. Redundant by 561
  )
 )
)
(defun aorist-4-P (dhaatu class pada upasargas)
 (let (ans seT-code tok lc types parts)
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
  (setq seT-code (solution seT-code))
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0)) ; last
  (let ( wparts)
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
  )
  (cond
   ((member (list dhaatu pada) ; Kale 536
    '((daa A) (dhaa A) (sthaa A)))
    t
   )
   ((equal dhaatu 'han) ; Kale 569
    (if (equal pada 'A) t nil) ; is 'A' with prefix 'aa'
   )
   ((and (equal dhaatu 'i) (equal pada 'A) (equal upasargas '(adhi)))
    t; Kale 536
   )
   ((and (equal dhaatu 'vas) (equal pada 'A)) nil) ; is seT, takes 5  p350 ftn
    
   ((member (list dhaatu class pada)
    '((lip 6 A) (sich 6 A) (hve 6 A) ; Kale footnote p.335
      (shak 4 A) (vid 6 A) (lup 4 A) (lup 6 A) ; Kale footnote p. 336
     )
    )
    t
   )
   ((member dhaatu '(rich vich nij yuj vij kShud Chid
                     bhid skand budh rudh dRip dRish shliSh duh
		     tRip ; this is 'veT'
		    )
    )
    ; Kale#542. This aniT roots take 2nd and 4th varieties
    t
   )
   ((and (equal dhaatu 'stu) (equal pada 'P)) nil); Kale 565 in variety5
   ((and (equal dhaatu 'su) (equal pada 'P)) nil); Kale 565 in variety5
   ((and (equal dhaatu 'a~nj) (equal pada 'P)) nil) ;Kale 565 . variety5
   ((and (equal dhaatu 'dhuu) (equal pada 'P)) nil) ;Kale 565 . variety5
   ((and (equal dhaatu 'dhuu) (equal pada 'A)) t) ;Kale 565 . 4th or 5th
   ((and (equal seT-code 'aniT)
	 (aorist-6-P dhaatu class pada upasargas)
    )
    nil ; Kale 565(a)
   )
   ((and (equal seT-code 'aniT)
	 (not (aorist-1-P dhaatu class pada upasargas))
	 (not (aorist-2-P dhaatu class pada upasargas))
	 (not (aorist-3-P dhaatu class pada upasargas))
	 (not (aorist-6-P dhaatu class pada upasargas))
	 (not (aorist-7-P dhaatu class pada upasargas))
    )
    t ; Kale 565(a)
   )
   ((equal seT-code 'veT) t) ; Kale 565(a)
   ((and (equal pada 'A)
	 (equal lc 'Ri)
	 (equal types "CV") (< 1 (length (elt parts 0))) ; conjunct cons
    )
    ; Kale 565. Takes 4th or 5th
    t
   )
   ((and (equal seT-code 'seT)
	 (equal dhaatu 'vRi)
	 (equal pada 'A)) t) ; Kale 565(4) 4th or 5th
   ((and (equal seT-code 'seT)
	 (equal lc 'RI)
	 (equal pada 'A)) t) ; Kale 565(4) 4th or 5th
   ((and (equal dhaatu 'snu) (equal pada 'A)) t) ;Kale 565(4)
   ((and (equal dhaatu 'kram) (equal pada 'A)) t) ;Kale 565(4)
   ((member dhaatu '(mRish spRish kRiSh)) t) ; Kale 562.
  )
  
 )
)
(defun aorist-5-P (dhaatu class pada upasargas)
 (let (ans seT-code tok lc types parts)
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
  (setq seT-code (solution seT-code))
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (setq lc (elt (substring tok -1) 0)) ; last
  (let ( wparts)
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
  )
  (cond
   ((equal dhaatu 'han) t) ; Kale 569
   ((and (equal dhaatu 'bhuu) (equal pada 'A)) t) ; Kale 536
   ((member (list dhaatu class pada)
    '((shak 4 A) (vid 6 A)  ; Kale footnote p. 336
     )
    )
    t
   )
   ((member dhaatu '(shvi jRI gruch gluch glu~nch
                     mruch mluch shuch sphuT chut
		     chyut jyut shchut shchyut ChRid
		     tRid bund rud stambh ghuSh
		     uh tuh bRih
		     tRip ; this is veT
		    )
     )
    ; Kale#542. This seT roots take 2nd and 5th varieties
    t
   )
   ((equal dhaatu 'daridraa) t) ; Kale 578
   ((and (equal dhaatu 'stu) (equal pada 'P)) t); Kale 565 in variety5
   ((and (equal dhaatu 'su) (equal pada 'P)) t); Kale 565 in variety5
   ((and (equal dhaatu 'a~nj) (equal pada 'P)) t) ;Kale 565 . variety5
   ((and (equal dhaatu 'dhuu) (equal pada 'P)) t) ;Kale 565 . variety5
   ((and (equal dhaatu 'dhuu) (equal pada 'A)) t) ;Kale 565 . 4th or 5th
   ((and (equal pada 'A)
	 (equal lc 'Ri)
	 (equal types "CV") (< 1 (length (elt parts 0))) ; conjunct cons
    )
    ; Kale 565. Takes 4th or 5th
    t
   )
   ((and (equal seT-code 'seT)
	 (equal dhaatu 'vRi)
	 (equal pada 'A)) t) ; Kale 565(4) 4th or 5th
   ((and (equal seT-code 'seT)
	 (equal lc 'RI)
	 (equal pada 'A)) t) ; Kale 565(4) 4th or 5th
   ((aorist-3-P dhaatu class pada upasargas) nil) ; exclude these
   ((and (not (aorist-1-P dhaatu class pada upasargas))
	 (not (aorist-2-P dhaatu class pada upasargas))
	 (not (aorist-3-P dhaatu class pada upasargas))
	 (not (aorist-6-P dhaatu class pada upasargas))
	 (not (aorist-7-P dhaatu class pada upasargas))
	 (not (aorist-4-P dhaatu class pada upasargas))
    )
    t ; Kale 573(a)
   )
   ((and (equal dhaatu 'vas) (equal pada 'A)) t) ; is seT, takes 5  p350 ftn
   ((and (equal dhaatu 'gaah) (equal pada 'A)) t) ; ftnnote p. 350. Also 4th
   ((or (consonant-P lc)
	(and (vowel-P lc) (not (equal lc 'aa)))
    )
    t ; Antoine2#221 p. 155
   )
  )
 )
)

(defun aorist (dhaatu class pada &optional upasargas voice)
 (mapcar
  (lambda (x)
   (let (y)
    (setq y (intern (format "lu~N%s" x)))
    (list x (construct-conjtab1a dhaatu class pada upasargas y voice))
   )
  )
  [1 2 3 4 5 6 7]
 )
)
(defun conjugation-tab-aorist (upasargas class pada dhaatu &optional variety voice dbg)
 (when dbg
  (fol-msg (format "conjugation-tab-aorist %s\n" (list upasargas class pada dhaatu variety voice)))
 )
; When 'variety' is non-nil, the answer is nil or 
;   an array which is the conjugation table for this variety of the aorist.
; When 'variety' is nil and 'voice' is 'ACTIVE', 
;   the answer is a list with elements of the form
;   (n ctab), where 'n' is a variety (1-7) and 'ctab' the associated 
;   conjugation table.
; When 'variety is nil and 'voice' is 'PASSIVE',
;  the answer is 'nil' or a sequence (the conj. tab. for PASSIVE aorist)
; When 'variety' is 'ALL, then all aorists are returned, without
;   regard for the applicability screening of 'aorist-varieties'
 (when nil
      (fol-msg (format "conjugation-tab-aorist : %s %s %s %s : %s %s\n"
	       dhaatu class pada upasargas variety voice))
  )
 (setq aorist-voice (if (equal voice 'PASSIVE) 'PASSIVE 'ACTIVE))
 (setq aorist-passive-P (equal voice 'PASSIVE))
 (let (varieties ans fname thisans lc)
  (let (d)
   (setq d
    (cond
     ((symbolp dhaatu) dhaatu) ; normal case
     ((listp dhaatu) (car dhaatu)) ; used for causals
    )
   )
   (setq varieties (aorist-varieties d class pada upasargas))
  )
  ; 04-14-05. Essentially omit 'aorist-varieties' check
  (setq varieties '(1 2 3 4 5 6 7))
  (setq lc (if aorist-tok (elt (substring aorist-tok -1) 0)))
  (when variety
   (if (member variety varieties)
    (setq varieties (list variety))
    (progn
     (when nil
      (fol-msg (format "conjugation-tab-aorist Err: %s %s %s %s : %s %s\n"
	       dhaatu class pada upasargas variety varieties))
     )
     (setq varieties nil)
    )
   )
  )
  (when (equal variety 'ALL)
   (setq varieties '(1 2 3 4 5 6 7))
  )
  (mapcar 
   (lambda (z) ; z=given aorist variety
    (let (x) ; x=computed aorist variety (different for passive)
     (cond
      ((not aorist-passive-P) (setq x z))
      ((member z '(4 5 7)) (setq x z))
      ((member z '(1 2))
       (cond
	((member dhaatu '(jRI))
	 (setq x 5) ; either 4 or 5 works
	)
	((member dhaatu '())
	 (setq x 4)
	)
	(t
	 (if (vowel-P lc)
	  (setq x 4)
	  (setq x 5)
         )
	)
       )
      )
      ((equal z 6) (setq x 7))
      ((member class '(10 11)) (setq x 5))
      ((equal z 3)
       (cond
	((member dhaatu '(dru sru))
	 (setq x 4)
	)
	(t (setq x 5))
       )
      )
      (t (setq x z))
     )
     (setq fname (format "conjugation-tab-aorist%s" x))
     (setq aorist-id x)
     (setq aorist-sym (intern-soft (format "lu~N%s" x)))
;     (fol-msg (format "chk: %s\n" fname))
     (setq thisans (funcall (intern-soft fname) upasargas class pada dhaatu))
     (when nil
      (fol-msg (format "conjugation-tab-aorist. thisans=%s\n" thisans))
     )
     (when thisans 
      (when (and aorist-passive-P
;		 (not (member class '(10 11)))
	    )
	; get 3S independently
       (let (thisans0)
	(setq thisans0 (aorist-passive-3S upasargas class pada dhaatu))
        (when nil
         (fol-msg (format "conjugation-tab-aorist. thisans0=%s\n" thisans0))
        )
	(aset thisans 0 thisans0)
       )
      )
      (if (not ans) (setq ans thisans) (setq ans (join-arrays ans thisans)))
      (when nil
      (fol-msg (format "conjugation-tab-aorist. ans=%s\n" ans))
      )
      (when aorist-passive-P
       ; Kale 597(e), refers 596(b)
       (let (n btab endings i x y z w tok ending wold w1)
	(setq endings (aorist-endings 'lu~N5))
	(setq n (length endings))
	(setq tok aorist-tok)
        (cond
	 ((member lc '(aa e ai o))
	  (setq x (vconcat (substring tok 0 -1) [aa y]))
	 )
	 ((vowel-P lc)
	  (setq x (vconcat (substring tok 0 -1)
			   (if (equal lc 'e) [ai] (vrddhi lc))))
	 )
	 ((equal dhaatu 'han) (setq x [gh aa n]))
	 ((equal dhaatu 'grah) (setq x [g r aa h]))
	 ((equal dhaatu 'dRish) (setq x [d a r sh]))
; 	 ((equal class 10) (setq x tok)) ; Kale 598 (moved to aorist5)
	)
	(when x
	 (setq btab (make-vector n nil))
	 (setq i 1) ; 3S (i=0) does not get this form
	 (while (< i n)
	  (setq ending (elt endings i))
	  (setq y (augment-a x))
	  (setq z (conjugation-join y ending))
	  (setq w (sym-without-space z))
	  (setq wold (elt ans i))
	  (if (not (listp wold)) (setq wold (list wold)))
	  (setq w1 (append-if-new wold w))
	  (aset ans i w1)
	  (setq i (1+ i))
	 )
	)
       )
      )
     )
    )
   )
   varieties
  )
  ans
 )
)
(defun conjugation-tab-aorist1 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq tok aorist-tok)
  (setq aorist-sym 'lu~N1)
  (let ( wparts)
   (setq ylast (elt (substring tok -1) 0))
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
   (if (equal types "CVC") (setq pc (elt (elt parts 1) 0)))
  )
  (setq seT-code '(aniT))
  (setq ans (conjugation-tab-aorist1-main
    upasargas class pada dhaatu seT-code))
  ans
 )
)
(defun conjugation-tab-aorist1-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab
       wparts parts types tense-sym)
  (when nil
   (fol-msg (format "aorist-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5b. get table of base-seT codes (bitab)
  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (cond
      ((member types '("CVC" "VC"))
       ;e.g. prachCh -> ([[p r] [a] [ch Ch]] "CVC"), pc = 'a'
       (setq pc (elt (elt (substring parts -2 -1) 0) 0))
      )
      (t (setq pc (elt b (- nb 2))) ; penultimate char
      )
     )
    )
    ;-- step1: modify 'b' as appropriate for this aorist
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((or (kale-459-P dhaatu class pada b)
	(member dhaatu '(sho so Cho))
      )
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
     ((equal dhaatu 'i)  (setq b [g aa]))
    )
    (setq bc b) ; before consonant endings
    (setq bv (substring b 0 -1)) ; before vowel endings
    (cond
     ((equal dhaatu 'bhuu)
      (setq bv (vconcat b [v]))
      (aset endings 2 [a n]) ; 3P is 'an' instead of 'us'
     )
     ((equal (substring b -1) [aa])
      ;  [a m] treated like a consonant except for 'bhuu'
      (aset endings 6 (substring (elt endings 6) 1))
     )
    )
    ;-- step1a: All aorists use augment 'a'
;    (fol-msg (format "bc=%s, bv=%s\n" bc bv))
;    (fol-msg (format "endings=%s\n" endings))
    (let (n i x y ending z w efirst )
     (setq n (length endings))
     (setq btab (make-vector n nil))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq efirst (elt ending 0))
      (if (vowel-P efirst)
       (setq x bv)
       (setq x bc)
      )
      ; prefix augment 'a'
      (setq y (augment-a x))
;      (if (equal i 6) (fol-msg (format "%s %s\n" y ending)))
      (setq z (conjugation-join y ending))
      (setq w (sym-without-space z))
      (aset btab i w)
      (setq i (1+ i))
     )
    )
    ;-- step2: construct bitab
   )
  btab
 )
)
(defun conjugation-tab-aorist2 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq tok aorist-tok)
  (setq aorist-sym 'lu~N2)
  (let ( wparts)
   (setq ylast (elt (substring tok -1) 0))
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
   (if (equal types "CVC") (setq pc (elt (elt parts 1) 0)))
  )
  (setq seT-code '(aniT))
;  (fol-msg (format "chk1: %s %s %s %s\n" dhaatu class pada upasargas))
  (when (and (equal pada 'A) (aorist-2-544-P dhaatu class pada upasargas))
   ;Kale 544. these atmanepada verbs are conjugated in Parasmaipada
   ; when in the 2nd variety
   (setq pada 'P)
   (setq aorist-pada pada)
  )
;  (fol-msg (format "chk1: %s %s %s %s\n" dhaatu class pada upasargas))
  (setq ans (conjugation-tab-aorist2-main
    upasargas class pada dhaatu seT-code))
  ans
 )
)
(defun conjugation-tab-aorist2-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab
       wparts parts types tense-sym)
  (when nil
   (fol-msg (format "aorist-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))  
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5b. get table of base-seT codes (bitab)
  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (setq pc (elt b (- nb 2)))
    )
;   (fol-msg (format "pc=%s\n" pc))
    ;-- step1: modify 'b' as appropriate for this aorist
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((or (member lc '(Ri RI))
	  (equal dhaatu 'dRish)
      )
      ; Kale 538: gunate final 'Ri' 'RI' or the 'Ri' in 'dRish'
      (setq b (gunate-final-vowel b))
     )
     ((or (nasal-P pc)
	  (equal pc 'M)
      )
      ; Kale 539: drop penultimate nasal
      (setq b (vconcat (substring b 0 -2) (substring b -1)))
     )
     ((equal dhaatu 'as) (setq b [a s th])) ; Kale 541
     ((equal dhaatu 'khyaa) (setq b [kh y]))
     ((equal dhaatu 'pat) (setq b [p a p t]))
     ((equal dhaatu 'vach) (setq b [v o ch]))
     ((equal dhaatu 'shaas) (setq b [sh i Sh]))
     ((equal dhaatu 'shvi) (setq b [sh v]))
     ((equal dhaatu 'hve) (setq b [h v]))
    )
;    (setq bc b) ; before consonant endings
;    (setq bv (substring b 0 -1)) ; before vowel endings
    
    ;-- step1a: All aorists use augment 'a'
   (when nil
    (fol-msg (format "b=%s\n" b))
    (fol-msg (format "endings=%s\n" endings))
   )
    (let (n i x y ending z w efirst )
     (setq n (length endings))
     (setq btab (make-vector n nil))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq efirst (elt ending 0))
      (setq x b)
      ; In this aorist, 'a' is added to the root;
      ; this 'a' is
      ;   dropped before 1S [a m]
      ;   dropped before 3P [a n]
      ;   dropped before 3P [a n t a]
      ;   lengthened before 'v' and 'm'
      (cond
       ((equal efirst 'a)) ; no action
       ((member efirst '(v m)) (setq x (conjugation-join x [aa])))
       (t (setq x (conjugation-join x [a])))
      )
      (setq y (augment-a x)) ; prefix augment 'a'
      (setq z (conjugation-join y ending))
      (setq w (sym-without-space z))
      (aset btab i w)
      (setq i (1+ i))
     )
    )
    ;-- step2: construct bitab
   )
  btab
 )
)
(defun conjugation-tab-aorist3 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq tok aorist-tok)
  (setq aorist-sym 'lu~N3)
  (setq seT-code '(aniT))
  (setq ans (conjugation-tab-aorist3-main
    upasargas class pada dhaatu seT-code))
  ans
 )
)
(defun conjugation-tab-aorist3-main (upasargas class pada dhaatux
					  &optional seTCode i-insert)
 (let (dhaatu base endings strengths ans n atok seT-gen btab
       wparts parts types tense-sym Eng-def dbg)
  ;(setq dbg t)
  (when dbg
   (fol-msg (format "aorist3-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatux seTCode i-insert))
  )
  ;--- 0. interpret 'dhaatux'
  ;--- dhaatux is special in aorist3.
  ; If 'dhaatux' is a symbol, then it is just the 'dhaatu'
  ; If 'dhaatux' is a list, its first symbol should be the 'dhaatu'
  ; and its second symbol should be the base, ending in 'ay'
  ; This second form allows for the distinction by definition
  ; of certain class 10 forms otherwise indistinguishable.
  ; aorist-tok refers to 'dhaatu' in either sense of 'dhaatux'
  (cond
   ((listp dhaatux)
    (setq dhaatu (car dhaatux))
    (setq base (cadr dhaatux)) ; a symbol
   )
   (t
    (setq dhaatu dhaatux)
   )
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)  
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )  
  ;--- 5b. get table of base codes (btab)
  (let (b nb lc pc b1)
    (if base
     (setq b (car (ITRANS-parse-words-1 (symbol-name base))))
     (setq b atok)
    )
    ;-- step1: modify 'b' as appropriate for this aorist
    ;-- reduplicate the root
    (setq b1 (reduplicate b)) ; Kale 547
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((equal dhaatu 'dhe)
      (setq b1 [d a dh]) ; Kale 547
     )
     ((member class '(10 11)) ; we use this for causal and class=10
      (or
       (setq b1 (aorist-causal-base-irreg dhaatu class pada upasargas Eng-def))
       (setq b1 (aorist-causal-base dhaatu class pada upasargas Eng-def))
      )
     )
    )
    ;-- step2: construct btab
    (when dbg
     (fol-msg (format "aorist3-main: b1=%s\n" b1))
    )
    (setq b1 (solution b1))
    
    (if (not (listp b1)) ; usual case
     (setq btab (aorist3-make-btab b1 endings))
     (progn ; alternate case : 2 options
      (let (b1tab b2tab)
       (setq b1tab (aorist3-make-btab (elt b1 0) endings))
;       (fol-msg (format "b1tab=%s\n" b1tab))
       (setq b2tab (aorist3-make-btab (elt b1 1) endings))
;       (fol-msg (format "b2tab=%s\n" b2tab))
       (setq btab (join-arrays b1tab b2tab))
;       (fol-msg (format "btab=%s\n" btab))
      )
     )
    )
   )
  btab
 )
)
(defun aorist3-make-btab (b1 endings)
    (let (n i x y ending z w efirst btab)
     (setq n (length endings))
     (setq btab (make-vector n nil))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq efirst (elt ending 0))
      (setq x b1)
      ; Kale 547. final 'o' is dropped
      ;Kale 547: final 'i' changes to 'iy' and final 'u' changes to 'uv
      ; before the appended 'a'
      (cond
       ((equal (substring x -1) [o])
	(setq x (substring x 0 -1))
       )
       ((equal (substring x -1) [i])
	(setq x (vconcat (substring x 0 -1) [i y]))
       )
       ((equal (substring x -1) [u])
	(setq x (vconcat (substring x 0 -1) [u v]))
       )
      )
      ; In this aorist, 'a' is added to the root;
      ; this 'a' is
      ;   dropped before 1S [a m]
      ;   dropped before 3P [a n]
      ;   dropped before 3P [a n t a]
      ;   lengthened before 'v' and 'm'
      (cond
       ((equal efirst 'a)) ; no further action required.
       ((member efirst '(v m)) (setq x (conjugation-join x [aa])))
       (t (setq x (conjugation-join x [a])))
      )
      (setq y (augment-a x)) ; prefix augment 'a'
      (setq z (conjugation-join y ending))
      (setq w (sym-without-space z))
      (aset btab i w)
      (setq i (1+ i))
     )
     btab
    )
)
(defun conjugation-tab-aorist6 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq tok aorist-tok)
  (setq aorist-sym 'lu~N6)
  (setq seT-code '(aniT))
  (setq ans (conjugation-tab-aorist6-main
    upasargas class pada dhaatu seT-code))
  ans
 )
)
(defun conjugation-tab-aorist6-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab
       wparts parts types tense-sym)
  (when nil
   (fol-msg (format "aorist-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))  
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )  
  ;--- 5b. get table of base-seT codes (bitab)
  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (setq pc (elt b (- nb 2)))
    )
;   (fol-msg (format "pc=%s\n" pc))
    ;-- step1: modify 'b' as appropriate for this aorist
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((vowel-P lc) ; Kale 559 examples
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
    )
    
    ;-- step1a: All aorists use augment 'a'
   (when nil
    (fol-msg (format "b=%s\n" b))
    (fol-msg (format "endings=%s\n" endings))
   )
    (let (n i x y ending z w efirst)
     
     (setq n (length endings))
     (setq btab (make-vector n nil))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq efirst (elt ending 0))
      (setq x b)

      (setq y (augment-a x)) ; prefix augment 'a'
      (setq z (conjugation-join y ending))
      (setq w (sym-without-space z))
      (aset btab i w)
      (setq i (1+ i))
     )
    )
   )
  btab
 )
)
(defun conjugation-tab-aorist7 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq aorist-sym 'lu~N7)
  (setq seT-code '(aniT))
  (if aorist-passive-P 
   (progn
    (setq aorist-pada 'A)
    (setq ans (conjugation-tab-aorist7-main
     upasargas class 'A dhaatu seT-code))
    (setq aorist-pada pada)
   )
   (setq ans (conjugation-tab-aorist7-main
     upasargas class pada dhaatu seT-code))
  )
  ans
 )
)
(defun conjugation-tab-aorist7-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab
       wparts parts types tense-sym)
  (when nil
   (fol-msg (format "aorist-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seT-code i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))  
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5b. get table of base-seT codes
  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (setq pc (elt b (- nb 2)))
    )
    ;-- step1a: All aorists use augment 'a'
   (when nil
    (fol-msg (format "b=%s\n" b))
    (fol-msg (format "endings=%s\n" endings))
   )
    (let (n i x y ending z w efirst)
     (setq n (length endings))
     (setq btab (make-vector n nil))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq x b)

      (setq y (augment-a x)) ; prefix augment 'a'
;      (setq z (conjugation-join y ending))
      (setq z (aorist-join y seT-gen  ending dhaatu 'S i-insert))
      (setq w (sym-without-space z))
      (aset btab i w)
      (when (and (equal pada 'A)
		 (member dhaatu '(duh dih lih guh))
		 (member i '(0 3 5 7)) ; 3S 2S 2P 1D
	    )
       ; Kale 563. optionally drop the initial 'sa' or 'saa' of ending
       (let (w1)
	(when (<= 2 (length ending)) ; in passive this will be overwritten
	 (setq ending (substring ending 2)) ; drop 1st two
	)
;	(setq z (conjugation-join y ending))
	(setq z (aorist-join y seT-gen  ending dhaatu 'S i-insert))
	(setq w1 (sym-without-space z))
	(aset btab i (list w w1))
       )
      )

      (setq i (1+ i))
     )
    )
   )
  btab
 )
)
(defun conjugation-tab-aorist4 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq aorist-sym 'lu~N4)
  (if aorist-passive-P 
   (progn
    (setq aorist-pada 'A)
    (setq ans (conjugation-tab-aorist4-main
     upasargas class 'A dhaatu))
    (setq aorist-pada pada)
   )
   (setq ans (conjugation-tab-aorist4-main
     upasargas class pada dhaatu))
  )
  ans
 )
)
(defun conjugation-tab-aorist4-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab 
       wparts parts types tense-sym dbg)
  (setq dbg nil)
  (when dbg
   (fol-msg (format "aorist-main4: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  
  ;--- 5b. ans (btab)

  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (cond
     ((equal types "VC")
      (setq pc (elt (elt parts 0) 0)) ;the vowel
     )
     ((equal types "CVC")
      (setq pc (elt (elt parts 1) 0)) ;the vowel
     )
     ((equal types "CV")
      (setq pc (elt (elt parts 0) 0)) ;the consonant
     )
     (t (setq pc nil))
    )
;   (fol-msg (format "pc=%s\n" pc))
    ;-- step1a: All aorists use augment 'a'
    (when nil
     (fol-msg (format "b=%s\n" b))
     (fol-msg (format "endings=%s\n" endings))
    )
    (let (n i x y ending z w efirst xalt lcalt)
     (setq n (length endings))
     (setq btab (make-vector n nil))
     ; alter base 'b' into 'x'. Get xalt (usu. nil)
     (setq x b)
     (cond
      ((member dhaatu '(sRij dRish))
	; Kale p. 348, footnote 1
	; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
        ; is changed to 'ra' before
        ; a consonantal strong termination in the general tenses.
	(setq b (vconcat (substring b 0 -2) [r a] (substring b -1)))
	(setq pc 'a)
      )
      ((equal dhaatu 'vas)
       ; Antoine2#220  p. 155.  'vas' becomes 'vat' in the aorist
       ; Without this, the result differs in 3D 2D and 2P, namely
       ; 'avaastaam' instead of 'avaattaam', etc.
       ; Kale (footnote p. 350, argues a derivation
       ; 'avaas' + 'staam' -> 'avaattaam' by 480; however, by 567,
       ; the ending would have been modified to 'taam', so I don't agree
       ; with the derivation. Thus, the inclusion of this special case.
       (setq b [v a t])
       (setq lc 't)
      )
     )
;    (fol-msg (format "%s : %s %s %s\n" dhaatu b lc pc))
     (cond
      ((equal pada 'P)
       (setq x (aorist-gunate-final-vowel b t)) ; vrddhi. Kale 566a
      )
      ((equal pada 'A)
       (cond
	((equal dhaatu 'mii) (setq x [m aa])) ; Kale example p.352
        ((member lc '(i ii u uu))
         (setq x (aorist-gunate-final-vowel b)) ; gunate. Kale 566b
        )
        ((equal lc 'RI)
	 ; see also function 'kale-394 in gram2.el
         (setq x (substring b 0 -1))
         (if (or (labial-P pc) (equal pc 'v))
          (setq x (vconcat x [uu r]))
          (setq x (vconcat x [ii r]))
         )
        )
        ((or (member dhaatu '(daa dhaa sthaa))
	     (kale-459-P dhaatu class pada))
	 ; Kale 568.
	 (setq x (vconcat (substring b 0 -1) [i]))
	)
	((equal dhaatu 'han)
	 (setq x (substring b 0 -1)) ; Kale 569
	)
	((and (equal dhaatu 'yam) (not (equal upasargas '(upa))))
	 ; Kale 570.
	 (setq x (substring b 0 -1))
	)
       )
      )
     )
     (cond
      ((and (or
	     (member seT-gen '(aniT))
	     (equal dhaatu 'tRip) ; I have tRip as 'veT' (not sure)
	    )
            (equal pc 'Ri)
	    (equal types "CVC")
	    (equal pada 'P)
       )
       ; Kale 566c.
       (setq xalt (vconcat (elt parts 0) [r aa] (elt parts 2)))
       (setq lcalt (elt (substring xalt -1) 0))
      )
      ((and (equal dhaatu 'gam) (equal pada 'A)) ; Kale 570
       (setq xalt (substring x 0 -1)) ; drop nasal optionally
       (setq lcalt (elt (substring xalt -1) 0))
      )
      ((and (equal dhaatu 'yam) (equal pada 'A)
	    (equal upasargas '(upa))) ; Kale 570
       (setq xalt (substring x 0 -1)) ; drop nasal optionally
       (setq lcalt (elt (substring xalt -1) 0))
      )
      ((and (equal dhaatu 'i) (equal pada 'A) (equal upasargas '(adhi)))
       ; Kale p. 352
       (setq xalt [g ii])
       (setq lcalt (elt (substring xalt -1) 0))
      )
     )
     ; reset lc as last letter of 'x'
     (setq lc (elt (substring x -1) 0))
     (when dbg
      (fol-msg (format "lc=%s,lcalt=%s,x=%s,xalt=%s\n" lc lcalt x xalt))
     )
     ;loop through combining base with endings, and prefixing 'a'
;     (fol-msg (format "%s %s %s\n" dhaatu class pada))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
	
;      (fol-msg (format "lc=%s, ending=%s\n" lc ending))
      (when (and
	     (equal aorist-voice 'ACTIVE)
	     (member (substring ending 0 2) '([s t] [s th]))
	     (or
	      (and (consonant-P lc)
		   (not (or (nasal-P lc) (semivowel-P lc))))
	      (equal lc 'h) ; (semivowel-P 'h) -> t
	      (shortvowel-P lc)
             )
	    )
       ; Kale 567
       (setq ending (substring ending 1))
;       (fol-msg (format "dropped s : %s \n" lc))
      )
      (setq y (augment-a x)) ; prefix augment 'a'
      
      (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
      (when nil
       (fol-msg (format "aorist4: i=%s, y=%s, ending=%s => %s\n" i y ending z))
      )

      (setq w (sym-without-space z))
      (aset btab i w)
      (when xalt
       (let (w1)
	(setq ending (elt endings i))
	(when (and
	       (equal aorist-voice 'ACTIVE)
	       (member (substring ending 0 2) '([s t] [s th]))
	       (or
	        (and (consonant-P lcalt)
		     (not (or (nasal-P lcalt) (semivowel-P lcalt))))
	        (equal lcalt 'h) ; (semivowel-P 'h) -> t
	        (shortvowel-P lcalt)
	       )
              )
         ; Kale 567
         (setq ending (substring ending 1))
        )
        (setq y (augment-a xalt)) ; prefix augment 'a'
        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
        (setq w1 (sym-without-space z))
	(aset btab i (list w w1))
       )
      )
      
      (when (and (equal dhaatu 'pad) (equal pada 'A))
       (if (equal i 0) (aset btab i 'apaadi)) ; Kale #571 p. 351
       (if (equal i 5) (aset btab i 'apadhvam)) ; Kale p. 352. was 'apaddhvam'
      )
      (when (and (equal dhaatu 'budh) (equal pada 'A))
       (if (equal i 0) ; Kale #571 p. 351
	 (aset btab i (list (elt btab i) 'abodhi)))
       (if (equal i 5) (aset btab i 'abhudvam)) ; was 'abhudhdhvam'
      )
      (setq i (1+ i))
     )
    )
    ;-- step2: construct btab
   )
  
  btab
 )
)
(defun conjugation-tab-aorist5 (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  ; assumes validity of aorist-tok, aorist-id
  (setq aorist-sym 'lu~N5)
  (if aorist-passive-P 
   (progn
    (setq aorist-pada 'A)
    (setq ans (conjugation-tab-aorist5-main
     upasargas class 'A dhaatu))
    (setq aorist-pada pada)
   )
   (setq ans (conjugation-tab-aorist5-main
     upasargas class pada dhaatu))
  )
  ans
 )
)
(defun conjugation-tab-aorist5-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab
        wparts parts types tense-sym)
  (when nil ;nil
   (fol-msg (format "aorist5-main: %s %s %s %s %s %s (PASSIVE=%s) \n"
		    upasargas class pada dhaatu seTCode i-insert
		    aorist-passive-P))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (setq endings (aorist-endings))
  (setq n (length endings))
  
  ;--- 3a. atok
  (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (setq temp (construct-seT-code1a dhaatu class pada upasargas))
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5b. get table of base-seT codes (bitab)

  (let (b nb lc pc bc bv)
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (cond
     ((equal types "VC")
      (setq pc (elt (elt parts 0) 0)) ;the vowel
     )
     ((equal types "CVC")
      (setq pc (elt (elt parts 1) 0)) ;the vowel
     )
     ((equal types "CV")
      (setq pc (elt (elt parts 0) 0)) ;the consonant
     )
     (t (setq pc nil))
    )
    ;-- step1a: All aorists use augment 'a'
   (when nil
    (fol-msg (format "b=%s\n" b))
    (fol-msg (format "endings=%s\n" endings))
   )
    (let (n i x y ending z w efirst xalt xalt1 x1)
     (setq n (length endings))
     (setq btab (make-vector n nil))
     ; alter base 'b' into 'x'. Get xalt (usu. nil)
     (setq x b)
     (cond
      ((member dhaatu '(sRij dRish))
	; Kale p. 348, footnote 1
	; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
        ; is changed to 'ra' before
        ; a consonantal strong termination in the general tenses.
	(setq b (vconcat (substring b 0 -2) [r a] (substring b -1)))
	(setq pc 'a)
      )
      ((equal dhaatu 'vas)
       ; Antoine2#220  p. 155.  'vas' becomes 'vat' in the aorist
       ; Without this, the result differs in 3D 2D and 2P, namely
       ; 'avaastaam' instead of 'avaattaam', etc.
       ; Kale (footnote p. 350, argues a derivation
       ; 'avaas' + 'staam' -> 'avaattaam' by 480; however, by 567,
       ; the ending would have been modified to 'taam', so I don't agree
       ; with the derivation. Thus, the inclusion of this special case.
       (setq b [v a t])
       (setq lc 't)
      )
      ((equal dhaatu 'han)
       (setq b [v a gh]) ; Kale 569
      )
     )
;    (fol-msg (format "%s : %s %s %s %s %s\n" dhaatu b lc pc types parts))
     (cond
      ((equal pada 'P)
       (cond
	((equal dhaatu 'daridraa)
         ; Kale 578
         (setq x (substring b 0 -1))
        )
	((equal dhaatu 'grah)
         ; Kale 474. The augment 'i' as added to 'grah' is long
         ; in all non-conjugational tenses, except in the Perfect
	 (setq x (vconcat x [i]))
	)
	((equal dhaatu 'guh)
	 ; Kale p. 249 ftnote
	 ; The penultimate 'u' of 'guh' is lengthened in the
	 ; special tenses and before a strong termination beginning
	 ; with a vowel
	 (setq x [g uu h])
	)
	((equal dhaatu 'vij)); Kale 466, and example p. 354
	((member dhaatu '(kShaN shvas shvi kaT chaT chat chad
			  path math lag has hlas))
	 (setq x (aorist-gunate-final-vowel b)) ; guna. Kale 574d
	)
	((member lc '(h m y))
	 (setq x (aorist-gunate-final-vowel b)) ; guna. Kale 574d
	)
	((vowel-P lc)
	 (setq x (aorist-gunate-final-vowel b t)) ; vrddhi. Kale 574a
	 (when (equal dhaatu 'uurNu)
	  ; Kale 577
	  (setq xalt (list [uu r N a v] [uu r N u]))
	 )
	)
	((and (equal pc 'a) (member lc '(r l)))
	 (setq x (aorist-gunate-final-vowel b t)) ; vrddhi. Kale 574a
	)
	((member dhaatu '(vad vraj))
	 (setq x (aorist-gunate-final-vowel b t)) ; vrddhi. Kale 574a
	)
	((equal dhaatu 'mRij) 
	 (setq x (aorist-gunate-final-vowel b t)) ; vrddhi. Kale p. 354 ex.
	)
	((and (equal types "CVC")
	      (equal pc 'a)
	      (= (length (elt parts 2)) 1)
	      (not (member lc '(r l)))
	      (not (equal dhaatu 'han)) ; by example p. 354
	 )
	 (setq xalt (aorist-gunate-final-vowel b t)) ; vrddhi. Kale 574c
	)
	((shortvowel-P pc)
	 (setq x (aorist-gunate-final-vowel b)) ; guna. Kale 574b
	)
       )
      )
      ((equal pada 'A)
       (cond
       	((equal dhaatu 'vij)); Kale 466, and example p. 354
	((equal dhaatu 'grah)
	 (setq x (vconcat x [i]))
	)
	((equal dhaatu 'guh)
	 ; Kale p. 249 ftnote
	 ; The penultimate 'u' of 'guh' is lengthened in the
	 ; special tenses and before a strong termination beginning
	 ; with a vowel
	 (setq x [g uu h])
	)
;	((equal dhaatu 'gaah)) ; Kale example p. 354
	((equal dhaatu 'uurNu)
	 ; Kale 577
	 (setq xalt (list [uu r N u]))
	)
;	(aorist-passive-P) ; no vowel-strengthening (example 'pach dish')
        (t
         (setq x (gunate-final-vowel b)) ; Only applies to final and short
	 (when (or (equal dhaatu 'vRi) (equal lc 'RI))
	  ; Kale 475. The intermediate 'i' is optionally lengthened in the
	  ; case of 'vRi' and roots ending in 'RI' except
	  ; in the Perfect, the Benedictive A and the aorist P
	  ; In particular, it is optionally lengthened in the aorist 'A'.
	  ; Since the endings in the 5th form all start with 'i',
	  ; and since 'x' has been gunated, we can force the lengthening
	  ; by adding 'i' to 'x'.
	  (setq xalt (vconcat x [i]))
	 )
        )
       )
      )
     )
     (when (and aorist-passive-P
		(member class '(10 11)) 
	   )
      ; Kale 598
      (setq xalt (vconcat x [a y]))
;      (fol-msg (format "chk@59a: %s %s\n" x xalt))
     )
;     (fol-msg (format "chkx: %s\n" x))
     (if (member dhaatu '(gup dhuup vichCh paN pan kam Rit))
      (setq xalt (kale-461-alt dhaatu))
     )
     ; reset lc as last letter of 'x'
     (setq lc (elt (substring x -1) 0))
     (when (not (listp xalt)) (setq xalt (list xalt)))
;     (setq lcalt (mapcar (lambda (x1) (elt (substring x1 -1) 0)) xalt))
;     (when xalt (setq lcalt (elt (substring xalt -1) 0)))
     ;loop through combining base with endings, and prefixing 'a'
;     (fol-msg (format "x=%s, xalt=%s, seT-gen=%s, endings=%s\n"
;		      x xalt seT-gen endings))
     (setq i 0)
     (while (< i n)
      (setq ending (elt endings i))
      (setq y (augment-a x)) ; prefix augment 'a'
      (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
;      (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
      (setq w (sym-without-space z))
      (aset btab i w)
;      (when (= i 6) (fol-msg (format "chk: w=%s\n" w)))
      (when xalt
       (aset btab i (list w))
       (setq xalt1 xalt)
      )
      (while xalt1
       (setq x1 (car xalt1))
       (setq xalt1 (cdr xalt1))
       (let (w1 w2)
	(setq ending (elt endings i))
        (setq y (augment-a x1)) ; prefix augment 'a'
        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
;        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
        (setq w1 (sym-without-space z))
	(setq w (elt btab i))
	(setq w2 (append w (list w1)))
;	(fol-msg (format "%s %s %s\n" w w1 w2))
	(aset btab i w2)
       )
      )
      (when (and (equal pada 'A)
		 (member dhaatu '(diip jan puur taay pyaay))
		 (equal i 0)
	    )
       ;Kale 575. The roots mentioned optionally substitute 'i' for
       ; the ending 'iShTa' (third pers. sing. Atm)
       (let (w1)
	(setq ending [i])
        (setq y (augment-a x)) ; prefix augment 'a'
        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
;        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
        (setq w1 (sym-without-space z))
	(aset btab i (list w w1))
       )
      )
      (when (and (equal pada 'A)
		 (equal class 8)
		 (member lc '(N n))
		 (member i '(0 3))
	    )
       ; Kale 576
       (let (w1 )
	(if (equal i 0) (setq ending [t a]))
	(if (equal i 3) (setq ending [th aa H]))
        (setq y (augment-a (substring b 0 -1))) ; prefix augment 'a'
        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
;        (setq z (aorist-join y 'aniT ending dhaatu 'S i-insert))
        (setq w1 (sym-without-space z))
	(aset btab i (list w w1))
       )
      )
      (setq i (1+ i))
     )
    )
    ;-- step2: construct btab
   )
;  (fol-msg (format "aorist5 -> %s\n" btab))
  btab
 )
)
(defun aorist-passive-3S (upasargas class pada dhaatu)
 ; See Kale 597(c)
 (let (ans tok lc pc b ending ntok x y z w)
  (setq ending [i])
  (setq tok aorist-tok)
  (when nil
   (fol-msg (format "aorist-passive-3S %s, aorist-tok=%s\n" (list upasargas class pada dhaatu) aorist-tok))
  )
  (setq ntok (if (arrayp tok) (length tok) 0))
  (when (< 0 ntok)
   (setq tok (copy-sequence aorist-tok))
   (setq lc (elt (substring tok -1) 0))
   (if (< 1 ntok) (setq pc (elt (substring tok -2 -1) 0)))
  )
  ; get 'x'
;  (fol-msg (format "enter 3S: %s %s %s %s\n" upasargas class pada dhaatu))
  (cond
   ((member lc '(aa e ai o))
    ; Kale 597c2
    (setq x (vconcat (substring tok 0 -1) [aa y]))
   )
   ((equal dhaatu 'sRij) (setq x [s aa r j]))
   ((equal dhaatu 'guh) (setq x [g uu h]))
   ((equal dhaatu 'i)
    (if (equal upasargas '(adhi))
     (setq x (list [aa y] [g aa y]))
     (setq x [g aa y])
    )
   )
   ((member dhaatu '(kRiSh))
    (setq x (vconcat (substring tok 0 -2) (vrddhi pc) (substring tok -1)))
   )
   ((member pc '(i u Ri Li))
    ;prosodially short penultimate short vowel not 'a'
    (setq x (vconcat (substring tok 0 -2) (guna pc) (substring tok -1)))
   )
   ((member dhaatu '(radh jabh rabh)) ; insert nasal
    (let (nasal)
     (setq nasal (if (equal dhaatu 'radh) [n] [m]))
     (setq x (vconcat (substring tok 0 -1) nasal (substring tok -1)))
    )
   )
   ((equal dhaatu 'labh)
    (let (nasal b)
     (setq nasal [m])
     (setq b (vconcat (substring tok 0 -1) nasal (substring tok -1)))
     (if upasargas
      (setq x b)
      (setq x (list tok b)) ; two forms
     )
    )
   )
   ((equal dhaatu 'bha~nj)
    (setq x (list [bh a ~n j] [bh aa j]))
   )
   ((and (equal dhaatu 'sham) (equal class 10) (equal pada 'A))
    (setq x (list [sh a m] [sh aa m]))
   )
   ((equal pc 'a) ; prosodially short penultimate 'a'
    (cond
     ((equal dhaatu 'jan) (setq x tok)) ; unchanged
     ((and (equal dhaatu 'cham) (equal upasargas '(aa))) ; takes vrddhi
      (setq x (vconcat (substring tok 0 -2) [aa] (substring tok -1)))
     )
     ((member dhaatu '(kram vam)) ; takes vrddhi
      (setq x (vconcat (substring tok 0 -2) [aa] (substring tok -1)))
     )
     ((progn
      (let (seT-code)
       (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
       (setq seT-code (solution seT-code))
;       (fol-msg (format "chka: %s %s %s\n" dhaatu lc seT-code))
       (if (and (equal lc 'm) ; so root ends in [a m]
		(equal seT-code 'seT)
		(not (equal dhaatu 'kam)) ; Kale shows vrddhi
	     )
	(setq x tok); no change
	nil
       )
      )
     ))
     (t ; generally, apply vrddhi
;      (fol-msg (format "chk: %s\n" tok))
      (setq x (vconcat (substring tok 0 -2) [aa] (substring tok -1)))
     )
    )
   )
   ((vowel-P lc)
    (setq x (vconcat (substring tok 0 -1) (vrddhi lc)))
   )
   (t (setq x tok)
   )
  )
  (when (member dhaatu '(gup dhuup vichCh paN pan kam Rit))
   (let (usual xalt)
    (setq usual x)
    (setq xalt (kale-461-alt dhaatu))
    (setq x (list usual xalt))
   )
  )
  (if (not (listp x)) (setq x (list x)))
  (let (xalt tok1  cb)
   (setq cb (causal-base dhaatu class pada upasargas nil))
   (setq cb (solution cb))
   
   (when (and (equal pc 'a)
	     (member class '(10 11))
;	     (not (causal-603-P tok dhaatu))
	     (arrayp cb)
	     (equal tok (substring cb 0 -2))
			       
	)
   ; Kale 598 (b,c)
    (setq xalt (aorist3-shorten-1st-vowel tok))
    (setq x (append-if-new x xalt))
   )
  )
;  (fol-msg (format "chk x=%s\n" x))
  (while x
   (setq b (car x))
   (setq x (cdr x))
   (setq y (augment-a b))
   (setq z (conjugation-join y ending))
   (setq w (sym-without-space z))
   (setq ans (append ans (list w)))
  )
  (setq ans (solution ans))
  ans
 )
)
(defun aorist-bitab (btab itab)
 (let (n i ans b c thisans b1 c1 c0)
  (setq n (length btab))
  (setq ans (make-vector n nil))
  (setq i 0)
  (while (< i n)
   (setq b (elt btab i))
   (setq c (elt itab i))
   (if (not (listp b)) (setq b (list b)))
   (if (not (listp c)) (setq c (list c)))
   (setq c0 c)
   (setq thisans nil)
   (while b
    (setq b1 (car b))
    (setq b (cdr b))
    (setq c c0)
    (while c
     (setq c1 (car c))
     (setq c (cdr c))
     (setq thisans (append thisans (list (list b1 c1))))
    )
   )
   (aset ans i thisans)
   (setq i (1+ i))
  )
  ans
 )
)
(defun aorist-bitab-join (bitab endings dhaatu strengths i-insert)
 (let (ans i n y thisans base ending strength e a seT-code bi thisbi thisans1
	   thisans2 strength)
  (setq n (length bitab))
  (setq ans (make-vector n nil))
   (setq i 0)
   (while (< i n)
    (setq ending (elt endings i)) ; a tok arr
    (setq strength (elt strengths i)) ;
    (setq bi (elt bitab i)) ; a list of base-seTcode pairs
    (setq thisans nil)
    (while bi
     (setq thisbi (car bi))
     (setq bi (cdr bi))
     (setq base (elt thisbi 0))
     (setq seT-code (elt thisbi 1))
     (setq thisans1
	   (aorist-join base seT-code ending dhaatu strength i-insert))
     (if (not (listp thisans1)) (setq thisans1 (list thisans1)))
     (while thisans1
      (setq thisans2 (car thisans1))
      (setq thisans1 (cdr thisans1))
      (setq thisans2 (sym-without-space thisans2))
      (setq thisans (append-if-new thisans thisans2))
     )
    )
;    (fol-msg (format "%s: %s (%s %s)\n" i thisans (elt bitab i) ending ))
    (setq thisans (solution thisans)) ; so singletons appear w/o parens
    (aset ans i thisans)
    (setq i (1+ i))
   )
   ans
  )
)
(defun aorist-join (base-tok seT-code sup dhaatu strength i-insert)
 (let (ans1 ans)
  (setq ans
   (cond 
    ((listp sup)
     (mapcar (lambda (x)
	 (aorist-join base-tok seT-code x dhaatu strength i-insert)) sup))
    ((listp base-tok)
     (mapcar (lambda (x)
	   (aorist-join x seT-code sup dhaatu strength i-insert)) base-tok))
    ((equal seT-code 'veT)
     (mapcar (lambda (x)
	   (aorist-join base-tok x sup dhaatu strength i-insert)) '(seT aniT))
    )
    (t (aorist-join1 base-tok seT-code sup dhaatu strength i-insert))
   )
  )
;  (setq ans (flatten ans1))
;  (fol-msg (format "%s -> %s \n" ans1 ans))
  ans
 )
)
(defun aorist-join1 (y seT-code ending0 dhaatu strength i-insert)
 ; based on 'conjugation-join'
 ; seT-code is either nil, 'seT' or 'aniT'
 (let (ans skiprefs ylast efirst ending y0 ny yfirst)
  ; insert 'i' if needed
  ; Note 'ending' may then be either a token, or a list of 2 tokens
  (setq ending
   (if (equal seT-code 'seT)
    (conjugation-join i-insert ending0)
    ending0
   )
  )
  (sandhi-pair-skiprefs-set (list 'Antoine72-4))
  (setq efirst (elt ending 0))
  ; NOTE 2: The logic is put here, because other changes, e.g.,
  ;  before 'tha', are required. This logic applies to other
  ;  forms than the future
  (setq y (aorist-join1-adjust-y y efirst dhaatu))
  (setq ny (length y))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (when nil ;dbg
    (fol-msg (format "y = %s ylast=%s ending= %s efirst=%s\n"
		     y ylast ending efirst))
  )
  (cond
   ((and (vowel-P efirst)
	 (setq ans (aorist-join1-vowel y ending dhaatu))
    )
    ans
   )
   ((and (equal efirst 'dh) (vowel-P ylast) (not (member ylast '(a aa i))))
    ; 3rd special sandhi rule for future (Antoine2#110)
    (setq ans (vconcat y [Dh] (substring ending 1)))
    (when nil
     (fol-msg (format "aorist-join: %s + %s => %s\n" y ending ans))
    )
   )
   ((and (member efirst '(t th))
         (setq ans (aorist-join1-t-th y ending dhaatu)))
;    (fol-msg (format "y ending ans=%s %s %s\n" y ending ans))
    ans
   )
   ((and (equal efirst 'dh)
	(setq ans (aorist-join1-dh y ending dhaatu)))
;    (fol-msg (format "y ending ans=%s %s %s\n" y ending ans))
    ans
   )
   ((and (equal efirst 's)
	(setq ans (aorist-join1-s y ending dhaatu)))
    ;(fol-msg (format "ans from aorist-join1-s: y=%s, ending=%s, ans=%s \n" y ending ans))
    ans
   )
   ((and (equal efirst 'm) (equal ylast 'ch))
    (setq ans (vconcat y ending)) ; otherwise, 'ch' is changed to 'j'
   )
   ((and (member efirst '(m v)) (equal ylast 'm))
    ;Kale p. 321 (footnote)
    ;Roots ending in 'm' change it to 'n' when followed by 'm' or 'v'
    ;note 'n' may be changed to 'N' by sandhi-single (see below)
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [n] ending)) 
   )
   ((and (equal dhaatu 'guh) (equal seT-code 'seT))
    ; the 'u' is lengthened, rather than gunated (Kale example p. 304)
    (setq ans (vconcat [g uu h] ending))
   )
   (t
    ; joining for other cases like 'conjugation-join'
    (sandhi-pair-skiprefs-set (list 'Antoine72-4 'Antoine72-5))
    (setq ans
     (or
      (solution (sandhi-pair y ending 'internal 'join))
      (solution (sandhi-pair y ending nil 'join))
      (vconcat y ending)
     )   
    )
    ;(fol-msg (format "aorist-join1. sandhi. %s + %s => %s\n" y ending ans))
    (sandhi-pair-skiprefs-set nil)
   )
  )
  (setq ans (or (sandhi-single ans) ans))
  (setq ans (solution ans))
  ans
 )
)
(defun aorist-join1-adjust-y (y efirst dhaatu)
  (cond
   ((and (consonant-P efirst)
	 (not (semivowel-P efirst))
	 (equal dhaatu 'nash)
    )
    ;Kale 476. nash
    ; 'n' is inserted before the ending consonant of 'nash' when
    ; it is followed by any consonant except a nasal or a semi-vowel.
    ; NOTE 1: I represent 'n' as 'M', consistent with printing in Kale
    (setq y (vconcat (substring y 0 -1) [M] (substring y -1)))
   )
   ((and (consonant-P efirst)
	 (equal dhaatu 'masj)
	 (not (semivowel-P efirst))
    )
    ; Kale 476. masj
    ; 'n' is inserted before the ending consonant and
    ; the  's' is dropped when they are followed by any consonant
    ; except a nasal or a semi-vowel. In particular this is applicable
    ; to all the periphrastic future forms:  ma~Nktaa.
    ; When the 'n' is not dropped, the 's' is changed to 'j': mamajja
    (setq y (vconcat (substring y 0 -2) [~n] (substring y -1)))
   )
   ((and (vowel-P efirst)
	 (equal dhaatu 'jabh)
    )
    ; Kale p.320 footnote
    ; 'jabh' inserts a nasal when its final is followed by a vowel
    (setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
   )
   ((and (vowel-P efirst)
	 (equal dhaatu 'rabh)
    )
    ; Kale p.320 footnote
    ; 'rabh' inserts a nasal when its final is followed by a vowel;
    ; however, 'rabh' does not do it
    ;  (a) in the Aorist
    ;  (b) when it takes 'i', except in the Perfect
    ; In this case, (luT lRiT lRi~N), the only way 'efirst' is a vowel
    ; is if it is an 'i', presetn because seT-code is 'seT'. Since 
    ; the tense is not Perfect, no nasal is inserted
    ;(setq y (vconcat (substring y 0 -1) [m] (substring y -1)))
    y
   )
   (t ; no adjustment
    y
   ) 
  )
)
(defun aorist-join1-vowel (y ending dhaatu)
 ; when 1st char of ending is a vowel
 ; returns 'nil' when not applicable
 (let (ans  ylast efirst y0 ny yfirst)
  (setq efirst (elt ending 0))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (setq ny (length y))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  (cond
   ((and (member ylast '(i ii Ri))
	 (equal aorist-sym 'luT)
    )
    ; 1st special sandhi rule for perfect (Antoine2#110)
    (let (wp wp0 wp1 nw y1)
     (setq wp (word-parts y))
     (setq wp1 (elt wp 1)) ; string like "CVCV"
     (setq wp0 (elt wp 0))
     (setq nw (length wp1))
     (setq y0 (substring y 0 -1)) ; all but last char
     (if (equal y0 []) (setq y0 y)) ; 07-24-03 for dhaatu='i', y='ii'
;     (fol-msg (format "wp=%s, y0=%s\n" wp y0))
     (if (and (< 1 nw) (< 1 (length (elt wp0 (- nw 2)))))
      ; compound consonant precedes the final 'i ii Ri'
      (setq ans
       (cond
        ((equal ylast 'i) (vconcat y0 [i y] ending))
        ((equal ylast 'ii) (vconcat y0 [i y] ending))
        ((equal ylast 'Ri) (vconcat y0 [a r] ending))
       )
      )
     ; word is a single vowel ('i ii Ri') or
     ; a simple constant  precedes final 'i ii Ri'
      (setq ans
       (cond
        ((equal ylast 'i) (vconcat y0 [y] ending))
        ((equal ylast 'ii) (vconcat y0 [y]  ending))
        ((equal ylast 'Ri) (vconcat y0 [r]  ending))
       )
      )
      ans
     )
    )
   )
   ((and (member ylast '(u uu RI))
;	 (equal aorist-sym 'luT)
    )
    ; 2nd special sandhi rule for future (Antoine2#110)
    (setq y0 (substring y 0 -1))
    (setq ans
     (cond
      ((equal ylast 'u) (vconcat y0 [u v] ending))
      ((equal ylast 'uu) (vconcat y0 [u v] ending))
      ((equal ylast 'RI) (vconcat y0 [a r] ending))
     )
    )
   )
   ;
  )
  ans
 )
)
(defun aorist-join1-t-th (y ending dhaatu)
 ; when 1st char of ending is 't' or 'th'
 ; returns 'nil' when not applicable
 (let (ans skiprefs ylast efirst y0 ny yfirst)
  (setq efirst (elt ending 0))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (setq ny (length y))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  (setq y0 (substring y 0 -1))
  (cond
   ((and  (<= 2 ny)
	 (member (substring y -2) '([a r] [aa r])))
    ; this rule so [ch a k a r] + [th a]
    ; becomes [ch a k a r th a] rather than [ch a k a s th a], which
    ; is what 'sandhi-pair' does
    (setq ans (vconcat y ending))
   )
   ((and (< 2 ny)
	 (member (substring y -2) '([ch Ch] [sh ch] [r j] [k Sh] [s j] [j j]))
    )
    ; prachCh , vrashch, mRij, akSh
    (setq y0 (substring y 0 -2))
    (if (equal (substring y -2) [r j])
     (setq y0 (substring y 0 -1))
    )
    (when (equal (substring y0 -1) [~n])
     (setq y0 (vconcat (substring y0 0 -1) [~N]))
    )
    (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
    (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
   )
   ((and  (equal ylast 'sh))
    ;Kale p. 321. Example of klish
    (setq y0 (substring y 0 -1))
    (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
    (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
   )
   ((member dhaatu '(vah sah))
    ; Kale #506. p. 317
    ; When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
    ; is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
    ;  vavah + tha =
    ;   uvah + tha =
    ;   uvaDh + Dha (by #416-3,4) =
    ;   uvaDh + Dha =
    ;   uvoDha
    (if (<= 2 (length y))
     (setq ans (vconcat (substring y 0 -2) [o Dh] (substring ending 1)))
     ; for 'tvaa' of 'vah', y = [uu]
     (setq ans (vconcat y [Dh] (substring ending 1))) 
    )
;    (setq ans (vconcat (substring y 0 1) [o Dh] (substring ending 1)))
   )
   ((and (equal efirst 't)
	 (equal aorist-sym 'luT) ; redundant
	 (member dhaatu '(muh druh snih snuh))
    )
    ; A few verbs take two forms
    ; muh : mogdhaa moDhaa (Kale p. 305)
    ; druh : drogdhaa droDhaa (Kale, dhaatukosha)
    (let (ans1 ans2 ylast1)
     (setq y0 (substring y 0 -1))
     (setq ans1 (vconcat y0 [Dh] (substring ending 1)))
     (setq ylast1 [g])
     (setq ans2 (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
     (setq ans (list ans1 ans2))
    )
   )
   ((and (< 2 ny)
	 (member (substring y -2) '([a h] [aa h])))
    ; 'dah' : dagdhaa (luT), adaagdham (aorist4 p. 349)
    ; 'nah' : naddhaasmi (luT)
    (let (ylast1)
     (setq ylast1 (cond
      ((equal yfirst 'd) [g])
      ((equal yfirst 'n) [d])
      (t (vector ylast))
     ))
     (if (equal yfirst 'g)
      ; gaah
      (setq ans (vconcat (substring y 0 -1)  [Dh] (substring ending 1)))
      ; dah
      (setq ans (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
     )
    )
   )
   ((and (member dhaatu '(dih duh))
	 (equal aorist-sym 'lu~N7)
    )
    ; digdha Kale p. 346
    (let (ylast1)
     (setq ylast1 (cond
      ((equal yfirst 'd) [g])
      ((equal yfirst 'n) [d])
      (t (vector ylast))
     ))		
     (setq ans (vconcat (substring y 0 -1) ylast1 [dh] (substring ending 1)))
    )
   )
   ((and (member dhaatu '(lih guh))
	 (equal aorist-sym 'lu~N7)
    )
    ; digdha Kale p. 346
    (setq ylast (elt (substring y0 -1) 0))
    (setq y0 (vconcat (substring y0 0 -1)
		      (vector (lengthen-vowel ylast))))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))    
   )
   ((equal ylast 'h)
    ;Kale p. 322. Example of 'muh', 'druh', 'snih', 'snuh'
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))
   )
   ((and (member ylast '(j ch)))
    ; this rule [bh a j] + [th a] -> [bh a k th a]
    ; rather than ([bh a ch th a] [bh a ch Ch a]) which sandhi-pair does
    ; but [bh a ~n j] + [th a] -> [bh a ~N k th a]
;    (fol-msg (format "y=%s ending=%s\n" y ending))
    (cond
     ((and (< 2 ny) (member (substring y -2) '([a j] [aa j])))
      ; yaj, sRij (has been changed to 'sraj')
      (setq y0 (substring y 0 -1))
      (if (equal efirst 't) (setq efirst 'T) (setq efirst 'Th))
      (setq ans (vconcat y0 [Sh] (vector efirst) (substring ending 1)))
     )
     ((and (< 2 ny) (equal (substring y -2) [~n j]))
      ;bha~nj, masj
      (setq ans (vconcat (substring y 0 -2) [~N k] ending))
     )
     (t
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [k] ending))
     )
    )
   )
   ((and (member ylast '(dh bh)))
    ; so [v i v y a dh] + [th a] -> [v i v y a d dh a]
    ; sandhi-pair gives [v i v y a d dh a] and also [v i v y a th th a]
    (setq y0 (substring y 0 -1))
    (setq ans (vconcat y0
		       (vector (de-aspirate ylast))
		       [dh] (substring ending 1)))
   )
   ((and (member ylast '(m n)))
    ; For 'gam', sandhi-pair gives 'jagaMtha', but
    ; Kale and Antoine both show 'jagantha'
    ; Similaraly, for 'han' we want 'jaghantha' rather than 'jagaMtha'
    (setq ans
     (vconcat (substring y 0 -1) ; all but last char
	      [n]
	      ending))
   )
  )
  ans
 )
)
(defun aorist-join1-s (y ending dhaatu)
 ; when 1st char of ending is 's'
 ; returns 'nil' when not applicable
 (let (ans skiprefs ylast efirst y0 ny yfirst)
  (setq efirst (elt ending 0))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (setq ny (length y))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  (cond
     ((member ylast '(sh))
      ; Kale p. 321. based on example of 'ash'
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [k Sh] (substring ending 1))) 
     )
     ((member ylast '(bh b))
      ; case of 'labh': Kale p. 301
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [p] ending))
     )
     ((member ylast '(dh d))
      ; case of 'vRidh': Kale p. 301
      ; case of 'bandh': Kale p. 303 : previous 'b' gets the aspiration
      (setq y0 (substring y 0 -1))
      (if (equal ylast 'dh) (setq y0 (aspirate-first-cons y0)))
      (setq ans (vconcat y0 [t] ending))
     )
     ((equal ylast 'h)
      ; Examples:
      ; 'nah' : natsyaami
      ; 'vah' : vakShyaami
      ; 'muh' : mokShyaami
      ; 'tRih' : tarkShyati
      ; The following also aspirate the first consonant: 
      ; 'dah' : dhakShyaami
      ; 'duh' : dhokShyaami
      ; 'guh' : ghokShyaami
      ; 'gaah' : ghaakShyate
      ; 'gRih' : gharkShyate
;        (fol-msg (format "check-h: %s: %s %s \n" dhaatu y ending ))
      (setq y0 (substring y 0 -1))
      (cond
       ((member yfirst '(n))
        (setq ans (vconcat y0 [t] ending))
       )
       ((member yfirst '(v m t))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
       )
       ((member yfirst '(d g))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
        (setq ans (aspirate-first-cons ans))
       )
       (t
;        (setq ans (vconcat y ending))
        (setq ans (vconcat y0 [k Sh] (substring ending 1)))
       )
      )
     )
     ((equal ylast 's)
      ; Kale 480. 't' is substituted for the ending 's' of a root
      ; when followed by any non-conjugational termination
      ; beginning with 's'
      (setq y0 (substring y 0 -1))
      (setq ans (vconcat y0 [t] ending))
     )
     ((and (< 2 ny)
	   (member (substring y -2) '([ch Ch] [sh ch])))
      ; from example of 'prachCh' (Antoine2 p. 89)
      (setq y0 (substring y 0 -2))
      (setq ans (vconcat y0 [k Sh] (substring ending 1)))
     )
     ((member ylast '(j ch Sh))
      ; from example of 'sRij' (Antoine2 p. 89),
      ; and of 'kRiSh'
      ; takSh (p. 304)
      (setq y0 (substring y 0 -1))
      (cond
       ((equal (substring y0 -1) [~n])
        (setq y0 (vconcat (substring y0 0 -1) [~N]))
       )
       ((member (elt (substring y0 -1) 0) '(k j s))
	(setq y0 (substring y0 0 -1)) ; drop the last penultimate letter
       )
      )
      (setq ans (vconcat y0 [k Sh] (substring ending 1)))
     )
    )
  ans
 )
)
(defun aorist-join1-dh (y ending dhaatu)
 ; when 1st char of ending is 'dh' (atmanepada 'dhvam')
 ; returns 'nil' when not applicable
 (let (ans  ylast efirst y0 ny yfirst)
  (setq efirst (elt ending 0))
  (setq ylast (elt (substring y -1) 0)) ;last char
  (setq yfirst (first-cons y))
  (setq ny (length y))
  (when nil  ;dbg
    (fol-msg (format "y = %s ylast=%s efirst=%s\n" y ylast efirst))
  )
  (setq y0 (substring y 0 -1))
  (cond
   ((member dhaatu '(dih duh))
    (setq ans (vconcat y0 [g] ending))
    (setq ans (aspirate-first-cons ans))
   )
   ((member dhaatu '(lih guh))
    (setq ylast (elt (substring y0 -1) 0))
    (setq y0 (vconcat (substring y0 0 -1)
		      (vector (lengthen-vowel ylast))))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))
    (setq ans (aspirate-first-cons ans))
   )
   ((member dhaatu '(vah sah))
    ; Kale #506. p. 317
    ; When the 'd' substituted for the 'h' of the roots 'sah' and 'vah'
    ; is dropped, the preceeding 'a' is changed to 'o' and not to 'aa':
    ;  vavah + tha =
    ;   uvah + tha =
    ;   uvaDh + Dha (by #416-3,4) =
    ;   uvaDh + Dha =
    ;   uvoDha
    (setq ans (vconcat (substring y 0 -2) [o Dh] (substring ending 1)))
   )
   ((member ylast '(ch j))
    ;lu~N4. pach -> apagdhvam
    (setq ans (vconcat y0 [g] ending))
   )
   ((equal ylast 'r)
    (setq ans (vconcat y [Dh] (substring ending 1)))
   )
   ((equal ylast 'h)
    (setq y0 (aspirate-first-cons y0))
    (setq ans (vconcat y0 [Dh] (substring ending 1)))
   )
  )
  ans
 )
)
(defun conjugation-tab-aoristX (upasargas class pada dhaatu)
 (let (ans  tok ylast seT-code types pc parts)
  (let ( wparts)
   (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   (setq ylast (elt (substring tok -1) 0))
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
   (if (equal types "CVC") (setq pc (elt (elt parts 1) 0)))
  )
  (setq seT-code (construct-seT-code1a dhaatu class pada upasargas))
  (setq seT-code (solution seT-code))
  (cond
   ((equal pada 'PASSIVE)
    (let (ans1 ans2 tok2 dhaatu2 i)
     (setq ans1 (conjugation-tab-aoristX upasargas class 'A dhaatu ))
     (cond
      ((equal ylast 'aa)
       (setq tok2 (vconcat tok [y]))
       (setq dhaatu2 (sym-without-space tok2))
      )       
      ((vowel-P ylast)
       (setq tok2 (gunate-final-vowel tok 't)) ;vrddhi
       (setq dhaatu2 (sym-without-space tok2))
      )
      ((equal dhaatu 'han) (setq dhaatu2 'ghaan))
      ((equal dhaatu 'grah) (setq dhaatu2 'graah) )
      ((equal dhaatu 'dRish) (setq dhaatu2 'darsh))
     )
     (when dhaatu2
      (setq ans2
	(conjugation-tab-aoristX-main upasargas class 'PASSIVE dhaatu2 '(seT)))
     )
     (if ans2
      (setq ans (join-arrays ans1 ans2))
      (setq ans ans1)
     )
    )
   )
   ((and (equal dhaatu 'han)
	 (equal pada 'A)
	 (equal aorist-sym 'aashiirli~N)
    )
    ; 'han' is 2P in dhaatukosha.
    ; however, its passive uses the 'A' logic.
    ; In Kale examples on p. 364, the normal passive benedictive is
    ; given as 'vaghiShiiya'.  So this
    ; (a) uses the base 'vagh'
    ; (b) inserts 'i'.
    ; Since 'han' is an 'aniT', this also must be adjusted
    ; By Kale 483, 'han' admits 'i' in lRiT, lRi~N (handled elsewhere)
    (setq ans
	(conjugation-tab-aoristX-main upasargas class 'A 'vagh '(seT)))
   )
   
   ((and (equal dhaatu 'aj) (equal pada 'P))
    ; Kale #477 p. 300
    ; 'vii' is substituted for 'aj' (1 P 'to go') necessarily before any
    ; non-conjugational termination, and optionally before such as
    ; begin with any consonant except 'y':
    ;   'vetaa  ajitaa'
    ;   'veShyati ajiShyati'
    (let (ans1 ans2)
     (setq ans1
	 (conjugation-tab-aoristX-main upasargas class pada 'vii ))
     ;all forms of 'aj' have optional answers with 'vii'
     (setq ans2
         (conjugation-tab-aoristX-main upasargas class pada dhaatu ))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ; some other irregularites
   ((and (equal dhaatu 'dRish)
	 (equal pada 'A)
	 (equal aorist-sym 'aashiirli~N))
    ; Note2: for the 1 P root dRish, when it appears as 'A', in
    ; formation of passive, the change of Kale 465 (below)
    ; does not occur (Kale p. 364, example)
    (setq ans
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu))
   )
   ((member dhaatu '(sRij dRish))
    ; Kale 465. The penultimate 'Ri' of 'sRij' and of 'dRish'
    ; is changed to 'ra' before
    ; a consonantal strong termination in the general tenses.
    ; Note: both are 'aniT'
    (let ( dhaatu2 c1 v c2)
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu2 seT-code))
    )
   )
   ((equal dhaatu 'tRip)
    ; Kale 471. In the example of 'tRip' on p. 303, there are three
    ; forms : an 'i' form with 'Ri' (gunated), and two forms without
    ; 'i' (one with 'Ri' gunated, and one with 'ra' instead of 'Ri').
    ; This logic (along with that in '..-main') achieve these three
    ; forms.
    ; Note: 'tRip' has seT-code 'veT'
    (let (ans1 ans2 dhaatu2 c1 v c2)
     (setq ans1
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu2 '(aniT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal seT-code 'aniT)
	 (equal pc 'Ri)
	 (equal types "CVC")
	 (equal aorist-sym 'luT)
    )
    ; Kale 471.
    ; 'aniT' roots with a penultimate 'Ri' change it to
    ; 'ra' optionally before a strong termination beginning
    ; with any consonant except a nasal or a semi-vowel.
    (let (ans1 ans2 dhaatu2 c1 v c2)
     (setq ans1
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu2 '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal seT-code 'aniT)
	   (member dhaatu '(kRiSh spRish tRip))
	   (member aorist-sym '(lRiT lRi~N))
    )
    ; Antoine2#137, p.89. gives the optional 'ra' forms for kRiSh, spRish
    ; Kale p. 303 gives the optional 'ra' form for 'tRip'
    ; in simple future
    ; For these roots
    ; 'ra' optionally before a strong termination beginning
    ; with any consonant except a nasal or a semi-vowel.
    (let (ans1 ans2 dhaatu2 c1 v c2)
     (setq ans1
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq c1 (elt parts 0)) ; initial consonant
     (setq v [r a]) ; replace [Ri] with [r a]
     (setq c2 (elt parts 2)) ; final consonant
     (setq dhaatu2 (sym-without-space (vconcat c1 v c2)))
     (setq ans2
	    (conjugation-tab-aoristX-main upasargas class pada dhaatu2 seT-code))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal aorist-sym 'luT)
	 (or (member dhaatu '(iSh sah lubh riSh ruSh))
	     (and (equal dhaatu 'sah) (equal class 1) (equal pada 'A))
	 )
    )
    ;Kale 472. These roots admit 'i' optionally in the First Future
    (setq ans
	(conjugation-tab-aoristX-main upasargas class pada dhaatu  '(veT)))
   )
   ((and (equal dhaatu 'kLip) (equal pada 'P))
    ; Kale 473. 'kLip' is optionally 'P' in the futures and conditional,
    ; and when so it rejects 'i'
    (setq ans
      (conjugation-tab-aoristX-main upasargas class pada dhaatu  '(aniT)))
   )
   ((equal dhaatu 'grah)
    ; Kale 474. The augment 'i' as added to 'grah' is long
    ; in all non-conjugational tenses, except in the Perfect
    (setq ans
      (conjugation-tab-aoristX-main upasargas class pada dhaatu nil [ii]))
   )
   ((and (or (equal dhaatu 'vRi)
	     (equal ylast 'RI)
	     (and (equal ylast 'Ri) (< 1 (length (elt parts 0))))
	 )
	 (and (equal aorist-sym 'aashiirli~N) (equal pada 'A))
    )
    ; Kale 586 . These roots admit 'i' optionally in benedictive A
    ; For all changes to work properly, it is needed to
    ; call '..-main' with aniT and with seT
    (let (ans1 ans2) 
     (setq ans1
      (conjugation-tab-aoristX-main upasargas class pada dhaatu '(aniT)))
     (setq ans2
      (conjugation-tab-aoristX-main upasargas class pada dhaatu '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal aorist-sym 'aashiirli~N) (equal pada 'A) (equal seT-code 'veT))
    ; For logic to work properly, 'veT' must be separated here
    (let (ans1 ans2) 
     (setq ans1
      (conjugation-tab-aoristX-main upasargas class pada dhaatu '(aniT)))
     (setq ans2
      (conjugation-tab-aoristX-main upasargas class pada dhaatu '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (or (equal dhaatu 'vRi)
	     (equal ylast 'RI)
	 )
	(not (and (equal aorist-sym 'aashiirli~N) (equal pada 'A)))
    )
    ; Kale 475. The intermediate 'i' is optionally lengthened in the
    ; case of 'vRi' and roots ending in 'RI', except in the
    ; Perfect, the Benedictive atmanepada, and the Aorist parasmaipada
    (let (ans1 ans2)
     (setq ans1
       (conjugation-tab-aoristX-main upasargas class pada dhaatu nil [i]))
     (setq ans2
       (conjugation-tab-aoristX-main upasargas class pada dhaatu nil [ii]))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and
     (or (and (equal dhaatu 'gam) (equal pada 'P))
	(equal dhaatu 'han)
	(equal ylast 'Ri)
     )
     (member aorist-sym '(lRiT lRi~N))
    )
    ; Kale 483 p. 301
    ; 'gam P', 'han', and 'aniT' roots ending in 'Ri'
    ; admit 'i' in the Second Future and the conditional.
    ; 'gam P' - also that substituted for 'i' (to go) and
    ;  with 'adhi' (to remember) - also admits it in the Desiderative
    ; svRi is 'seT' in the 2nd Fut and Conditional (Kale p.305)
    ; NOTE: 'han' also admits 'i' in passive (atmanepada)
    ;  of 'aashiirli~N' (p .364 example)
    ;       This is handled elsewhere
    ; however, normal benedictive of 'han' does not admit 'i' (dhaatukosha)
    (when (and (equal ylast 'Ri)
	       (not (equal seT-code 'aniT))
	       ; next are already checked to be ok
	       (not (member dhaatu '(svRi)))
	  )
      (fol-msg (format "aorist-warning: %s %s %s %s\n"
		       dhaatu class pada seT-code))
    )
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu '(seT)))
   )
   ((and (member dhaatu '(kLip vRit vRidh shRidh syand))
	 (equal pada 'A)
	 (member aorist-sym '(lRiT lRi~N))
    )
    ; Kale 484 p. 301
    ; The roots (above) optionally take parasmaipada terminations in
    ; the Second Future, Conditional, and Desiderative.
    ; They reject the augment 'i' when parasmaipada terminations are taken.
    ; Note: the present logic (implicitly) assumes this applies
    ; in the passive voice (whose form is 'A'), as well as active voice
    (let (ans1 ans2)
     (setq ans1 (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq ans2
	   (conjugation-tab-aoristX-main upasargas class 'P dhaatu '(aniT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (member dhaatu '(kRit chRit ChRid tRid nRit))
	 (not (equal aorist-sym 'luT))
    )
    ; Kale 485 p.302
    ; The roots above take 'i' optionally when followed by
    ; an 'aarchadhaatuka' (non-conjugational) termination beginning
    ; with an 's' except in the Aorist
    ; By the examples on p. 305 and on p. 306, I inferred that
    ; this rule does not apply for the 'luT'.
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu '(veT)))
   )
   ((and (equal dhaatu 'nRit) (equal aorist-sym 'luT))
    ; 'nRit' is classed as 'veT'
    ; however, from Kale dhaatukosha, its 'luT' (periphrastic future)
    ; is 'seT' (inserts 'i'). Also, on p. 306, Kale says 'nRit P' is
    ; to be conjugated like 'Chrid'
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu '(seT)))
   )
   ((and (equal dhaatu 'i)
	 (equal upasargas '(adhi))
	 (equal pada 'A)
	 (equal aorist-sym 'lRi~N)
    )
    ; Kale 486. p. 302
    ; In the case of 'i' with 'adhi', the root 'gaa' is
    ; optionally substituted for 'i' in the conditional and the aorist.
    ; In this case, 'i' is substituted for the final vowel of 'gaa'
    ; before a consonantal weak termination; all terminations added
    ; to 'gaa' for 'i' are weak. This can be due to the establishment
    ; of 'gaa' as a 'seT' verb
    ; NOTE: By the example, actually 'ii' is substituted for 'aa'
    (let (ans1 ans2)
     (setq ans1 (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq ans2 (conjugation-tab-aoristX-main upasargas class pada 'gaa '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((or
     (and (equal dhaatu 'daa) (member class '(3 1)))
     (member dhaatu '(dhaa do de de maa sthaa paa haa so))
    )
    ; Kale 486. 'i' is substituted for the final vowel of these verbs
    ; before a consonantal weak termination.
    ; Note: for lRiT and lRi~N, all terminations are consonantal but
    ;  strong, thus these changes do not apply
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu))
   )
   ((member dhaatu '(a~nj ash))
    ; Kale p. 304. Roots which are 'veT' in luT, lRit, lRi~N
    (setq ans
       (conjugation-tab-aoristX-main upasargas class pada dhaatu '(veT)))
   )
   ((member dhaatu '(gup dhuup vichCh paN pan kam Rit))
    ; Kale 461.
    ; These roots preserved their conjugational bases optionally.
    ; These have present-system conjugational forms that look
    ; like conjugation 10 forms.
    ; When the conjugation-10 form is used, the roots are 'seT',
    ;  which is the conjugation-10 standard.
    ; When the non-conjugation-10-form is used, the roots take
    ; the general seT-code associated with the root.
    (let (ans1 ans2 dhaatu2)
     (setq dhaatu2
      (cond
       ((equal dhaatu 'gup) 'gopaay)
       ((equal dhaatu 'dhuup) 'dhuupaay)
       ((equal dhaatu 'vichCh) 'vichChaay)
       ((equal dhaatu 'paN) 'paNaay)
       ((equal dhaatu 'pan) 'panaay)
       ((equal dhaatu 'kam) 'kaamay)
       ((equal dhaatu 'Rit) 'Ritiiy)
      )
     )
     (setq ans1
       (conjugation-tab-aoristX-main upasargas class pada dhaatu))
     (setq ans2
       (conjugation-tab-aoristX-main upasargas class pada dhaatu2 '(seT)))
     (setq ans (join-arrays ans1 ans2))
    )
   )
   ((and (equal dhaatu 'dhuu) (equal class 6))
    ; Kale example p. 305
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu '(seT)))
   )
   ((and (equal dhaatu 'dhuu) (equal class 1))
    ; Kale example p. 305
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu '(veT)))
   )
   (t
    (setq ans (conjugation-tab-aoristX-main upasargas class pada dhaatu))
   )
  )
  ans
 )
)
(defun conjugation-tab-aoristX-main (upasargas class pada dhaatu 
					  &optional seTCode i-insert)
 (let (endings strengths ans n atok seT-gen btab itab
       bitab wparts parts types tense-sym)
  (when nil
   (fol-msg (format "aorist-main: %s %s %s %s %s %s\n"
		    upasargas class pada dhaatu seTCode i-insert))
  )
  ;--- 1. construct endings and strengths; init ans
  (setq tense-sym aorist-sym)
  (if (not i-insert) (setq i-insert [i]))
  (let (conj-class sym name apada)
   (setq conj-class 1) 
   (if (equal pada 'PASSIVE)
    (setq apada 'A)
    (setq apada pada)
   )
   (setq name (format "%s-%s-%s" tense-sym conj-class  apada))
   (setq sym (intern-soft name))
   (setq endings (sanget 'Sup sym))
   (setq endings (copy-sequence endings))
   (setq n (length endings))
;   (fol-msg (format "(endings)%s : %s\n" name endings))
   (setq name (format "%s-%s-%s-strengths" tense-sym conj-class  apada))
   (setq sym (intern-soft name))
   (setq strengths (sanget 'Sup sym))
   (setq strengths (copy-sequence strengths))
;   (fol-msg (format "%s: %s %s %s: %s\n" aorist-sym dhaatu class pada strengths);   )
  )
  ;--- 3a. atok
  (cond
   (t
    (setq atok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
   )
  )
  ;--- 3b. wparts , parts , types
  (setq wparts (word-parts atok))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  ;--- 4. seT code
  (let (temp)
   (if seTCode
    (setq temp seTCode)
    (progn
     (if (and (equal aorist-sym 'aashiirli~N) (equal pada 'P))
      (setq temp '(aniT))
      (setq temp (construct-seT-code1a dhaatu class pada upasargas))
     )
    )
   )
   (setq temp (solution temp))
   (if (and temp (not (listp temp))) (setq temp (list temp)))
   (setq seT-gen (solution temp))
  )
  ;--- 5a. get default table of i-inserts:
  ; All  endings in the luT, lRiT, and lRi~N begin with a consonant
  ; ('t' or 's'), so it is applicable to insert 'i' if this
  ; is required by seT-code.
  (if (equal pada 'P)
    ; parasmaipada
    (setq itab (vector
     seT-gen seT-gen seT-gen
      seT-gen seT-gen seT-gen
      seT-gen seT-gen seT-gen
    ))
    ; atmanepada
    (setq itab (vector
     seT-gen seT-gen seT-gen
     seT-gen seT-gen seT-gen
     seT-gen seT-gen seT-gen
    ))
  )
  (let (i x)
   (setq i 0)
   (while (< i n)
    (setq x (aref itab i))
    (when (equal x 'veT)
     (aset itab i '(aniT seT))
    )
    (setq i (1+ i))
   )
  )
  ;--- 5b. get table of base-seT codes (bitab)
  ; Usually, each element is the list of elements of btab and itab.
  ; However, for some exceptions, e.g. 'aa' roots, elements are
  ; made idiosyncratically
  (let (b nb lc pc strong)
    (if (and (equal aorist-sym 'aashiirli~N) (equal pada 'P))
     (setq strong nil)
     (setq strong t)
    )
    (setq b atok)
    (setq nb (length atok))
    (setq lc (elt b (1- nb))) ; last char
    (if (= nb 1) ; pc is penultimate char
     (setq pc nil)
     (cond
      ((member types '("CVC" "VC"))
       ;e.g. prachCh -> ([[p r] [a] [ch Ch]] "CVC"), pc = 'a'
       (setq pc (elt (elt (substring parts -2 -1) 0) 0))
      )
      (t (setq pc (elt b (- nb 2))) ; penultimate char
      )
     )
    )
    ;-- step1: modify 'b' as appropriate for this general tense
    (cond
     ((equal pada 'PASSIVE)
      ; do no adjustments to 'b'. They have been done already
     )
     ((and (equal aorist-sym 'aashiirli~N) (equal pada 'P))
      ; benedictive P
      (cond
       ((and (equal dhaatu 'ii) upasargas)
	; Kale 588. 'ii' shortens its 'ii' when joined with prepositions
	(setq b (vconcat (substring b 0 -1) [i]))
       )
       ((and (equal dhaatu 'uuh) upasargas)
	; Kale 588. 'uuh' shortens its 'uu' when joined with prepositions
	; before weak terminations beginning with 'y', as is the
	; case in benedictive parasmaipada.
	(setq b (vconcat [u] (substring b 1) ))
       )
       ((equal lc 'i)
	; Kale 581. final 'i' lengthened
	(setq b (vconcat (substring b 0 -1) [ii]))
       )
       ((equal lc 'u)
	; Kale 581. final 'u' lengthened
	(setq b (vconcat (substring b 0 -1) [uu]))
       )
       ((equal dhaatu 'Ri)
	; Kale 582. The root 'Ri' is gunated
	(setq b (gunate-final-vowel b))
       )
       ((and (equal lc 'Ri) (< 2 nb))
	; Kale 582. Final 'Ri' preceded by conjunct consonant is gunated
	(setq b (gunate-final-vowel b))
       )
       ((equal lc 'Ri)
	; Kale 581. Final 'Ri' changed to 'ri'
	(setq b (vconcat (substring b 0 -1) [r i]))
       )
       ((equal lc 'RI)
	; Kale 581. Final 'RI' becomes 'iir' after non-labial
	; and becomes 'uur' after labial or 'v'
	(if (or (labial-P pc) (equal pc 'v))
	 (setq b (vconcat (substring b 0 -1) [uu r]))
	 (setq b (vconcat (substring b 0 -1) [ii r]))
	)
       )
       ((member dhaatu '(a~nch a~nj bha~nj ra~nj sa~nj sva~nj
			granth manth  und skand syand
			indh bandh dambh stambh
			daMsh bhraMsh sraMs tRiMh))
        ; Kale 584. A penultimate nasal is dropped for these
        (setq b (vconcat (substring b 0 -2) (substring b -1)))
       )
       ((samprasaaraNa-P dhaatu class)
	 ; Kale 583. Roots capable of taking samprasaaraNa take it
	(setq b (samprasaaraNa b))
       )
       ((equal dhaatu 'shaas)
	; 583. 'shaas' substitutes 'i' for its vowel
	(setq b [sh i Sh])
       )
       ((or (member dhaatu '(daa dhaa maa sthaa gai))
	    (and (equal dhaatu 'paa) (equal class 1))
	    (and (equal dhaatu 'haa) (equal class 3))
	)
        ;Kale 585. in these cases, the final 'aa' changes to 'e'
        (setq b (vconcat (substring b 0 -1) [e]))
       )
       ((and (equal types "CV")
	    (member lc '(aa e ai o))
	    (< 1 (length (elt parts 0))) ; initial conjunct consonant
        )
        ;Kale 585. final 'aa' (original or substituted), if it be
        ; preceded by a conjunct consonant, is changed to 'e' optionally:
	(let (b1 b2)
	 (setq b1 (vconcat (substring b 0 -1) [e]))
	 (setq b2 (vconcat (substring b 0 -1) [aa]))
	 (setq b (list b1 b2))
	)
       )
       ((equal dhaatu 'han)
	; Kale p. 359 -example
	(setq b [v a gh])
       )
      )
     )
     ((and (equal aorist-sym 'aashiirli~N) (equal pada 'A))
      ; benedictive A
      (cond
       ((and (equal dhaatu 'ii) upasargas)
	; Kale 588. 'ii' shortens its 'ii' when joined with prepositions
	(setq b (vconcat (substring b 0 -1) [i]))
       )
       ((equal dhaatu 'dRish)
        ; By Kale p. 364 example
	; as 'dRish is 1 'P in dhaatukosha, this case only
	; occurs during the 'regular' passive formation
	; There is no gunation, so nothing to do
       )
       ((and (equal lc 'Ri) (equal seT-gen 'aniT))
	; Kale 587 Final 'Ri' unchanged when 'i' not prefixed
       )
       ((and (equal lc 'RI) (equal seT-gen 'aniT))
	; Kale 587 When 'i' is not prefixed,
	; final 'RI' preceded by a labial or 'v' is changed to 'uur';
	; otherwise, it is changed to 'iir'
	(if (or (labial-P pc) (equal pc 'v))
	 (setq b (vconcat (substring b 0 -1) [uu r]))
	 (setq b (vconcat (substring b 0 -1) [ii r]))
	)
       )
       ((equal lc 'aa)
	; don't gunate final 'aa' - leave it unchanged
       )
       ((and (equal types "CV")
	    (member lc '(e ai o))
        )
        ;Kale 585. final 'e', 'ai', 'o' become 'aa'
	(setq b (vconcat (substring b 0 -1) [aa]))
       )
       ((equal dhaatu 'bhrasj)
        ; Kale 464. The root 'bhrasj' (6 P 'to fry') assumes the
        ; forms 'bhrasj' and 'bharj' in the non-conjugational tenses
        (let (b1)
         (setq b (list [bh r a j j] [bh a r j]))
        )
       )
       ; otherwise, the radical vowel takes its guNa substitute
       (t
	 (setq b (gunate-final-vowel b))
       )
      )
     )     
     ((equal dhaatu 'daridraa)
      ; Kale 467. 'daridraa' drops its 'aa' before a non-conjugation
      ; termination except in the Desiderative and the Aorist where
      ; it retains it optionally
      (setq b (substring b 0 -1))
     )
     ((and (equal dhaatu 'gaa) (equal upasargas '(adhi)))
      ; Kale 486. See note in 'conjugation-tab-aorist'
      ; No guna but final vowel changed to 'ii'
      (setq b (vconcat (substring b 0 -1) [i]))
     )
     ((member lc '(aa e ai o))
      ; Kale 459. Roots ending in 'e', 'ai', and 'o' are treated as
      ; roots ending in 'aa'
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
     ((and (member dhaatu '(mii mi dii)) strong)
      ; Kale 459. The roots 'mi' (5 U 'to throw'),
      ; 'mii' (9 U 'to kill'), and 'dii' (4 A 'to perish')
      ; are treated as roots ending in 'aa' before a termination
      ; causing guna or vrddhi.
      ; In particular, this is the case for luT, lRiT, lRi~N;
      ; but not the case for benedictive-A
      (setq b (vconcat (substring b 0 -1) [aa]))
     )
     ((and (equal dhaatu 'lii) strong)
      ; Kale 459. The root 'lii' (9 P, 4 A 'to adhere or cling to') changes
      ; its vowel optionally to 'aa' before a termination causing
      ; guna or vrddhi.  
      ; In particular, this is the case for luT, lRiT, lRi~N
      (let (b1)
       (setq b (gunate-final-vowel b))
       (setq b1 [l aa])
       (setq b (list b b1))
      )
     )
     ((equal class 10)
      ; Kale 460 In the general tenses,
      ; roots of the tenth class preserve their 'ay' (i.e.,
      ; 'aya' with the final 'a' dropped), with all the changes that the
      ; root undergoes before it.
      ; The function 'dhaatu-a~Nga' performs this task.
      ; (dhaatu-a~Nga 'gaN 10 'P) -> (([g a N a y] ((count)) nil))
      (let (angas)
       (setq angas (dhaatu-a~Nga dhaatu class pada))
       (setq b (mapcar (lambda (anga) (elt anga 0)) angas))
       (setq b (solution b))
      )
     )
     ((and (equal dhaatu 'as) (equal class 2))
      ; Kale 462. 'as' substitutes 'bhuu' for itself
      ; NOTE: Although Kale does not say that the class 2 form of 'as'
      ; is intended, I have assumed this is meant. i.e., the
      ; class 4 form of 'as' (meaning 'to throw') is not altered
      (setq b [bh u u])
      (if strong (setq b (gunate-final-vowel b)))
     )
     ((equal dhaatu 'bruu)
      ; Kale 462. 'bruu' substitutes 'vach' for itself
      (setq b [v a ch])
      (if strong (setq b (gunate-final-vowel b)))
     )
     ((kuTaadi-P dhaatu)
      ; Kale 463.
      ; Neither guNa nor vRiddhi is substituted for the vowel of a few
      ; roots of the 6th class even before a strong termination except
      ;  (a) the 'a' of 1S and 3S of the Perfect
      ;  (b) the 'ay' of the causal
      ;  (c) the 'i' of the 3S of the Passive Aorist
      ; no change to b. It is already 'atok'
     )
     ((equal dhaatu 'bhrasj)
      ; Kale 464. The root 'bhrasj' (6 P 'to fry') assumes the
      ; forms 'bhrajj' and 'bharj' in the non-conjugational tenses
      (let (b1 b2)
;       (setq b (gunate-final-vowel b))
;       (setq b1 (gunate-final-vowel [bh a r j]))
;       (setq b1 [bh a r j])
;       (setq b (list b b1))
       (setq b (list [bh r a j j] [bh a r j]))
      )
     )
     ((and (equal dhaatu 'vij) (member class '(6 7)))
      ; Kale 466. The intermediate 'i' is weak in the case of the
      ; root 'vij' (6 A 7 P 'to tremble')
      ; In the case of luT, lRiT, lRi~N, I think this means
      ; no gunation. Thus, no change to 'b' is required here
     )
     ((and (equal dhaatu 'dhuu) (equal class 6))
      ; Kale example p. 305.
      ; end result is 'dhuvitaasmi', etc.
      ; thus, no gunation - no change to 'b' is required here
     )
     ((and (equal dhaatu 'uurNu) strong)
      ; Kale 466. The intermediate 'i' is optionally weak in the case of the
      ; root 'uuRNu' (to cover)
      ; In the case of luT, lRiT, lRi~N, I think this means
      ; no gunation as an option.
      (let (b1)
       (setq b1 (gunate-final-vowel b))
       (setq b (list b b1))
      )
     )
     ((member dhaatu '(diidhii vevii))
      ; Kale 467. The root 'diidhii' (2 A 'to shine') does not take
      ; guNa or vRiddhi before any termination. It also drops its
      ; final vowel before the intermediate 'i' and before 'y'.
      ; The root 'vevii' (2 A 'to go') takes the same changes.
      ; Note: As these are roots of more than one syllable, they are 'seT'.
      ; For the luT, lRiT, lRi~N, this means that the final 'ii'
      ; is always dropped.
      ; Similarly, for ashiirli~N, whether P or A
      (setq b (substring b 0 -1))
     )
     (t
      ; the default situation. gunate the vowel
      (setq b (gunate-final-vowel b))
     )
    )
    ;-- step1a: In case of 'lRi~N', join prefix 'a' to b
    (when (equal aorist-sym 'lRi~N)
     (let (bfirst b1 allb ansb b0)
      (setq allb (if (not (listp b)) (list b) b))
      (setq b nil)
      (while allb
       (setq b0 (car allb))
       (setq allb (cdr allb))
       (setq b1 (augment-a b0))
       (setq b (append-if-new b b1))
      )
     )
     (setq b (solution b))
    )
    ;-- step2: construct btab and bitab
    (setq btab (vector
      b b b
      b b b
      b b b
    ))
    (setq bitab (aorist-bitab btab itab))
   )

  (when nil ; 't' for debug
   (fol-msg (format "bitab=%s\n" bitab))
   (fol-msg (format "endings=%s\n" endings))
  )
  ;--- 6. combine base and endings to get ans
   (setq ans (aorist-bitab-join bitab endings dhaatu strengths i-insert))
  ;--- 7. Irregularities not yet covered
  ; None of these for luT, lRiT, lRi~N
  (when (and (equal aorist-sym 'aashiirli~N) (equal pada 'A))
   (cond
    ((equal dhaatu 'kRi)
     (aset ans 5 'kRiShiiDhvam) ; was kRiShiidhvam
    )
    ((equal dhaatu 'chi)
     (aset ans 5 'cheShiiDhvam) ; was cheShiidhvam ; Kale p. 358
    )
   )
  )
  ans
 )
)

(defun aorist-causal-base-irreg (dhaatu class pada upasargas Eng-def)
 (cond
  ((equal dhaatu 'suuch) '([s u s uu ch])) ;
  ((equal dhaatu 'chur) '[ch uu ch u r]) ; p. 344
  ; The rest from Kale 556
  ((and (equal dhaatu 'i) (equal upasargas '(adhi)))
   '([a p i p] [j ii g a p] [j ii g a m])) ; study, last=remember
  ((equal dhaatu 'iirShy) '([e r Sh i Sh y] [e r Sh y i y]))
  ((equal dhaatu 'uuRNu) [o r N uu n a v])  ; uurNu ?
  ((equal dhaatu 'gaN) '([j a g a N] [j ii g a N]))
  ((equal dhaatu 'ghraa) '([j i gh r a p] [j i gh r i p]))
  ((equal dhaatu 'chakaas) '([ch ii ch a k aa s] [ch a ch a k aa s]))
  ((equal dhaatu 'dyut) [d u d y u t])
  ((equal dhaatu 'paa) '([p ii p y] [p ii p a l])) ; drink, protect
  ((equal dhaatu 'sthaa) [t i Sh Th i p])
  ((equal dhaatu 'sphur) [p u s ph a r])
 )
)
 
(defun aorist-causal-base (dhaatu class pada upasargas Eng-def)
 ; get the base for aorist3 for the causal of the given dhaatu
 ; Also gets the base for class-10 roots
 ; Returns a list of token arrays.
 (let (ans b b1 b2 tok nb pc cb lc parts types i cb0 dbg)
  ;(setq dbg t)
  (setq tok (car (ITRANS-parse-words-1 (symbol-name dhaatu))))
  (let ( wparts)
   (setq lc (elt (substring tok -1) 0))
   (setq wparts (word-parts tok))
   (setq parts (elt wparts 0))
   (setq types (elt wparts 1))
   (if (equal types "CVC") (setq pc (elt (elt parts 1) 0)))
  )
  ; get 'cb' = the causal base
  (if (equal class 11)
   (setq cb (causal-base dhaatu class pada upasargas nil)) ;  'causal.el'
   (setq cb (class10-base tok))
  )
  (setq cb0 cb)
  (when dbg
   (fol-msg (format "before: cb=%s\n" cb))
  )
  ; implement Kale 550
  (let (cb1 cbnew cb2 cb1a)
   (while cb
    (setq cb1a (car cb))
    (setq cb (cdr cb))
    (setq cb1 (aorist3-alter10 cb1a dhaatu))
    (when dbg
     (fol-msg (format "after aorist3-alter10(%s,%s), cb1=%s\n" cb1a dhaatu cb1))
    )
    (when (member lc '(u uu))
     (setq cb2 (aorist3-alter10a cb1))
     (when dbg
      (fol-msg (format "after aorist3-alter10a: cb1=%s, cb2=%s\n" cb1 cb2))
     )
     (if (member dhaatu '(sru shru dru pru plu chyu)) ;Kale 550(a)
      (setq cb1 (list cb1 cb2))
      (setq cb1 cb2)
     )
    )
    (setq cbnew (append-if-new cbnew cb1))
   )
   (setq cb cbnew)
  )
  (when dbg
   (fol-msg (format "after 550: cb=%s\n" cb))
  )
  ; implement Kale 551
  (let (cb1 cbnew)
   (when (member dhaatu '(bhraaj bhaas bhaaSh diip jiiv miil piiD
		        kaN chaN raN bhaN vaN shraN lup heT heTh hve
			luT luTh lup))
;    (fol-msg (format  "tok=%s\n" cb0))
    (setq cb1 (substring (car cb0) 0 -2))
    (setq cbnew (reduplicate cb1))
;    (fol-msg (format "tok=%s, dhaatu=%s\n" tok dhaatu))
    (cond
     ((causal-603-P tok dhaatu)
      ; lengthen the final vowel
      ; this logic is required for agreement with Kale 551 in examples
      ; kaN, chaN , raN, shraN
      (setq cbnew (aorist3-lengthen-final-vowel cbnew))
     )
     ((member dhaatu '(lup luT luTh))
      ; based on examples, the initial 'u' is lengthened in the first form
       '(fol-msg (format "chk: cb=%s\n" cb))
       (aset (car cb) 1 'uu)
     )
    )
    (setq cb (append cb (list cbnew)))
    ; 'hve' is exceptional (it takes samprasaaraNa somehow. Kale 553.
    (when (equal dhaatu 'hve)
     (setq cb (list [j uu h a v] [j u h aa v]))
    )
   )
  )
  ;(fol-msg (format "after 551: cb=%s\n" cb))

  ; 'shvi' is exceptional (it optionally takes samprasaaraNa
  (when (equal dhaatu 'shvi)
   (setq cb (append cb (list [sh uu sh a v])))
  )

  (setq b tok) ; the dhaatu
  (setq nb (length b))
  ; pc is penultimate char 
  (if (= nb 1) 
   (setq pc nil)
   (setq pc (elt b (- nb 2)))
  )
  (cond
   ((and (kale-400-P dhaatu) (not (equal dhaatu 'aMs)))
    ; kale 554: preserve vowel unchanged (no 'i' substituted)
    (let (tok i n v v1 all ans1 this this1)
     (if (not (listp cb)) (setq all (list cb)) (setq all cb))
;     (fol-msg (format "Kale-400: dhaatu=%s cb=%s\n" dhaatu cb))
     (while all
      (setq this (car all))
      (setq all (cdr all))
;      (setq tok (car (ITRANS-parse-words-1 (symbol-name this))))
      (setq tok (copy-sequence this))
      (setq n (length tok))
      (setq i 0)
      (while (< i n)
       (setq v (elt tok i))
       (when (vowel-P v)
        (aset tok i 'a)
        (setq i n)
       )
       (setq i (1+ i))
      )
      (setq this1 tok)
      (setq ans1 (append ans1 (list this1)))
     )
     (setq cb ans1)
     (setq ans cb)
;     (fol-msg (format "cb=%s\n" cb))
    )
   )
   ((and (vowel-P (elt tok 0)) (equal types "VC")); initial vowel
    (setq i [i])
    (when (member dhaatu '(uun a~Nk a~Ng andh aMs arth))
     ; Kale 549 (c) p. 342
     (setq i [a])
    )
    
    (cond
     ((and (equal (length (elt parts 1)) 2) ;  conjunct cons
	  (or (nasal-P pc) (equal pc 'M) (member pc '(d r)))
      )
      ; Kale 549 (b)
      (let (v c1 c2 n1)
       (setq c1 (elt parts 1))
       (setq v (elt parts 0)) ; the vowel
       (setq c2 (vector (reduplicate-cons lc)))
       (if (or (equal pc 'M) (not (nasal-P pc)))
	(setq n1 pc)
	(setq n1 (nasal-match (elt c2 0)))
       )
       (setq n1 (vector n1))
       (setq ans (vconcat v (substring c1 0 -2) n1 c2 i (substring c1 1)))
       (when dbg
        (fol-msg (format "Kale 549 (b). ans =%s\n" ans))
       )
       ans
      )
     )
     ((equal (length (elt parts 1)) 1) ; simple cons
      ; Kale 549 (a)
       (setq ans (vconcat (substring tok 0 -1)
 	  	        (vector (reduplicate-cons lc))
 		        i
 		        (substring tok -1)))
     )
     (t ; ends with some other conjunct consonant
      ; Kale 549 (a')
      (let (v c1 c2)
       (setq c1 (elt parts 1))
       (setq v (elt parts 0)) ; the vowel
       (setq c2 (vector (reduplicate-cons lc)))
       (setq ans (vconcat v (substring c1 0 -1) c2 i (substring c1 -1)))
      )
     )
    ) 
   ); end of initial-vowel
   ((and (equal types "CVC")
	 (member (elt (elt parts 1) 0) '(Ri RI))
    )
    ; Kale 548(c) p. 341
    ; Roots having a penultimate 'Ri' or 'RI' optionally preserve it,
    ; with 'RI' being changed to 'Ri'
    (let (b0)
     (setq b0 b)
     (setq b (vconcat (substring b 0 -2) [Ri] (substring b -1))); 'Ri'
     (setq b (vconcat b [a y])) ; base preserving 'Ri'
     (setq b (aorist3-alter10 b))
     (when dbg
      (fol-msg (format "Kale 548(c), after aorist3-alter10. b0=%s,b=%s\n" b0 b))
     )
    )
    ; make two options
    (setq ans (append cb (list b)))
   )
   (t
    (setq ans cb)
   )
  )
  ;(fol-msg (format "aorist-causal-base: after: cb=%s, ans=%s\n" cb ans))
  (if (not (listp ans)) (setq ans (list ans)))
  ans
 )
)

(defun nasal-match (c)
 ; Find nasal matching the class of 'c' 
 ; e.g., if 'c' is dental, return 'n'
 ; If no match is found, return 'M'
  (cond
   ((guttural-P c) '~N)
   ((palatal-P c) '~n)
   ((cerebral-P c) 'N)
   ((dental-P c) 'n)
   ((labial-P c) 'm)
   (t 'M)
  )
)
(defun reduplicate-cons (c)
 ; c assumed to be a token representing a single consonant
 ; a consonant-token is returned
 (let (r ans)
  (setq r (reduplicate (vector c 'a)))
  (setq ans (elt r 0)) 
 )
)
(defun aorist3-alter10 (tok &optional dhaatu)
 ; carry out the alterations of Kale 548(a), applicable to
 ; roots of the 10th class and of causals. Includes reduplication
 ; Assumes, if class 10, the conjugation-10 base already reflected in 'tok'.
 (let (ans b wparts parts types ntypes ivowel jvowel b1 dbg)
;   (fol-msg (format "tok=%s\n" tok))
  (setq b (aorist3-reduplicate tok dhaatu))
  ;(setq dbg t)
  (when dbg
   (fol-msg (format "aorist3-alter10(%s,%s) after aorist3-reduplicate, b=%s\n" tok dhaatu b))
  )
  ; Kale 548 b.
  ; If the vowel of the reduplicative syllable is 'a',
  ; it is changed to 'i' if the syllable following it
  ; be short and not prosodially long (i.e., not followed by conj cons. p.14)
  ; This having been checked, if the reduplicative 
  ; syllable has 'i' for its vowel, and if the following
  ; syllable is not followed by either a long syllable or
  ; a conjunct consonant, then
  ; the 'i' of the reduplicative syllable is lengthened to 'ii'
  ; Note: based on Whitney, this also applies when the reduplicative
  ; syllable is 'u'
  ; Kale examples:
  ;  [bh aa v a y] -> [b ii bh a v] (causal of 'bhuu')
  ;  [ch e t a y] -> [ch ii ch i t] (causal of 'chi')
  ;  [s kh a l a y] -> [ch i s kh a l] (causal of 'skhal')
  ;  [s p a n d a y] -> [p a s p a n d] (causal of 'spand')
  ;  [v a r t a y] -> [v a v a r t]
  ;  [v Ri t a y] -> [v ii v Ri t]
  ;  [k ii r t a y] -> [ch i k ii r t] : this is an exception,
  ;    implemented above
  (setq wparts (word-parts b))
  (setq parts (elt wparts 0))
  (setq types (elt wparts 1))
  (setq ntypes (length types)) 
  (cond
   ((and (<= 5 ntypes) (equal (substring types 0 5) "CVCVC"))
    (setq ivowel 1)
   )
   ((and (<= 4 ntypes) (equal (substring types 0 4) "VCVC"))
    (setq ivowel 0)
   )
  )
  (when dbg
   (fol-msg (format "chk: parts=%s, types=%s, ivowel=%s\n" parts types ivowel))
  )
  (cond
   (ivowel
    (setq jvowel (+ ivowel 2))
    (when (and (equal (elt parts ivowel) [a]) ; 1st V
		(shortvowel-P (elt (elt parts jvowel) 0))
		(not (prosodially-long-P parts jvowel))
	   )
      (aset parts ivowel [i]) ; change vowel to 'i'
     )
     (when (and (equal (elt parts ivowel) [i])
	        (shortvowel-P (elt (elt parts jvowel) 0))
		(not (prosodially-long-P parts jvowel))
		(not (prosodially-long-P parts ivowel))
	   )
      (aset parts ivowel [ii]) ; lengthen 'i'
      (when dbg
       (fol-msg (format "chk: parts now =%s\n" parts))
      )
     )
     (when (and (equal (elt parts ivowel) [u])
	        (shortvowel-P (elt (elt parts jvowel) 0))
		(not (prosodially-long-P parts jvowel))
		(not (prosodially-long-P parts ivowel))
	   )
      (aset parts ivowel [uu]) ; lengthen 'u'
     )
   )
  )
  
;  (fol-msg (format "parts changed to %s\n" parts))
  (setq b1 (vconcat (flatten parts)))
  (setq ans b1)
;  (fol-msg (format "chk: %s %s %s\n" dhaatu b b1))
  (cond
   ((and (equal aorist-id 3)
	 (member dhaatu '(smRi dRi tvar prath mrad stRI spash veShT cheShT)))
    ; Kale 552. Definitely or optionally change the vowel
    ; in the redup syllable to 'a'
;    (fol-msg (format "check 552\n"))
    (let (b2 n i v)
     (setq b2 (copy-sequence b1))
     (setq n (length b2))
     (setq i 0)
     (while (< i n)
      (setq v (elt b2 i))
      (when (vowel-P v)
       (aset b2 i 'a)
       (setq i n) ; end loop
      )
      (setq i (1+ i))
     )
     (cond
      ((member dhaatu '( veShT cheShT))
       (setq ans (list b1 b2))
      )
      (t (setq ans b2)
      )
     )
    )
   )
  )   
  ans
 )
)
(defun aorist3-reduplicate (tok &optional dhaatu)
 (let (b b1)
  ; remove the [a y]
;  (fol-msg (format "3-redup: tok=%s, dhaatu=%s\n" tok dhaatu))
  (setq b (substring tok 0 -2))
  ; change long vowel
  ; Note: 'kRIt' , which enters here as [k ii r t], does not
  ; shorten the 'ii', based on examples in Kale p. 341 and dhaatukosha
  ; Other examples (cheShT veShT from Kale 552)
  ; O
;  (fol-msg (format "redup: b=%s\n" b))
;   (if (not (member b '([k ii r t] [ch e Sh T] [v e Sh T])))
;    (setq b (aorist3-shorten-1st-vowel b))
;   )
  (if (not (member dhaatu
    '( ; Kale 555. These roots do not shorten penultimate
   shaas ej kaash kriiD kShiiv khaad khel
   Dhauk taay daash dev naath proth 
   baadh yaach yodh raadh raaj laagh
   lep lok loch vep vel shlaagh shlok
   sek sev heSh
    )))
   (if (not (and (< 2 (length b))
		(consonant-P (elt (substring b -1) 0))
		(consonant-P (elt (substring b -2 -1) 0))
           )
      )
    (progn
    ;(fol-msg (format "aorist3-reduplicate chk: b before=%s\n" b))
    (setq b (aorist3-shorten-1st-vowel b))
    ;(fol-msg (format "aorist3-reduplicate chk: b  after=%s\n" b))
    )
   )
  )
  ; reduplicate in 'usual' way
  (setq b1 (reduplicate b))
;  (fol-msg (format " redup: tok=%s, b=%s\n" tok b))
  b1
 )
)
(defun aorist3-alter10a (tok)
 ; implements Kale 550
; (fol-msg (format "chk: %s\n" tok))
 (let (ans i n j v v1 nc)
  (setq ans (copy-sequence tok))
  (setq n (length tok))
  (setq i 0)
  (while (< i n)
   (setq v (elt tok i))
   (when (vowel-P v)
    (setq j i)
    (setq i n)
   )
   (setq i (1+ i))
  )
  (when (and j (member v '(i ii)))
   (setq v1 (if (equal v 'i) 'u 'uu)) ; i->u, ii->uu
   (setq nc (elt tok (1+ j)))
;   (fol-msg (format "nc=%s, nxt vow=%s\n" nc (elt tok (+ 2 j))))
   (when (not (and 
	       (or (labial-P nc)
		   (semivowel-P nc)
		   (equal nc 'j)
	       )
	       (member (elt tok (+ 2 j)) '(a aa))
	      )
	 )
    (aset ans j v1)
   )   
;    (when (not
; 	  (or (labial-P nc)
; 	      (semivowel-P nc)
; 	      (and (equal nc 'j) (member (elt tok (+ 2 j)) '(a aa))))
; 	 )
;     (aset ans j v1)
;    )
  )
  ans
 )
)
(defun aorist3-shorten-1st-vowel (tok)
 (let (ntok ans m  v v1 j)
  (setq ans (copy-sequence tok))
  (setq m (length ans))
  (setq j 0)
  (while (< j m)
   (setq v (elt tok j))
   (when (vowel-P v)
    (setq v1 (aorist3-shorten-vowel v))
    (aset ans j v1)
    (setq j m) ; end loop
   )
   (setq j (1+ j))
  )
  ans
 )
)
(defun aorist3-shorten-vowel (v)
 (let (allv shortv i n j ans)
  (setq allv (vconcat vowel-set)) ;[a i u Ri Li aa ii uu RI LI e ai o au]
  (setq shortv [a i u Ri Li a i u Ri Li i i u u])
  (setq n (length allv))
  (setq i 0)
  (setq ans v)
  (while (< i n)
   (when (equal v (elt allv i))
    (setq ans (elt shortv i))
    (setq i n) ; end loop
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun aorist3-lengthen-final-vowel (tok)
 (let (ntok ans m  v v1 j)
  (setq ans (copy-sequence tok))
  (setq m (length ans))
  (setq j m)
  (while (< 0 j)
   (setq j (1- j))
   (setq v (elt tok j))
   (when (vowel-P v)
    (setq v1 (aorist3-lengthen-vowel v))
    (aset ans j v1)
    (setq j 0) ; end loop
   )
  )
  ans
 )
)
(defun aorist3-lengthen-vowel (v)
 (let (allv longv i n j ans)
  (setq allv (vconcat vowel-set)) ;[a i u Ri Li aa ii uu RI LI e ai o au]
  (setq longv [aa ii uu RI LI aa ii uu RI LI e ai o au])
  (setq n (length allv))
  (setq i 0)
  (setq ans v)
  (while (< i n)
   (when (equal v (elt allv i))
    (setq ans (elt longv i))
    (setq i n) ; end loop
   )
   (setq i (1+ i))
  )
  ans
 )
)
(defun short-syllable-P (parts)
 ; parts is a sequence of tokens with 2 elements of form CV (constants-vowel)
 ; it is considered short, and not prosodially long (?)
 ; if (a) the vowel is short
 ; and (b) the consonant is not conjunct
  (shortvowel-P (elt (elt parts 1) 0))
)
(defun prosodially-long-P (parts ivowel)
 ; Kale #11, p. 14
 ; a short vowel followed by a conjunct consonant is
 ; prosodially long.
 (let (n)
  (setq n (length parts))
  (and
   (< ivowel (1- n))
   (shortvowel-P (elt (elt parts ivowel) 0))
   (< 1 (length (elt parts (1+ ivowel))))
  )
 )
)
(defun old-aorist3-class10-base (tok)
 ; Given the token representation 'tok' of a root,
 ; construct a list of token arrays, representing 
 ; the base(s) (ending in '[a y]') of the root as
 ; formed for the 10th conjugational class.
 (let (b1 b2 ans dhaatu)
  (setq dhaatu (sym-without-space tok))
  (cond 
   ((member dhaatu '(
     ; Kale 400 : these preserve the root unchanged
     agh ; to sin
     kath ; to tell
     kShap ; to send , to pass
     gaN ; to count
     ; NOTE : 'gal' ; U to filter , A to throw appears in
     ; Kale 'dhaautkosha' as 10A, and lengthens 'a': gaalayate
     ; Thus, I exclude it here
    ; gal 
     var ; choose , seek
     dhvan ; sound
     mah ; to honor
     rach ; to compose
     ras ; taste
     rah ; forsake
     ; 'raT' (shout) appears only as class 1 in dhaatukosha of Kale
     stan ; thunder
     svar ; blame
     pad ; go
     vaT ; separate
     karN ; bore , pierce : This is prosodially long. so 'a' normally short
     ; Chad ; conceal (This shows as lengthening 'a' - Why here?
     chap ; grind , cheat
     shrath ; be-weak
     ; shlath ; be-weak (not in dhaatukosha or Apte dictionary)
     vyay ; spend , give
     spRih ; desire
     mRig ; hunt
     ; mRiSh ; bear ; 'marShayati - e' in dhaatukosha and Apte dictionary
     guN ; invade , advise , multiply
     ; kuuN  ; speak , converse ('kuuNay' is REGULAR)
     ; grah ; take (not in dhaatukosha or Apte dictionary)
     kuh ; astonish
     sphuT ; break-open (Kale dhaatukosha shows 'sphoTayati')?
     sukh ; make-happy
    ))
    (setq ans (vconcat tok [a y]))
   )
   ((member dhaatu '(
    ; these have two forms, one with the root vowel unchanged,
    ; and one with the root vowel lengthened
     shaTh ; 'shaaThay' leave unfinished - 'shaThay' speak ill - ashiishaThata
     paT ; 'paTay' weave - 'paaTay' tear - apapaTas apapaTata
     kal ; 'kalay' count - 'kaalay' throw - achakalat achakalata
     laj ; 'lajay' shine - 'laajay' conceal - alalajat aliilajat
     vas ; 'vasay' dwell - 'vaasay' love , cut - aviivasat 
     puT ; 'puTay' bind - 'poTay' speak , shine - apupuTat apuupuTat
    ))
    (setq b1 (dhaatu-a~Nga-10 tok))
    (setq b2 (vconcat tok [a y]))
    (setq ans (list b1 b2))
   )
   ((equal dhaatu 'aMs)
    (setq b1 (dhaatu-a~Nga-10 tok))
    (setq b2 [a M s aa p a y])
    (setq ans (list b1 b2))
   )
   ((equal dhaatu 'kRip)
    (setq b1 (dhaatu-a~Nga-10 tok))
    (setq b2 [k Ri p aa y])
    (setq ans (list b1 b2))
   )
   (t
    (setq ans (dhaatu-a~Nga-10 tok)) ; the regular formation
   )
  )
  (if (not (listp ans)) (setq ans (list ans)))
  ans
 )
)


(defun aorist-gunate-final-vowel (tok &optional vriddhi-flag)
 (let (ans m  v v1 j)
  (setq ans tok)
  (setq m (length tok))
  (setq j m)
  (while (< 0 j)
   (setq j (1- j))
   (setq v (elt tok j))
   (when (vowel-P v)
    (if vriddhi-flag
     (setq v1 (vrddhi1 v))
     (setq v1 (guna v))
    )
;    (setq v1 (elt v1 0))
;    (aset ans j v1)
    (setq ans (vconcat (substring tok 0 j) v1 (substring tok (1+ j) m)))
    (setq j 0) ; end loop
   )
  )
  ans
 )
)
(defun aorist-endings (&optional tense-sym)
 (let (endings strengths n pada)
  (if (not tense-sym) (setq tense-sym aorist-sym))
  (setq pada aorist-pada)
  (let (conj-class sym name apada)
   (setq conj-class 1) 
   (if aorist-passive-P
    (setq apada 'A)
    (setq apada pada)
    
   )
   (setq name (format "%s-%s-%s" tense-sym conj-class  apada))
;   (fol-msg (format "aorist-pada=%s, name=%s\n" aorist-pada name))
   (setq sym (intern-soft name))
   (setq endings (sanget 'Sup sym))
   (setq endings (copy-sequence endings))
   (setq name (format "%s-%s-%s-strengths" tense-sym conj-class  apada))
   (setq sym (intern-soft name))
   (setq strengths (sanget 'Sup sym))
   (setq strengths (copy-sequence strengths))
  )
  (when aorist-passive-P
   (aset endings 0 [i])
  )
  endings
 )
)
(defun kale-461-alt (dhaatu)
 (let (xalt)
     ; Kale 461. The roots 'gup' 'dhuup' 'vichCh' 'paN' 'pan'
     ; 'kam' and ? 'Rit' preserve their conjugational bases optionally
     ; Note: I am no sure of 'Rit'. The form given works for
     ; aorist passive example on p. 366.
     (when (member dhaatu '(gup dhuup vichCh paN pan kam Rit))
      (cond
       ((equal dhaatu 'gup) (setq xalt [g o p aa  y]))
       ((equal dhaatu 'dhuup) (setq xalt [dh uu p aa y]))
       ((equal dhaatu 'vichCh) (setq xalt [v i ch Ch aa y]))
       ((equal dhaatu 'paN) (setq xalt [p a N aa y]))
       ((equal dhaatu 'pan) (setq xalt [p a n aa y]))
       ((equal dhaatu 'kam) (setq xalt [k a m aa y]))
       ((equal dhaatu 'Rit) (setq xalt [Ri t i y] )) 
      )
     )
  xalt
 )
)
