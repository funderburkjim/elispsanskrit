;
(let (sets set)
(setq sets '(shortsimplevowel-set
longsimplevowel-set
simplevowel-set
diphthong-set
vowel-set
K-set
CH-set
TT-set
T-set
P-set
semivowel-set
sibilant-set
consonant-set
guttural-set
palatal-set
cerebral-set
dental-set
labial-set
hardnonaspirate-set
hardaspirate-set
softnonaspirate-set
softaspirate-set
nasal-set
hard-set
soft-set
mute-set
))
(mapcar 
 (lambda (s) 
  (let (x y z)
   (setq x (translate-ITRANS-SLP1 (eval s)))
   ; x is a list of characters (or maybe, strings)
   (setq y "")
   (while x
    (setq y (format "%s%s" y (car x)))
    (setq x (cdr x))
   )
   ; change '-' to '_' in s
   (setq z (format "%s" s)) ; z is string form of s
   (setq z (replace-regexp-in-string "-" "_" z))
   (insert (format "%s = '%s'\n" z y))
  )
 )
 sets
)
t
)
Result:
shortsimplevowel_set = 'aiufx'
longsimplevowel_set = 'AIUFX'
simplevowel_set = 'aiufxAIUFX'
diphthong_set = 'eEoO'
vowel_set = 'aiufxAIUFXeEoO'
K_set = 'kKgGN'
CH_set = 'cCjJY'
TT_set = 'wWqQR'
T_set = 'tTdDn'
P_set = 'pPbBm'
semivowel_set = 'yrlvh'
sibilant_set = 'zSsH'
consonant_set = 'kKgGNcCjJYwWqQRtTdDnpPbBmyrlvhzSsHM'
guttural_set = 'kKgGNhH'
palatal_set = 'cCjJYyS'
cerebral_set = 'wWqQRrz'
dental_set = 'tTdDnls'
labial_set = 'pPbBmvH'
hardnonaspirate_set = 'kcwtp'
hardaspirate_set = 'KCWTP'
softnonaspirate_set = 'gjqdb'
softaspirate_set = 'GJQDB'
nasal_set = 'NYRnm'
hard_set = 'kcwtpKCWTPzSsH'
soft_set = 'gjqdbGJQDBNYRnmyrlvh'
mute_set = 'kcwtpKCWTPgjqdbGJQDBNYRnm'
