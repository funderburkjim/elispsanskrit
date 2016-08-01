
This initializes the SLP1 Python version of the PRESPART-endings data structure.
This part is the Parasmaipada. 
It creates lines for init.py such as:
sup-m-prap-s=n:ntO:ntaH:ntam:ntO:taH:tA:dByAm:dBiH:te:dByAm:dByaH:taH:dByAm:dByaH:taH:toH:tAm:ti:toH:tsu:n:ntO:ntaH

(let (endings pada code gender data out ndata ndata1 pada)
 (setq pada 'A)
 (setq data (plist-get PRESPART-endings pada))
 (mapc 
  (lambda (code)
   (setq data1 data)
   (mapc
    (lambda (g)
     (setq data2 (plist-get data1 g))
     (setq endings (translate-ITRANS-SLP1 data2))
     (setq endings1
      (mapconcat
       (lambda (e)
        (when (listp e)
         (setq e1 (mapconcat (lambda (e1) (format "%s" e1)) e ","))
         ;(setq e1 (format "[%s]" e1))
        )
        (when (not (listp e))
         (setq e1 (format "%s" e))
        )
        e1
       )
       endings
       ":"
      )
     )
     ;(setq endings1 (format "[%s]" endings1))
     ;(setq key (format "'%s-%s-%s'" pada code g))
     (setq key (format "sup-%s-prmp" g))
     (setq key (downcase key))
     (fol-msg (format "%s=%s\n" key endings1))
    )
    '(M F N)
   )
  )
  '(dummy)
  )
)
This is the older code.
(let (endings pada code gender data out ndata ndata1 pada)
 (setq pada 'P)
 (setq data (plist-get PRESPART-endings 'P))
 (mapc 
  (lambda (code)
   (setq data1 (plist-get data code))
   (mapc
    (lambda (g)
     (setq data2 (plist-get data1 g))
     (setq endings (translate-ITRANS-SLP1 data2))
     (setq endings1
      (mapconcat
       (lambda (e)
        (when (listp e)
         (setq e1 (mapconcat (lambda (e1) (format "'%s'" e1)) e ","))
         (setq e1 (format "[%s]" e1))
        )
        (when (not (listp e))
         (setq e1 (format "'%s'" e))
        )
        e1
       )
       endings
       ","
      )
     )
     (setq endings1 (format "[%s]" endings1))
     (setq key (format "'%s-%s-%s'" pada code g))
     (setq key (downcase key))
     (fol-msg (format "  PRESPART.d[%s] = %s\n" key endings1))
    )
    '(M F N)
   )
  )
  '(S W SW VW)
  )
)

  PRESPART.d['p-s-m'] = ['n','ntO','ntaH','ntam','ntO','taH','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','n','ntO','ntaH']
  PRESPART.d['p-s-f'] = ['ntI','ntyO','ntyaH','ntIm','ntyO','ntIH','ntyA','ntIByAm','ntIBiH','ntyE','ntIByAm','ntIByaH','ntyAH','ntIByAm','ntIByaH','ntyAH','ntyoH','ntInAm','ntyAm','ntyoH','ntIzu','nti','ntyO','ntyaH']
  PRESPART.d['p-s-n'] = ['t','ntI','nti','t','ntI','nti','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','t','ntI','nti']
  PRESPART.d['p-w-m'] = ['n','ntO','ntaH','ntam','ntO','taH','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','n','ntO','ntaH']
  PRESPART.d['p-w-f'] = ['tI','tyO','tyaH','tIm','tyO','tIH','tyA','tIByAm','tIBiH','tyE','tIByAm','tIByaH','tyAH','tIByAm','tIByaH','tyAH','tyoH','tInAm','tyAm','tyoH','tIzu','ti','tyO','tyaH']
  PRESPART.d['p-w-n'] = ['t','tI','nti','t','tI','nti','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','t','tI','nti']
  PRESPART.d['p-sw-m'] = ['n','ntO','ntaH','ntam','ntO','taH','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','n','ntO','ntaH']
  PRESPART.d['p-sw-f'] = [['tI','ntI'],['tyO','ntyO'],['tyaH','ntyaH'],['tIm','ntIm'],['tyO','ntyO'],['tIH','ntIH'],['tyA','ntyA'],['tIByAm','ntIByAm'],['tIBiH','ntIBiH'],['tyE','ntyE'],['tIByAm','ntIByAm'],['tIByaH','ntIByaH'],['tyAH','ntyAH'],['tIByAm','ntIByAm'],['tIByaH','ntIByaH'],['tyAH','ntyAH'],['tyoH','ntyoH'],['tInAm','ntInAm'],['tyAm','ntyAm'],['tyoH','ntyoH'],['tIzu','ntIzu'],['ti','nti'],['tyO','ntyO'],['tyaH','ntyaH']]
  PRESPART.d['p-sw-n'] = ['t',['tI','ntI'],'nti','t',['tI','ntI'],'nti','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','t',['tI','ntI'],'nti']
  PRESPART.d['p-vw-m'] = ['t','tO','taH','tam','tO','taH','tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','t','tO','taH']
  PRESPART.d['p-vw-f'] = ['tI','tyO','tyaH','tIm','tyO','tIH','tyA','tIByAm','tIBiH','tyE','tIByAm','tIByaH','tyAH','tIByAm','tIByaH','tyAH','tyoH','tInAm','tyAm','tyoH','tIzu','ti','tyO','tyaH']
  PRESPART.d['p-vw-n'] = ['t','tI',['ti','nti'],'t','tI',['ti','nti'],'tA','dByAm','dBiH','te','dByAm','dByaH','taH','dByAm','dByaH','taH','toH','tAm','ti','toH','tsu','t','tI',['ti','nti']]

And this is the Atmanepada part.
