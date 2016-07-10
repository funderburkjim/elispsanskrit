(defun convert-gender-form-data ()
 (let (varnames varname val pyvarname pyval)
  (setq varnames '(
   declension-cons-forms gender-form-data-0 gender-form-data-1
   gender-form-data-2 gender-form-data-3 gender-form-data-4
  ))
  (while varnames
   (setq varname (car varnames))
   (setq varnames (cdr varnames))
   (setq pyvarname (replace-regexp-in-string "-" "_" (format "%s" varname)))
   (setq val (eval varname))
   (setq pyval 
    (mapconcat 
     (lambda (x)
      (if (listp x)
       (mapconcat (lambda (y) (format "%s" y)) x ",")
       (format "%s" x)
      )
     )
     val
     ":"
    )
   )
   (insert (format "%s_string='%s'\n" pyvarname pyval))
  )
  ;(mapcar (lambda (varname) (insert (format "%s=%s\n" varname (eval varname)))) varnames)
 )
)
