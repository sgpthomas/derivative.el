;;; -*- lexical-binding: t; -*-

(require 'thunk)

(cl-defmacro lazy-struct (name lazy &rest fields)
  (let ((thunked-fields (-map (lambda (x) (if lazy `(thunk-delay ,x) x)) fields)))
    `(progn
       (defun ,name ,fields
	 (record ',name ,@thunked-fields))
       (defun ,(intern (format "%s-p" name)) (rec)
	 (equal (type-of rec) ',name))
       ,@(-map-indexed
	  (lambda (idx field)
	    `(defun ,(intern (format "%s-%s" name field)) (rec)
	       (let ((v (aref rec ,(+ 1 idx))))
		 (if (functionp v)
		     (thunk-force v)
		   v))))
	  fields))))

(macroexpand
 '(lazy-struct my/test t a))

(lazy-struct my/test t a)
(my/test-a (my/test 'y))

(provide 'lazy-struct)
