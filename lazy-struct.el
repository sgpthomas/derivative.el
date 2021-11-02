;;; -*- lexical-binding: t; -*-

(require 'thunk)

(cl-defmacro lazy-struct (name &rest fields)
  (let ((thunked-fields fields))
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
	  fields)
       (defun ,(intern (format "%s-setf" name)) (rec thing &optional name)
	 (let ((sym (if name name 'rme)))
	   ,@(-map-indexed
	      (lambda (idx field)
		`(when (equal (aref rec ,(+ 1 idx)) sym)
		   (setf (aref rec ,(+ 1 idx)) thing)))
	      fields))))))

(provide 'lazy-struct)
