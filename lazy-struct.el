;;; -*- lexical-binding: t; -*-

()

(setq-local l/a '(:field-a 4 :field-b 5))

(plist-get '(:field-a 4 :field-b 5) :field-b)

(aref (record 'food 4 "a") 2)

(setq-local l (record 'lazy-test/struct 'nil 'nil))

(make-record 'lazy-test 2 'nil)

(defmacro thunk (val)
  `(lambda () ,val))

(defun lazy-test (a b)
  (record 'lazy-test (thunk a) (thunk b)))

(defun lazy-test-a (rec)
  (when (equal 'lazy-test (type-of rec))
    (aref rec 1)))

(defmacro lazy-struct (name &rest fields)
  (let ((thunked-fields (-map (lambda (x) `(thunk ,x)) fields)))
    `(progn
       (defun ,name ,fields
	 (record ',name ,@thunked-fields))
       ,@(-map-indexed
	  (lambda (idx field)
	    `(defun ,(make-symbol (format "%s-%s" name field)) (rec)
	       (aref rec ,(+ 1 idx))
	       )
	    )
	  fields))))

(macroexpand
 '(lazy-struct my/test a b))

(lazy-struct my/test2 a b)
(my/test2-a (my/test2 "a" "b"))

(defstruct-lazy d/empty a b c)

(funcall (lazy-test-a (lazy-test 0 1)))
