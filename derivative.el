;;; -*- lexical-binding: t; -*-

(require 'dash)

(cl-defstruct d/empty)
(cl-defstruct d/eps)
(cl-defstruct d/char value)
(cl-defstruct d/rep lang)
(cl-defstruct d/cat left right)
(cl-defstruct d/alt this that)

(defun d/langp (obj)
  (or (d/empty-p obj)
      (d/eps-p obj)
      (d/char-p obj)
      (d/rep-p obj)
      (d/cat-p obj)
      (d/alt-p obj)))

(defun make-word-L (word)
  (if (not (equal word ""))
      (make-d/cat :left (make-d/char :value (substring word 0 1))
		  :right (make-word-L (substring word 1 (string-width word))))
    (make-d/eps)))

(defun d/infer (obj)
  (cl-typecase obj
    (d/langp obj)
    (characterp (make-d/char :value (string obj)))
    (stringp (make-word-L obj))))

(defun nullable (L)
  (cl-typecase L
    (d/empty 'nil)
    (d/eps 't)
    (d/char 'nil)
    (d/rep 't)
    (d/alt (or (nullable (d/alt-this L))
	       (nullable (d/alt-that L))))
    (d/cat (and (nullable (d/cat-left L))
		(nullable (d/cat-right L))))))

(defun derivative (c L)
  (cl-typecase L
    (d/empty (make-d/empty))
    (d/eps (make-d/empty))
    (d/char (if (equal c (d/char-value L))
		(make-d/eps)
	      (make-d/empty)))
    (d/rep (make-d/cat :left (derivative c (d/rep-lang L))
		       :right L))
    (d/alt
     (let ((l1 (d/alt-this L))
	   (l2 (d/alt-that L)))
       (make-d/alt :this (derivative c l1)
		   :that (derivative c l2))))
    (d/cat
     (let ((l1 (d/cat-left L))
	   (l2 (d/cat-right L)))
       (if (not (nullable l1))
	   (make-d/cat :left (derivative c l1)
		       :right l2)
	 (make-d/alt :this (make-d/cat :left (derivative c l1)
				       :right l2)
		     :that (derivative c l2)))))))



(defun check-word (word L)
  (if (equal word "")
      (nullable L)
    (let ((c (substring word 0 1))
	  (rest (substring word 1 (string-width word))))
      (check-word rest (derivative c L)))))

(check-word "hello" (make-word-L "hello"))

(setq L (make-d/cat :left L
		    :right (make-d/alt :this (make-d/char :value "a")
				       :that (make-d/char :value "b"))))

(define-lang L
  ;; (abs (cat (word "lambda")
  ;; 	    binder
  ;; 	    (word ".")
  ;; 	    expr))
  (id (alt (word "a")
	   (word "b")
	   (word "c")
	   (word "x")
	   (word "y")
	   (word "z")))
  (ws (word " "))
  (binder (cat id (rep (cat ws id))))
  )

(defun c-alt (&rest args)
  (if (consp args)
      (make-d/alt :this (d/infer (car args))
		  :that (apply 'c-alt (cdr args)))
    (make-d/eps)))

(defun c-cat (&rest args)
  (if (consp args)
      (make-d/cat :left (d/infer (car args))
		  :right (apply 'c-cat (cdr args)))
    (make-d/eps)))

(defun c-wscat (&rest args)
  (apply 'c-cat (-interpose " " args)))

(defun c-* (L)
  (make-d/rep :lang L))

(check-word
 "lambda x . x"
 (c-wscat "lambda" ?x  "." ?x))

(c-alt ?a ?b ?c)

(c-alt (make-d/char :value "a"))

(apply 'c-alt (list (make-d/char :value "a")
		    (make-d/char :value "b")
		    (make-d/char :value "c")))


(setq L
      (let* ((id (c-alt ?a ?b ?c ?d ?e ?f ?g ?x ?y ?z))
	     (ws " ")
	     (binder (c-cat id (c-* (c-cat ws id))))
	     (plus (c-wscat id "+" id))
	     (minus (c-wscat id "-" id))
	     (expr (c-alt plus minus))
	     )
	;; (c-cat id (c-* (c-cat ws id)))
	(c-wscat "lambda" binder "." expr)
	))

(letrec ((id (c-alt ?a ?b ?c ?d ?e ?f ?g ?x ?y ?z))
	 (plus (c-wscat expr "+" expr))
	 (minus (c-wscat expr "-" expr))
	 (expr (c-alt plus minus)))
  plus
  )


(check-word "lambda f x y . x + y" L)

lambda x y . x + x

(--> (make-word-L "hell")
     (derivative "h" it)
     (derivative "e" it)
     (derivative "l" it)
     (derivative "l" it)
     ;; (derivative "o" it)
     )

(provide 'derivative)
