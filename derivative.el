;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'lazy-struct "~/Research/derivative.el/lazy-struct.el")

(lazy-struct d/empty nil)
(lazy-struct d/eps nil)
(lazy-struct d/char nil value)
(lazy-struct d/rep nil lang)
(lazy-struct d/cat nil left right)
(lazy-struct d/alt nil this that)
(lazy-struct d/rec nil item)

;; (cl-defstruct d/empty)
;; (cl-defstruct d/eps)
;; (cl-defstruct d/char value)
;; (cl-defstruct d/rep lang)
;; (cl-defstruct d/cat left right)
;; (cl-defstruct d/alt this that)

(defun d/langp (obj)
  (or (d/empty-p obj)
      (d/eps-p obj)
      (d/char-p obj)
      (d/rep-p obj)
      (d/cat-p obj)
      (d/alt-p obj)
      (d/rec-p obj)))

(defun make-word-L (word)
  (if (not (equal word ""))
      (d/cat (d/char (substring word 0 1))
	     (make-word-L (substring word 1 (string-width word))))
    (d/eps)))

(defun d/infer (obj)
  (cl-typecase obj
    (d/langp obj)
    (symbolp (d/rec obj))
    (characterp (d/char (string obj)))
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
		(nullable (d/cat-right L))))
    (d/rec (nullable (d/rec-item L)))))

(defun derivative (c L)
  (cl-typecase L
    (d/empty (d/empty))
    (d/eps (d/empty))
    (d/char (if (equal c (d/char-value L))
		(d/eps)
	      (d/empty)))
    (d/rep (d/cat (derivative c (d/rep-lang L))
		  L))
    (d/alt
     (let ((l1 (d/alt-this L))
	   (l2 (d/alt-that L)))
       (d/alt (derivative c l1)
	      (derivative c l2))))
    (d/cat
     (let ((l1 (d/cat-left L))
	   (l2 (d/cat-right L)))
       (if (not (nullable l1))
	   (d/cat (derivative c l1)
		  l2)
	 (d/alt (d/cat (derivative c l1)
		       l2)
		(derivative c l2)))))
    (d/rec
     (progn
       (message "L: %s" L)
       (derivative c (d/rec-item L))))))

(defun check-word (word L)
  (if (equal word "")
      (nullable L)
    (let ((c (substring word 0 1))
	  (rest (substring word 1 (string-width word))))
      (check-word rest (derivative c L)))))

(check-word "hello" (make-word-L "hello"))

;; (setq L (make-d/cat :left L
;; 		    :right (make-d/alt :this (make-d/char :value "a")
;; 				       :that (make-d/char :value "b"))))

;; (define-lang L
;;   ;; (abs (cat (word "lambda")
;;   ;; 	    binder
;;   ;; 	    (word ".")
;;   ;; 	    expr))
;;   (id (alt (word "a")
;; 	   (word "b")
;; 	   (word "c")
;; 	   (word "x")
;; 	   (word "y")
;; 	   (word "z")))
;;   (ws (word " "))
;;   (binder (cat id (rep (cat ws id))))
;;   )

(defun c-alt (&rest args)
  (if (consp args)
      (d/alt (d/infer (car args))
	     (apply 'c-alt (cdr args)))
    (d/eps)))

(defun c-cat (&rest args)
  (if (consp args)
      (d/cat (d/infer (car args))
	     (apply 'c-cat (cdr args)))
    (d/eps)))

(defun c-wscat (&rest args)
  (apply 'c-cat (-interpose " " args)))

(defun c-* (L)
  (d/rep L))

(check-word
 "lambda x . x"
 (c-wscat "lambda" ?x  "." ?x))

(setq L
      (let* ((id (c-alt ?a ?b ?c ?d ?e ?f ?g ?x ?y ?z))
	     (ws " ")
	     (binder (c-cat id (c-* (c-cat ws id))))
	     (plus (c-wscat id "+" id))
	     (minus (c-wscat id "-" id))
	     (expr (c-alt plus minus)))
	;; (c-cat id (c-* (c-cat ws id)))
	(c-wscat "lambda" binder "." expr)
	))

(letrec ((id (c-alt ?a ?b))
	 (expr (c-alt id
		      (c-wscat (d/rec 'expr) "+" (d/rec 'expr)))))
  (check-word "a + a" expr)
  )

(setq x '(+ x 1))
(eval x '((x . 0)))

(defun unwind (var expr root-expr)
  (cl-typecase expr
    (d/empty expr)
    (d/eps expr)
    (d/char expr)
    (d/rep (d/rep (unwind var (d/rep-lang expr) root-expr)))
    (d/cat (d/cat (unwind var (d/cat-left expr) root-expr)
		  (unwind var (d/cat-right expr) root-expr)))
    (d/alt (d/alt (unwind var (d/alt-this expr) root-expr)
		  (unwind var (d/alt-that expr) root-expr)))
    (d/rec (if (equal (d/rec-item expr) var)
	       root-expr
	     expr))))

(defun unwind-n (var n expr)
  (if (zerop n)
      expr
    (unwind var (unwind-n var (- n 1) expr) expr)))

(let* ((id (c-alt ?a ?b ?c))
       (expr (c-alt id
		    (c-wscat 'expr "+" 'expr))))
  ;; (unwind-n 'var 1 var)
  (check-word "a + a + a"
	      (unwind-n 'expr 2 expr))
  ;; expr
  )



(letrec ((id (c-alt ?a ?b ?c ?d ?e ?f ?g ?x ?y ?z))
	 (plus (c-wscat expr "+" expr))
	 (minus (c-wscat expr "-" expr))
	 (expr (c-alt plus minus)))
  plus
  )

(check-word "lambda f x y . x + y" L)

(provide 'derivative)
