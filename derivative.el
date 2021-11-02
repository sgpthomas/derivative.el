;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'lazy-struct "~/Development/derivative.el/lazy-struct.el")
(require 's)

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
  (if (equal 1 (string-width word))
      (d/char word)
    (apply 'c-cat (--map (d/char (string it)) word))))

(defun d/infer (obj)
  (cl-typecase obj
    (d/langp obj)
    (symbolp obj)
    (characterp (d/char (string obj)))
    (stringp (make-word-L obj))))

(cl-defun nullable (L &optional (prev-input (make-hash-table :test 'equal)))
  (cond ((not (equal (gethash L prev-input 'no) 'no))
	 'nil)
	(t (progn
	     (puthash L (or (d/eps-p L) (d/rep-p L)) prev-input)
	     (cl-typecase L
	       (d/empty 'nil)
	       (d/eps 't)
	       (d/char 'nil)
	       (d/rep 't)
	       (d/alt (or (nullable (d/alt-this L) prev-input)
			  (nullable (d/alt-that L) prev-input)))
	       (d/cat (and (nullable (d/cat-left L) prev-input)
			   (nullable (d/cat-right L) prev-input))))))))

(defun derivative (c L memo)
  (let ((prev (gethash `(,c . ,L) memo 'dne)))
    (if (equal prev 'dne)
	(let ((res (cl-typecase L
		     (d/empty (d/empty))
		     (d/eps (d/empty))
		     (d/char (if (equal c (d/char-value L))
				 (d/eps)
			       (d/empty)))
		     (d/rep (d/cat (thunk-delay (derivative c (d/rep-lang L) memo))
				   (thunk-delay L)))
		     (d/alt
		      (let ((l1 (d/alt-this L))
			    (l2 (d/alt-that L)))
			(d/alt (thunk-delay (derivative c l1 memo))
			       (thunk-delay (derivative c l2 memo)))))
		     (d/cat
		      (let ((l1 (d/cat-left L))
			    (l2 (d/cat-right L)))
			(if (not (nullable l1))
			    (d/cat (thunk-delay (derivative c l1 memo))
				   (thunk-delay l2))
			  (d/alt (thunk-delay (d/cat (thunk-delay (derivative c l1 memo))
						     (thunk-delay l2)))
				 (thunk-delay (derivative c l2 memo)))))))))
	  (puthash `(,c . ,L) res memo)
	  res)
      prev)))

(defun check-word (word L memo)
  (if (equal word "")
      (nullable L)
    (let ((c (substring word 0 1))
	  (rest (substring word 1 (string-width word))))
      (check-word rest (derivative c L memo) memo))))

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
  (cond
   ((equal 2 (length args))
    (d/alt (d/infer (car args))
	   (d/infer (cadr args))))
   ((consp args)
    (d/alt (d/infer (car args))
	   (apply 'c-alt (cdr args))))
   (t (d/eps))))

(defun c-cat (&rest args)
  (cond
   ((equal 2 (length args))
    (d/cat (d/infer (car args))
	   (d/infer (cadr args))))
   ((consp args)
    (d/cat (d/infer (car args))
	   (apply 'c-cat (cdr args))))
   (t (d/eps))))

(defun c-wscat (&rest args)
  (apply 'c-cat (-interpose " " args)))

(defun c-* (L)
  (d/rep L))

(cl-defun resolve (L &key ((:expr expr) L) ((:sym sym) 'rme))
  (cl-typecase L
      (d/rep
       (progn
	 (resolve (d/rep-lang L) :expr expr :sym sym)
	 (d/rep-setf L expr sym)))
      (d/alt
       (progn
	 (resolve (d/alt-this L) :expr expr :sym sym)
	 (resolve (d/alt-that L) :expr expr :sym sym)
	 (d/alt-setf L expr sym)))
      (d/cat
       (progn
	 (resolve (d/cat-left L) :expr expr :sym sym)
	 (resolve (d/cat-right L) :expr expr :sym sym)
	 (d/cat-setf L expr sym)))))

(cl-defun pp-grammar (L &optional (top L) (depth 10))
  (if (zerop depth)
      "..."
    (cl-typecase L
      (d/empty "emp")
      (d/eps "eps")
      (d/char (format "%s" (d/char-value L)))
      (d/rep (format "(%s)*"
		     (pp-grammar (d/rep-lang L) top (- depth 1))))
      (d/alt (format "(%s | %s)"
		     (if (equal (d/alt-this L) top)
			 "**"
		       (pp-grammar (d/alt-this L) top (- depth 1)))
		     (if (equal (d/alt-that L) top)
			 "**"
		       (pp-grammar (d/alt-that L) top (- depth 1)))))
      (d/cat (format "(%s . %s)"
		     (if (equal (d/cat-left L) top)
			 "**"
		       (pp-grammar (d/cat-left L) top (- depth 1)))
		     (if (equal (d/cat-right L) top)
			 "**"
		       (pp-grammar (d/cat-right L) top (- depth 1))))))))

(let* ((id (c-alt "a" "b" "c" "d"))
       (ops (c-alt "+" "*" "/" "-"))
       (math-expr (c-alt id (c-wscat 'math-expr ops 'math-expr)))
       (bind (c-wscat "lambda" id "." 'expr))
       (expr (c-alt bind math-expr))
       (memo (make-hash-table :test 'equal)))

  (resolve math-expr :expr math-expr :sym 'math-expr)
  (resolve bind :expr expr :sym 'expr)
  (check-word "lambda a . lambda b . a / b" bind memo))

(provide 'derivative)
