;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'lazy-struct "~/Development/derivative.el/lazy-struct.el")

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
    (symbolp obj)
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
		(nullable (d/cat-right L))))))

;; (defun memo-nullable (L memo)
;;   (let ((h (gethash L memo) 'not-in-table))
;;     (if (equal h 'not-in-table)
;; 	(puthash L (nullable L) memo)
;;       h)))

(defun derivative (c L memo)
  ;; (message "L: %s" L)
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
				 (thunk-delay (derivative c l2 memo))))))
		     ;; (d/rec
		     ;;  (progn
		     ;; 	;; (message "L: %s" L)
		     ;; 	;; (message "J: %s" (d/rec-item L))
		     ;; 	(if (equal (gethash `(,c . ,(d/rec-item L)) memo 'dne) 'dne)
		     ;; 	    (message "no")
		     ;; 	    ;; (message "no %s\n%s" (hash-table-keys memo) L)
		     ;; 	  (message "yes %s" (hash-table-keys memo))
		     ;; 	  )
		     ;; 	(d/rec (thunk-delay (derivative c (d/rec-item L) memo)))))
		     )))
	  (puthash `(,c . ,L) res memo)
	  (message "<-" (hash-table-keys memo))
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

(defun resolve (L &optional expr)
  (if (null expr)
      (resolve L L)
    (cl-typecase L
      (d/rep
       (progn
	 (resolve (d/rep-lang L) expr)
	 (d/rep-setf L expr)))
      (d/alt
       (progn
	 (resolve (d/alt-this L) expr)
	 (resolve (d/alt-that L) expr)
	 (d/alt-setf L expr)))
      (d/cat
       (progn
	 (resolve (d/cat-left L) expr)
	 (resolve (d/cat-right L) expr)
	 (d/cat-setf L expr))))
    )
  )

;; (setq L
;;       (let* ((id (c-alt ?a ?b ?c ?d ?e ?f ?g ?x ?y ?z))
;; 	     (ws " ")
;; 	     (binder (c-cat id (c-* (c-cat ws id))))
;; 	     (plus (c-wscat id "+" id))
;; 	     (minus (c-wscat id "-" id))
;; 	     (expr (c-alt plus minus)))
;; 	;; (c-cat id (c-* (c-cat ws id)))
;; 	(c-wscat "lambda" binder "." expr)
;; 	))

;; (letrec ((id (c-alt ?a ?b))
;; 	 (expr (c-alt id
;; 		      (c-wscat (d/rec 'expr) "+" (d/rec 'expr)))))
;;   (check-word "a + a" expr)
;;   )

(defun pp-grammar (L top depth)
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
		       (pp-grammar (d/cat-right L) top (- depth 1)))))) 
    )
  )

(let* ((id (d/char "a"))
       (expr (c-alt id (c-cat 'rme "+" 'rme)))
       (memo (make-hash-table :test 'equal)))
  ;; (message "%s" (aref rexpr 1))
  (resolve expr)
  ;; (unwind-n 'var 1 var)
  ;; (check-word "a" expr memo)
  (--> expr
       (derivative "a" it memo)
       (derivative "+" it memo)
       ;; (derivative "a" it memo)
       ;; (nullable it)
       ;; (remove-rec it)
       ;; (pp-grammar it it 2)
       )
  ;; (nullable expr)
  )

(provide 'derivative)
