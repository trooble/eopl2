;; ; TODO: FIND A WAY WITHOUT CALL/CC
;; ; un-lexical-address (expression)
;; ; return expression with lexical addresses (: d p) and (v free)
;; ; replaced by corresponding definitions. Return #f if no such expression
;; ; can be formed.
;; ;
;; ; BNF
;; ; <lex-var> ::= (: <digit> <digit>) | (<identifier> free)
;; ; <lex-expr> ::= <lex-var>
;; ;            ::= (if <lex-expr> <lex-expr> <lex-expr>)
;; ;            ::= (lambda ({<identifier>}*) <lex-expr>)
;; ;            ::= ({<lex-expr>}+)
(define un-lexical-address
  (lambda (e)
    (un-lexical-address-1 e '())))

(define lexical->symbol
  (lambda (e env)
    (if (null? env)
	#f
	(let ((depth (cadr e)) (pos (caddr e)))
	  (cond ((< (length env) (+ depth 1)) #f)
		((< (length (list-ref env depth)) pos) #f)
		(else (let ((sym (list-ref (list-ref env depth) pos)))
			(if (bound-nearer? sym (- depth 1) env)
			    #f
			    sym))))))))

(define bound-nearer?
  (lambda (sym distance env)
    (if (or (< distance 0) (null? env))
	#f
	(if (member sym (car env))
	    #t
	    (bound-nearer? sym (- distance 1) (cdr env))))))

(define un-lexical-address-1
  (lambda (expr env)
    (if (null? expr)
	'()
	(cond
	 ((eqv? (car expr) 'if)
	  (cons 'if (un-lexical-address-1 (cdr expr) env)))
	 ((eqv? (car expr) 'lambda)
	  (cons 'lambda
		(cons (cadr expr) (un-lexical-address-1 (cddr expr) (cons (cadr expr) env)))))
	 ((eqv? (car expr) ':)
	  (lexical->symbol expr env))
	 ((symbol? (car expr))
	  (if (eqv? (cadr expr) 'free)
	      (car expr)
	      #f))
	 (else
	  (cons (un-lexical-address-1 (car expr) env)
		(un-lexical-address-1 (cdr expr) env)))))))

;; (define un-lexical-address-h
;;   (lambda (expr env res)
;;     (if (null? expr)
;; 	(res)
;; 	(cond
;; 	 ((eqv? (car expr) 'if)
;; 	  (un-lexical-address-h (cdr exp) env (cons 'if res)))
;; 	 ((eqv? (car expr) 'lambda)
;; 	  (un-lexical-address-h
;; 	   (cddr exp)
;; 	   (cons (cadr expr) env)
;; 	   (cons 'lambda (cons (cadr expr) res))))
;; 	 ((eqv? (car expr) ':)
;; 	  (let ((sym (lexical->symbol expr)))
;; 	    (if (sym)
;; 		(un-lexical-address-h '() env 
