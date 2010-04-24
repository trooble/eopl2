; <expression> ::= <identifier>
;              ::= (lambda ({<identifier>}+) <expression>)
;              ::= (<expression> <expression>)
; or
; <lambda-list> ::= ()
;               ::= (<identifier> <lambda-list>)
; <expression>  ::= <identifier>
;               ::= (lambda <lambda-list> <expression>)
;               ::= (<expression> <expression>)
(define set-intersect
  (lambda (l1 l2)
    (if (null? l1)
	l2
	(if (null? l2)
	    l1
	    (if (member (car l1) l2)
		(set-intersect (cdr l1) l2)
		(cons (car l1) (set-intersect (cdr l1) l2)))))))

(define free-vars-h
  (lambda (expr lvars fvars)
    (if (null? expr)
	fvars
	(if (symbol? expr)
	    (if (not (member expr lvars))
		(set-intersect (list expr) fvars)
		fvars)
	    (if (eqv? 'lambda (car expr))
		(free-vars-h (caddr expr) (append (cadr expr) lvars) fvars)
		(set-intersect (free-vars-h (car expr) lvars fvars)
				  (free-vars-h (cadr expr) lvars fvars)))))))

(define free-vars
  (lambda (expr)
    (free-vars-h expr '() '())))

;; .... and bound-vars similarly