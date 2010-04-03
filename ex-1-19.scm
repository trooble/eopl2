; Lambda expression
; <expression> ::== <identifier>
;              ::== (lambda (<identifier>) <expression>)
;              ::== (<expression> <expression>)

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
  (lambda (expr lvars res)
    (if (null? expr)
	res
	(if (symbol? expr)
	    (if (not (member expr lvars))
		(set-intersect (list expr) res)
		res)
	    (if (eqv? 'lambda (car expr))
		(free-vars-h (caddr expr) (cons (caadr expr) lvars) res)
		(set-intersect (free-vars-h (car expr) lvars res)
				  (free-vars-h (cadr expr) lvars res)))))))

(define free-vars
  (lambda (expr)
    (free-vars-h expr '() '())))
