(lexical-address '(lambda (a b c)
		    (if (eqv? b c)
			((lambda (c)
			   (cons (a c)))
			 a)
			b)))
;; =>
;; (lambda (a b c)
;;   (if (eqv> free) (b : 0 1) (c : 0 2))
;;   ((lambda (c)
;;      ((cons free) (a : 1 0) (c : 0 0)))
;;    (a: 0 0))
;;   (b : 0 1))

(define lexical-address-h
  (lambda (e depth env)
    (cond ((null? e) '())
	  ((symbol? e) (symbol->lexical e depth depth env))
	  ((eqv? (car e) 'if)
	   (cons 'if (lexical-address-h (cdr e) depth env)))
	  ((eqv? (car e) 'lambda)
	   (list 'lambda (cons (cadr e)
			       (lexical-address-h (cddr e) (+ depth 1)
						  (cons (cadr e) env)))))
	  (else
	   (cons (lexical-address-h (car e) depth env)
		 (lexical-address-h (cdr e) depth env))))))

(define lexical-address
  (lambda (e)
    (lexical-address-h e -1 '())))

(define symbol->lexical
  (lambda (sym depth here env)
    (cond ((null? env) (list sym 'free))
	  ((member sym (car env))
	   (list sym ': (- depth here) (list-index sym (car env))))
	  (else (symbol->lexical sym depth (- here 1) (cdr env))))))

(define list-index
  (lambda (sym lst)
    (list-index-h sym lst 0)))

(define list-index-h
  (lambda (sym lst pos)
    (cond ((null? lst) -1)
	  ((eqv? sym (car lst)) pos)
	  (else (list-index-h sym (cdr lst) (+ pos 1))))))