;; See code/interps/1.scm
;; Rewrite subst using map and Kleene star
; <s-list>              ::= ({<symbol-expression>}*)
; <symbol-expression>   ::= (<symbol> | <s-list>)
;; Map should take a function takes a symbol expression as argument
;; and a list of symbol expressions
(define subst
  (lambda (new old slist)
    (map
     (lambda (se)
       (if (symbol? se)
	   (if (eqv? se old) new se)
	   (subst new old se)))
     slist)))

(define subst
  (lambda (new old slist)
    (if (null? slist)
      '()
      (cons ;\new3
        (subst-in-symbol-expression new old (car slist)) 
        (subst new old (cdr slist))))))          

(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se) ;\new3
      (if (eqv? se old) new se)
      (subst new old se))))

