;; See code/interps/1.scm
(define subst
  (lambda (new old slist)
    (if (null? slist)
	'()
      (cons ;\new3
       (let ((se (car slist)))
	 (if (symbol? se)
	     (if (eqv? se old) new se)
	     (subst new old se)))
       (subst new old (cdr slist))))))          

