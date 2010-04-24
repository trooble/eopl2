; Add if expressions
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda) 
       (and (not (eqv? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      ((eqv? (car exp) 'if)
       (or (occurs-free? var (cadr exp))
	   (occurs-free? var (caddr exp))
	   (occurs-free? var (cadddr exp))))
      (else (or (occurs-free? var  (car exp))
                (occurs-free? var (cadr exp)))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (eqv? (caadr exp) var)
                (occurs-free? var (caddr exp)))))
      ((eqv? (car exp) 'if)
       (or (occurs-free? var (cadr exp))
	   (occurs-free? var (caddr exp))
	   (occurs-free? var (cadddr exp))))
      (else (or (occurs-bound? var  (car exp))
                (occurs-bound? var (cadr exp)))))))
