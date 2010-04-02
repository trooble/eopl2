;;; 1
(define compose
  (lambda (p1 p2)
    (lambda (x) (p1 (p2 x)))))

;;; 2
;; <slist>             ::= '() | (<symbol-expression> . <slist>)
;; <symbol-expression> ::= <symbol> | <slist>

;; Done!
;; What was needed was an iterative function that returned either a
;; success or error value. Since the success value is a function, the
;; error value had to be wrapped as a constant function.

(define symbol-or-compose
  (lambda (e exp)
    (if (null? exp)
	e
	(compose e exp))))

(define car&cdr-h
  (lambda (s slist success-val failure-val)
    (if (null? slist)
	failure-val
	(if (symbol? (car slist))
	    (if (eqv? s (car slist))
		(symbol-or-compose car success-val)
		(car&cdr-h
		 s (cdr slist) (symbol-or-compose cdr success-val) failure-val))
	    (let ((res
		   (car&cdr-h s (car slist) (symbol-or-compose car success-val)
			      failure-val)))
	      (if (not (eqv? res failure-val))
		  res
		  (car&cdr-h s (cdr slist) (symbol-or-compose cdr success-val)
			     failure-val)))))))

(define car&cdr
  (lambda (s slist errvalue)
    (let ((res (car&cdr-h s slist '() errvalue)))
      (if (procedure? res)
	  res
	  (lambda (x) errvalue)))))

;;; 3
;;; Like car&cdr-h but without using compose. Will probably have to return
;;; lots of nested lambdas.

(define symbol-or-lambda
  (lambda (e exp)
    (if (null? exp)
	e
	(lambda (x) (e (exp x))))))

(define car&cdr-h2
  (lambda (s slist success-val failure-val)
    (if (null? slist)
	failure-val
	(if (symbol? (car slist))
	    (if (eqv? s (car slist))
		(symbol-or-lambda car success-val)
		(car&cdr-h2
		 s (cdr slist) (symbol-or-lambda cdr success-val) failure-val))
	    (let ((res
		   (car&cdr-h2 s (car slist) (symbol-or-lambda car success-val)
			       failure-val)))
	      (if (not (eqv? res failure-val))
		  res
		  (car&cdr-h2 s (cdr slist) (symbol-or-lambda cdr success-val)
			      failure-val)))))))

(define car&cdr2
  (lambda (s slist errvalue)
    (let ((res (car&cdr-h2 s slist '() errvalue)))
      (if (procedure? res)
	  res
	  (lambda (x) errvalue)))))
