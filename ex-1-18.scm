;;; 1
(define compose
  (lambda (p1 p2)
    (lambda (x) (p1 (p2 x)))))

;;; 2
;; <slist>             ::= '() | (<symbol-expression> . <slist>)
;; <symbol-expression> ::= <symbol> | <slist>
(define car&cdr-h
  (lambda (s slist errvalue rem ret)
    (if (null? slist)
	(if (null? rem)
	    errvalue
	    (compose ret cdr))
;	    (car&cdr-h s rem errvalue rem (compose ret cdr)))
	(if (symbol? (car slist))
	    (if (eqv? s (car slist))
		(if (null? ret)
		    car
		    (compose ret car))
		(car&cdr-h s (car slist) errvalue (cdr slist)
			   (compose ret (car&cdr s (cdr slist) errvalue))))))))

(define car&cdr
  (lambda (s slist errvalue)
    (car&cdr-h s slist errvalue slist '())))

(define car&cdr
  (lambda (s slist errvalue)
    (if (null? slist)
	errvalue
	(let ((first (car slist))
	      (rest (cdr slist)))
	  (if (symbol? first)
	      (if (eqv? first s)
		  car
		  (compose (car&cdr s rest errvalue) cdr))
	      (list compose (car&cdr s first errvalue) (car&cdr s rest errvalue)))))))

> ((car&cdr 'a '((a b) c ((d e))) 'err) '((x y) z ((q r))))

Error: attempt to call a non-procedure
       ('(#{Procedure 8515 compose} #{Procedure 654 car} #{Procedure 8516 (unnamed in compose)}) '((x y) z ((q r))))

