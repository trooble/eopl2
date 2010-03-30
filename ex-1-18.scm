;;; 1
(define compose
  (lambda (p1 p2)
    (lambda (x) (p1 (p2 x)))))

;;; 2
;; <slist>             ::= '() | (<symbol-expression> . <slist>)
;; <symbol-expression> ::= <symbol> | <slist>
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

