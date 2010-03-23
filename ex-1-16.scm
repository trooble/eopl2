;;; 1
;;; <list-of-symbols>    ::= ()
;;;                      ::= (<symbol> . <list-of-symbols>))
;;; <slist2>             ::= ()
;;;                      ::= (<symbol-expression2> . <slist2>)
;;; <symbol-expression2> ::= <symbol> | <list-of-symbols>
(define up
  (lambda (lst)    ; an slist2
    (if (null? lst)
	'()
	(if (symbol? (car lst)); (car lst) will be either a symbol or a list of symbols
	    (cons (car lst) (up (cdr lst)))
	    (up-los (car lst) (up (cdr lst)))))))

(define up-los   ; This is basically a special purpose append
  (lambda (lst tl)    ; a list-of-symbols
    (if (null? lst)
	tl
	(cons (car lst) (up-los (cdr lst) tl)))))

;;; 2
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
	'()
	(let ((se (car slist)))
	  (if (symbol? se)
	      (cons (swap s1 s2 se) (swapper s1 s2 (cdr slist)))
	      (cons (swapper s1 s2 se) (swapper s1 s2 (cdr slist))))))))

(define swap
  (lambda (s1 s2 s)
    (if (eqv? s1 s)
	s2
	(if (eqv? s2 s)
	    s1
	    s))))

;;; 3
(define count-occurences-iter
  (lambda (s slist n)
    (if (null? slist)
	n
	(let ((front (car slist))
	      (back (cdr slist)))
	  (if (symbol? front)
	      (if (eqv? s front)
		  (count-occurences-iter s back (+ n 1))
		  (count-occurences-iter s back n))
	      (count-occurences-iter s back (+ n (count-occurences-iter s front 0))))))))

(define count-occurences
  (lambda (s slist)
    (count-occurences-iter s slist 0)))

;;; 4
(define flatten
  (lambda (slist)
    (if (null? slist)
	'()
	(let ((se (car slist))
	      (rst (cdr slist)))
	  (if (symbol? se)
	      (cons se (flatten rst))
	      (if (null? (cdr se))
		  (cons (car se) (flatten rst))
		  (cons (car se) (flatten (cons (cdr se) rst)))))))))


