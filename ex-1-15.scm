;; Should check for < 0
(define (duple n x)
  (if (zero? n) '()
      (cons x (duple (- n 1) x))))

;; <2list>  ::= ()
;;          ::= (<tuple> . <2list>)
;; <tuple>  ::= (<datum> <datum>)
(define invert
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (reverse (car lst)) (invert (cdr lst))))))

(define filter-in
  (lambda (pred lst)
    (if (null? lst)
	'()
	(if (pred (car lst))
	    (cons (car lst) (filter-in pred (cdr lst)))
	    (filter-in pred (cdr lst))))))

(define every?
  (lambda (pred lst)
    (if (null? lst)
	#t
	(if (pred (car lst))
	    (every? pred (cdr lst))
	    #f))))

(define exists?
  (lambda (pred lst)
    (if (null? lst)
	#f
	(if (pred (car lst))
	    #t
	    (exists? pred (cdr lst))))))

(define vector-index-partial
  (lambda (pred v n max)
    (if (> n max)
	#f
	(if (pred (vector-ref v n))
	    n
	    (vector-index-partial pred v (+ n 1) max)))))

(define vector-index
  (lambda (pred v)
    (vector-index-partial pred v 0 (vector-length v))))

(define vector-index
  (lambda (pred v)
    (letrec ((vector-index-partial
	      (lambda (n max)
		(if (> n max)
		    	#f
			(if (pred (vector-ref v n))
			    n
			    (vector-index-partial (+ n 1) max))))))
      (vector-index-partial 0 (vector-length v)))))

(define list-set
  (lambda (lst n x)
    (letrec ((list-set-place
	      (lambda (lst place)
		(if (null? lst)
		    '()
		    (if (= place n)
			(cons x (cdr lst))
			(cons (car lst) (list-set-place (cdr lst) (+ place 1))))))))
      (list-set-place lst 0))))

(define product
  (lambda (los1 los2)
    (if (null? los1)
	'()
	(append (scalar-product (car los1) los2) (product (cdr los1) los2)))))

(define scalar-product
  (lambda (s los)
    (if (null? los)
	'()
	(cons (list s (car los)) (scalar-product s (cdr los))))))

(define my-reverse
  (lambda (lst)
    (letrec ((putpair
	      (lambda (inlst outlst)
		(if (null? inlst)
		    outlst
		    (putpair (cdr inlst) (cons (car inlst) outlst))))))
      (putpair lst '()))))

(define my-append-1
  (lambda (lst1 lst2)
    (if (null? lst1)
	lst2
	(if (null? lst2)
	    lst1
	    (cons (car lst1) (my-append-1 (cdr lst1) lst2))))))

(define my-append
  (lambda (lst1 lst2)
    (my-append-1 (reverse lst1) lst2)))

(define down
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (list (car lst)) (down (cdr lst))))))

;; Is there a less imperative way of doing this?
(define vector-append-list
  (lambda (v lst)
    (let ((v2 (make-vector (+ (vector-length v) (length lst)))))
      (letrec ((copy-vector
		(lambda (v1 v2 start end)
		  (if (< start end)
		      (begin
			(vector-set! v2 start (vector-ref v1 start))
			(copy-vector v1 v2 (+ start 1) end))))))
	(copy-vector v v2 0 (vector-length v)))
      (letrec ((vector-append-n
		(lambda (lst n)
		  (if (null? lst)
		      v2
		      (begin
			(vector-set! v2 n (car lst))
			(vector-append-n (cdr lst) (+ n 1)))))))
	(vector-append-n lst (vector-length v))))))