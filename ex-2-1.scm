; Implementation for bigint numbers
; rep(n) = () if n = 0
;         | (cons r rep(q)) if n = qN + r, 0 <= r < N

(define base 16)

(define b-zero '())

(define b-zero? null?)

(define b-succ
  (lambda (n)
    (if (b-zero? n)
	'(1)
	(let ((b-succ-lsd (+ (car n) 1)))
	  (if (= b-succ-lsd base)
	      (cons 0 (b-succ (cdr n)))
	      (cons b-succ-lsd (cdr n)))))))

;;; x > 0
;;; x y z -> (x - 1) y z
;;; 0 y z -> base pred (y z)
(define b-pred
  (lambda (n)
    (cond
     ((null? n) #f)
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  #f
	  (cons (- base 1) (b-pred (cdr n)))))
     (else (cons (- (car n) 1) (cdr n))))))

; some numbers
(define make-number
  (lambda (n)
    (if (zero? n)
	b-zero
	(b-succ (make-number (- n 1))))))

; factorial 0 = 1
;         | n = n * factorial (n - 1)
(define plus
  (lambda (x y)
    (if (b-zero? x)
	y
	(b-succ (plus (b-pred x) y)))))

(define mult-h
  (lambda (x y)
    (if (b-zero? (b-pred x))
	y
	(plus y (mult-h (b-pred x) y)))))

;; (define mult
;;   (lambda (x y)
;;     (if (or (zero? x) (zero? y))
;; 	zero
;; 	(

(define (do-succ start n)
  (let ((x (b-succ start)))
    (display x)
    (newline)
    (if (> n 0)
	(do-succ x (- n 1)))))