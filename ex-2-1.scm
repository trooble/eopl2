; Implementation for bigint numbers
; rep(n) = () if n = 0
;         | (cons r rep(q)) if n = qN + r, 0 <= r < N

(define base 16)

(define zero '())

(define b-zero? null?)

(define succ
  (lambda (n)
    (if (b-zero? n)
	'(1)
	(let ((succ-lsd (+ (car n) 1)))
	  (if (= succ-lsd base)
	      (cons 0 (succ (cdr n)))
	      (cons succ-lsd (cdr n)))))))

(define pred
  (lambda (n)
    (cond
     ((null? n) #f)
     ((zero? (car n))
      (if (null? (cdr n))
	  #f
	  (if (zero? (cadr n))
	      (pred (cdr n))
	      (cons (- base 1) (pred (cdr n))))))
     (else (cons (- (car n) 1) (cdr n))))))

; some numbers
(define make-number
  (lambda (n)
    (if (zero? n)
	zero
	(succ (make-number (- n 1))))))

; factorial 0 = 1
;         | n = n * factorial (n - 1)
(define plus
  (lambda (x y)
    (if (b-zero? x)
	y
	(succ (plus (pred x) y)))))

(define mult-h
  (lambda (x y)
    (if (b-zero? (pred x))
	y
	(plus y (mult-h (pred x) y)))))

;; (define mult
;;   (lambda (x y)
;;     (if (or (zero? x) (zero? y))
;; 	zero
;; 	(
