;;; 1
;;; <binary-search-tree> ::= ()
;;;                      ::= (<number> <binary-search-tree> <binary-search-tree>)
;;;
;;; If first < n, left path, if first > n, right path, if first = n found,
;;; if first is null, return what we have.

;;; This only works if the number is contained in the tree,
;;; i.e. if the number does not exist, it returns all the
;;; steps of the unsuccessful search.
(define path-iter
  (lambda (n bst lst)
    (if (null? bst)
	lst
	(let ((first (car bst))
	      (rest (cdr bst)))
	  (if (= first n)
	      (reverse lst)
	      (if (< n first)
		  (path-iter n (car rest) (cons 'left lst))
		  (path-iter n (cadr rest) (cons 'right lst))))))))

(define path
  (lambda (n bst)
    (path-iter n bst '())))

;;; 2
;;; <lon> ::= () | (<num> . <lon>)

;; Insert n after the first element in lon (a sorted list of numbers)
;; less than or equal to n
(define insert
  (lambda (n lon)
    (if (null? lon)
	(list n)
	(if (<= n (car lon))
	    (cons n lon)
	    (cons (car lon) (insert n (cdr lon)))))))

(define sort
  (lambda (lon)
    (if (null? lon)
	'()
	(insert (car lon) (sort (cdr lon))))))

;; (sort2 pred lon)
;; Can we go to the end of the list and work backwards?
;; No, that leads back to the problem of trying to insert something
;; in the right place.
;; So, do the same thing as for the first sort - recursively sorting
;; the cdr guarantees that we first sort a list of length 1, then
;; length 2 and so on, so each successive application works on a sorted
;; list.

(define insert2
  (lambda (n pred lon)
    (if (null? lon)
	(list n)
	(if (not (pred n (car lon)))
	    (cons (car lon) (insert2 n pred (cdr lon)))
	    (cons n lon)))))

(define sort2
  (lambda (pred lon)
    (if (null? lon)
	'()
	(insert2 (car lon) pred (sort2 pred (cdr lon))))))
