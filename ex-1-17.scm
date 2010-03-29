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

;; This change is to make a merge conflict