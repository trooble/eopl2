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
(define sort
  (lambda (lon)
    ...))