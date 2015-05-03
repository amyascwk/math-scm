;;;; Utilities

;;; ############################################################################
;;; Comparators

;;; Specific comparators =======================================================

;;; Pair comparison
(define (pair<? p1 p2)
  (or (expr<? (car p1) (car p2))
      (and (expr=? (car p1) (car p2))
	   (expr<? (cdr p1) (cdr p2)))))

;;; Type order comparator
(define (type<? obj1 obj2)
  (let loop ((type-list type-priority-list))
    (if (null? type-list)
	(error "Unknown type comparison:" obj1 obj2)
	(let ((predicate (car type-list)))
	  (cond ((predicate obj1)
		 (if (predicate obj2)
		     #f
		     #t))
		((predicate obj2) #f)
		(else (loop (cdr type-list))))))))

;;; Ascending priority order of types
(define type-priority-list
  (list number? symbol? char? string? null? pair?))

;;; Generic type comparator ====================================================

;;; Generic less than operator
(define expr<?
  (make-generic-operator 2 'expr<? type<?))

(defhandler expr<? <
  number? number?)

(defhandler expr<? symbol<?
  symbol? symbol?)

(defhandler expr<? char<?
  char? char?)

(defhandler expr<? string<?
  string? string?)

(defhandler expr<? pair<?
  pair? pair?)

(defhandler expr<? (lambda (x y)
	(pair<? (vector->list x) (vector->list y)))
  vector? vector?)

;;; Generic equality operator
(define expr=?
  (make-generic-operator 2 'expr=? equal?))


;;; Tests ======================================================================
(test-true (expr<? 1 2))
(test-false (expr<? 1 1))
(test-false (expr<? 2 1))
(test-true (expr<? 1 'a))
(test-false (expr<? 'a 1))
(test-true (expr<? 'a 'b))
(test-false (expr<? 'a 'a))
(test-false (expr<? 'b 'a))
(test-true (expr<? 'a #\a))
(test-false (expr<? #\a 'a))
(test-true (expr<? #\a #\b))
(test-false (expr<? #\a #\a))
(test-false (expr<? #\b #\a))
(test-true (expr<? #\a "a"))
(test-false (expr<? "a" #\a))
(test-true (expr<? "a" "b"))
(test-false (expr<? "a" "a"))
(test-false (expr<? "b" "a"))
(test-true (expr<? "a" '()))
(test-false (expr<? '() "a"))
(test-false (expr<? '() '()))
(test-true (expr<? '() '(1 . 2)))
(test-false (expr<? '(1 . 2) '()))
(test-true (expr<? '(1 . 2) '(1 2)))
(test-false (expr<? '(1 . 2) '(1 . 2)))
(test-false (expr<? '(1 2) '(1 . 2)))
(test-true (expr<? '(1 2) '(1 2 3)))

(test-true (expr=? 1 1))
(test-true (expr=? 'a 'a))
(test-true (expr=? #\a #\a))
(test-true (expr=? "a" "a"))
(test-true (expr=? '() '()))
(test-true (expr=? '(1 2) '(1 2)))


;;; ############################################################################
;;; Binary search

;;; Binary search on list
;;; using generic strict comparator by which list is already sorted
(define (binary-search target lst obj<? obj=?)
  (if (null? lst)
      #f
      (let ((last (- (length lst) 1)))
	(if (obj<? target (list-ref lst 0))
	    #f ;out of list
	    (if (obj<? (list-ref lst last) target)
		#f ;out of list
		(cond ((obj=? target (list-ref lst 0)) lst) ;first entry
		      ((obj=? target (list-ref lst last)) (last-pair lst)) ;last entry
		      ;;commence binary search
		      (else
		       (let loop ((left 0) (right last))
			 (let* ((center (quotient (+ right left) 2))
				(center-entry (list-ref lst center)))
			   (cond ((eqv? center left) #f)
				 ((obj=? target center-entry) (list-tail lst center))
				 ((obj<? target center-entry) (loop left center))
				 ((obj<? center-entry target) (loop center right))))))))))))

;;;; Permutation functions

(define (permutation? p)
  (and (list? p)
       (every integer? p)
       (equal? (merge-sort p (lambda (x y) (< x y)))
	       (iota (length p)))))

(define (set-index! l idx val)
  (set-car! (list-tail l idx) val)
  l)

(define (permutations items)
  (define (remove ls elem)
    (cond ((null? ls) '())
	  ((equal? (car ls) elem) (remove (cdr ls) elem))
	  (else (cons (car ls) (remove (cdr ls) elem)))))
    (if (null? items) '(())
	(apply append
	       (map (lambda (element)
		      (map (lambda (permutation)
			     (cons element permutation))
			   (permutations (remove items element))))
		    items))))

(define (compose-permutation p1 p2)
  (assert (and (permutation? p1)
	       (permutation? p2)
	       (= (length p1) (length p2))))
  (let ((result (make-list (length p1) '*)))
    (for-each
     (lambda (i)
       (set-index! result
		  (list-index
		   (lambda (x)(= x (list-index
				    (lambda (x)(= x i))
				    p1)))
		   p2)
		  i))
     (iota (length p1)))
    result))

;;;; Useful function for non-deterministic search

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (distinct-from el rest)
  (not (member el rest)))

(define (permute range)
  (let ((lp 0)
	(n (length range)))
      (set! lp (lambda (so-far)
      (if (= (length so-far) n)
	  so-far
	  (let ((new (an-element-of range)))
	    (require (distinct-from new so-far))
	    (lp (cons new so-far))))))
      (lp '())))



;;; Tests
(test-false (binary-search 'a '() expr<? expr=?))
(test-false (binary-search 0 '(1 2 3 4 5 6 8 9) < =))
(test-false (binary-search 10 '(1 2 3 4 5 6 8 9) < =))
(test-equal (binary-search 1 '(1 2 3 4 5 6 8 9) < =)
	    '(1 2 3 4 5 6 8 9))
(test-equal (binary-search 9 '(1 2 3 4 5 6 8 9) < =)
	    '(9))
(test-equal (binary-search 3 '(1 2 3 4 5 6 8 9) < =)
	    '(3 4 5 6 8 9))
(test-false (binary-search 7 '(1 2 3 4 5 6 8 9) < =))
(test-equal (binary-search 8 '(1 2 3 4 5 6 8 9) < =)
	    '(8 9))
(test-equal (binary-search 8 '(2 4 6 8 a c e) expr<? expr=?)
	    '(8 a c e))
(test-equal (binary-search 'a '(2 4 6 8 a c e) expr<? expr=?)
	    '(a c e))
(test-false (binary-search 'b '(2 4 6 8 a c e) expr<? expr=?))
