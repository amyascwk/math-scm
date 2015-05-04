;;;; Set definitions and syntax

;;; #############################################################################################
;;; Datatype methods

;;; Primitive constructor
;;; Note: Set elements cannot be boolean! #f is used to indicate that
(define (make-set . element-list)
  (let* ((sorted-list
	  ;;Sort list to gather identical entries
	  (merge-sort element-list (lambda (obj1 obj2)
				     (expr<? obj2 obj1))))
	 (sorted-list-without-duplicates
	  ;;Remove identical entries
	  (let loop ((collected '())
		     (remaining sorted-list))
	    (if (null? remaining)
		collected
		(loop (if (and (pair? collected)
			       (expr=? (car remaining) (car collected)))
			  collected
			  (cons (car remaining) collected))
		      (cdr remaining))))))
    (make-math-object
     ;;Property alist
     (list (list
	    ;;Property name
	    'sorted-element-list
	    ;;Property value
	    sorted-list-without-duplicates)))))

;;; Set test
(define (set? obj)
  (and (math-object? obj)
       (has-math-property? obj 'sorted-element-list)
       #t))

;;; Get sorted-element-list
(define (set/sorted-element-list set)
  (if (set? set)
      (get-math-property set 'sorted-element-list)
      (error "Not a set:" set)))

;;; Building from list of elements
(define (list->set lst)
  (if (list? lst)
      (apply make-set lst)
      (error "Not a list:" lst)))

;;; Convert to list
(define set->list set/sorted-element-list)

;;; Get cardinality
(define (set/cardinality set) (length (set->list set)))

;;; ############################################################################
;;; Set construction

;;; Axiom of Separation
;;; or set construction by splicing larger set
(define (set/splice large-set predicate)
  (list->set (filter predicate
		     (set->list large-set))))

;;; Axiom of Existence of Empty Set
(define (set/empty) (make-set))

;;; Axiom of Existence of Union Set
;;; or definition of set union
(define (set/union set1 set2)
  (list->set (append (set->list set1)
		     (set->list set2))))

;;; Set intersection
(define (set/intersection set1 set2)
  (set/splice set1 (lambda (element)
		     (set/member element set2))))

;;; Axiom of Existence of Power Set
;;; or definition of power set
(define (set/power set)
  (list->set
   (map list->set
	(let add-loop ((collected '(()))
		       (remaining (set->list set)))
	  (if (null? remaining)
	      collected
	      (add-loop (let loop ((new-collected collected)
				   (new-remaining collected))
			  (if (null? new-remaining)
			      new-collected
			      (loop (cons (cons (car remaining) (car new-remaining))
					  new-collected)
				    (cdr new-remaining))))
			(cdr remaining)))))))


;;; ############################################################################
;;; Quantifiers

;;; Universal quantifier
;;; Syntax: (for-all element set . predicate-body)
(define-syntax for-all
  (er-macro-transformer
   (lambda (exp r c)
     (let ((x (cadr exp))
	   (set (caddr exp))
	   (predicate-body (cdddr exp)))
       `(,(r 'every) (,(r 'lambda) (,x)
		                   ,@predicate-body)
	             (,(r 'set->list) ,set))))))

;;; Existential quantifier
;;; Returns first element satisfying predicate body if found, #f otherwise
;;; Syntax: (there-exists element set . predicate-body)
(define-syntax there-exists
  (er-macro-transformer
   (lambda (exp r c)
     (let ((x (cadr exp))
	   (set (caddr exp))
	   (predicate-body (cdddr exp)))
       `(,(r 'find) (,(r 'lambda) (,x)
		                 ,@predicate-body)
	            (,(r 'set->list) ,set))))))


;;; ############################################################################
;;; Set relations

;;; Set membership
;;; If element is in set, returns its index in the internal list, otherwise returns #f
;;; Assumes sorted internal list, uses binary search for efficiency
;;; Question: Is there any advantage in the binary search over linear search, since
;;;           accessing elements already takes linear time? I can imagine an advantage
;;;           only if equality checking is significantly slower than accessing.
(define (set/member element set)
  (let* ((lst (set->list set))
	 (result
	  ;;binary search replaces (member element lst)
	  (binary-search element lst expr<? expr=?)))
    (and result
	 (- (length lst) (length result)))))

;;; Non-strict subset
;;; can make use of sorted element list to optimize checking
(define (set/subset? small-set large-set)
  (for-all x small-set
	   (set/member x large-set)))

;;; Strict subset
(define (set/proper-subset? small-set large-set)
  (and (set/subset? small-set large-set)
       (not (set/subset? large-set small-set))))

;;; Axiom of Extensionality
;;; or definition of set equality
(define (set/equal? set1 set2)
  (and (set/subset? set1 set2)
       (set/subset? set2 set1)))

;;; Add set equality to generic operator expr=? used in set membership predicate
;;; This allows sets to contain other sets
(defhandler expr=? set/equal?
  set? set?)


;;; ############################################################################
;;; Tests

;;; make-set
(test-equal (make-set)
	    (make-set))
(test-equal (make-set 0 1 2)
	    (make-set 2 0 1))
(test-equal (make-set 0 1 1 2 3 3)
	    (make-set 0 1 2 3))
(test-equal (make-set 1 0 'a 2 '() '(3 4))
	    (make-set 0 1 2 'a '() '(3 4)))
(test-equal (make-set 1 0 'a 'a 2 '() '() '(3 4) '(3 4) '(3 4))
	    (make-set 0 1 2 'a '() '(3 4)))

;;; set?
(test-true (set? (make-set)))
(test-true (set? (make-set 0 1 2 'a 'b '() '(3 4))))

;;; set->list
(test-equal (set->list (make-set))
	    '())
(test-equal (set->list (make-set 0 1 2 'a 'b '() '(3 4)))
	    '(0 1 2 a b () (3 4)))
(test-equal (set->list (make-set 1 0 '(3 4) 'b '() 'a 2))
	    '(0 1 2 a b () (3 4)))
(test-error (set->list '(1 2 3)))

;;; list->set
(test-equal (list->set '())
	    (make-set))
(test-equal (list->set '(0 1 2 a b () (3 4)))
	    (make-set 0 1 2 'a 'b '() '(3 4)))
(test-equal (list->set (set->list (make-set 0 1 2 'a 'b '() '(3 4))))
	    (make-set 0 1 2 'a 'b '() '(3 4)))

;;; set/splice
(test-equal (set/splice (make-set 0 1 2 'a 'b '() '(3 4)) number?)
	    (make-set 0 1 2))
(test-equal (set/splice (make-set 0 1 2 'a 'b '() '(3 4)) symbol?)
	    (make-set 'a 'b))
(test-equal (set/splice (make-set 0 1 2 'a 'b '() '(3 4)) list?)
	    (make-set '() '(3 4)))

;;; set/empty
(test-equal (set/empty) (make-set))

;;; set/union
(test-equal (set/union (make-set 0 1 2 'a 'b '() '(3 4))
		       (make-set 0 1 2 'a 'b '() '(3 4)))
	    (make-set 0 1 2 'a 'b '() '(3 4)))
(test-equal (set/union (make-set 0 1 2 'a 'b '() '(3 4))
		       (make-set 1 2 3 'b 'c '(3 4) '(5 6)))
	    (make-set 0 1 2 3 'a 'b 'c '() '(3 4) '(5 6)))

;;; set/intersection
(test-equal (set/intersection (make-set 0 1 2 'a 'b '() '(3 4))
			      (make-set 0 1 2 'a 'b '() '(3 4)))
	    (make-set 0 1 2 'a 'b '() '(3 4)))
(test-equal (set/intersection (make-set 0 1 2 'a 'b '() '(3 4))
			      (make-set 1 2 3 'b 'c '(3 4) '(5 6)))
	    (make-set 1 2 'b '(3 4)))
(test-equal (set/intersection (make-set 0 1 2 'a 'b '() '(3 4))
			      (make-set 3 4 5 'c 'd '(5 6) '(7 8)))
	    (set/empty))

;;; set/power
(test-equal (set/power (make-set 'a 'b 'c))
	    (make-set (set/empty) (make-set 'a) (make-set 'b) (make-set 'c)
		      (make-set 'a 'b) (make-set 'b 'c) (make-set 'c 'a)
		      (make-set 'a 'b 'c)))

;;; for-all
(test-true (for-all x (make-set 0 1 2)
		     (number? x)))
(test-false (for-all x (make-set 0 1 2 'a)
		     (number? x)))

;;; there-exists
(test-false (there-exists x (make-set 0 1 2)
			  (symbol? x)))
(test-equal (there-exists x (make-set 0 1 2 'a)
			  (symbol? x))
	    'a)

;;; set/member
(test-false (set/member 'a (set/empty)))
(test-equal (set/member 'a (make-set 'a 'b 'c))
	    0)
(test-equal (set/member 'a (make-set 0 1 2 'a 'b 'c))
	    3)
(test-false (set/member 'd (make-set 'a 'b 'c)))

;;; set/subset?
(test-true (set/subset? (set/empty) (make-set 0 1 2 3)))
(test-false (set/subset? (make-set 0 1 2 3) (set/empty)))
(test-true (set/subset? (set/empty) (set/empty)))
(test-true (set/subset? (make-set 0 1 2) (make-set 0 1 2)))
(test-true (set/subset? (make-set 0 1 2) (make-set 0 1 2 3)))
(test-false (set/subset? (make-set 0 1 2) (make-set 0 1)))
(test-false (set/subset? (make-set 0 1 2) (make-set 0 1 3)))
(test-true (set/subset? (make-set 0 1 2 'a) (make-set 0 1 2 3 'a)))
(test-false (set/subset? (make-set 0 1 'a) (make-set 0 1 2 3)))

;;; set/proper-subset?
(test-true (set/proper-subset? (set/empty) (make-set 0 1 2 3)))
(test-false (set/proper-subset? (make-set 0 1 2 3) (set/empty)))
(test-false (set/proper-subset? (set/empty) (set/empty)))
(test-false (set/proper-subset? (make-set 0 1 2) (make-set 0 1 2)))
(test-true (set/proper-subset? (make-set 0 1 2) (make-set 0 1 2 3)))
(test-false (set/proper-subset? (make-set 0 1 2) (make-set 0 1)))
(test-false (set/proper-subset? (make-set 0 1 2) (make-set 0 1 3)))
(test-true (set/proper-subset? (make-set 0 1 2 'a) (make-set 0 1 2 3 'a)))
(test-false (set/proper-subset? (make-set 0 1 'a) (make-set 0 1 2 3)))

;;; set/equal?
(test-false (set/equal? (set/empty) (make-set 0 1 2 3)))
(test-true (set/equal? (set/empty) (set/empty)))
(test-true (set/equal? (make-set 0 1 2) (make-set 0 1 2)))
(test-false (set/equal? (make-set 0 1 2) (make-set 0 1 2 3)))
(test-true (set/equal? (make-set 0 1 2 'a) (make-set 0 1 2 'a)))
(test-false (set/equal? (make-set 0 1 'a) (make-set 0 1 2 3)))

