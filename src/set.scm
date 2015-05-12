;;;; ############################################################################################
;;;; ############################################################################################
;;;; Set definitions and syntax

;;; #############################################################################################
;;; Set construction, conversion and datatype methods

;;; Set test
(define (set? obj)
  (and (math-object? obj)
       (eq? (math-object-structure obj) 'set)
       #t))

;;; =============================================================================================
;;; Explicit sets (which are always finite)

(load-option 'wt-tree)

;;; Define a wt-tree type for explicit sets
(define explicit-set-wt-tree-type (make-wt-tree-type expr<?))

;;; Primitive constructor of explicit finite sets
;;; Note: Set elements cannot be boolean (or at least cannot be #f), since #f is sometimes used
;;;       to indicate failure to return an element
(define (make-set . element-list)
  (list->set element-list))

;;; Convert from list of elements
(define (list->set element-list)
  (let* ((inclusion-alist
	  (map (lambda (x) (cons x #t))
	       element-list))
	 (element-wt-tree
	  (alist->wt-tree explicit-set-wt-tree-type
			  inclusion-alist)))
    (wt-tree->set element-wt-tree)))

;;; Convert from weight-balanced tree of elements
(define (wt-tree->set element-wt-tree)
  (make-math-object 'set
		    (list (list 'element-wt-tree element-wt-tree))
		    (list (list 'explicit-set? #t)
			  (list 'finite? #t))))

;;; Checks whether object is a set that stores its elements explicitly, in a weighted
;;; binary tree. Returns #f if unknown.
(define (set/explicit? obj)
  (and (set? obj)
       ;;Check if 'explicit-set? property is set
       (if (has-math-property? obj 'explicit-set?)
	   ;;property set, so return its value
	   (get-math-property obj 'explicit-set?)
	   ;;property not set, so check its value and return it
	   (and (has-math-datum? obj 'element-wt-tree)
		(set-math-property! obj 'explicit-set? #t)
		#t))))

;;; Convert to weight-balanced tree
(define (set->wt-tree set)
  (if (set/explicit? set)
      (get-math-datum set 'element-wt-tree)
      (error "Not an explicit set:" set)))

;;; Convert to list
(define (set->list set)
  (wt-tree/fold (lambda (key datum list) (cons key list))
		'() (set->wt-tree set)))

;;; =============================================================================================
;;; Implicit sets (for implementing infinite sets)

;;; !!! TEMPORARY IMPLEMENTATION !!!
;;; Can only test for set membership. Stores the expression for a procedure testing for
;;; membership of the set. The procedure is not stored directly because sets should also be
;;; elements of other sets, and a consistent comparator for procedures cannot be implemented.

;;; Currently awaiting implementation of a proper logic system in order to write set containment
;;; tests for infinite sets, since it is impossible to recurse through the elements of an
;;; infinite set. Also the logic system reflects the way infinite sets are used in real life,
;;; by deducing from statements about the sets rather than exhaustively checking every element.

;;; Convenience procedure for writing other methods, do NOT use as a constructor!
;;; Eg: The predicate set? does not define a set
(define (set-from-membership-predicate membership-predicate-expression)
  (make-math-object 'set
		    (list (list 'membership-predicate-expression
				membership-predicate-expression))
		    (list (list 'explicit-set? #f))))

;;; Checks whether object is a set that stores information about its elements implicitly,
;;; as a membership predicate expression. Returns #f if unknown.
(define (set/implicit? obj)
  (and (set? obj)
       ;;Check if 'explicit-set? property is set
       (if (has-math-property? obj 'explicit-set?)
	   ;;property set, so return its value
	   (not (get-math-property obj 'explicit-set?))
	   ;;property not set, so check its value and return it
	   (and (has-math-datum? obj 'membership-predicate-expression)
		(set-math-property! obj 'explicit-set? #f)
		#t))))

;;; Membership predicate of an implicit set
(define (set/membership-predicate set)
  (if (set/implicit? set)
      (eval (get-math-datum set 'membership-predicate-expression)
	    (nearest-repl/environment))
      (error "Not an implicit set:" set)))

;;; Some standard examples:

;;; The set of integers
(define set/integers
  (make-math-object 'set
		    (list (list 'membership-predicate-name 'integer?))
		    (list (list 'explicit-set? #f)
			  (list 'finite? #f))))

;;; The set of real numbers
(define set/reals
  (make-math-object 'set
		    (list (list 'membership-predicate-name 'number?))
		    (list (list 'explicit-set? #f)
			  (list 'finite? #f))))


;;; #############################################################################################
;;; Set properties

;;; Set membership
(define set/member? (make-generic-operator 2 'set/member?))

(defhandler set/member?
  (lambda (obj set)
    (wt-tree/member? obj (set->wt-tree set)))
  any? set/explicit?)

(defhandler set/member?
  (lambda (obj set)
    ((set/membership-predicate set) obj))
  any? set/implicit?)

;;; Checks whether an object is a finite set. Returns #f if unknown.
(define (set/finite? obj)
  (and (set? obj)
       ;;Check if 'finite? property is set
       (if (has-math-property? obj 'finite?)
	   ;;property set, so return its value
	   (get-math-property obj 'finite?)
	   ;;property not set, so check its value and return it
	   (and (set/explicit? obj)
		(set-math-property! obj 'finite? #t)
		#t))))

;;; Checks whether an object is an infinite set. Returns #f if unknown.
(define (set/infinite? obj)
  (and (set? obj)
       (has-math-property? obj 'finite?)
       (not (get-math-property obj 'finite?))))

;;; Get cardinality
(define set/cardinality (make-generic-operator 1 'set/cardinality))

(defhandler set/cardinality
  (lambda (set)
    ;;Check if 'cardinality property is set
    (if (has-math-property? set 'cardinality)
	;;property set, so return its value
	(get-math-property? set 'cardinality)
	;;property not set, so check its value and return it
	(let ((cardinality (wt-tree/size (set->wt-tree set))))
	  (set-math-property! set 'cardinality cardinality)
	  cardinality)))
  set/explicit?)

;;; Check if set is empty
(define (set/empty? set)
  (= (set/cardinality set) 0))


;;; ############################################################################
;;; Quantifiers
;;; Currently implemented only for explicit sets

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

;;; Non-strict subset test
(define set/subset? (make-generic-operator 2 'set/subset?))

(defhandler set/subset?
  (lambda (small-set large-set)
    (for-all x small-set
	     (set/member x large-set)))
  set/explicit? set?)

(defhandler set/subset?
  (lambda (small-set large-set)
    (wt-tree/subset? (set->wt-tree small-set)
		     (set->wt-tree large-set)))
  set/explicit? set/explicit?)

;;; Strict subset test
(define (set/proper-subset? small-set large-set)
  (and (set/subset? small-set large-set)
       (not (set/subset? large-set small-set))))

;;; Axiom of Extensionality
;;; or definition of set equality
;;; This should coincide with expr=? for explicit sets
(define (set/equal? set1 set2)
  (and (set/subset? set1 set2)
       (set/subset? set2 set1)))

;;; Add set equality to generic operator expr<? used in set membership predicate
;;; This allows sets to contain other sets
(defhandler expr<?
  (lambda (set1 set2)
    (expr<? (set->list set1)
	    (set->list set2)))
  set/explicit? set/explicit?)


;;; ############################################################################
;;; Set construction

;;; Axiom of Separation
;;; or set construction by splicing larger set
(define set/splice (make-generic-operator 2 'set/splice))

(defhandler set/splice
  (lambda (large-set predicate)
    (set-from-membership-predicate
     `(lambda (obj)
	(and (set/member? obj ,large-set)
	     (,predicate obj)))))
  set? procedure?)

(defhandler set/splice
  (lambda (large-set predicate)
    (list->set (filter predicate
		       (set->list large-set))))
  set/explicit? procedure?)

;;; Axiom of Existence of Empty Set
(define (set/empty) (make-set))

;;; Axiom of Existence of Union Set
;;; or definition of set union
(define set/union (make-generic-operator 2 'set/union))

(defhandler set/union
  (lambda (set1 set2)
    (set-from-membership-predicate
     `(lambda (obj)
	(or (set/member? obj ,set1)
	    (set/member? obj ,set2)))))
  set? set?)

(defhandler set/union
  (lambda (set1 set2)
    (let ((tree1 (set->wt-tree set1))
	  (tree2 (set->wt-tree set2)))
      (wt-tree->set (wt-tree/union tree1 tree2))))
  set/explicit? set/explicit?)

;;; Set intersection
(define set/intersection (make-generic-operator 2 'set/intersection))

(defhandler set/intersection
  (lambda (set1 set2)
    (set-from-membership-predicate
     `(lambda (obj)
	(and (set/member? obj ,set1)
	     (set/member? obj ,set2)))))
  set? set?)

(defhandler set/intersection
  (lambda (set1 set2)
    (set/splice set1
		(lambda (obj)
		  (set/member? obj set2))))
  set/explicit? set?)

(defhandler set/intersection
  (lambda (set1 set2)
    (set/splice set2
		(lambda (obj)
		  (set/member? obj set1))))
  set? set/explicit?)

(defhandler set/intersection
  (lambda (set1 set2)
    (wt-tree->set (wt-tree/intersection (set->wt-tree set1)
					(set->wt-tree set2))))
  set/explicit? set/explicit?)

;;; Set difference
(define set/difference (make-generic-operator 2 'set/difference))

(defhandler set/difference
  (lambda (set1 set2)
    (set-from-membership-predicate
     `(lambda (obj)
	(and (set/member? obj ,set1)
	     (not (set/member? obj ,set2))))))
  set? set?)

(defhandler set/difference
  (lambda (set1 set2)
    (wt-tree->set (wt-tree/difference (set->wt-tree set1)
				      (set->wt-tree set2))))
  set/explicit? set/explicit?)

;;; Axiom of Existence of Power Set
;;; or definition of power set
(define set/power (make-generic-operator 1 'set/power))

(defhandler set/power
  (lambda (set)
    (set-from-membership-predicate
     `(lambda (obj)
	(set/subset? obj ,set))))
  set?)

(defhandler set/power
  (lambda (set)
    (list->set
     (map list->set
	  (let add-loop ((collected '(()))
			 (remaining (set->list set)))
	    ;;Loop over elements to generate subsets with and without each element
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
  set/explicit?)

;;; Set Cartesian Product
(define set/cart-pdt (make-generic-operator 2 'set/cartesian-product))

(defhandler set/cart-pdt
  (lambda (set1 set2)
    (set-from-membership-predicate
     `(lambda (obj)
	(and (list? obj)
	     (set/member? (car obj) ,set1)
	     (set/member? (cadr obj) ,set2)
	     (null? (cddr obj))))))
  set? set?)

(defhandler set/cart-pdt
  (lambda (set1 set2)
    (list->set (apply append
		      (map (lambda (x)
			     (map (lambda (y) (list x y))
				  (set->list set2)))
			   (set->list set1)))))
  set/explicit? set/explicit?)


;;; #############################################################################################
;;; Tests

;;; set?
(test-true (set? (make-set)))
(test-true (set? (make-set 0 1 2 'a 'b '() '(3 4))))

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

;;; set/member?
(test-false (set/member? 'a (set/empty)))
(test-equal (set/member? 'a (make-set 'a 'b 'c))
	    #t)
(test-equal (set/member? 'a (make-set 0 1 2 'a 'b 'c))
	    #t)
(test-false (set/member? 'd (make-set 'a 'b 'c)))

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

