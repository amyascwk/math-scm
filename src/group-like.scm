;;;; ############################################################################################
;;;; ############################################################################################
;;;; Definitions and syntax of structures built from a set and a binary operation

;;; #############################################################################################
;;; Group-like construction and datatype methods

;;; Group-like test
(define (group-like? obj)
  (and (math-object? obj)
       (eq? (math-object-structure obj) 'group-like)
       #t))

;;; Primitive constructor
;;; operation must be binary operation between elements of the set
(define (make-group-like set operation)
  (if (set? set)
      (make-math-object 'group-like
			(list (list 'underlying-set set)
			      (list 'operation operation))
			'())
      (error "Not a set:" set)))

;;; Get underlying set
(define (group-like/underlying-set group-like)
  (if (group-like? group-like)
      (get-math-datum group-like 'underlying-set)
      (error "Not a group-like object:" group-like)))

;;; Get operation
(define (group-like/operation group-like)
  (if (group-like? group-like)
      (get-math-datum group-like 'operation)
      (error "Not a group-like object:" group-like)))

;;; Convert to set
(define (group-like->set group-like)
  (group-like/underlying-set group-like))


;;; #############################################################################################
;;; Other constructors

;;; Make Cartesian product
(define (group-like/cart-pdt group-like1 group-like2)
  (let* ((op1 (group-like/operation group-like1))
	 (op2 (group-like/operation group-like2))
	 (op-pdt (lambda (x1y1 x2y2)
		   (list (op1 (first x1y1) (first x2y2))
			 (op2 (second x1y1) (second x2y2))))))
    (make-group-like (set/cart-pdt (group-like->set group-like1)
				   (group-like->set group-like2))
		     op-pdt)))


;;; #############################################################################################
;;; Basic properties of group-like objects
;;; Currently implemented only for groups with explicit underlying sets

;;; Test closure of operation
(define (group-like/closed-operation? group-like)
  ;;Check if closure is satisfied if not already computed
  (if (has-math-property? group-like 'closed-operation?)
      ;;property set, so return its value
      (get-math-property group-like 'closed-operation?)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result
	      ;;in the future, this should be replaced with a function image check
	      (for-all x set
		       (for-all y set
				(set/member? (operation x y) set)))))
	;;Set closure property to result
	(set-math-property! group-like 'closed-operation? result)
        result)))

;;; Test associativity of operation
(define (group-like/associative-operation? group-like)
  ;;Check if associativity is satisfied if not already computed
  (if (has-math-property? group-like 'associative-operation?)
      ;;property set, so return its value
      (get-math-property group-like 'associative-operation?)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result
	      ;;in the future, this should be replaced with a function image check
	      (for-all x set
		       (for-all y set
				(for-all z set
					 (expr=? (operation (operation x y) z)
						 (operation x (operation y z))))))))
	;;Set closure property to result
	(set-math-property! group-like 'associative-operation? result)
        result)))

;;; Test commutativity of operation
(define (group-like/commutative-operation? group-like)
  ;;Check if commutativity is satisfied if not already computed
  (if (has-math-property? group-like 'commutative-operation?)
      ;;property set, so return its value
      (get-math-property group-like 'commutative-operation?)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result
	      ;;in the future, this should be replaced with a function image check
	      (for-all x set
		       (for-all y set
				(expr=? (operation x y)
					(operation y x))))))
	;;Set closure property to result
	(set-math-property! group-like 'commutative-operation? result)
        result)))

;;; Test existence of identity element
(define (group-like/identity group-like)
  ;;Check for existence of identity element if not already computed
  (if (has-math-property? group-like 'identity)
      ;;property set, so return its value
      (get-math-property group-like 'identity)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result
	      (there-exists e set
			    (for-all x set
				     (and (expr=? (operation e x) x)
					  (expr=? (operation x e) x))))))
	;;Set closure property to result
	(set-math-property! group-like 'identity result)
        result)))

;;; Get alist of inverses of elements (if they exist)
(define (group-like/inverses-alist group-like)
  ;;Check for existence of inverses alist if not already computed
  (if (has-math-property? group-like 'inverses-alist)
      ;;property set, so return its value
      (get-math-property group-like 'inverses-alist)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (identity (group-like/identity group-like))
	     (result
	      (map (lambda (x)
		     (list x
			   (there-exists y set
					 (and (equal? (operation x y) identity)
					      (equal? (operation y x) identity)))))
		   (set->list set))))
	;;Set closure property to result
	(set-math-property! group-like 'inverses-alist result)
        result)))

;;; Compute order of each element
(define (group-like/order-alist group-like)
  (if (not (group-like? group-like))
      (error "Not a group-like object:" group-like))
  (if (not (group-like/identity group-like))
      (error "Not implemented for group-like objects without identity element yet:" group-like))
  ;;Find orders if not already computed
  (if (not (has-math-property? group-like 'order-alist))
      (let ((set (group-like/underlying-set group-like))
	    (operation (group-like/operation group-like))
	    (identity (group-like/identity group-like)))
	(define (get-order elt elt-power i)
	  (if (equal? elt-power identity)
	      i
	      (get-order elt
			 (operation elt-power elt)
			 (+ i 1))))
	;;Set inverses-alist field to result
	(set-math-property!
	 group-like 'order-alist
	 (map (lambda (x) (list x (get-order x x 1)))
	      (set->list set)))))
  (get-math-property group-like 'order-alist))

;;; Get order of group
(define (group-like/order group-like)
  ;;Check for order of group-like object if not already computed
  (if (has-math-property? group-like 'order)
      ;;property set, so return its value
      (get-math-property group-like 'order)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result (set/cardinality (group-like->set group-like))))
	;;Set closure property to result
	(set-math-property! group-like 'order result)
        result)))

;;; Test invertibility
(define (group-like/invertible? group-like)
  ;;Check for invertibility of group-like object if not already computed
  (if (has-math-property? group-like 'invertible?)
      ;;property set, so return its value
      (get-math-property group-like 'invertible?)
      ;;property not set, so check its value and return it
      (let* ((set (group-like/underlying-set group-like))
	     (operation (group-like/operation group-like))
	     (result
	      (and (every cadr
			  (group-like/inverses-alist group-like))
		   #t)))
	;;Set closure property to result
	(set-math-property! group-like 'invertible? result)
        result)))

;;; Test if abelian
(define (group-like/abelian? group-like)
  (group-like/commutative-operation? group-like))

;;; Return list of elements
(define (group-like/elements group-like)
  (set->list (group-like->set group-like)))


;;; #############################################################################################
;;; Miscellaneous methods

(define (invert-alist alist)
  (let ((result (set->list
		 (list->set
		  (map (lambda (i) (list (cadr i) '()))
		       alist)))))
    (for-each (lambda (i)
		(let ((entry (assq (cadr i) result)))
		  (set-cdr! entry (list (cons (car i) (cadr entry))))))
	      alist)
    result))

(define (alist-key-count alist)
  (let ((inverted (invert-alist alist)))
    (map (lambda (i) (list (car i) (length (cadr i))))
	 inverted)))




