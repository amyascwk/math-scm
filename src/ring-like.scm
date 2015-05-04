;;;; Definitions and syntax of structures built from a set, an
;;;; additive operation and a multiplicative operation.

;;; ###########################################################################
;;; Datatype methods

;;; Primitive constructor
;; operations must be binary operations between elements of the set
(define (make-ring-like set additive-operation multiplicative-operation)
  (if (set? set)
      (make-math-object
       (list (list 'underlying-set set)
	     (list 'additive-operation additive-operation)
	     (list 'multiplicative-operation multiplicative-operation)))
      (error "Not a set:" set)))

;;; Test ring-like
(define (ring-like? obj)
  (and (math-object? obj)
       (has-math-property? obj 'underlying-set)
       (has-math-property? obj 'additive-operation)
       (has-math-property? obj 'multiplicative-operation)))

;;; Get underlying set
(define (ring-like/underlying-set ring-like)
  (if (ring-like? ring-like)
      (get-math-property ring-like 'underlying-set)
      (error "Not a ring-like object:" ring-like)))

;;; Get additive operation
(define (ring-like/additive-operation ring-like)
  (if (ring-like? ring-like)
      (get-math-property ring-like 'additive-operation)
      (error "Not a ring-like object:" ring-like)))

;;; Get multiplicative operation
(define (ring-like/multiplicative-operation ring-like)
  (if (ring-like? ring-like)
      (get-math-property ring-like 'multiplicative-operation)
      (error "Not a ring-like object:" ring-like)))

;;; Convert to set
(define (ring-like->set ring-like)
  (ring-like/underlying-set ring-like))

;;; Get order
(define (ring-like/size ring-like)
  (set/cardinality (ring-like->set ring-like)))

;;; Get underlying monoid from set and additive operation
(define (ring-like/additive-monoid ring-like)
  (make-monoid 
   (ring-like/underlying-set ring-like)
   (ring-like/additive-operation ring-like)))

;;; Get underlying monoid from set and multiplicative operation
(define (ring-like/multiplicative-monoid ring-like)
  (make-monoid 
   (ring-like/underlying-set ring-like)
   (ring-like/multiplicative-operation ring-like)))
   
;;; Get additive identity
(define (ring-like/additive-identity ring-like)
  (monoid/identity (ring-like/additive-monoid ring-like)))

;;; Get multiplicative identity
(define (ring-like/multiplicative-identity ring-like)
  (monoid/identity (ring-like/multiplicative-monoid ring-like)))

;;; ############################################################################
;;; Properties

;;; Test if underlying set and additive operation form monoid
(define (ring-like/additive-monoid? ring-like)
  (is-monoid? (ring-like/additive-monoid ring-like)))

;;; Test if underlying set and multiplicative operation form monoid
(define (ring-like/multiplicative-monoid? ring-like)
  (is-monoid? (ring-like/multiplicative-monoid ring-like)))

;;; Test if underlying additive monoid is commutative
(define (ring-like/commutative-additive-monoid? ring-like)
  (group-like/commutative-operation? (ring-like/additive-monoid ring-like)))

;;; Test if underlying additive monoid is an abelian group
(define (ring-like/abelian-additive-monoid? ring-like)
  (is-abelian-group? (ring-like/additive-monoid ring-like)))

;;; Test if underlying multiplicative monoid is commutative
(define (ring-like/commutative-multiplicative-monoid? ring-like)
  (group-like/commutative-operation? (ring-like/multiplicative-monoid ring-like)))

;;; Test if multiplication is distributive with respect to addition
(define (ring-like/distributive? ring-like)
  ;;Check if distributivity is satisfied if not already computed
  (if (not (has-math-property? ring-like 'distributive?))
      (let ((set (ring-like/underlying-set ring-like))
	    (add (ring-like/additive-operation ring-like))
	    (multiply (ring-like/multiplicative-operation ring-like)))
	;;Set distributivity property to result
	(set-math-property!
	 ring-like 'distributive?
	 (for-all x set
		  (for-all y set
			   (for-all z set
				    (and (equal? (multiply x (add y z))
						 (add
						  (multiply x y)
						  (multiply x z)))
					 (equal? (multiply (add y z) x)
						 (add
						  (multiply y x)
						  (multiply z x))))))))))
  (get-math-property ring-like 'distributive?))

;;; Test if multiplication by 0 annihilates the ring-like
(define (ring-like/annihilation-by-0 ring-like)
  (let ((set (ring-like/underlying-set ring-like))
	(multiply (ring-like/multiplicative-operation ring-like))
	(zero (ring-like/additive-identity ring-like)))
    (for-all x set
	     (and (equal? (multiply zero x)
			  (multiply x zero))
		  (equal? (multiply x zero)
			  zero)))))

;;; Returns additive inverse of element
(define (ring-like/get-additive-inverse ring-like element)
  (let ((set (ring-like/underlying-set ring-like))
	(add (ring-like/additive-operation ring-like))
	(zero (ring-like/additive-identity ring-like)))
    (let ((y (there-exists x set
			   (and (equal? (add element x) zero)
				(equal? (add x element) zero)))))
      (if (not y)
	  (error "Element has no additive inverse:" element)
	  y))))

;;; Returns multiplicative inverse of element
(define (ring-like/get-multiplicative-inverse ring-like element)
  (let ((set (ring-like/underlying-set ring-like))
	(multiply (ring-like/multiplicative-operation ring-like))
	(one (ring-like/multiplicative-identity ring-like)))
    (let ((y (there-exists x set
			   (and (equal? (multiply element x) one)
				(equal? (multiply x element) one)))))
      (if (not y)
	  (error "Element has no multiplicative inverse:" element)
	  y))))



