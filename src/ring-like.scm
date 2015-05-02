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

;;; Get underlying monoid from set and additive operation
(define (ring-like/additive-monoid ring-like)
  (if (ring-like? ring-like)
      (make-monoid 
       (ring-like/underlying-set ring-like)
       (ring-like/additive-operation ring-like))
      (error "Not a ring-like object:" ring-like)))

;;; Get underlying monoid from set and multiplicative operation
(define (ring-like/multiplicative-monoid ring-like)
  (if (ring-like? ring-like)
      (make-monoid 
       (ring-like/underlying-set ring-like)
       (ring-like/multiplicative-operation ring-like))
      (error "Not a ring-like object:" ring-like)))

;;; Convert to set
(define (ring-like->set ring-like)
  (ring-like/underlying-set ring-like))

;;; Get order
(define (ring-like/size ring-like)
  (set/cardinality (ring-like->set ring-like)))

;;; ############################################################################
;;; Properties

;;; Test if multiplication is left-distributive with respect to addition
(define (ring-like/left-distributive? ring-like)
  (if (not (ring-like? ring-like))
      (error "Not a ring-like object:" ring-like))
  ;;Check if left-distributivity is satisfied if not already computed
  (if (not (has-math-property? ring-like 'left-distributive?))
      (let ((set (ring-like/underlying-set ring-like))
	    (add (ring-like/additive-operation ring-like))
	    (multiply (ring-like/multiplicative-operation ring-like)))
	;;Set left-distributivity property to result
	(set-math-property!
	 ring-like 'left-distributive?
	 (for-all x set
		  (for-all y set
			   (for-all z set
				    (equal? (multiply x (add y z))
					    (add
					     (multiply x y)
					     (multiply x z)))))))))
  (get-math-property ring-like 'left-distributive?))

;;; Test if multiplication is right-distributive with respect to addition
(define (ring-like/right-distributive? ring-like)
  (if (not (ring-like? ring-like))
      (error "Not a ring-like object:" ring-like))
  ;;Check if right-distributivity is satisfied if not already computed
  (if (not (has-math-property? ring-like 'right-distributive?))
      (let ((set (ring-like/underlying-set ring-like))
	    (add (ring-like/additive-operation ring-like))
	    (multiply (ring-like/multiplicative-operation ring-like)))
	;;Set right-distributivity property to result
	(set-math-property!
	 ring-like 'right-distributive?
	 (for-all x set
		  (for-all y set
			   (for-all z set
				    (equal? (multiply (add y z) x)
					    (add
					     (multiply y x)
					     (multiply z x)))))))))
  (get-math-property ring-like 'left-distributive?))

;;; Test if multiplication is distributive with respect to addition
(define (ring-like/distributive? ring-like)
  (if (not (ring-like? ring-like))
      (error "Not a ring-like object:" ring-like))
  (if (not (has-math-property? ring-like 'distributive?))
      (set-math-property!
       ring-like 'distributive?
       (and (ring-like/left-distributive? ring-like)
	    (ring-like/right-distributive? ring-like))))
  (get-math-property ring-like 'distributive?))



