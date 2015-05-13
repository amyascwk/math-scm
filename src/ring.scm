;;;; Ring

;;; ##########################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-ring set add multiply)
  (let ((ring-like (make-ring-like set add multiply)))
    (if (is-ring? ring-like)
	ring-like
	(error "Not a ring"))))

(define (is-ring? obj)
  (and (ring-like? obj)
       ;; check if ring? property is set
       (if (has-math-property? obj 'ring?)
	   ;; property set, return its value
	   (get-math-property obj 'ring?)
	   ;; property not set, check its value and return it
	   (let ((result
		  (and (ring-like/abelian-additive-monoid? obj)
		       (ring-like/multiplicative-monoid? obj)
		       (ring-like/distributive? obj))))
	     (set-math-property! 
	      obj 'ring? result)
	     result))))

(define (ring/underlying-set ring)
  (ring-like/underlying-set ring))

(define (ring/additive-operation ring)
  (ring-like/additive-operation ring))

(define (ring/multiplicative-operation ring)
  (ring-like/multiplicative-operation ring))

(define (ring->set ring)
  (ring-like/underlying-set ring))


;;; ############################################################################
;;; Properties

(define (ring/additive-identity ring)
  (ring-like/additive-identity ring))

(define (ring/multiplicative-identity ring)
  (ring-like/multiplicative-identity ring))

(define (ring/size ring)
  (ring-like/size ring))

(define (ring/is-commutative? ring)
  (ring-like/commutative-multiplicative-monoid? ring))

(define (ring/is-zero-ring? ring)
  (equal? (ring/additive-identity ring)
	  (ring/multiplicative-identity ring)))

(define (ring/get-additive-inverse ring element)
  (ring-like/get-additive-inverse ring element))

(define (ring/get-multiplicative-inverse ring element)
  (ring-like/get-multiplicative-inverse ring element))

;;; Returns set of units in the ring
(define (ring/get-units ring)
  ;; check if 'units property has been set
  (if (has-math-property? ring 'units)
      ;; property set, return its value
      (get-math-property ring 'units)
      ;; property not set, check its value
      (let ((set (ring/underlying-set ring))
	    (multiply (ring/multiplicative-operation ring))
	    (one (ring/multiplicative-identity ring)))
	(let ((result
	       (set/splice set
			   (lambda (x)
			     (there-exists y set
					   (equal? (multiply x y)
						   one))))))
	  (set-math-property! ring 'units result)
	  result))))

;;; Returns set of non-zero elements in the ring
(define (ring/get-nonzero-elements ring)
  ;; check if 'nonzero-elements property has been set
  (if (has-math-property? ring 'nonzero-elements)
      ;; property set, return its value
      (get-math-property ring 'nonzero-elements)
      ;; property not set, check its value
      (let ((set (ring/underlying-set ring))
	    (zero (ring/additive-identity ring)))
	(let ((result
	       (set/splice set
			   (lambda (x)
			     (not (equal? x zero))))))
	  (set-math-property! ring 'nonzero-elements result)
	  result))))

;;; Tests if an element is a unit
(define (ring/is-unit? ring element)
  (let ((set (ring/underlying-set ring))
	(multiply (ring/multiplicative-operation ring))
	(one (ring/multiplicative-identity ring)))
    (there-exists y set
		  (equal? (multiply element y)
			  one))))

;;; Returns set of idempotents in the ring
(define (ring/get-idempotents ring)
  ;; check if property has been set
  (if (has-math-property? ring 'idempotents)
      ;; property set, return its value
      (get-math-property ring 'idempotents)
      ;; property not set, check its value
      (let ((set (ring/underlying-set ring))
	    (multiply (ring/multiplicative-operation ring)))
	(let ((result
	       (set/splice set
			   (lambda (x)
			     (equal? (multiply x x) x)))))
	  (set-math-property ring 'idempotents result)
	  result))))

;;; Returns set of left zero divisors in the ring
(define (ring/get-left-zero-divisors ring)
  ;; check if property has been set
  (if (has-math-property? ring 'left-zero-divisors)
      ;; property set, return its value
      (get-math-property ring 'left-zero-divisors)
      ;; property not set, check its value
      (let ((set (ring/underlying-set ring))
	    (multiply (ring/multiplicative-operation ring))
	    (zero (ring/additive-identity ring)))
	(let ((result
	       (set/splice set 
			   (lambda (x)
			     (there-exists y set
					   (and (equal? (multiply x y)
							zero)
						(not (equal? y zero))))))))
	  (set-math-property ring 'left-zero-divisors result)
	  result))))

;;; Returns set of right zero divisors in the ring
(define (ring/get-right-zero-divisors ring)
  ;; check if property has been set
  (if (has-math-property? ring 'right-zero-divisors)
      ;; property set, return its value
      (get-math-property ring 'right-zero-divisors)
      ;; property not set, check its value
      (let ((set (ring/underlying-set ring))
	    (multiply (ring/multiplicative-operation ring))
	    (zero (ring/additive-identity ring)))
	(let ((result
	       (set/splice set 
			   (lambda (x)
			     (there-exists y set
					   (and (equal? (multiply y x)
							zero)
						(not (equal? y zero))))))))
	  (set-math-property ring 'right-zero-divisors result)
	  result))))	
		   
;;; ##########################################################################
;;; Tests
(test-true (ring/is-zero-ring? (make-ring (make-set 0) + *)))

(let ((ring-z4 (make-ring (make-set 0 1 2 3)
			  (lambda (x y)
			    (modulo (+ x y) 4))
			  (lambda (x y)
			    (modulo (* x y) 4)))))
  (test-equal (ring/multiplicative-identity ring-z4) 1)
  (test-equal (ring/additive-identity ring-z4) 0)
  (test-equal (make-set 1 3)
	      (ring/get-units ring-z4))
  (test-equal (ring/get-additive-inverse ring-z4 1) 3)
  (test-equal (ring/get-multiplicative-inverse ring-z4 1) 1)
  (test-error (ring/get-multiplicative-inverse ring-z4 0))
  (test-error (ring/get-multiplicative-inverse ring-z4 2)))


