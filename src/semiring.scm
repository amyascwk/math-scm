;;;; Semiring

;;; ##############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-semiring set add multiply)
  (let ((ring-like (make-ring-like set add multiply)))
    (if (is-semiring? ring-like)
	ring-like
	(error "Not a semiring"))))

(define (is-semiring? obj)
  (and (ring-like? obj)
       ;; check if semiring? property is set
       (if (has-math-property? obj 'semiring?)
	   ;; property set, return its value
	   (get-math-property obj 'semiring?)
	   ;; property not set, check its value and return it
	   (let ((result
		  (and (ring-like/commutative-additive-monoid? obj)
		       (ring-like/multiplicative-monoid? obj)
		       (ring-like/distributive? obj)
		       (ring-like/annihilation-by-0 obj))))
	     (set-math-property! 
	      obj 'semiring? result)
	     result))))
