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
       (ring-like/commutative-additive-monoid? obj)
       (ring-like/multiplicative-monoid? obj)
       (ring-like/distributive? obj)
       (ring-like/annihilation-by-0 obj)))
