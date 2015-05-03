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
       (ring-like/abelian-additive-monoid? obj)
       (ring-like/multiplicative-monoid? obj)
       (ring-like/distributive? obj)))
