;;;; Monoid

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-monoid set operation)
  (let ((semigroup (make-semigroup set operation)))
    (monoid-check semigroup)))

(define (monoid-from-generators generators operation)
  (let ((semigroup (semigroup-from-generators generators operation)))
    (monoid-check semigroup)))

(define (monoid/cart-pdt monoid1 monoid2)
  (let ((semigroup-pdt (semigroup/cart-pdt monoid1 monoid2)))
    (monoid-check semigroup-pdt)))

(define (monoid-check semigroup)
  (if (semigroup/identity semigroup)
      semigroup
      (error "Identity of operation not in set:" operation set)))



(define (monoid/underlying-set monoid)
  (semigroup/underlying-set monoid))
(define (monoid/operation monoid)
  (semigroup/operation monoid))
(define (monoid->set monoid)
  (semigroup->set monoid))


;;; ############################################################################
;;; Properties

(define (monoid/identity monoid)
  (semigroup/identity monoid))
(define (monoid/inverses-alist monoid)
  (semigroup/inverses-alist monoid))
(define (monoid/invertible? monoid)
  (semigroup/invertible? monoid))
(define (monoid/order monoid)
  (set/cardinality (monoid/underlying-set monoid)))
(define (monoid/order-alist monoid)
  (semigroup/order-alist monoid))
(define (monoid/elements monoid)
  (set->list (monoid->set monoid)))


;;; ############################################################################
;;; Tests

(test-equal (monoid->set (make-monoid (make-set 0 1) *))
	    (make-set 0 1))




