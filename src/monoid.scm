;;;; Monoid

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-monoid set operation)
  (let ((semigroup (make-semigroup set operation)))
    (if (semigroup/identity semigroup)
	semigroup
	(error "Identity of operation not in set:" operation set))))

(define (monoid-from-generators generators operation)
  (let ((semigroup (semigroup-from-generators generators operation)))
    (if (semigroup/identity semigroup)
	semigroup
	(error "Identity of operation not in set:" operation set))))

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


;;; ############################################################################
;;; Tests

(test-equal (monoid->set (make-monoid (make-set 0 1) *))
	    (make-set 0 1))




