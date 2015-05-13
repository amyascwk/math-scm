;;;; ############################################################################################
;;;; ############################################################################################
;;;; Monoid definitions and syntax

;;; #############################################################################################
;;; Monoid datatype methods

;;; Monoid test
(define (monoid? obj)
  (and (semigroup? obj)
       ;;Check if 'monoid? property is set
       (if (has-math-property? obj 'monoid?)
	   ;;property set, so return its value
	   (get-math-property obj 'monoid?)
	   ;;property not set, so check its value and return it
	   (let ((result (and (group-like/identity obj)
			      #t)))
	     (set-math-property! group-like 'monoid? result)
	     result))))

;;; Throw an error if the semigroup argument is not a monoid
(define (test-monoid semigroup)
  (if (not (semigroup/identity semigroup))
      (error "Identity of operation not in underlying set in semigroup:" semigroup)))

;;; Primitive constructor
(define (make-monoid set operation)
  (let ((semigroup (make-semigroup set operation)))
    (test-monoid semigroup)
    semigroup))

;;; Wrappers for semigroup datatype methods
(define (monoid/underlying-set monoid)
  (semigroup/underlying-set monoid))
(define (monoid/operation monoid)
  (semigroup/operation monoid))
(define (monoid->set monoid)
  (semigroup->set monoid))


;;; #############################################################################################
;;; Other constructors

;;; Monoid cartesian product
(define (monoid/cart-pdt monoid1 monoid2)
  (let ((semigroup-pdt (semigroup/cart-pdt monoid1 monoid2)))
    (test-monoid semigroup-pdt)
    semigroup-pdt))

;;; From generators
(define (monoid-from-generators generators operation)
  (let ((semigroup (semigroup-from-generators generators operation)))
    (test-monoid semigroup)
    semigroup))


;;; #############################################################################################
;;; Properties

;;; Wrappers for semigroup properties methods
(define (monoid/identity monoid)
  (semigroup/identity monoid))
(define (monoid/inverses-alist monoid)
  (semigroup/inverses-alist monoid))
(define (monoid/invertible? monoid)
  (semigroup/invertible? monoid))
(define (monoid/order monoid)
  (semigroup/order monoid))
(define (monoid/order-alist monoid)
  (semigroup/order-alist monoid))
(define (monoid/elements monoid)
  (semigroup/elements monoid))


;;; #############################################################################################
;;; Tests

(test-equal (monoid->set (make-monoid (make-set 0 1) *))
	    (make-set 0 1))




