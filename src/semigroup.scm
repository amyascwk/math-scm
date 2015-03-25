;;;; Semigroup

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-semigroup set operation)
  (let ((magma (make-magma set operation)))
    (if (magma/associative-operation? magma)
	magma
	(error "Operation not associative:" operation))))

(define (semigroup/underlying-set semigroup)
  (magma/underlying-set semigroup))
(define (semigroup/operation semigroup)
  (magma/operation semigroup))
(define (semigroup->set semigroup)
  (magma->set semigroup))


;;; ############################################################################
;;; Properties

(define (semigroup/identity semigroup)
  (magma/identity semigroup))
(define (semigroup/inverses-alist semigroup)
  (magma/inverses-alist semigroup))
(define (semigroup/invertible? semigroup)
  (magma/invertible? semigroup))


;;; ############################################################################
;;; Tests

(test-equal (semigroup->set (make-magma (make-set 0 1)
					(lambda (x y) 0)))
	    (make-set 0 1))



