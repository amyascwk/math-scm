;;;; Magma

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-magma set operation)
  (let ((group-like (make-group-like set operation)))
    (if (group-like/closed-operation? group-like)
	group-like
	(error "Operation not closed with respect to set:" operation set))))

(define (magma/underlying-set magma)
  (group-like/underlying-set magma))
(define (magma/operation magma)
  (group-like/operation magma))
(define (magma->set magma)
  (group-like->set magma))

;;; ############################################################################
;;; Properties

(define (magma/associative-operation? magma)
  (group-like/associative-operation? magma))
(define (magma/identity magma)
  (group-like/identity magma))
(define (magma/inverses-alist magma)
  (group-like/inverses-alist magma))
(define (magma/invertible? magma)
  (group-like/invertible? magma))


;;; ############################################################################
;;; Tests

(test-equal (magma->set (make-magma (make-set 0 1)
				    (lambda (x y) 0)))
	    (make-set 0 1))
(test-error (make-magma (make-set 0 1)
			(lambda (x y) 2)))
