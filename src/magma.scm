;;;; ############################################################################################
;;;; ############################################################################################
;;;; Magma definitions and syntax

;;; #############################################################################################
;;; Magma datatype methods

;;; Magma test
(define (magma? obj)
  (and (group-like? obj)
       ;;Check if 'magma? property is set
       (if (has-math-property? obj 'magma?)
	   ;;property set, so return its value
	   (get-math-property obj 'magma?)
	   ;;property not set, so check its value and return it
	   (let ((result (group-like/closed-operation? obj)))
	     (set-math-property! obj 'magma? result)
	     result))))

;;; Throw an error if group-like argument is not a magma
(define (test-magma group-like)
  (if (not (group-like/closed-operation? group-like))
      (error "Operation not closed with respect to underlying set in group-like object:"
	     group-like)))

;;; Primitive constructor
(define (make-magma set operation)
  (let ((group-like (make-group-like set operation)))
    (test-magma group-like)
    group-like))

;;; Wrappers for group-like datatype methods
(define (magma/underlying-set magma)
  (group-like/underlying-set magma))
(define (magma/operation magma)
  (group-like/operation magma))
(define (magma->set magma)
  (group-like->set magma))


;;; #############################################################################################
;;; Other constructors

;;; Magma cartesian product
(define (magma/cart-pdt magma1 magma2)
  (let ((group-like-pdt (group-like/cart-pdt magma1 magma2)))
    (test-magma group-like-pdt)
    group-like-pdt))


;;; #############################################################################################
;;; Properties

;;; Wrappers for group-like properties methods
(define (magma/associative-operation? magma)
  (group-like/associative-operation? magma))
(define (magma/identity magma)
  (group-like/identity magma))
(define (magma/inverses-alist magma)
  (group-like/inverses-alist magma))
(define (magma/invertible? magma)
  (group-like/invertible? magma))
(define (magma/order-alist magma)
  (group-like/order-alist magma))
(define (magma/order magma)
  (group-like/order magma))
(define (magma/elements magma)
  (group-like/elements magma))
(define (magma/abelian? magma)
  (group-like/abelian? magma))


;;; #############################################################################################
;;; Tests

(test-equal (magma->set (make-magma (make-set 0 1)
				    (lambda (x y) 0)))
	    (make-set 0 1))
(test-error (make-magma (make-set 0 1)
			(lambda (x y) 2)))


