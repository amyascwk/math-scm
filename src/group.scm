;;;; Group

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-group set operation)
  (let ((monoid (make-monoid set operation)))
    (if (monoid/invertible? monoid)
	monoid
	(error "Not all inverses with respect to operation is in set:" operation set))))

(define (group-from-generators generators operation)
  (let ((monoid (monoid-from-generators generators operation)))
    (if (monoid/invertible? monoid)
	monoid
	(error "Identity of operation not in set:" operation set))))

(define (group/underlying-set group)
  (monoid/underlying-set group))
(define (group/operation group)
  (monoid/operation group))
(define (group->set group)
  (monoid->set group))


;;; ############################################################################
;;; Properties

(define (group/identity group)
  (monoid/identity group))
(define (group/inverses-alist group)
  (monoid/inverses-alist group))
(define (group/order monoid)
  (set/cardinality (group/underlying-set group)))
(define (group/order-alist group)
  (monoid/order-alist group))

;;; ############################################################################
;;; Special constructors

(define (make-cyclic n)
  (define (modn x y)
    (modulo (+ x y) n))
  (make-group (apply make-set (iota n)) modn))

;;; ############################################################################
;;; Tests

(test-equal (group->set (make-group (make-set 1) *))
	    (make-set 1))
(test-equal (group->set (make-group (make-set 0 1 2 3 4 5 6)
				    (lambda (x y)
				      (modulo (+ x y) 7))))
	    (make-set 0 1 2 3 4 5 6))
(test-equal (group->set (make-group (make-set '(1 1) '(1 -1) '(-1 1) '(-1 -1))
				    (lambda (x y)
				      (list (* (car x) (car y))
					    (* (cadr x) (cadr y))))))
	    (make-set '(1 1) '(1 -1) '(-1 1) '(-1 -1)))

;;; Beautiful, beautiful groups!




