;;;; Group

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-group set operation)
  (let ((monoid (make-monoid set operation)))
    (if (monoid/invertible? monoid)
	monoid
	(error "Not all inverses with respect to operation is in set:" operation set))))

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




