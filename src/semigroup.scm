;;;; Semigroup

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-semigroup set operation)
  (let ((magma (make-magma set operation)))
    (semigroup-check magma)))

(define (semigroup-check magma)
  (if (magma/associative-operation? magma)
      magma
      (error "Operation not associative:" operation)))

(define (is-semigroup? obj)
  (and (is-magma? obj)
       (magma/associative-operation? obj)))

(define (semigroup/cart-pdt semigroup1 semigroup2)
  (let ((magma-pdt (magma/cart-pdt semigroup1 semigroup2)))
    (semigroup-check magma-pdt)))

(define (semigroup/underlying-set semigroup)
  (magma/underlying-set semigroup))
(define (semigroup/operation semigroup)
  (magma/operation semigroup))
(define (semigroup->set semigroup)
  (magma->set semigroup))

(define (add-all-new-pairs new-value elts remaining-elt-pairs)
  (let ((new-value-before (map (lambda (x) (list new-value x)) elts))
	(new-value-after (map (lambda (x) (list x new-value)) elts)))
    (append (append new-value-before new-value-after) remaining-elt-pairs)))

(define (all-pairs elts)
  (cond ((= (length elts) 0) '())
	(else
	 (let* ((elt (car elts))
		(rest (cdr elts))
		(elt-before (map (lambda (x) (list elt x)) elts))
		(elt-after (map (lambda (x) (list x elt)) elts)))
	   (append (append elt-before elt-after) (all-pairs rest))))))

(define (semigroup-from-generators generators operation)
  (define (helper elts remaining-elt-pairs)
    (let* ((new-pair (car remaining-elt-pairs))
	   (new-value (apply operation new-pair)))
      (if (member new-value elts)
	  ;; proceed with remaining-elt-pairs
	  (if (= (length (cdr remaining-elt-pairs)) 0)
	      ;; no more to iterate through
	      elts
	      (helper elts (cdr remaining-elt-pairs)))
	  ;; add new element
	  (helper (cons new-value elts)
		  (add-all-new-pairs new-value
				     elts
				     remaining-elt-pairs)))))
  (let ((all-elts (helper generators
			  (all-pairs generators))))
    (pp (list "debug: " all-elts))
    (make-semigroup (apply make-set all-elts) operation)))


;;; ############################################################################
;;; Properties

(define (semigroup/identity semigroup)
  (magma/identity semigroup))
(define (semigroup/inverses-alist semigroup)
  (magma/inverses-alist semigroup))
(define (semigroup/invertible? semigroup)
  (magma/invertible? semigroup))
(define (semigroup/order semigroup)
  (set/cardinality (semigroup/underlying-set semigroup)))
(define (semigroup/order-alist semigroup)
  (magma/order-alist semigroup))
(define (semigroup/elements semigroup)
  (set->list (semigroup->set semigroup)))

;;; ############################################################################
;;; Tests

(test-equal (semigroup->set (make-magma (make-set 0 1)
					(lambda (x y) 0)))
	    (make-set 0 1))



