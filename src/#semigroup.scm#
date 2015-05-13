;;;; ############################################################################################
;;;; ############################################################################################
;;;; Semigroup definitions and syntax

;;; #############################################################################################
;;; Semigroup datatype methods

;;; Semigroup test
(define (semigroup? obj)
  (and (magma? obj)
       ;;Check if 'semigroup? property is set
       (if (has-math-property? obj 'semigroup?)
	   ;;property set, so return its value
	   (get-math-property obj 'semigroup?)
	   ;;property not set, so check its value and return it
	   (let ((result (group-like/associative-operation? obj)))
	     (set-math-property! obj 'semigroup? result)
	     result))))

;;; Throw an error if the magma argument is not a semigroup
(define (test-semigroup magma)
  (if (not (magma/associative-operation? magma))
      (error "Operation not associative with respect to underlying set in magma:" magma)))

;;; Primitive constructor
(define (make-semigroup set operation)
  (let ((magma (make-magma set operation)))
    (test-semigroup magma)
    magma))

;;; Wrappers for magma datatype methods
(define (semigroup/underlying-set semigroup)
  (magma/underlying-set semigroup))
(define (semigroup/operation semigroup)
  (magma/operation semigroup))
(define (semigroup->set semigroup)
  (magma->set semigroup))


;;; #############################################################################################
;;; Other constructors

;;; Semigroup cartesian product
(define (semigroup/cart-pdt semigroup1 semigroup2)
  (let ((magma-pdt (magma/cart-pdt semigroup1 semigroup2)))
    (test-semigroup magma-pdt)
    magma-pdt))

;;; From generators
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
	(begin
	  (helper (cons new-value elts)
		  (add-all-new-pairs new-value
				     elts
				     remaining-elt-pairs))))))
  (let ((all-elts (helper generators
			  (all-pairs generators))))
    (make-semigroup (apply make-set all-elts) operation)))

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


;;; #############################################################################################
;;; Properties

;;; Wrappers for magma properties methods
(define (semigroup/identity semigroup)
  (magma/identity semigroup))
(define (semigroup/inverses-alist semigroup)
  (magma/inverses-alist semigroup))
(define (semigroup/invertible? semigroup)
  (magma/invertible? semigroup))
(define (semigroup/order semigroup)
  (magma/order semigroup))
(define (semigroup/order-alist semigroup)
  (magma/order-alist semigroup))
(define (semigroup/elements semigroup)
  (magma/elements semigroup))


;;; #############################################################################################
;;; Tests

(test-equal (semigroup->set (make-semigroup (make-set 0 1)
					    (lambda (x y) 0)))
	    (make-set 0 1))



