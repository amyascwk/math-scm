1. Group theory definitions

2. Primitive constructors:
- set and operation
- generators and operations

Constructor wrappers:
- cyclic group, dihedral group, symmetric group

Higher order constructors:
- Cartesian product of two groups

Queries:
- order, element, inverses, element orders, isomorphic?

Abstractions:
- matrices, root lists

(define d5 (make-dihedral 5))
;Value: d5

(group/order d5)
;Value: 10

(group/elements d5)
;Value: ((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 0) (3 1) (4 0) (4 1))

((group/operation d5) '(2 1) '(4 0))
;Value: (3 1)

(pp (group/inverses-alist d5))
(((0 0) (0 0))
 ((0 1) (0 1))
 ((1 0) (4 0))
 ((1 1) (1 1))
 ((2 0) (3 0))
 ((2 1) (2 1))
 ((3 0) (2 0))
 ((3 1) (3 1))
 ((4 0) (1 0))
 ((4 1) (4 1)))
;Unspecified return value

(pp (invert-alist (group/order-alist d5)))
((1 ((0 0)))
 (2 ((4 1) (3 1) (2 1) (1 1) (0 1)))
 (5 ((4 0) (3 0) (2 0) (1 0))))
;Unspecified return value

(define c10 (make-cyclic 10))
;Value: c10

(pp (invert-alist (group/order-alist c10)))
((1 (0))
 (2 (5))
 (5 (8 6 4 2))
 (10 (9 7 3 1)))
;Unspecified return value

(group/isomorphic? d5 c10)
;Value: #f

(define c2xc5 (group/cart-pdt (make-cyclic 2)
			      (make-cyclic 5)))
;Value: c2xc5

(pp (invert-alist (group/order-alist c2xc5)))
((1 ((0 0)))
 (2 ((1 0)))
 (5 ((0 4) (0 3) (0 2) (0 1)))
 (10 ((1 4) (1 3) (1 2) (1 1))))
;Unspecified return value

(group/isomorphic? c10 c2xc5)
"Isomorphism found!"
(0 (0 0)) (5 (1 0)) (8 (0 1)) (6 (0 2)) (4 (0 3))
(2 (0 4)) (9 (1 3)) (7 (1 4)) (3 (1 1)) (1 (1 2))
;Unspecified return value


(define mystery-group
  (group-from-generators '((3 0 1 2) (1 0 3 2)) compose-permutation))
;Value: mystery-group

(pp (invert-alist (group/order-alist mystery-group)))
((1 ((0 1 2 3)))
 (2 ((3 2 1 0) (2 3 0 1) (2 1 0 3) (1 0 3 2) (0 3 2 1)))
 (4 ((3 0 1 2) (1 2 3 0))))
;Unspecified return value

;; is this C8?
(group/isomorphic? mystery-group (make-cyclic 8))
;Value: #f

;; is this C2 x C4?
(group/isomorphic? mystery-group (group/cart-pdt (make-cyclic 2)
						 (make-cyclic 4)))
;Value: #f

;; is this C2 x D2?
(group/isomorphic? mystery-group (group/cart-pdt (make-cyclic 2)
						 (make-dihedral 2)))
;Value: #f

;; is this D4?
(group/isomorphic? mystery-group (make-dihedral 4))
"Isomorphism found!"
((0 1 2 3) (0 0))
((3 2 1 0) (0 1))
((2 3 0 1) (2 0))
((2 1 0 3) (1 1))
((1 0 3 2) (2 1))
((0 3 2 1) (3 1))
((3 0 1 2) (1 0))
((1 2 3 0) (3 0))
;Unspecified return value


(define group-a (group-from-generators
		 (list (make-matrix '((0 -1) (1 0)))) *))
;Value: group-a

(group/order group-a)
;Value: 4

(group/isomorphic? group-a (make-cyclic 4))
"Isomorphism found!"
(#(matrix 2 2 ((1 0) (0 1))) 0)
(#(matrix 2 2 ((-1 0) (0 -1))) 2)
(#(matrix 2 2 ((0 1) (-1 0))) 1)
(#(matrix 2 2 ((0 -1) (1 0))) 3)
;Unspecified return value

(* (make-matrix '((0 -1) (1 0)))
   (make-matrix '((0 1) (1 0))))
;Value 22: #(matrix 2 2 ((-1 0) (0 1)))



(* (make-matrix '((0 -1) (1 0)))
   (make-matrix '((0 1) (1 0))))
;Value 22: #(matrix 2 2 ((-1 0) (0 1)))

(define group-b (group-from-generators
		 (list (make-matrix '((0 -1) (1 0)))
		       (make-matrix '((1 0) (0 -1))))
	  	*))
;Value: group-b

(group/order group-b)
;Value: 8

(group/isomorphic? group-b (make-dihedral 4))
"Isomorphism found!"
(#(matrix 2 2 ((1 0) (0 1))) (0 0))
(#(matrix 2 2 ((1 0) (0 -1))) (1 1))
(#(matrix 2 2 ((0 1) (1 0))) (0 1))
(#(matrix 2 2 ((0 -1) (-1 0))) (2 1))
(#(matrix 2 2 ((-1 0) (0 1))) (3 1))
(#(matrix 2 2 ((-1 0) (0 -1))) (2 0))
(#(matrix 2 2 ((0 1) (-1 0))) (1 0))
(#(matrix 2 2 ((0 -1) (1 0))) (3 0))
;Unspecified return value

;; root-lists

(simplify '(0 1 1 1))
;Value 24: (2 1 1)

(+ '(1 1) '(1 0 1) '(1 0 0 1))
;Value 25: (5 1 1)

(* '(0 1) '(0 1))
;Value 27: (2)

(* '(1 1) '(1 1))
;Value 28: (3 2)

(* '(1 0 1) '(1 0 1))
;Value 30: (4 0 2)

(* '(1 1 1) '(1 1 1))
;Value 31: (6 2 2 0 0 2)

(define group-b (group-from-generators
		 (list (make-matrix '(((-1/2) (0 0 -1/2)) ((0 0 1/2) (-1/2))))
		       (make-matrix '(((1) (0)) ((0) (-1)))))
		 *))
;Value: group-b

(pp (group/elements group-b))
(#(matrix 2 2 (((-1/2) (0 0 -1/2)) ((0 0 -1/2) (1/2))))
 #(matrix 2 2 (((-1/2) (0 0 -1/2)) ((0 0 1/2) (-1/2))))
 #(matrix 2 2 (((-1/2) (0 0 1/2)) ((0 0 -1/2) (-1/2))))
 #(matrix 2 2 (((-1/2) (0 0 1/2)) ((0 0 1/2) (1/2))))
 #(matrix 2 2 (((1) (0)) ((0) (-1))))
 #(matrix 2 2 (((1) (0)) ((0) (1)))))
;Unspecified return value

(group/isomorphic? group-b (make-dihedral 3))
"Isomorphism found!"
(#(matrix 2 2 (((1) (0)) ((0) (1))))                    (0 0))
(#(matrix 2 2 (((1) (0)) ((0) (-1))))                   (0 1))
(#(matrix 2 2 (((-1/2) (0 0 1/2)) ((0 0 1/2) (1/2))))   (1 1))
(#(matrix 2 2 (((-1/2) (0 0 -1/2)) ((0 0 -1/2) (1/2)))) (2 1))
(#(matrix 2 2 (((-1/2) (0 0 1/2)) ((0 0 -1/2) (-1/2)))) (2 0))
(#(matrix 2 2 (((-1/2) (0 0 -1/2)) ((0 0 1/2) (-1/2)))) (1 0))
;Unspecified return value



;;; ############################################################################
;;; Special constructors

(define (make-cyclic n)
  (define (modn x y)
    (modulo (+ x y) n))
  (make-group (apply make-set (iota n)) modn))

(define (make-symmetric n)
  (make-group (apply make-set (permutations (iota n))) compose-permutation))

(define (make-dihedral n)
  (let ((elts (set/cart-pdt (list->set (iota n))
			    (list->set '(0 1))))
	(op (lambda (e1 e2)
	      (let ((x1 (car e1))
		    (x2 (car e2))
		    (y1 (cadr e1))
		    (y2 (cadr e2)))
		(list (modulo (+ x1
				 (* (expt -1 y1) x2))
			      n)
		      (modulo (+ y1 y2) 2))))))
    (make-group elts op)))

;;; ############################################################################

;;; Make Cartesian product
(define (group-like/cart-pdt group-like1 group-like2)
  (let* ((op1 (group-like/operation group-like1))
	 (op2 (group-like/operation group-like2))
	 (op-pdt (lambda (x1y1 x2y2)
		   (list (op1 (first x1y1) (first x2y2))
			 (op2 (second x1y1) (second x2y2))))))
    (make-group-like (set/cart-pdt (group-like->set group-like1)
				   (group-like->set group-like2))
		     op-pdt)))

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

;;;; Isomorphism


(define (permute range)
  (let ((lp 0)
	(n (length range)))
      (set! lp (lambda (so-far)
      (if (= (length so-far) n)
	  so-far
	  (let ((new (an-element-of range)))
	    (require (distinct-from new so-far))
	    (lp (cons new so-far))))))
      (lp '())))


(define (permute-mapping order-count-alist)
  (let ((orders (map cadr order-count-alist)))
    (define (helper permutation-so-far orders-remaining)
      (if (null? orders-remaining)
	  permutation-so-far
	  (helper (append permutation-so-far
			  (map (lambda (x) (+ x (length
						 permutation-so-far)))
			       (permute (iota (car
					       orders-remaining)))))
		  (cdr orders-remaining))))
    (helper '() orders)))

(define (group/isomorphic? g1 g2)
  (let* ((g1/order-alist (group/order-alist g1))
	 (g2/order-alist (group/order-alist g2))
	 (g1/count (alist-key-count g1/order-alist))
	 (g2/count (alist-key-count g2/order-alist))
	 (g1/inverse-order (invert-alist g1/order-alist))
	 (g2/inverse-order (invert-alist g2/order-alist)))
    (and (equal? g1/count g2/count)
	 (begin (init-amb)
		(let ((mapping (permute-mapping g1/count)))
		  (for-each
		   (lambda (xy)
		     (let* ((x (car xy))
			    (y (cadr xy))
			    (phi-x (get-mapped-elt x
						   g1/inverse-order
						   g2/inverse-order
						   mapping))
			    (phi-y (get-mapped-elt y
						   g1/inverse-order
						   g2/inverse-order
						   mapping)))
		       (require (equal? ((group/operation g2)
					 phi-x
					 phi-y)
					(get-mapped-elt
					 ((group/operation g1) x y)
					 g1/inverse-order
					 g2/inverse-order
					 mapping)))))
		   (all-pairs (group/elements g1)))
		  (pp "Isomorphism found!")
		  (display-mapping mapping
				   g1/inverse-order
				   g2/inverse-order))))))
