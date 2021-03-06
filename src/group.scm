;;; Copyright 2015 Amyas Chew, Lynn Chua, Yongquan Lu
;;; 
;;; This file is part of math-scm.
;;; 
;;; math-scm is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; math-scm is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with math-scm.  If not, see <http://www.gnu.org/licenses/>.

;;;; ############################################################################################
;;;; ############################################################################################
;;;; Group definitions and syntax

;;; #############################################################################################
;;; Group datatype methods

;;; Group test
(define (group? obj)
  (and (monoid? obj)
       ;;Check if 'group? property is set
       (if (has-math-property? obj 'group?)
	   ;;property set, so return its value
	   (get-math-property obj 'group?)
	   ;;property not set, so check its value and return it
	   (let ((result (group-like/invertible? obj)))
	     (set-math-property! obj 'group? result)
	     result))))

;;; Throw an error if the monoid argument is not a group
(define (test-group monoid)
  (if (not (monoid/invertible? monoid))
      (error "Identity of operation not in underlying set in monoid:" monoid)))

;;; Primitive constructor
(define (make-group set operation)
  (let ((monoid (make-monoid set operation)))
    (test-group monoid)
    monoid))

;;; Wrappers for monoid datatype methods
(define (group/underlying-set group)
  (monoid/underlying-set group))
(define (group/operation group)
  (monoid/operation group))
(define (group->set group)
  (monoid->set group))


;;; #############################################################################################
;;; Other constructors

;;; Group cartesian product
(define (group/cart-pdt group1 group2)
  (let ((monoid-pdt (monoid/cart-pdt group1 group2)))
    (test-group monoid-pdt)
    monoid-pdt))

;;; From generators
(define (group-from-generators generators operation)
  (let ((monoid (monoid-from-generators generators operation)))
    (test-group monoid)
    monoid))


;;; #############################################################################################
;;; Properties

;;; Wrappers for monoid properties methods
(define (group/identity group)
  (monoid/identity group))
(define (group/inverses-alist group)
  (monoid/inverses-alist group))
(define (group/order group)
  (monoid/order group))
(define (group/order-alist group)
  (monoid/order-alist group))
(define (group/elements group)
  (monoid/elements group))
(define (group/abelian? group)
  (monoid/abelian? group))

;;; Abelian group test
(define (abelian-group? obj)
  (and (group? obj)
       (group/abelian? obj)))


;;; #############################################################################################
;;; Special constructors

(define (make-cyclic n)
  (define (modn x y)
    (modulo (+ x y) n))
  (make-group (apply make-set (iota n)) modn))

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

(define (make-symmetric n)
  (make-group (apply make-set (permutations (iota n))) compose-permutation))

(define (make-alternating n)
  (define (first-generator n) ;; assumes n > 4
    (append '(1 2 0) (iota (- n 3) 3)))
  (define (rest-generators n)
    (map (lambda (i)
	   (append '(1 0) (iota (- i 3) 2) (list i (- i 1))
		   (iota (- (- n 1) i) (+ i 1))))
	 (iota (- n 3) 3)))
	   
  (define (alternating-generators n)
    (cond ((= n 1) '((0)))
	  ((= n 2) '((0 1)))
	  ((= n 3) '((1 2 0)))
	  (else (cons (first-generator n)
		      (rest-generators n)))))
  (group-from-generators (alternating-generators n) compose-permutation))

;;; #############################################################################################
;;; Code for finding isomorphisms

(define (flatten inverse-order)
  (apply append (map cadr inverse-order)))

(define (get-mapped-elt e g1-inverse-order g2-inverse-order mapping)
  (list-ref (flatten g2-inverse-order)
	    (list-ref mapping
		      (list-index (lambda (x) (equal? x e))
				  (flatten g1-inverse-order)))))

(define (display-mapping mapping g1/inverse-order g2/inverse-order)
  (apply append (map (lambda (g1-elt)
	      (list g1-elt (get-mapped-elt g1-elt
					   g1/inverse-order
					   g2/inverse-order
					   mapping)))
	    (flatten g1/inverse-order))))

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
		  (display-mapping mapping
				   g1/inverse-order
				   g2/inverse-order))))))


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

#|
(group/isomorphic? (make-dihedral 3)
		   (make-symmetric 3))
"Isomorphism found!"
((0 0) (0 1 2))
((2 1) (0 2 1))
((1 1) (1 0 2))
((0 1) (2 1 0))
((2 0) (1 2 0))
((1 0) (2 0 1))
;Unspecified return value

(group/isomorphic? (make-dihedral 12)
		   (make-symmetric 4))
;Value: #f

(group/isomorphic? (group-from-generators '((4 0 1 2 3 5 6)
					    (0 1 2 3 4 6 5))
					  compose-permutation)
		   (group/cart-pdt (make-cyclic 5)
				   (make-cyclic 2)))
"Isomorphism found!"
((0 1 2 3 4 5 6) (0 0))
((0 1 2 3 4 6 5) (0 1))
((4 0 1 2 3 5 6) (1 0))
((3 4 0 1 2 5 6) (2 0))
((2 3 4 0 1 5 6) (3 0))
((1 2 3 4 0 5 6) (4 0))
((4 0 1 2 3 6 5) (1 1))
((3 4 0 1 2 6 5) (2 1))
((2 3 4 0 1 6 5) (3 1))
((1 2 3 4 0 6 5) (4 1))
;Unspecified return value

(group/isomorphic? (make-symmetric 3)
		   (make-cyclic 5))
;Value: #f

(group/isomorphic? (make-dihedral 9)
		   (group/cart-pdt (make-symmetric 3)
				   (make-cyclic 3)))
;Value: #f

(group/isomorphic? (make-cyclic 10)
		   (group/cart-pdt (make-cyclic 2)
				   (make-cyclic 5)))
"Isomorphism found!"
(0 (0 0))
(5 (1 0))
(8 (0 1))
(6 (0 2))
(4 (0 3))
(2 (0 4))
(9 (1 3))
(7 (1 4))
(3 (1 1))
(1 (1 2))
;Unspecified return value

(group/isomorphic? (make-cyclic 4)
		   (group-from-generators '((3 0 1 2))
					  compose-permutation))
"Isomorphism found!"
(0 (0 1 2 3))
(2 (2 3 0 1))
(3 (1 2 3 0))
(1 (3 0 1 2))
;Unspecified return value
|#


;;; Beautiful, beautiful groups!




