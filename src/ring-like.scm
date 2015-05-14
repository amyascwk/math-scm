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
;;;; Definitions and syntax of structures built from a set, an
;;;; additive operation and a multiplicative operation.

;;; #############################################################################################
;;; Datatype methods

;;; Primitive constructor
;; operations must be binary operations between elements of the set
(define (make-ring-like set additive-operation multiplicative-operation)
  (if (set? set)
      (make-math-object
       'ring-like
       (list (list 'underlying-set set)
	     (list 'additive-operation additive-operation)
	     (list 'multiplicative-operation multiplicative-operation))
       '())
      (error "Not a set:" set)))

;;; Test ring-like
(define (ring-like? obj)
  (and (math-object? obj)
       (eq? (math-object-structure obj) 'ring-like)))

;;; Get underlying set
(define (ring-like/underlying-set ring-like)
  (if (ring-like? ring-like)
      (get-math-datum ring-like 'underlying-set)
      (error "Not a ring-like object:" ring-like)))

;;; Get additive operation
(define (ring-like/additive-operation ring-like)
  (if (ring-like? ring-like)
      (get-math-datum ring-like 'additive-operation)
      (error "Not a ring-like object:" ring-like)))

;;; Get multiplicative operation
(define (ring-like/multiplicative-operation ring-like)
  (if (ring-like? ring-like)
      (get-math-datum ring-like 'multiplicative-operation)
      (error "Not a ring-like object:" ring-like)))

;;; Convert to set
(define (ring-like->set ring-like)
  (ring-like/underlying-set ring-like))

;;; #############################################################################################
;;; Properties

;;; Get order
(define (ring-like/size ring-like)
  ;; Check for size if already computed
  (if (has-math-property? ring-like 'size)
      ;; property set, return value
      (get-math-property ring-like 'size)
      ;; property not set, check value and return it
      (let ((size (set/cardinality (ring-like/underlying-set ring-like))))
	(set-math-property! ring-like 'size size)
	size)))

;;; Get underlying monoid from set and additive operation
(define (ring-like/additive-monoid ring-like)
  ;; Check for monoid if already computed
  (if (has-math-property? ring-like 'additive-monoid)
      ;; property set, return value
      (get-math-property ring-like 'additive-monoid)
      ;; property not set, check value and return it
      (let ((monoid (make-monoid 
		     (ring-like/underlying-set ring-like)
		     (ring-like/additive-operation ring-like))))
	(set-math-property! 
	 ring-like 'additive-monoid monoid)
	monoid)))

;;; Get underlying monoid from set and multiplicative operation
(define (ring-like/multiplicative-monoid ring-like)
  ;; Check for monoid if already computed
  (if (has-math-property? ring-like 'multiplicative-monoid)
      ;; property set, return value
      (get-math-property ring-like 'multiplicative-monoid)
      ;; property not set, check value and return it
      (let ((monoid (make-monoid 
		     (ring-like/underlying-set ring-like)
		     (ring-like/multiplicative-operation ring-like))))
	(set-math-property!
	 ring-like 'multiplicative-monoid monoid)
	monoid)))
   
;;; Get additive identity
(define (ring-like/additive-identity ring-like)
  ;; Check for identity if already computed
  (if (has-math-property? ring-like 'additive-identity)
      ;; property set, return value
      (get-math-property ring-like 'additive-identity)
      ;; property not set, check value and return it
      (let ((identity (monoid/identity 
		       (ring-like/additive-monoid ring-like))))
	(set-math-property!
	 ring-like 'additive-identity identity)
	identity)))

;;; Get multiplicative identity
(define (ring-like/multiplicative-identity ring-like)
  ;; Check for identity if already computed
  (if (has-math-property? ring-like 'multiplicative-identity)
      ;; property set, return value
      (get-math-property ring-like 'multiplicative-identity)
      ;; property not set, check value and return it
      (let ((identity (monoid/identity 
		       (ring-like/multiplicative-monoid ring-like))))
	(set-math-property!
	 ring-like 'multiplicative-identity identity)
	identity)))

;;; Test if underlying set and additive operation form monoid
(define (ring-like/additive-monoid? ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'additive-monoid?)
      ;; property set, return value
      (get-math-property ring-like 'additive-monoid?)
      ;; property not set, check value and return it
      (let ((result
	     (monoid? (ring-like/additive-monoid ring-like))))
	(set-math-property!
	 ring-like 'additive-monoid? result)
	result)))

;;; Test if underlying set and multiplicative operation form monoid
(define (ring-like/multiplicative-monoid? ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'multiplicative-monoid?)
      ;; property set, return value
      (get-math-property ring-like 'multiplicative-monoid?)
      ;; property not set, check value and return it
      (let ((result
	     (monoid? (ring-like/multiplicative-monoid ring-like))))
	(set-math-property!
	 ring-like 'multiplicative-monoid? result)
	result)))

;;; Test if underlying additive monoid is commutative
(define (ring-like/commutative-additive-monoid? ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'commutative-additive-monoid?)
      ;; property set, return value
      (get-math-property ring-like 'commutative-additive-monoid?)
      ;; property not set, check value and return it
      (let ((result
	     (group-like/commutative-operation? 
	      (ring-like/additive-monoid ring-like))))
	(set-math-property!
	 ring-like 'commutative-additive-monoid? result)
	result)))

;;; Test if underlying additive monoid is an abelian group
(define (ring-like/abelian-additive-monoid? ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'abelian-additive-monoid?)
      ;; property set, return value
      (get-math-property ring-like 'abelian-additive-monoid?)
      ;; property not set, check value and return it
      (let ((result
	     (abelian-group? (ring-like/additive-monoid ring-like))))
	(set-math-property!
	 ring-like 'abelian-additive-monoid? result)
	result)))

;;; Test if underlying multiplicative monoid is commutative
(define (ring-like/commutative-multiplicative-monoid? ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'commutative-multiplicative-monoid?)
      ;; property set, return value
      (get-math-property ring-like 'commutative-multiplicative-monoid?)
      ;; property not set, check value and return it
      (let ((result
	     (group-like/commutative-operation? 
	      (ring-like/multiplicative-monoid ring-like))))
	(set-math-property!
	 ring-like 'commutative-multiplicative-monoid? result)
	result)))

;;; Test if multiplication is distributive with respect to addition
(define (ring-like/distributive? ring-like)
  ;;Check if distributivity is satisfied if not already computed
  (if (not (has-math-property? ring-like 'distributive?))
      (let ((set (ring-like/underlying-set ring-like))
	    (add (ring-like/additive-operation ring-like))
	    (multiply (ring-like/multiplicative-operation ring-like)))
	;;Set distributivity property to result
	(set-math-property!
	 ring-like 'distributive?
	 (for-all x set
		  (for-all y set
			   (for-all z set
				    (and (equal? (multiply x (add y z))
						 (add
						  (multiply x y)
						  (multiply x z)))
					 (equal? (multiply (add y z) x)
						 (add
						  (multiply y x)
						  (multiply z x))))))))))
  (get-math-property ring-like 'distributive?))

;;; Test if multiplication by 0 annihilates the ring-like
(define (ring-like/annihilation-by-0 ring-like)
  ;; Check for property if already computed
  (if (has-math-property? ring-like 'annihilation-by-0?)
      ;; property set, return value
      (get-math-property ring-like 'annihilation-by-0?)
      ;; property not set, check value and return it
      (let ((set (ring-like/underlying-set ring-like))
	    (multiply (ring-like/multiplicative-operation ring-like))
	    (zero (ring-like/additive-identity ring-like)))
	(let ((result 
	       (for-all x set
			(and (equal? (multiply zero x)
				     (multiply x zero))
			     (equal? (multiply x zero)
				     zero)))))
	  (set-math-property!
	   ring-like 'annihilation-by-0? result)
	  result))))  

;;; Returns additive inverse of element
(define (ring-like/get-additive-inverse ring-like element)
  (let ((set (ring-like/underlying-set ring-like))
	(add (ring-like/additive-operation ring-like))
	(zero (ring-like/additive-identity ring-like)))
    (let ((y (there-exists x set
			   (and (equal? (add element x) zero)
				(equal? (add x element) zero)))))
      (if (not y)
	  (error "Element has no additive inverse:" element)
	  y))))

;;; Returns multiplicative inverse of element
(define (ring-like/get-multiplicative-inverse ring-like element)
  (let ((set (ring-like/underlying-set ring-like))
	(multiply (ring-like/multiplicative-operation ring-like))
	(one (ring-like/multiplicative-identity ring-like)))
    (let ((y (there-exists x set
			   (and (equal? (multiply element x) one)
				(equal? (multiply x element) one)))))
      (if (not y)
	  (error "Element has no multiplicative inverse:" element)
	  y))))



