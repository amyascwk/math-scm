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
;;;; Field

;;; #############################################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-field set add multiply)
  (let ((ring-like (make-ring-like set add multiply)))
    (if (is-field? ring-like)
	ring-like
	(error "Not a field"))))

(define (is-field? obj)
  (and (is-ring? obj)
       ;; check if field? property is set
       (if (has-math-property? obj 'field?)
	   ;; property set, return its value
	   (get-math-property obj 'field?)
	   ;; property not set, check its value and return it
	   (let ((result
		  (and (not (equal? (ring/additive-identity obj)
				    (ring/multiplicative-identity obj)))
		       (set/equal? (ring/get-units obj)
				   (ring/get-nonzero-elements obj)))))
	     (set-math-property! 
	      obj 'field? result)
	     result))))
       
(define (field/underlying-set field)
  (ring-like/underlying-set field))

(define (field/additive-operation field)
  (ring-like/additive-operation field))

(define (field/multiplicative-operation field)
  (ring-like/multiplicative-operation field))

(define (field->set field)
  (ring-like/underlying-set field))

;;; #############################################################################################
;;; Properties

(define (field/additive-identity field)
  (ring-like/additive-identity field))

(define (field/multiplicative-identity field)
  (ring-like/multiplicative-identity field))

(define (field/order field)
  (ring-like/size field))

(define (field/get-additive-inverse field element)
  (ring-like/get-additive-inverse field element))

(define (field/get-multiplicative-inverse field element)
  (ring-like/get-multiplicative-inverse field element))

;;; #############################################################################################
;;; Special constructors

;;; Construct prime field of characteristic p
(define (make-prime-field p)
  (make-field (list->set (iota p)) 
	      (lambda (x y)
		(modulo (+ x y) p))
	      (lambda (x y)
		(modulo (* x y) p))))

;;; #############################################################################################
;;; Tests

(let ((f7 (make-prime-field 7)))
  (test-equal (field/get-additive-inverse f7 3) 4)
  (test-equal (field/get-multiplicative-inverse f7 2) 4))


