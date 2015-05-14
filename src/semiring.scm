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
;;;; Semiring

;;; #############################################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-semiring set add multiply)
  (let ((ring-like (make-ring-like set add multiply)))
    (if (is-semiring? ring-like)
	ring-like
	(error "Not a semiring"))))

(define (is-semiring? obj)
  (and (ring-like? obj)
       ;; check if semiring? property is set
       (if (has-math-property? obj 'semiring?)
	   ;; property set, return its value
	   (get-math-property obj 'semiring?)
	   ;; property not set, check its value and return it
	   (let ((result
		  (and (ring-like/commutative-additive-monoid? obj)
		       (ring-like/multiplicative-monoid? obj)
		       (ring-like/distributive? obj)
		       (ring-like/annihilation-by-0 obj))))
	     (set-math-property! 
	      obj 'semiring? result)
	     result))))
