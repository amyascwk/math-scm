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
;;;; Utilities

;;; #############################################################################################
;;; Miscellaneous procedures

;;; Any object
(define (any? obj) #t)

;;; #############################################################################################
;;;; Permutation functions

(define (permutation? p)
  (and (list? p)
       (every integer? p)
       (equal? (merge-sort p (lambda (x y) (< x y)))
	       (iota (length p)))))

(define (set-index! l idx val)
  (set-car! (list-tail l idx) val)
  l)

(define (permutations items)
  (define (remove ls elem)
    (cond ((null? ls) '())
	  ((equal? (car ls) elem) (remove (cdr ls) elem))
	  (else (cons (car ls) (remove (cdr ls) elem)))))
    (if (null? items) '(())
	(apply append
	       (map (lambda (element)
		      (map (lambda (permutation)
			     (cons element permutation))
			   (permutations (remove items element))))
		    items))))

(define (compose-permutation p1 p2)
  (assert (and (permutation? p1)
	       (permutation? p2)
	       (= (length p1) (length p2))))
  (let ((result (make-list (length p1) '*)))
    (for-each
     (lambda (i)
       (set-index! result
		  (list-index
		   (lambda (x) (= x (list-index
				     (lambda (x)(= x i))
				     p1)))
		   p2)
		  i))
     (iota (length p1)))
    result))

;;;; Useful function for non-deterministic search

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (distinct-from el rest)
  (not (member el rest)))

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



