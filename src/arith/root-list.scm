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


(define (square-free-integer n)
  (define (helper i max n)
    (if (= 0 (modulo n (* i i)))
	(square-free (/ n (* i i)))
	(if (>= i max)
	    n
	    (helper (+ i 1) max n))))
  (helper 2 (floor->exact (sqrt n)) n))

(define (square-free n)
  (if (integer? n)
      (square-free-integer n)
      (/ (square-free-integer (numerator n))
	 (square-free-integer (denominator n)))))

(define (simplify exp)
  (define (reduce-sqs el)
    (let ((sqf (square-free (cdr el))))
      (cons (* (car el) (sqrt (/ (cdr el) sqf)))
	    sqf)))
  (define (unique lst rest)
    (if (null? rest)
	(reverse lst)
	(if (memq (car rest) lst)
	    (unique lst (cdr rest))
	    (unique (cons (car rest) lst) (cdr rest)))))
  (define (collect-coeffs lst)
    (lambda (radicand)
      (define (helper l sum)
	(if (null? l)
	    sum
	    (let ((el (car l)))
	      (if (= radicand (cdr el))
		  (helper (cdr l) (+ sum (car el)))
		  (helper (cdr l) sum)))))
      (cons (helper lst 0) radicand)))
  (define (non-zeros el)
    (not (= (car el) 0)))
  (let* ((reduced (map reduce-sqs exp))
	 (radicands (merge-sort (unique '()
					(map cdr
					     (filter non-zeros
						     reduced)))
				<))
	 (result (map (collect-coeffs reduced) radicands)))
    (filter non-zeros result)))


(define (root-list? lst)
  (and (list? lst)
       (every (lambda (el)
		(and (number? (car el))
		     (number? (cdr el))
		     (exact? (car el))
		     (exact? (cdr el))))
	      lst)))

(define (rl:+ rl1 rl2)
  (simplify (append rl1 rl2)))

(define (rl:- rl1 rl2)
  (simplify (append rl1
		    (map (lambda (el)
			   (cons (- (car el))
				 (cdr el)))
			 rl2))))

(define (all-pairs-between l1 l2)
  (apply append
	 (map (lambda (e1)
		(map (lambda (e2) (list e1 e2)) l2)) l1)))



(define (rl:* rl1 rl2)
  (let* ((pairs (all-pairs-between rl1 rl2))
	 (*ed-pairs (map
		     (lambda (el)
		       (let ((x (first el))
			     (y (second el)))
			 (cons (* (car x) (car y))
			       (* (cdr x) (cdr y)))))
		     pairs)))
    (simplify *ed-pairs)))

(define (rl:/ rl1 rl2)
  (define (get-conjugate rl)
    (cons (car rl)
	  (rl:- '() (cdr rl))))
  (if (= (length rl2) 1)
      (let ((denom (car rl2)))
	(map (lambda (el)
	       (cons (/ (car el) (car denom))
		     (/ (cdr el) (cdr denom))))
	     rl1))
      (let ((rl2-conj (get-conjugate rl2)))
	(rl:/ (rl:* rl1 rl2-conj)
	      (rl:* rl2 rl2-conj)))))

(let ((g (generic-arithmetic numeric-arithmetic)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (add-rule! '+ g (predicates root-list? root-list?) rl:+)
  (add-rule! '- g (predicates root-list? root-list?) rl:-)
  (add-rule! '* g (predicates root-list? root-list?) rl:*)
  (add-rule! '/ g (predicates root-list? root-list?) rl:/)
  (add-rule! '+ g (predicates matrix? matrix?)
	     (matrix-term-by-term +))
  (add-rule! '- g (predicates matrix? matrix?)
	     (matrix-term-by-term -))
  (add-rule! '* g (predicates matrix? matrix?)
	     (matrix-multiply-like + *))
  (install-arithmetic! g))

;; need to do it twice
(let ((g (generic-arithmetic numeric-arithmetic)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (add-rule! '+ g (predicates root-list? root-list?) rl:+)
  (add-rule! '- g (predicates root-list? root-list?) rl:-)
  (add-rule! '* g (predicates root-list? root-list?) rl:*)
  (add-rule! '/ g (predicates root-list? root-list?) rl:/)
  (add-rule! '+ g (predicates matrix? matrix?)
	     (matrix-term-by-term +))
  (add-rule! '- g (predicates matrix? matrix?)
	     (matrix-term-by-term -))
  (add-rule! '* g (predicates matrix? matrix?)
	     (matrix-multiply-like + *))
  (install-arithmetic! g))
