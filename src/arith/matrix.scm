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

;; utils to use matrices

(define (make-matrix m)
  (if (not (matrix-data? m))
      (error "Argument is not a valid matrix."))
  (vector 'matrix (length m) (length (list-ref m 0)) m))

(define (matrix-data? m)
  (and (list? m)
       (every list? m)
       (let ((cols (length (car m))))
	 (every (lambda (row) (= (length row) cols))
		m))))
;; more checks can be done here

(define (matrix? m)
  (and (vector? m)
       (eq? (vector-ref m 0) 'matrix)))

(define (matrix-#rows m)
  (and (matrix? m)
       (vector-ref m 1)))

(define (matrix-#cols m)
  (and (matrix? m)
       (vector-ref m 2)))

(define (matrix-data m)l
  (and (matrix? m)
       (vector-ref m 3)))

(define (matrix-term-by-term op)
  (lambda (m1 m2)
    (if (and (matrix? m1)
	     (matrix? m2)
	     (= (matrix-#cols m1) (matrix-#cols m2))
	     (= (matrix-#rows m1) (matrix-#cols m2)))
	(let ((m1-data (matrix-data m1))
	      (m2-data (matrix-data m2)))
	  (make-matrix (map (lambda (i j)
			      (map (lambda (x y) (+ x y))
				   i j))
			    m1-data m2-data)))
	(error "Not matrices of same dimensions"))))

(define transpose 
  (lambda (blist)
    (cond
    ((null? (car blist)) '())
    (else
      (cons (calc car blist) (transpose (calc cdr blist)))))))

(define calc
  (lambda (operation alist)
    (cond
      ((null? alist) '())
      (else
        (cons (operation (car alist)) (calc operation (cdr
						       alist)))))))

(define (dot-product-like op+ op*)
  (lambda (v1 v2)
    (apply op+ (map op* v1 v2))))

(define (matrix-multiply-like op+ op*)
  (let ((dot-pdt (dot-product-like op+ op*)))
    (lambda (m1 m2)
      (if (and (matrix? m1)
	       (matrix? m2)
	       (= (matrix-#cols m1) (matrix-#rows m2)))
	  (let ((m1-data (matrix-data m1))
		(m2-data (matrix-data m2)))
	    (make-matrix (map (lambda (m1-rowi)
				(map (lambda (m2-colj)
				       (dot-pdt m1-rowi m2-colj))
       				(transpose m2-data)))
			      m1-data)))
       	 (error "Not matrices of compatible dimensions")))))
