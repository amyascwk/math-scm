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
;;;; Test tools

;;; Test not #f
(define (test-true result)
  (if (not result)
      (error "Test for non-false value failed:" result)))

;;; Test #f
(define (test-false result)
  (if result
      (error "Test for false value failed:" result)))

;;; Test equality
(define (test-equal result expectation)
  (if (not (expr=? result expectation))
      (error "Test for equality failed:" result expectation)))

;;; Test error
(define-syntax test-error
  (er-macro-transformer
   (lambda (exp r c)
     (let ((body (cdr exp)))
       `(,(r 'if) (,(r 'not)
		   (,(r 'condition?)
		    (,(r 'ignore-errors) (,(r 'lambda) ()
					  ,@body))))
	          (,(r 'error) "Test for error failed" (,(r 'quote) ,body)))))))

