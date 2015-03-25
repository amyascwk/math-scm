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

