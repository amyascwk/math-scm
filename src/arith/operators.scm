(define (operation? object)
  (or (procedure? object)
      (annotated-procedure? object)))

(define (annotated-procedure? object)
  (and (pair? object)
       (procedure? (car object))
       (keyword-list? (cdr object))))

(define (annotate procedure . annotations)
  (if (not (keyword-list? annotations))
      (error "Bad annotations" procedure annotations))
  (make-operation procedure annotations))

(define (make-operation procedure annotations)
  (if (null? annotations)
      procedure
      (cons procedure annotations)))

(define (operation-procedure operation)
  (cond ((procedure? operation) operation)
	((annotated-procedure? operation)
	 (car operation))
	(else
	 (error "Not an operation" operation))))

(define (operation-annotations operation)
  (cond ((procedure? operation) '())
	((annotated-procedure? operation)
	 (cdr operation))
	(else
	 (error "Not an operation" operation))))

(define (operation-annotation operation name)
  (get-keyword-value (operation-annotations operation) name))

(define (override-operation new-operation original-operation)
  (make-operation (operation-procedure new-operation)
		  (merge-annotations
		   (operation-annotations new-operation)
		   (operation-annotations original-operation))))

(define (merge-annotations new old)
  (let lp ((lst (append new old)))
    (cond ((null? lst) '())
          ((symbol? (car lst))
           (cons (car lst)
                 (cons (cadr lst)
                       (lp (remove-annotation (car lst)
                                              (cddr lst))))))
          (else (error "Bad annotations" new old)))))

(define (remove-annotation key lst)
  (cond ((null? lst) '())
        ((eq? key (car lst)) (cddr lst))
        (else
         (cons (car lst)
               (cons (cadr lst)
                     (remove-annotation key (cddr lst)))))))

(define (operation-arity operation)
  (operation-annotation operation 'arity))

(define (operation-applicability operation)
  (operation-annotation operation 'applicability))

(define (is-operation-applicable? operation args)
  (is-applicable? (operation-applicability operation) args))

(define (apply-operation o args)
  (apply (operation-procedure o) args))

;;;; Applicability

;;; An applicability attribute is a list of lists, representing an OR
;;; of some per-argument ANDs.	There may or may not be an advantage
;;; to using operations in the lists rather than procedures.

(define (is-applicable? applicability args)
  (any (lambda (and-clause)
	 (and (n:= (length and-clause) (length args))
	      (every (lambda (predicate arg)
		       (predicate arg))
		     and-clause
		     args)))
       applicability))

(define (applicability-union op1 op2)
  (append (operation-applicability op1)
	  (operation-applicability op2)))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-arg arity predicate base-predicate)
  (let ((joint-predicate (disjoin base-predicate predicate)))
    (map (lambda (i)
	   (let ((l (make-list arity joint-predicate)))
	     (list-set! l i predicate)
	     l))
	 (iota arity))))

(define (predicates . pred-args)
  (list (apply list pred-args)))
