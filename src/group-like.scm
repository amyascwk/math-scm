;;;; Definitions and syntax of structures built from a set
;;;; and a binary operation

;;; ############################################################################
;;; Datatype methods

;;; Primitive constructor
;;; operation must be binary operation between elements of the set
;;; Suggestion: Make a special function datatype for the operation, so that
;;;             domain and ranges can be checked more easily?
(define (make-group-like set operation)
  (if (set? set)
      (make-math-object
       (list (list 'underlying-set set)
	     (list 'operation operation)))
      (error "Not a set:" set)))

;;; Test group-like
(define (group-like? obj)
  (and (math-object? obj)
       (has-math-property? obj 'underlying-set)
       (has-math-property? obj 'operation)))

;;; Get underlying set
(define (group-like/underlying-set group-like)
  (if (group-like? group-like)
      (get-math-property group-like 'underlying-set)
      (error "Not a group-like object:" group-like)))

;;; Get operation
(define (group-like/operation group-like)
  (if (group-like? group-like)
      (get-math-property group-like 'operation)
      (error "Not a group-like object:" group-like)))

;;; Convert to set
(define (group-like->set group-like)
  (group-like/underlying-set group-like))


;;; ############################################################################
;;; Properties

;;; Test closure of operation
(define (group-like/closed-operation? group-like)
  (if (not (group-like? group-like))
      (error "Not a group-like object:" group-like))
  ;;Check if closure is satisfied if not already computed
  (if (not (has-math-property? group-like 'closed-operation?))
      (let ((set (group-like/underlying-set group-like))
	    (operation (group-like/operation group-like)))
	;;Set closure property to result
	(set-math-property!
	 group-like 'closed-operation?
	 (for-all x set
		  (for-all y set
			   (set/member (operation x y) set))))))
  (get-math-property group-like 'closed-operation?))

;;; Test associativity of operation
(define (group-like/associative-operation? group-like)
  (if (not (group-like? group-like))
      (error "Not a group-like object:" group-like))
  ;;Check if associativity is satisfied if not already computed
  (if (not (has-math-property? group-like 'associative-operation?))
      (let ((set (group-like/underlying-set group-like))
	    (operation (group-like/operation group-like)))
	;;Set associativity property to result
	(set-math-property!
	 group-like 'associative-operation?
	 (for-all x set
		  (for-all y set
			   (for-all z set
				    (equal? (operation (operation x y) z)
					    (operation x (operation y z)))))))))
  (get-math-property group-like 'associative-operation?))

;;; Test existence of identity element
(define (group-like/identity group-like)
  (if (not (group-like? group-like))
      (error "Not a group-like object:" group-like))
  ;;Check for existence of identity element if not already computed
  (if (not (has-math-property? group-like 'identity))
      (let ((set (group-like/underlying-set group-like))
	    (operation (group-like/operation group-like)))
	;;Set identity field to result
	(set-math-property!
	 group-like 'identity
	 (there-exists e set
		       (for-all x set
				(and (expr=? (operation e x) x)
				     (expr=? (operation x e) x)))))))
  (get-math-property group-like 'identity))

;;; Get inverses alist of elements (if they exist)
(define (group-like/inverses-alist group-like)
  (if (not (group-like? group-like))
      (error "Not a group-like object:" group-like))
  (if (not (group-like/identity group-like))
      (error "Not implemented for group-like objects without identity element yet:" group-like))
  ;;Find inverses if not already computed
  (if (not (has-math-property? group-like 'inverses-alist))
      (let ((set (group-like/underlying-set group-like))
	    (operation (group-like/operation group-like))
	    (identity (group-like/identity group-like)))
	;;Set inverses-alist field to result
	(set-math-property!
	 group-like 'inverses-alist
	 (map (lambda (x)
		(list x
		      (there-exists y set
				    (and (equal? (operation x y) identity)
					 (equal? (operation y x) identity)))))
	      (set->list set)))))
  (get-math-property group-like 'inverses-alist))

;;; Test invertibility
(define (group-like/invertible? group-like)
  (and (every cadr
	      (group-like/inverses-alist group-like))
       #t))

