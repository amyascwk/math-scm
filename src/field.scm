;;;; Field

;;; #########################################################################
;;; Datatype methods

;;; Primitive constructor
(define (make-field set add multiply)
  (let ((ring-like (make-ring-like set add multiply)))
    (if (is-field? ring-like)
	ring-like
	(error "Not a field"))))

(define (is-field? obj)
  (and (is-ring? obj)
       (not (equal? (ring/additive-identity obj)
		    (ring/multiplicative-identity obj)))
       (set/equal? (ring/get-units obj)
		   (ring/get-nonzero-elements obj))))
       
(define (field/underlying-set field)
  (ring-like/underlying-set field))

(define (field/additive-operation field)
  (ring-like/additive-operation field))

(define (field/multiplicative-operation field)
  (ring-like/multiplicative-operation field))

(define (field->set field)
  (ring-like/underlying-set field))

;;; ############################################################################
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

;;; ############################################################################
;;; Special constructors

;;; Construct prime field of characteristic p
(define (make-prime-field p)
  (make-field (list->set (iota p)) 
	      (lambda (x y)
		(modulo (+ x y) p))
	      (lambda (x y)
		(modulo (* x y) p))))

;;; ############################################################################
;;; Tests

(let ((f7 (make-prime-field 7)))
  (test-equal (field/get-additive-inverse f7 3) 4))


