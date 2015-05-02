(define (any-object? object)
  #t)

(define (disjoin . predicates)
  (lambda (object)
    (any (lambda (predicate)
	   (predicate object))
	 predicates)))

(define (conjoin . predicates)
  (lambda (object)
    (every (lambda (predicate)
	     (predicate object))
	   predicates)))

(define (negate predicate)
  (lambda (object)
    (not (predicate object))))

(define (compose . args)
  (compose* args))

(define (compose* args)
  (case (length args)
    ((0) (lambda (x) x))
    ((1) (car args))
    (else (reduce-right (lambda (f g)
			  (lambda (x) (f (g x))))
			(lambda (x) x)
			args))))

(define (close-enuf? h1 h2 tolerance)
  (not (> (magnitude (- h1 h2))
	  (* .5
	     (max tolerance *machine-epsilon*)
	     (+ (magnitude h1) (magnitude h2) 2.)))))

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
	 (* 2 e)
	 (loop (/ e 2)))))

(define (sign x)
  (cond ((> x 0) 1)
	((< x 0) -1)
	(else 0)))