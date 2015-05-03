;; utils to use matrices

(define (make-matrix m)
  (if (not (matrix-data? m))
      (error "Invalid argument."))
  (vector 'matrix (length m) (length (list-ref m 0)) m))

(define (matrix-data? m)
  (and (list? m)
       (every list? m)))
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

(define (matrix-data m)
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
