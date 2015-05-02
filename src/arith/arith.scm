;;;; Extending arithmetic with combinators

;;; Accessors for arithmetic packages.

(define (in-domain-predicate arithmetic)
  (package-get arithmetic 'in-domain?))

(define (arithmetic-name arithmetic)
  (package-debug-name arithmetic))

(define (arithmetic-operation arithmetic operator-name)
  (package-value arithmetic operator-name))

(define (arithmetic-operators arithmetic)
  (package-names arithmetic))


;;; This is a convenience that combines an accessor from operators.scm
;;; with the accessors above.

(define (arithmetic-procedure arithmetic name)
  (operation-procedure (arithmetic-operation arithmetic name)))

(define (make-arithmetic-1 name base-arithmetics get-operation)
  (let ((base-arithmetics
         ;; All of the base arithmetics should be compatible, meaning
         ;; that they define the same names, and that their shared
         ;; annotations are individually compatible (in some as-yet
         ;; undefined sense).
         (if (list? base-arithmetics)
             base-arithmetics
             (list base-arithmetics))))
    (make-package (cons name
                        (map arithmetic-name
                             base-arithmetics))
      (map (lambda (name)
             (let ((base-operations
                    (map (lambda (arithmetic)
                           (arithmetic-operation arithmetic name))
                         base-arithmetics)))
               (cons name
                     (override-operation
                      (apply get-operation
                             name
                             base-arithmetics)
                      ;; Why is only the car is needed here?
                      (car base-operations)))))
           (arithmetic-operators (car base-arithmetics))))))

(define (make-arithmetic name base-arithmetics in-domain?
                         get-operation)
  (let ((p
         (make-arithmetic-1 name
                            base-arithmetics
                            get-operation)))
    (package-put! p 'in-domain? in-domain?)
    p))

(define (add-2-arithmetics a1 a2)
  (make-arithmetic 'add
                   (list a1 a2)
                   (disjoin (in-domain-predicate a1)
                            (in-domain-predicate a2))
    (lambda (name base-arithmetic1 base-arithmetic2)
      (let ((op1 (arithmetic-operation base-arithmetic1 name))
            (op2 (arithmetic-operation base-arithmetic2 name)))
        (annotate (lambda args
                    (operation-dispatch op1 op2 args name))
                  'applicability
                  (applicability-union op1 op2))))))

(define (operation-dispatch op1 op2 args name)
  (cond ((is-operation-applicable? op1 args)
         (apply-operation op1 args))
        ((is-operation-applicable? op2 args)
         (apply-operation op2 args))
        (else
         (error "Inapplicable operator:" name))))

(define (extend-arithmetic base-arithmetic extension)
  (add-2-arithmetics base-arithmetic (extension base-arithmetic)))

;;; install-arithmetic! takes an arithmetic package.
;;;  An arithmetic package is a mapping from names to operations.
;;;  An operation is a procedural implementation with annotations.
;;;  The procedure fixed-arity-to-variable takes an arithmetic package
;;;   and constructs a package that maps the names to nary procedures
;;;   with null annotation.

(define (install-arithmetic! arithmetic)
  (install-package! (fixed-arity-to-variable arithmetic)
                    null-mangler
                    operation-procedure))

(define (fixed-arity-to-variable arithmetic)
  (make-package (list 'nary- (arithmetic-name arithmetic))
    (modified arithmetic
              (+-like '+ 'additive-identity)
              (--like '- 'negate)
              (+-like '* 'multiplicative-identity)
              (--like '/ 'invert)
              (comparator '<)
              (comparator '=)
              (comparator '>)
              (comparator '<=)
              (comparator '>=)
              (min-like 'min)
              (min-like 'max)
              )))

(define (modified arithmetic . modifications)
  (map (lambda (name)
         (let ((v (assq name modifications)))
           (if v
               (cons (car v) ((cdr v) arithmetic))
               (unchanged name arithmetic))))
       (arithmetic-operators arithmetic)))

(define (unchanged name arithmetic)
  (cons name (arithmetic-operation arithmetic name)))

;;; These are the n-ary extension procedures for common arithmetic
;;; operations.

(define (+-like operator identity-name)
  (cons operator
        (lambda (arithmetic)
          (let ((proc
                 (arithmetic-procedure arithmetic operator))
                (identity-procedure
                 (arithmetic-procedure arithmetic identity-name)))
            (lambda args
              (case (length args)
                ((0) (identity-procedure))
                ((1) (car args))
                (else
                 (reduce-left proc #f args))))))))

(define (--like operator unary-operator)
  (cons operator
        (lambda (arithmetic)
          (let ((proc
                 (arithmetic-procedure arithmetic operator))
                (unary
                 (arithmetic-procedure arithmetic unary-operator)))
            (lambda (arg . args)
              (case (length args)
                ((0) (unary arg))
                (else
                 (reduce-left proc #f (cons arg args)))))))))

(define (comparator operator)
  (cons operator
        (lambda (arithmetic)
          (let ((proc
                 (arithmetic-procedure arithmetic operator)))
            (lambda args
              (case (length args)
                ((0) #t)
                ((1) #t)
                (else
                 (let loop ((args args))
                   (if (proc (car args) (cadr args))
                       (if (pair? (cddr args))
                           (loop (cdr args))
                           #t)
                       #f)))))))))

(define (min-like operator)
  (cons operator
        (lambda (arithmetic)
          (let ((proc (arithmetic-procedure arithmetic operator)))
            (lambda (arg . args)
              (reduce-left proc #f (cons arg args)))))))

;;; Initialization of arithmetic stuff.

(define raw-scheme-arithmetic
  (let ((entry
         (lambda (name pname arity . annotations)
           (cons name
                 (apply annotate
                        (environment-lookup system-global-environment pname)
                        'arity arity
                        annotations)))))
    (let ((p
           (make-package 'raw-scheme
             (list (entry '+ '+ 2)
                   (entry '- '- 2)
                   (entry '* '* 2)
                   (entry '/ '/ 2)
                   (entry 'additive-identity '+ 0)
                   (entry 'negate '- 1)
                   (entry 'multiplicative-identity '* 0)
                   (entry 'invert '/ 1)
                   (entry 'abs 'abs 1)
                   (entry 'expt 'expt 2)
                   (entry '< '< 2)
                   (entry '= '= 2)
                   (entry '> '> 2)
                   (entry '<= '<= 2)
                   (entry '>= '>= 2)
                   (entry 'min 'min 2)
                   (entry 'max 'max 2)
                   (entry 'floor 'floor 1)
                   (entry 'ceiling 'ceiling 1)
                   (entry 'truncate 'truncate 1)
                   (entry 'round 'round 1)
                   (entry 'exp 'exp 1)
                   (entry 'log 'log 1)
                   (entry 'cos 'cos 1)
                   (entry 'sin 'sin 1)
                   (entry 'tan 'tan 1)
                   (entry 'asin 'asin 1)
                   (entry 'acos 'acos 1)
                   (entry 'atan 'atan 2)
                   (entry 'sqrt 'sqrt 1)
                   (entry 'make-rectangular 'make-rectangular 2)
                   (entry 'make-polar 'make-polar 2)
                   (entry 'real-part 'real-part 1)
                   (entry 'imag-part 'imag-part 1)
                   (entry 'magnitude 'magnitude 1)
                   (entry 'angle 'angle 1)
                   ))))
      (package-put! p 'in-domain? number?)
      p)))

;;; Needs n:stuff

(install-package! (fixed-arity-to-variable raw-scheme-arithmetic)
                  (lambda (name) (symbol 'n: name))
                  operation-procedure)

(define numeric-arithmetic
  (make-arithmetic 'numeric raw-scheme-arithmetic number?
    (lambda (name base-arithmetic)
      (let ((base-operation
             (arithmetic-operation base-arithmetic name)))
        (annotate (lambda args
                    (apply-operation base-operation args))
                  'applicability
                  (all-args (operation-arity base-operation)
                            (in-domain-predicate base-arithmetic)))))))

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))

;;; Implementation null annotation:
(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic raw-scheme-arithmetic
    (lambda (name base-arithmetic)
      (lambda args (cons name args)))))

(define (symbolic-arithmetic base-arithmetic)
  (make-arithmetic 'symbolic base-arithmetic symbolic?
    (lambda (name base-arithmetic)
      (let ((base-operation
             (arithmetic-operation base-arithmetic name)))
        (annotate (lambda args (cons name args))
                  'applicability
                  (any-arg (operation-arity base-operation)
                           symbolic?
                           (in-domain-predicate base-arithmetic)))))))



(define function? procedure?)

(define (pure-function-arithmetic range-arithmetic)
  (make-arithmetic 'pure-function range-arithmetic function?
    (lambda (name range-arithmetic)
      (let ((range-operation
             (arithmetic-operation range-arithmetic name)))
        (annotate (lambda functions
                    (lambda args
                      (apply-operation range-operation
                                       (map (lambda (function)
                                              (apply function args))
                                            functions))))
                  'applicability
                  (all-args (operation-arity range-operation)
                            function?))))))

(define (function-arithmetic range-arithmetic)
  (let ((range-predicate (in-domain-predicate range-arithmetic)))
    (make-arithmetic 'function
                     range-arithmetic
                     (disjoin range-predicate function?)
      (lambda (name range-arithmetic)
        (let ((range-operation
               (arithmetic-operation range-arithmetic name)))
          (annotate (lambda things
                      (lambda args
                        (apply-operation range-operation
                                         (map (lambda (thing)
                                                (if (function? thing)
                                                    (apply thing args)
                                                    thing))
                                              things))))
                    'applicability
                    (any-arg (operation-arity range-operation)
                             function?
                             range-predicate)))))))

(define (function-arithmetic-with-coercion range-arithmetic)
  (let ((range-predicate (in-domain-predicate range-arithmetic)))
    (make-arithmetic 'function-with-coercion
                     (function-arithmetic range-arithmetic)
                     (disjoin range-predicate function?)
      (lambda (name function-arithmetic)
        (let ((function-operation
               (arithmetic-operation function-arithmetic name)))
          (annotate (lambda things
                      (apply-operation function-operation
                                       (map (lambda (thing)
                                              (if (function? thing)
                                                  thing
                                                  (lambda args thing)))
                                            things)))
                    'applicability
                    (any-arg (operation-arity function-operation)
                             function?
                             range-predicate)))))))


(define (vector-element-wise element-procedure)
  (lambda vecs ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
         (not (n:= (vector-length v)
       first-vec-length)))
       vecs)
  (error "Vector dimension mismatch" vecs)
  #t)))

; helper base predicate for dot product
(define (symbol-or-number? x)
  (or (symbol? x)
      (number? x)))

(define (scalar-product-maker *)
  (lambda (vec scalar)
    (vector-map (lambda (x) (* x scalar))
    vec)))

(define (scalar-product-maker-flipped *)
  (let ((scalar-product (scalar-product-maker *)))
    (lambda (scalar vec)
      (scalar-product vec scalar))))


(define (dot-product-maker + *)
  (lambda (vec1 vec2)
    (apply + (map (lambda (pair) (apply * pair))
      (zip (vector->list vec1) (vector->list vec2))))))

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    (lambda (v)
      (sqrt (dot-product v v)))))

(define (maxnorm-maker max abs)
  (lambda (v)
    (apply max (map abs (vector->list v)))))
#|
(let ((g (generic-arithmetic numeric-arithmetic)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g function-arithmetic)
  (add-to-generic-arithmetic! g
            (symbolic-arithmetic
             numeric-arithmetic))
  (install-arithmetic! g))
; first let the operations in the environment support numbers, symbols
; and functions


(let ((g (generic-arithmetic numeric-arithmetic)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g function-arithmetic)
  (add-to-generic-arithmetic! g
            (symbolic-arithmetic
             numeric-arithmetic))
  (add-rule! '+ g (predicates vector? vector?)
       (vector-element-wise +))
  (add-rule! '- g (predicates vector? vector?)
       (vector-element-wise -))
  (add-rule! '* g (predicates vector? symbol-or-number?)
       (scalar-product-maker *))
  (add-rule! '* g (predicates symbol-or-number? vector?)
       (scalar-product-maker-flipped *))
  (add-rule! '* g (predicates vector? vector?)
       (dot-product-maker + *))
  (add-rule! 'magnitude g (predicates vector?)
       (vector-magnitude-maker + * sqrt))
  (insert-generic-operation! g 1 'maxnorm)
  (add-rule! 'maxnorm g (predicates vector?)
       (maxnorm-maker max abs))
  (install-arithmetic! g))
|#

#|
(install-arithmetic!
 (extend-arithmetic numeric-arithmetic
                    pure-function-arithmetic))
;Unspecified return value

((+ cos sin) 3)
;Value: -.8488724885405782

(* 2 ((+ cos sin) 3))
;Value: -1.6977449770811563

(* 2 ((+ 1 cos sin) 3))
;Inapplicable operator: +

(install-arithmetic!
 (extend-arithmetic numeric-arithmetic
                    function-arithmetic))

((+ cos sin) 3)
;Value: -.8488724885405782

(* 2 ((+ cos sin) 3))
;Value: -1.6977449770811563

(* 2 ((+ 1 cos sin) 3))
;Value: .3022550229188436

(define combined-arithmetic
  (extend-arithmetic numeric-arithmetic
                     symbolic-arithmetic))

(install-arithmetic!
 (extend-arithmetic combined-arithmetic
                    pure-function-arithmetic))

((+ cos sin) 3)
;Value: -.8488724885405782

((+ cos sin) 'a)
;Value: (+ (cos a) (sin a))

(* 'b ((+ cos sin) (+ 3 'a)))
;Value 121: (* b (+ (cos (+ 3 a)) (sin (+ 3 a))))

((+ 'b cos sin) (+ 3 'a))
;;No coercion

;;; The following two are equivalent.
(install-arithmetic!
 (extend-arithmetic combined-arithmetic
                    function-arithmetic))
(install-arithmetic!
 (extend-arithmetic combined-arithmetic
                    function-arithmetic-with-coercion))

(* 'b ((+ 4 cos sin) (+ 3 'a)))
;Value: (* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))

(+ 'a ((+ cos sin) 'b))
;Value: (+ a (+ (cos b) (sin b)))

(+ 'a ((+ cos sin) 3))
;Value: (+ a -.8488724885405782)

(+ 'a ((+ 3 cos sin) 'b))
;Value: (+ a (+ (+ 3 (cos b)) (sin b)))
|#

