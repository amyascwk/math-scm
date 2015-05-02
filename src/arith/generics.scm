(define (make-generic-operation arity name)
  (let ((metadata (make-operation-metadata name arity)))
    (let ((operation (lambda args (generic-dispatch metadata args))))
      (set-operation-metadata! operation metadata)
      operation)))

(define (add-to-generic-operation! operation applicability handler)
  (add-operation-rule! (make-operation-rule applicability handler)
                       (get-operation-metadata operation)))

(define (generic-dispatch metadata args)
  (let ((rule (find-applicable-rule metadata args)))
    (if rule
        (apply-operation-rule rule args)
        (error "Inapplicable operator:"
               (operation-metadata-name metadata)))))

(define (insert-generic-operation! package arity name)
  (let ((new-operation (make-generic-operation arity name)))
    (package-add-binding! package name new-operation)))

;;;; rules

(define (add-rule! operator-name package applicability handler)
  (add-to-generic-operation! (arithmetic-procedure package
						   operator-name)
			     applicability
			     handler))

(define (find-applicable-rule metadata args)
  (find (lambda (rule)
          (is-operation-rule-applicable? rule args))
        (operation-metadata-rules metadata)))

(define (apply-operation-rule rule args)
  (apply (operation-rule-handler rule) args))

(define (is-operation-rule-applicable? rule args)
  (is-applicable? (operation-rule-applicability rule)
                  args))

(define (make-operation-rule applicability handler)
  (list applicability handler))

(define (operation-rule-applicability rule)
  (car rule))

(define (operation-rule-handler rule)
  (cadr rule))

;;;; metadata

(define (make-operation-metadata name arity)
  (%make-operation-metadata name arity '()))

(define-record-type <operation-metadata>
    (%make-operation-metadata name arity rules)
    operation-metadata?
  (name operation-metadata-name)
  (arity operation-metadata-arity)
  (rules operation-metadata-rules set-operation-metadata-rules!))

(define (add-operation-rule! rule metadata)
  (set-operation-metadata-rules! metadata
      (cons rule (operation-metadata-rules metadata))))

;;;; metadata table

(define (set-operation-metadata! operation metadata)
  (hash-table-set! operation-metadata-table
                   operation
                   metadata))

(define (get-operation-metadata operation)
  (hash-table-ref operation-metadata-table operation))

(define (generic-operation? object)
  (hash-table-exists? operation-metadata-table object))

(define operation-metadata-table
  (make-key-weak-eqv-hash-table))

;;;; Interface with arithmetic

(define (generic-arithmetic base-arithmetic)
  (make-arithmetic 'generic base-arithmetic any-object?
    (lambda (name base-arithmetic)
      (let ((base-operation
             (arithmetic-operation base-arithmetic name)))
        (make-generic-operation (operation-arity base-operation)
                                name)))))

(define (add-to-generic-arithmetic! generic-arithmetic arithmetic)
  (for-each (lambda (name)
              (let ((generic-operation
                     (arithmetic-operation generic-arithmetic name))
                    (operation
                     (arithmetic-operation arithmetic name)))
                (add-to-generic-operation!
                 (operation-procedure generic-operation)
                 (operation-applicability operation)
                 (operation-procedure operation))))
            (arithmetic-operators arithmetic)))

(define (extend-generic-arithmetic! generic-arithmetic extension)
  (add-to-generic-arithmetic! generic-arithmetic
                              (extension generic-arithmetic)))
