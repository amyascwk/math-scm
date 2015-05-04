;;; make-key-weak-eqv-hash-table was introduced in MIT Scheme 9.2
;;; This patch defines an equivalent behavior for MIT Scheme 9.1.1
(define make-key-weak-eqv-hash-table
  (if (environment-bound? (the-environment)
			  'make-key-weak-eqv-hash-table)
      make-key-weak-eqv-hash-table
      make-weak-eqv-hash-table))
