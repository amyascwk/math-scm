;;; A package has a name for use in debugging.
;;; It has a set of named bindings.
;;;   It can produce the set of names it knows about.
;;;   Given a name it can produce the value for that name.
;;; It also has a property list.

(define-record-type <package>
    (%make-package name properties bindings-alist)
    package?
  (name package-debug-name)
  (properties %package-props %set-package-props!)
  (bindings-alist package-bindings set-package-bindings!))

(define (make-package name bindings-alist)
  (guarantee-alist bindings-alist)
  (%make-package name '() bindings-alist))

(define (package-names package)
  (map car (package-bindings package)))

(define (package-value package name)
  (let ((p (assq name (package-bindings package))))
    (if p
	(cdr p)
	(error "Unknown binding name:" name))))

(define (similar-packages? p1 p2)
  (lset= eq? (package-names p1) (package-names p2)))

(define (package-add-binding! package name value)
  (let ((p (assq name (package-bindings package))))
    (if p
	(set-cdr! p value)
	(set-package-bindings! package
			     (cons (cons name value)
				   (package-bindings package))))))


;;; Package properties

(define (package-get package name #!optional default-value)
  (let ((p (assq name (%package-props package))))
    (if p
	(cdr p)
	(if (default-object? default-value)
	    (error:bad-range-argument name 'package-get)
	    default-value))))

(define (package-put! package name value)
  (let ((p (assq name (%package-props package))))
    (if p
	(set-cdr! p value)
	(%set-package-props! package
			     (cons (cons name value)
				   (%package-props package))))))

;;; This installer is MIT/GNU Scheme specific:
(define (package-installer environment)
  (lambda (package name-mangler value-mangler)
    (for-each (lambda (binding)
		(environment-define environment
				    (name-mangler (car binding))
				    (value-mangler (cdr binding))))
	      (package-bindings package))))

(define (null-mangler object)
  object)

;;; THE-ENVIRONMENT is also MIT/GNU Scheme specific:
(define install-package! (package-installer (the-environment)))


;;; This is MIT/GNU Scheme specific:
(set-record-type-unparser-method! <package>
  (simple-unparser-method 'package
    (lambda (package)
      (list (package-debug-name package)))))
