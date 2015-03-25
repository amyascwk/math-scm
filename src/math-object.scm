;;;; Generic Mathematical Object syntax and definitions

;;; ############################################################################
;;; Datatype Methods

;;; Generic Math Object
;;; properties is an alist of property names and values
(define-record-type math-object
  (make-math-object properties)
  math-object?
  (properties math-object-properties set-math-object-properties!))

;;; Construct copy of math object with property set to desired value
(define (set-math-property mathobj property-name value)
  (let ((properties
	 (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (set-cdr! property-pair value)
	  (set! properties (cons (list property-name value)
				 properties)))
      (make-math-object properties))))

;;; Mutate the property alist of math object in place to set property to desired value
(define (set-math-property! mathobj property-name value)
  (let ((properties
	 (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (set-cdr! property-pair value)
	  (set! properties (cons (list property-name value)
				 properties)))
      (set-math-object-properties! mathobj properties))))

;;; Access value of property in math object
(define (get-math-property mathobj property-name)
  (let ((properties
	 (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (cadr property-pair)
	  (error "Property not found in math object:" property-name mathobj)))))

;;; Check if property is defined in math object
(define (has-math-property? mathobj property-name)
  (and (math-object? mathobj)
       (let ((properties
	      (math-object-properties mathobj)))
	 (let ((property-pair (assq property-name properties)))
	   (and property-pair
		#t)))))

;;; Comparator for generic math-objects with other objects
(set! type-priority-list
      (append type-priority-list
	      (list math-object?)))

;;; Comparator between math objects
(defhandler expr<?
  (lambda (obj1 obj2)
    ;;Sort property list by name, since expr<? selects the pair<? handler
    ;;which prioritizes the earlier elements when sorting property pairs
    (let ((prop1 (sort (math-object-properties obj1)
		       expr<?))
	  (prop2 (sort (math-object-properties obj2)
		       expr<?)))
      ;;Now sort by values of corresponding properties
      (expr<? prop1 prop2)))
  math-object? math-object?)

