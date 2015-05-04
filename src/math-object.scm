;;;; ############################################################################################
;;;; ############################################################################################
;;;; Generic Mathematical Object syntax and definitions

;;; #############################################################################################
;;; Datatype Methods

;;; Generic Math Object
;;; Basic record type that allows construction of a large variety of mathematical entities
;;; Fields: "structure" is a symbol to be used in deciding how to interpret the data
;;;         "data" is an immutable alist of datum names and values, by which the identity of the
;;;         math object is defined
;;;         "properties" is an alist of property names and values, it is mutable to allow dynamic
;;;         adding of properties, which do not contribute to the identity of the math object
(define-record-type math-object
  (make-math-object structure data properties)
  math-object?
  (structure math-object-structure)
  (data math-object-data)
  (properties math-object-properties set-math-object-properties!))

;;; Return a newly allocated copy of a math object
(define (copy-math-object mathobj)
  (make-math-object (math-object-structure mathobj)
		    (math-object-data mathobj)
		    (math-object-properties mathobj)))

;;; Access value of datum in math object
(define (get-math-datum mathobj datum-name)
  (let ((data (math-object-data mathobj)))
    (let ((datum-pair (assq datum-name data)))
      (if datum-pair
	  (cadr datum-pair)
	  (error "Datum not found in math object:" datum-name mathobj)))))

;;; Check if property is defined in math object
(define (has-math-datum? mathobj datum-name)
  (and (math-object? mathobj)
       (let ((data (math-object-data mathobj)))
	 (let ((datum-pair (assq datum-name data)))
	   (and datum-pair
		#t)))))

;;; Construct copy of math object with property set to desired value
(define (set-math-property mathobj property-name value)
  (let ((structure (math-object-structure mathobj))
	(data (math-object-data mathobj))
	(properties (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (set-cdr! property-pair (list value))
	  (set! properties (cons (list property-name value)
				 properties)))
      (make-math-object structure data properties))))

;;; Mutate the property alist of math object in place to set property to desired value
(define (set-math-property! mathobj property-name value)
  (let ((properties (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (set-cdr! property-pair (list value))
	  (set! properties (cons (list property-name value)
				 properties)))
      (set-math-object-properties! mathobj properties))))

;;; Access value of property in math object
(define (get-math-property mathobj property-name)
  (let ((properties (math-object-properties mathobj)))
    (let ((property-pair (assq property-name properties)))
      (if property-pair
	  (cadr property-pair)
	  (error "Property not found in math object:" property-name mathobj)))))

;;; Check if property is defined in math object
(define (has-math-property? mathobj property-name)
  (and (math-object? mathobj)
       (let ((properties (math-object-properties mathobj)))
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
    ;;Sort data list by name, since expr<? selects the pair<? handler
    ;;which prioritizes the earlier elements when sorting property pairs
    (let ((struct1 (math-object-structure obj1))
	  (data1 (sort (math-object-data obj1) expr<?))
	  (struct2 (math-object-structure obj2))
	  (data2 (sort (math-object-data obj2) expr<?)))
      ;;Now sort by values of corresponding data
      (or (expr<? struct1 struct2)
	  (and (expr=? type1 type2) (expr<? data1 data2))
	  #f)))
  math-object? math-object?)

;;; Equality comparator between math objects
(defhandler expr=?
  (lambda (obj1 obj2)
    ;;Sort data list by name, since expr=? defaults to equal? which takes
    ;;order of the datum pairs into account
    (let ((struct1 (math-object-structure obj1))
	  (data1 (sort (math-object-data obj1) expr<?))
	  (struct2 (math-object-structure obj2))
	  (data2 (sort (math-object-data obj2) expr<?)))
      (and (expr=? struct1 struct2)
	   (expr=? data1 data2)
	   #t)))
  math-object? math-object?)

