;;;; ############################################################################################
;;;; ############################################################################################
;;;; Generic Comparators

;;; #############################################################################################
;;; Specific comparators for scheme objects

;;; Boolean comparison
(define (boolean<? b1 b2)
  (and (not b1) b2))

;;; Pair comparison
(define (pair<? p1 p2)
  (or (expr<? (car p1) (car p2))
      (and (not (expr<? (car p2) (car p1)))
	   (expr<? (cdr p1) (cdr p2)))))

;;; Vector comparison
(define (vector<? v1 v2)
  (expr<? (vector->list v1) (vector->list v2)))

;;; Bit string comparison
(define (bit-string<? bs1 bs2)
  (expr<? (bit-string->unsigned-integer bs1)
	  (bit-string->unsigned-integer bs1)))

;;; 1D table comparison
(define (1d-table<? t1 t2)
  (expr<? (1d-table/alist t1) (1d-table/alist t2)))

;;; Hash table comparison
(define (hash-table<? ht1 ht2)
  (expr<? (hash-table->alist ht1) (hash-table->alist ht1)))

;;; Red-black tree comparison
(load-option 'rb-tree)

(define (rb-tree<? rb1 rb2)
  (expr<? (rb-tree->alist rb1) (rb-tree->alist rb1)))

;;; Weight-balanced tree comparison
(load-option 'wt-tree)

(define (wt-tree->alist wt-tree)
  (wt-tree/fold (lambda (key datum list)
		  (cons (list key datum) list))
		'() wt-tree))

(define (wt-tree<? wt1 wt2)
  (expr<? (wt-tree->alist wt1) (wt-tree->alist wt2)))

;;; Procedure comparison
(define (procedure<? proc1 proc2)
  (expr<? (hash proc1) (hash proc2)))

;;; Type order comparator
;;; Order according to position in type priority list
(define (type<? obj1 obj2)
  (let loop ((type-list type-priority-list))
    (if (null? type-list)
	(error "Unknown type comparison:" obj1 obj2)
	(let ((predicate (car type-list)))
	  (cond ((predicate obj1)
		 (if (predicate obj2)
		     #f
		     #t))
		((predicate obj2) #f)
		(else (loop (cdr type-list))))))))

;;; Ascending priority order of types
(define type-priority-list
  (list boolean? number? symbol? char? string? null? pair? vector?
	bit-string? 1d-table? hash-table? rb-tree? wt-tree? procedure?))


;;; #############################################################################################
;;; Generic type comparator

;;; Generic less than operator
;;; Imposes a deterministic total ordering on scheme objects
;;; Required as a comparator that ignores the property field in math-objects
(define expr<?
  (make-generic-operator 2 'expr<? type<?))

(defhandler expr<? boolean<?
  boolean? boolean?)

(defhandler expr<? <
  number? number?)

(defhandler expr<? symbol<?
  symbol? symbol?)

(defhandler expr<? char<?
  char? char?)

(defhandler expr<? string<?
  string? string?)

(defhandler expr<? pair<?
  pair? pair?)

(defhandler expr<? vector<?
  vector? vector?)

(defhandler expr<? bit-string<?
  bit-string? bit-string?)

(defhandler expr<? 1d-table<?
  1d-table? 1d-table?)

(defhandler expr<? hash-table<?
  hash-table? hash-table?)

(defhandler expr<? rb-tree<?
  rb-tree? rb-tree?)

(defhandler expr<? wt-tree<?
  wt-tree? wt-tree?)

(defhandler expr<? procedure<?
  procedure? procedure?)

;;; Generic equality operator
(define (expr=? x y)
  (and (not (expr<? x y))
       (not (expr<? y x))))


;;; Tests =======================================================================================
(test-true (expr<? 1 2))
(test-false (expr<? 1 1))
(test-false (expr<? 2 1))
(test-true (expr<? 1 'a))
(test-false (expr<? 'a 1))
(test-true (expr<? 'a 'b))
(test-false (expr<? 'a 'a))
(test-false (expr<? 'b 'a))
(test-true (expr<? 'a #\a))
(test-false (expr<? #\a 'a))
(test-true (expr<? #\a #\b))
(test-false (expr<? #\a #\a))
(test-false (expr<? #\b #\a))
(test-true (expr<? #\a "a"))
(test-false (expr<? "a" #\a))
(test-true (expr<? "a" "b"))
(test-false (expr<? "a" "a"))
(test-false (expr<? "b" "a"))
(test-true (expr<? "a" '()))
(test-false (expr<? '() "a"))
(test-false (expr<? '() '()))
(test-true (expr<? '() '(1 . 2)))
(test-false (expr<? '(1 . 2) '()))
(test-true (expr<? '(1 . 2) '(1 2)))
(test-false (expr<? '(1 . 2) '(1 . 2)))
(test-false (expr<? '(1 2) '(1 . 2)))
(test-true (expr<? '(1 2) '(1 2 3)))

(test-true (expr=? 1 1))
(test-true (expr=? 'a 'a))
(test-true (expr=? #\a #\a))
(test-true (expr=? "a" "a"))
(test-true (expr=? '() '()))
(test-true (expr=? '(1 2) '(1 2)))
(test-true (expr=? '#(1 2) '#(1 2)))
(test-true (expr=? '#*000 '#*000))


