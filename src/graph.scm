;;;; Graph (finite, unweighted, undirected)

;;; ##########################################################################
;;; Datatype mthods

;;; Primitive constructor
;;; vertex-set is a set of the vertices
;;; edges is a list of sets, each set consists of elements from vertex-set
(define (make-graph vertex-set edges)
  (if (set? vertex-set)
      (if (and (pair? edges)
	       (every set? edges))
	  ;; check if edges consist only of elements from vertex-set
	  (if (graph/consistent-edges? vertex-set edges)
	      (make-math-object
	       'graph
	       (list (list 'vertex-set vertex-set)
		     (list 'edges edges))
	       '())
	      (error "Edges not consistent with vertex set"))
	  (error "Edges are not valid:" edges))
      (error "Vertex set is not a set:" vertex-set)))

;;; Test if input is graph
(define (graph? obj)
  (and (math-object? obj)
       (eq? (math-object-structure obj) 'graph)))

;;; Make edges from lists of vertices
(define (make-edges .  edges)
  (map list->set edges))

;;; Test if edges are consistent with vertex set
(define (graph/consistent-edges? vertex-set edges)
  (every (lambda (x)
	   (set/subset? x vertex-set))
	 edges))

;;; Get vertex set
(define (graph/vertex-set graph)
  (if (graph? graph)
      (get-math-datum graph 'vertex-set)
      (error "Not a graph:" graph)))

;;; Get edge list
(define (graph/edges graph)
  (if (graph? graph)
      (get-math-datum graph 'edges)
      (error "Not a graph:" graph)))

      
;;; ##########################################################################
;;; Properties

;;; Get order, or number of vertices
(define (graph/order graph)
  ;; Check if 'order property has been set
  (if (has-math-property? graph 'order)
      ;; property set, so return its value
      (get-math-property graph 'order)
      ;; property not set, so check its value and return it
      (let ((order (set/cardinality (graph/vertex-set graph))))
	(set-math-property! graph 'order order)
	order)))

;;; Get size, or number of edges
(define (graph/size graph)
  ;; Check if 'size property has been set
  (if (has-math-property? graph 'size)
      ;; property set, so return its value
      (get-math-property graph 'size)
      ;; property not set, so check its value and return it
      (let ((size (length (graph/edges graph))))
	(set-math-property! graph 'size size)
	size)))

;;; Test if graph contains loops
(define (graph/contains-loops? graph)
  (let ((edges (graph/edges graph)))
    (not (every (lambda (x)
		  (> (set/cardinality x)
		     1))
		edges))))

;;; Test if graph contains multi-edges
(define (graph/contains-multi-edges? graph)
  (let ((edges (graph/edges graph)))
    (not (equal? (set/cardinality 
		  (list->set edges))
		 (length edges)))))

;;; Test if graph is simple
(define (graph/simple? graph)
  ;; Check if 'simple? property has been set
  (if (not (has-math-property? graph 'simple?))
      ;; property not set, check value and set it
      (set-math-property! 
       graph 'simple?
       (and (not (graph/contains-multi-edges? graph))
	    (equal? (graph/uniform? graph) 2))))
  (get-math-property graph 'simple?))

;;; If graph is k-uniform for some k, returns k, else returns #f
(define (graph/uniform? graph)
  ;; Check if 'uniform? property has been set
  (if (not (has-math-property? graph 'uniform?))
      ;; property not set, check value and return it
      (set-math-property!
       graph 'uniform?
       (let ((edges (graph/edges graph)))
	 (let ((size-edges (map set/cardinality edges)))
	   ;; check if all edges have same size
	   (if (equal? (set/cardinality (list->set size-edges)) 1)
	       (car size-edges)
	       #f)))))
  (get-math-property graph 'uniform?))

;;; Compute degree of a vertex
(define (graph/get-vertex-degree graph vertex)
  (let ((edges (graph/edges graph))
	(count 0))
    (for-each (lambda (x)
		;; check if vertex is in edge
		(if (set/member? vertex x)
		    ;; increment count for each edge that contains vertex
		    (set! count (+ count 1))))
	      edges)
    count))

;;; Compute maximum degree in the graph
(define (graph/get-max-degree graph)
  ;; Check if 'max-degree property has been set
  (if (not (has-math-property? graph 'max-degree))
      ;; property not set, check value and return it
      (set-math-property!
       graph 'max-degree
       (let ((vertex-set (graph/vertex-set graph))
	     (max-degree 0))
	 (for-each (lambda (x)
		     (let ((degree (graph/get-vertex-degree graph x)))
		       (if (> degree max-degree)
			   (set! max-degree degree))))
		   (set->list vertex-set))
	 max-degree)))
  (get-math-property graph 'max-degree))

;;; Compute minimum degree in the graph
(define (graph/get-min-degree graph)
  ;; Check if 'min-degree property has been set
  (if (not (has-math-property? graph 'min-degree))
      ;; property not set, check value and return it
      (set-math-property!
       graph 'min-degree  
       (let ((vertex-set (graph/vertex-set graph))
	     (min-degree (* 2 (graph/size graph))))
	 (for-each (lambda (x)
		     (let ((degree (graph/get-vertex-degree graph x)))
		       (if (< degree min-degree)
			   (set! min-degree degree))))
		   (set->list vertex-set))
	 min-degree)))
  (get-math-property graph 'min-degree))

;;; Returns degree of vertices if graph is regular, and #f otherwise
(define (graph/regular? graph)
  ;; Check if 'regular property has been set
  (if (not (has-math-property? graph 'regular))
      ;; property not set, check value and return it
      (set-math-property!
       graph 'regular
       (let ((degree (graph/get-max-degree graph)))
	 (if (equal? degree
		     (graph/get-min-degree graph))
	     degree
	     #f))))
  (get-math-property graph 'regular))

;;; Test if graph is complete
(define (graph/complete? graph)
  ;; Check if 'complete property has been set
  (if (not (has-math-property? graph 'complete))
      ;; property not set, check value and return it
      (set-math-property!
       graph 'complete
       (and (graph/simple? graph)
	    (equal? (graph/regular? graph)
		    (- (graph/order graph) 1)))))
  (get-math-property graph 'complete))

;;; ##########################################################################
;;; Tests

(let ((triangle (make-graph (make-set 0 1 2)
			    (make-edges '(0 1) '(0 2) '(1 2)))))
  (test-equal (graph/order triangle) 3)
  (test-equal (graph/size triangle) 3)
  (test-equal (graph/uniform? triangle) 2)
  (test-true (graph/simple? triangle))
  (test-true (graph/regular? triangle))
  (test-true (graph/complete? triangle)))

(let ((path (make-graph (make-set 0 1 2 3 4)
			(make-edges '(0 1) '(1 2) '(2 3) '(3 4)))))
  (test-true (graph/simple? path))
  (test-false (graph/regular? path))
  (test-false (graph/complete? path)))

(let ((cycle (make-graph (make-set 0 1 2 3 4 5)
			 (make-edges '(0 1) '(1 2) '(2 3) '(3 4) '(4 5) '(5 0)))))
  (test-true (graph/simple? cycle))
  (test-true (graph/regular? cycle))
  (test-false (graph/complete? cycle)))

(let ((k4 (make-graph (make-set 0 1 2 3)
		      (make-edges '(0 1) '(0 2) '(0 3) '(1 2) '(1 3) '(2 3)))))
  (test-true (graph/simple? k4))
  (test-true (graph/regular? k4))
  (test-true (graph/complete? k4)))

(let ((loop (make-graph (make-set 0 1 2)
			(make-edges '(0 0) '(1 2)))))
  (test-true (graph/contains-loops? loop))
  (test-false (graph/simple? loop)))

(let ((multi-edges (make-graph (make-set 1 2)
			       (make-edges '(1 2) '(1 2)))))
  (test-true (graph/contains-multi-edges? multi-edges))
  (test-false (graph/simple? multi-edges)))

;;; We can also construct hypergraphs!
(let ((hypergraph (make-graph (make-set 0 1 2 3 4)
			      (make-edges '(0) '(1 2 3) '(2 4)))))
  (test-false (graph/simple? hypergraph)))

(let ((hypergraph2 (make-graph (make-set 0 1 2 3)
			       (make-edges '(0 1 2) '(0 1 3) '(0 2 3) '(1 2 3)))))
  (test-false (graph/simple? hypergraph2))
  (test-true (graph/regular? hypergraph2))
  (test-equal (graph/uniform? hypergraph2) 3))
