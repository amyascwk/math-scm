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
	  (if (graph/consistent-edges? vertex-set edges)
	      (make-math-object
	       (list (list 'vertex-set vertex-set)
		     (list 'edges edges)))
	      (error "Edges not consistent with vertex set"))
	  (error "Edges are not valid:" edges))
      (error "Vertex set is not a set:" vertex-set)))

;;; Test if input is graph
(define (graph? obj)
  (and (math-object? obj)
       (has-math-property? obj 'vertex-set)
       (has-math-property? obj 'edges)))

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
      (get-math-property graph 'vertex-set)
      (error "Not a graph:" graph)))

;;; Get edge list
(define (graph/edges graph)
  (if (graph? graph)
      (get-math-property graph 'edges)
      (error "Not a graph:" graph)))

;;; Get order, or number of vertices
(define (graph/order graph)
  (set/cardinality (graph/vertex-set graph)))

;;; Get size, or number of edges
(define (graph/size graph)
  (length (graph/edges graph)))

;;; ##########################################################################
;;; Properties

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
  (and (not (graph/contains-multi-edges? graph))
       (equal? (graph/uniform? graph) 2)))

;;; If graph is k-uniform for some k, returns k, else returns #f
(define (graph/uniform? graph)
  (let ((edges (graph/edges graph)))
    (let ((size-edges (map set/cardinality edges)))
      (if (equal? (set/cardinality (list->set size-edges)) 1)
	  (car size-edges)
	  #f))))

;;; Compute degree of a vertex
(define (graph/get-vertex-degree graph vertex)
  (let ((edges (graph/edges graph))
	(count 0))
    (for-each (lambda (x)
		(set! count
		      (+ count 
			 (length (filter (lambda (y)
					   (equal? vertex y))
					 (set->list x))))))
	      edges)
    count))

;;; Compute maximum degree in the graph
(define (graph/get-max-degree graph)
  (let ((vertex-set (graph/vertex-set graph))
	(max-degree 0))
    (for-each (lambda (x)
		(let ((degree (graph/get-vertex-degree graph x)))
		  (if (> degree max-degree)
		      (set! max-degree degree))))
	      (set->list vertex-set))
    max-degree))

;;; Compute minimum degree in the graph
(define (graph/get-min-degree graph)
  (let ((vertex-set (graph/vertex-set graph))
	(min-degree (* 2 (graph/size graph))))
    (for-each (lambda (x)
		(let ((degree (graph/get-vertex-degree graph x)))
		  (if (< degree min-degree)
		      (set! min-degree degree))))
	      (set->list vertex-set))
    min-degree))

;;; Returns degree of vertices if graph is regular, and #f otherwise
(define (graph/regular? graph)
  (let ((degree (graph/get-max-degree graph)))
    (if (equal? degree
		(graph/get-min-degree graph))
	degree
	#f)))

;;; Test if graph is complete
(define (graph/complete? graph)
  (and (graph/simple? graph)
       (equal? (graph/regular? graph)
	       (- (graph/order graph) 1))))

;;; ##########################################################################
;;; Tests

(let ((triangle (make-graph (make-set 0 1 2)
			    (make-edges '(0 1) '(0 2) '(1 2)))))
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
