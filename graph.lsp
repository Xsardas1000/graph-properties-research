(defun flatten (X)
	(cond
		((null X)	nil)
		((atom X)	(list X))
		(T	(append (flatten (car X)) (flatten (cdr X))))
	)
)

;;the beginning of the programm - 3 options
(defun main(mod)
 	(cond
		((= mod 1) (connectivity (read)))
		((= mod 2) (bridgesAndJunctions (read)))
		((= mod 3) (euler (read)))
	)
)

;;check if the graph is euler and find euler cycle
(defun euler (L)
	(cond
		((null L) "No edges")
		((and (isConnected (findComponents (krascal L) ())) (evenVertices (allVertices (flatten L)) (flatten L)))
			        (cons "Euler cycle:" (cycle1 L (caar L) (list (caar L)) () )))
		(T "Not euler")
	)
)

;;the kraskal algorithm returns resKraskal = (loopEdges listOfSubsets spanTree) or nil if there no vertices
(defun krascal(L)
(let ((loopEdges (car (parseEdges L () ()))) (nonLoopEdges (cadr (parseEdges L () ()))))
	(cond
		((null L) nil)
		(T (step1 (deleteMeaninglessLoops loopEdges (flatten nonLoopEdges)) nonLoopEdges (allVertices (flatten L))))
	)
)
)

;;find number of bridges anf junction vertices
(defun bridgesAndJunctions (L)
(let ((components (findComponents (krascal L) ())))
	(cond
		((null L) "No edges")
		(T (list (cons "Num bridges:" (countBridges L (length components) 0 0 (length L)))
			       (cons "Num junctions:" (countJunctons L
							 																		 (allVertices (flatten L))
							                                     (length components)
																									 0
																									 ))))
	)
)
)

;;returns the number of junction vertices
(defun countJunctons (listOfEdges setOfVertices originNum count)
(let ((components (findComponents (krascal (makeNewList (car setOfVertices) listOfEdges (flatten listOfEdges))) ())))
	(cond
		((null (cdr listOfEdges)) count)
		((null setOfVertices) count)
		((> (length components) originNum) (countJunctons listOfEdges (cdr setOfVertices) originNum (+ count 1)))
		(T (countJunctons listOfEdges (cdr setOfVertices) originNum count))
	)
)
)

;;make new list after deleting edges, which are incident to the deleted vertices (if the vertice is kernel we add it)
(defun makeNewList (v listOfEdges vertices)
	(cond
		((null listOfEdges) nil)
		((and (member v (car listOfEdges)) (isKernel v vertices)) (cons (list v v) (makeNewList v (cdr listOfEdges) vertices)))
		((member v (car listOfEdges)) (makeNewList v (cdr listOfEdges) vertices))
		(T (cons (car listOfEdges) (makeNewList v (cdr listOfEdges) vertices)))
	)
)

;;returns the number of bridges
(defun countBridges (listOfEdges originNum count curNum num)
(let ((components (findComponents (krascal (cdr listOfEdges)) ())))
	(cond
		((null (cdr listOfEdges)) count)
		((= curNum num) count)
		((> (+ (length components) (addKernels (car listOfEdges) listOfEdges)) originNum)
			                      (countBridges (append (cdr listOfEdges) (list (car listOfEdges)))
			  																 	originNum
																					(+ count 1)
																					(+ curNum 1)
																					num))
		(T (countBridges (append (cdr listOfEdges) (list (car listOfEdges)))
		                  originNum
											count
											(+ curNum 1)
											num))
	)
)
)

;;if we delete kernel edge we have to add number of kernel vertices (0, 1 or 2)
(defun addKernels (edge listOfEdges)
(let ((kernels (allKernels (allVertices (flatten listOfEdges)) (flatten listOfEdges))))
	(cond
		((and (member (car edge) kernels) (member (cadr edge) kernels)) 2)
		((or (member (car edge) kernels) (member (cadr edge) kernels)) 1)
		(T 0)
	)
)
)

;;find all kernel vertices without single vertices like (v v)
(defun allKernels (setOfVertices vertices)
	(cond
		((null setOfVertices) nil)
		((isKernel (car setOfVertices) vertices) (cons (car setOfVertices) (allKernels (cdr setOfVertices) vertices)))
		(T (allKernels (cdr setOfVertices) vertices))
	)
)

;;checks if the vertice is kernel
(defun isKernel (v vertices) (= (isKernel1 v vertices 0) 1)) ;;T or nil
(defun isKernel1 (v vertices num)
	(cond
		((null vertices) num)
		((eql v (car vertices)) (isKernel1 v (cdr vertices) (+ num 1)))
		(T (isKernel1 v (cdr vertices) num))
	)
)


;;every time we chose the edge with orange color
(defun cycle1 (listOfEdges v S C)
(let ((L (bubbleEdge v listOfEdges)))
		(cond
			((null S) C)
			(T (cond
				 	 ((eql v (caar L)) (cycle1 (cdr L) (cadar L) (cons (cadar L) S) C))
					 ((eql v (cadar L)) (cycle1 (cdr L) (caar L) (cons (caar L) S) C))
					 (T (cycle1 L (cadr S) (cdr S) (cons (car S) C)))
				 )
			)
		)
)
)

;;pops the edge which is incindent to the vertice on the top (if no returns nil)
(defun bubbleEdge (v listOfEdges)
	(cond
		((null listOfEdges) nil)
		((or (eql v (caar listOfEdges)) (eql v (cadar listOfEdges))) listOfEdges)
		(T (append (bubbleEdge v (cdr listOfEdges)) (list (car listOfEdges))))
	)
)

;;returns T if all vertices have even number of edges else returns nil
(defun evenVertices (setOfVertices vertices)
	(cond
		((null setOfVertices) T)
		((= (rem (numOfVertices (car setOfVertices) vertices '0) '2) '0) (evenVertices (cdr setOfVertices) vertices))
		(T nil)
	)
)

;;returns the number of edges which are incident to the vertice
(defun numOfVertices (v vertices num)
	(cond
		((null vertices) num)
		((eql v (car vertices)) (numOfVertices v (cdr vertices) (+ num '1)))
		(T (numOfVertices v (cdr vertices) num))
	)
)

(defun connectivity (L)
(let ((components (findComponents (krascal L) ())))
	(cond
		((null L) "No vertices")
		((isConnected components) (cons "Connected graph:" (cons (car (func2 components L)) (func (func2 components L)))))  ;;return the spanning tree
		(T (cons "Not Connected graph:" (cons (func2 components L) (func (func2 components L)))))  ;;return all components
	)
)
)

;;returns components for each karkas
(defun func2 (karkases L)
	(cond
		((null karkases) nil)
		(T (cons (func3 (car karkases) L) (func2 (cdr karkases) L)))
	)
)

;;returns component for  karkas
(defun func3 (karkas L)
	(cond
		((null L) nil)
		((or (member (caar L) (flatten karkas)) (member (cadar L) (flatten karkas))) (cons (car L) (func3 karkas (cdr L))))
		(T (func3 karkas (cdr L)))
	)
)

;;returns bridgesAndJunctions for each component
(defun func (components)
	(cond
		((null components) nil)
		(T (cons (bridgesAndJunctions (car components)) (func (cdr components))))
	)
)

;;input: components
(defun isConnected (components)
	(cond
		((null (cdr components)) T) ;;only one component
		(T nil)
	)
)

;;input: (loopEdges listOfSubsets spanTree) components=()
;;returns the list of components
(defun findComponents (resKraskal components)
(let ((loopEdges (car resKraskal)) (listOfSubsets (cadr resKraskal)) (spanTree (caddr resKraskal)))
	(cond
		((null resKraskal) nil) ;;no vertices
		((null (cdr resKraskal)) loopEdges) ;;only loopEdges
		((null listOfSubsets) (append loopEdges components))
		(T (findComponents
					(cons loopEdges (list (cdr listOfSubsets) spanTree))
					(cons (oneComponent (car listOfSubsets) spanTree) components)))
	)
)
)

;;returns the corresponding component of the subset
(defun oneComponent (subset spanTree)
	(cond
		((null spanTree) nil)
		((or (member (caar spanTree) subset) (member (cadar spanTree) subset))
				(cons (car spanTree) (oneComponent subset (cdr spanTree))))
		(T (oneComponent subset (cdr spanTree)))
	)
)






(defun parseEdges (L loopEdges nonLoopEdges)
	(cond
		((null L) (list loopEdges nonLoopEdges))
		((eql (caar L) (cadar L)) (parseEdges (cdr L) (cons (caar L) loopEdges) nonLoopEdges))
		(T (parseEdges (cdr L) loopEdges (cons (car L) nonLoopEdges)))
	)
)

(defun deleteMeaninglessLoops (loopEdges vertices)
	(cond
		((null loopEdges) nil)
		((member (car loopEdges) vertices) (deleteMeaninglessLoops (cdr loopEdges) vertices))
		(T (cons (car loopEdges) (deleteMeaninglessLoops (cdr loopEdges) vertices)))
	)
)

;;make the set of vertices, input: (v1 v2 v1 v3 v3 v4 ...), output: (v1 v2 v3 v4...)
(defun allVertices(L)
  (cond
    ((null L) nil)
    ((atom L) L)
    (T (cons (car L) (allVertices (removeDups (car L) (cdr L)))))
  )
)

(defun removeDups(x L)
  (cond
    ((null L) nil)
    ((eql (car L) x) (removeDups x (cdr L)))
    (T (cons (car L) (removeDups x (cdr L))))
  )
)





;;the first step in kraskal algorithm
(defun step1(loopEdges nonLoopEdges setOfVertices)
  (cond
		((null nonLoopEdges) (list loopEdges))  ;;Only loop edges
    (T (cons loopEdges (step2  (cdr nonLoopEdges)  (list (car nonLoopEdges)) (list (car nonLoopEdges)) setOfVertices)))
  )
)

;;input: 1st par = list of edges, first edge has color blue (1), 2nd par = list of Subsets with numeration
;;which consists of subset of the first edge: ((v1 v2)), 3rd par = spanning tree (list of blue edges)
(defun step2(listOfEdges listOfSubsets spanTree setOfVertices)
  (cond
    ((null listOfEdges) (list listOfSubsets spanTree)) ;;return all blue edges and list of subsets
    ((switch (caar listOfEdges)
		 				 (cadar listOfEdges)
						 (cdr listOfEdges)
						 listOfSubsets
						 spanTree
						 setOfVertices))
  )
)

;;there are 4 cases in kraskal algorithm
(defun switch(v1 v2 listOfEdges listOfSubsets spanTree setOfVertices)
	(let ((list1 (bubbleSubset v1 listOfSubsets)) (list2 (bubbleSubset v2 listOfSubsets)))
  (cond
    ((and (member v1 (car list1))
		      (member v2 (car list1)))
						    			 (step2 listOfEdges listOfSubsets spanTree setOfVertices))
		((and (member v1 (car list1))
		      (not (member v2 (car (bubbleSubset v2 (cdr list1))))))
					             (step2 listOfEdges
											 				(cons (cons v2 (car list1)) (cdr list1))
											 				(cons (list v1 v2) spanTree)
											 				setOfVertices))
		((and (member v2 (car list2))
					(not (member v1 (car (bubbleSubset v1 (cdr list2))))))
										 	 (step2 listOfEdges
										 			    (cons (cons v1 (car list2)) (cdr list2))
										 					(cons (list v1 v2) spanTree)
															setOfVertices))
		((and (member v1 (car list1)) (member v2 (car list2)))
		  								 (step2 listOfEdges
											 				(cons (append (car list1) (car (bubbleSubset v2 (cdr list1)))) (cdr (bubbleSubset v2 (cdr list1))))
															(cons (list v1 v2) spanTree)
															setOfVertices))
		((and (not (member v1 (car list1))) (not (member v2 (car list2))))
											 (step2 listOfEdges
											 				(cons (list v1 v2) listOfSubsets)
															(cons (list v1 v2) spanTree)
															setOfVertices))
  )
  )
)

;;the check if the sets are equal (contain similar elements)
(defun check(set1 set2)
	(cond
		((null set1) T)
		((member (car set1) set2) (check (cdr set1) set2))
		(T nil)
	)
)

;;pops the subset which member the vertice on the top
(defun bubbleSubset(v listOfSubsets)
	(cond
		((null listOfSubsets) nil)
		((member v (car listOfSubsets)) listOfSubsets)
		(T (append (bubbleSubset v (cdr listOfSubsets)) (list (car listOfSubsets))))
	)
)
