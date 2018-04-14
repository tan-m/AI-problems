
;(astar '(1 2 3 5 6 0 7 8 4) '(1 2 3 5 8 6 0 7 4) )


; given the position of the e and the position to swap with, swap them

(defun swap (neighbour swapwith e_pos)
	(setq ele (nth swapwith neighbour))
	(setf (nth e_pos neighbour) ele)
	(setf (nth swapwith neighbour) 'e)
)

; function to generate the successors
; each successor is a tuple (node, distFromCurrentToThis)
(defun successors (state)

	(setq e_pos (position 'e state) )
	(setq swap-list (case e_pos
			  ((0) '(1 3))
			  ((1) '(0 2 4))
			  ((2) '(1 5))
			  ((3) '(0 4 6))
			  ((4) '(1 3 5 7))
			  ((5) '(2 4 8))
			  ((6) '(3 7))
			  ((7) '(4 6 8))
			  ((8) '(5 7))))
	(setq neighbourList nil)
	(dolist (swapwith swap-list)
		(setq neighbour (copy-list state) )
		(swap neighbour swapwith e_pos)
		(setq neighbour (cons neighbour 1))
		(if (null neighbourList)
			(setq neighbourList (list neighbour))
			(setq neighbourList (cons neighbour neighbourList))
		)
	)
	neighbourList			  
)


; heuristic is the number of missplaced tiles
(defun heuristic-dist (start goal) 
    	(setq len (- (length start) 1))
    	(setq count 0)
    	(loop
    		(if (< len 0) (return count))
    		(if (not (equalp (nth len start) (nth len goal)))
    			(setq count (+ count 1))
    		)
    		(setq len (- len 1))
    	)
)


; I have written the entire A-star code, by refering to the wikipedia page on A-star and following the pseudo-code.
; https://en.wikipedia.org/wiki/A*_search_algorithm (last accessed Dec 7, 2017)


; To trace back the path followed.
(defun reconstructPath (cameFrom current)
	(print "The states change as follows:")
	(setq total_path (list current))
	(loop
		(setq current (gethash current cameFrom))
		(if (null current) (return nil) )
		(setq total_path (cons current total_path))
	)
	(print total_path)
	(print "Length of optimal path")
	(print (length total_path))
)


(defun astar(start goal)

	(progn
		
		; The set of nodes already evaluated
		(setq closedSet nil) 
		
		; The set of nodes yet to be evaluated
		(setq openSet (list start)) 
		
		; For each node, the previous node in the optimal path. 
		(setq cameFrom (make-hash-table :test #'equalp)) 
		
		; For each node, the cost of getting from the start node to that node.
		(setq gScore (make-hash-table :test #'equalp)) 
		
		; For each node, the total cost of getting from the start node to the goal by passing by that node. 
		; This value is sum of current cost and estimate form current node to goal
		(setq fScore (make-hash-table :test #'equalp)) 
		
		; Setting the values for the start node
		(setf (gethash start fscore) (heuristic-dist start goal) )
		(setf (gethash start gScore) 0 )
		
		
		
		(loop
			
			; if openSet is empty exit loop. Goal state isnt reachable
			(if (null openSet) (return nil))
			
			; Pick the node with least fScore
			(sort openSet #'< :key (lambda (p) (gethash p fScore) ))	
			(setq current (pop openSet))
			
			; we reach the goal state! 
			(if (equalp current goal) 
				(progn
					(reconstructPath cameFrom current)
					(return nil)
				)
			)
			
			; we add the current to the closedSet
			(if (null closedSet) 
				(setq closedSet (list current)) 
				(setq closedSet (cons current closedSet))
			)
			
			; the successor function will give pairs of (neighbour, distanceToNeighbour)
			(dolist (duo (successors current))
				
				(setq neighbour (first duo))
	  			(setq neighbourDist (rest duo))
	  			
	  			; if neighbour is in closedSet, do nothing.
	  			(if (null (find neighbour closedSet :test #'equalp))
	  				(progn
	  					
	  					; if neighbour is not in openSet add to openSet
	  					(if ( null (find neighbour openSet :test #'equalp))
	  						(setq openSet (cons neighbour openSet))
	  					)
	  					
	  					; The distance from start to a neighbor
	  					(setf tentative_gScore (+ (gethash current gScore)  neighbourDist ))
	  					
	  					; check if this path is better than the existing
	  					(if (or (null (gethash neighbour gScore)) (< tentative_gScore (gethash neighbour gScore) ))
	  						(progn
		  						(setf (gethash neighbour cameFrom) current)
		  						(setf (gethash neighbour gScore) tentative_gScore)
		  						(setf (gethash neighbour fScore) (+ tentative_gScore (heuristic-dist neighbour goal) )) 
	  						)
	  					)
	  				)
				
				)
		
			) ; end of dolist
	
		) ; end of loop

	)
)
