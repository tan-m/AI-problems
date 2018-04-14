(defun missionaries (missionaries cannibals boatCapacity)
	
	(setq lc 0)
	(setq lm 0)
	(setq rc missionaries)
	(setq rm cannibals)
	(setq bc 0)
	(setq bm 0)
	(setq max boatCapacity)
	

	; transfer the maximum number of cannibals to the left
	(setq rc (- rc max))
	(setq bc max)
	
	(format t "lc: ~a lm: ~a ||		(bc: ~a bm: ~a) || rc: ~a rm: ~a ~%" lc lm bc bm rc rm)
	
	(setq lc bc)
	(setq bc 0)

	(format t "lc: ~a lm: ~a || (bc: ~a bm: ~a)		|| rc: ~a rm: ~a ~%" lc lm bc bm rc rm)

	; the half of the boat capacity
	(setq half (ceiling (/ max 2)))

	(loop
		; break loop when right side is empty
		(if (and ( <= rc  0) (<= rm  0) )
			(return nil)
		)

		
		(cond 
			; if on the right everything is balanced then send one C and one M
			((= rc rm)  ( progn
							(setq lc (- lc 1))
							(setq lm (- lm 1))
							(setq bc 1)
							(setq bm 1)

							(format t "lc: ~a lm: ~a || (bc: ~a bm: ~a)		|| rc: ~a rm: ~a ~%" lc lm bc bm rc rm)
							
						)
			)
			; Cannibals are less than missionaries on the right. Just send one cannibal.
			((< rc  rm) (progn
							(setq lc (- lc 1))
							(setq bc (+ bc 1))
							(format t "lc: ~a lm: ~a || (bc: ~a bm: ~a)		|| rc: ~a rm: ~a ~%" lc lm bc bm rc rm)
						)
			)
		) ; end of cond

		; common block to update the right side and reset the boat
		(setq rc (+ rc bc) )				
		(setq rm (+ rm bm) )
		(setq bc 0)
		(setq bm 0)				

		(format t "lc: ~a lm: ~a ||		(bc: ~a bm: ~a) || rc: ~a rm: ~a ~%" lc lm bc bm rc rm)



		(cond
			((= lc lm) (progn

							(if (and (< rc half)(< rm half))
								; send all of them
								(progn
									(setq bc rc)
									(setq bm rm)
									(setq rc 0)
									(setq rm 0)
								)
								; fill the boat with half cannibals and half missionaries
								(progn
									(setq bc half)
									(setq bm half)
									(setq rc (- rc half))
									(setq rm (- rm half))
								)
							)

							(format t "lc: ~a lm: ~a ||		(bc: ~a bm: ~a) || rc: ~a rm: ~a ~%" lc lm bc bm rc rm)		
													
						)
			)

			(t (progn
					; send as many missionaries (from the right to the left ) as the number of cannibals to the left
					(setq bm lc)
					(setq rm (- rm lc))

					(format t "lc: ~a lm: ~a ||		(bc: ~a bm: ~a) || rc: ~a rm: ~a ~%" lc lm bc bm rc rm)	

				)
			)
		) ; end of cond 

		; common block to update the left side and reset the boat
		(setq lc (+ lc bc))
		(setq lm (+ lm bm))
		(setq bc 0)
		(setq bm 0)

		(format t "lc: ~a lm: ~a || (bc: ~a bm: ~a)		|| rc: ~a rm: ~a ~%" lc lm bc bm rc rm)	

	) ; end of loop
	
	
)

