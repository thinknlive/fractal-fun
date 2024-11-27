(de plotLSys (L)
  (mapc prin L) )

(de fnLSys (L)
   (fish atom (mapcar
      '((X)
         (cond
            ((= X "A") A)
            ((= X "B") B)
            (T X))) L) ) )


(de runLSys (L D)
  (if (>= D MaxDepth)
    (plotLSys L)
    (map '((X)
      (runLSys
        (fnLSys (list (car X)))
        (inc D))) L)) )

(de gosperR (MaxDepth)
  (let
    (A (chop "A-B--B+A++AA+B-")
     B (chop "+A-BB--B-A++A+B"))

    (default MaxDepth 1)

    (runLSys A 1)))
	 
  (let
    (A (chop "A-B--B+A++AA+B-")
     B (chop "+A-BB--B-A++A+B"))

(setq Bits (make (do N (link (rand 0 1)))))
	  
(de testb (Bits)
   (use (Bits X Nxt VR A B)
		(setq
			A (chop "A-B--B+A++AA+B-")
			B (chop "+A-BB--B-A++A+B"))
		(setq VR (make
			(while Bits
				(for X (chop "A-B--B+A++AA+B-")
					(setq Nxt
						(cond 
							((or (= "A" X) (= "B" X))
								(setq Bit (car Bits) Bits (cdr Bits))
								(cond
									((= 0 Bit) (list X))
									((= 1 Bit) (if (= "A" X) A B))
									(T (list X) )))
							(T (list X))))
					(link Nxt)))))
		VR ))
			
(de testb1 (Bits)
   (use (Bits X Nxt VR A B)
		(setq
			A (chop "A-B--B+A++AA+B-")
			B (chop "+A-BB--B-A++A+B"))
		(setq VR (make
			(while Bits
				(for X (chop "A-B--B+A++AA+B-")
					(setq Nxt
						(cond 
							((or (= "A" X) (= "B" X))
								(setq Bit (car Bits) Bits (cdr Bits))
								(cond
									((= 0 Bit) (list X))
									((= 1 Bit) (if (= "A" X) A B))
									(T (list X) )))
							(T (list X))))
					(link Nxt)))))
		VR ))


(de testc (Bits)
	(gosperGen T)
   (use (X Nxt VR A B)
		(setq
			A (chop "A-B--B+A++AA+B-")
			B (chop "+A-BB--B-A++A+B"))
		(setq VR (make
			(while Bits
				(setq X (gosperGen))
				(setq Nxt
					(cond 
						((or (= "A" X) (= "B" X))
							(setq Bit (car Bits) Bits (cdr Bits))
							(cond
								((and (= "A" X) (= 0 Bit)) X)
								((and (= "B" X) (= 1 Bit)) X)
								((and (= "B" X) (= 0 Bit)) "A") 
								((and (= "A" X) (= 1 Bit)) "B")
								(T "?" )))
						(T X))
				(link Nxt)))))
		VR ))


			