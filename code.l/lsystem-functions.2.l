(de gosperGenL (D) 
   (if (=T D)
      (co 'gospergenl)
      (co 'gospergenl
         (let
            (Cnt 0
					Bit NIL
					A (chop "A-B--B+A++AA+B-")
               B (chop "+A-BB--B-A++A+B")
               _MaxDepth D
               _Plot '((L) (mapc yield L))
               _Fn
               '((X)
						(setq Bit (car *Bits) Bits (cdr *Bits) Cnt (inc Cnt))
						(if (not Bit) (setq _MaxDepth 0))
                  (cond
                     ((and Bit (= X "A")) 
								(if (=0 Bit) 
									"A" 
									(msg (text "Cnt: @1 A: @2" Cnt A))
									A))
                     ((and Bit (= X "B")) 
								(if (=0 Bit) 
									"B" 
									(msg (text "Cnt: @1 B: @2" Cnt B))
									B))
                     (T (list X)) ) )
               _L-Run
               '((L D)
                  (if (or (>= D _MaxDepth) (not *Bits))
                     (_Plot L)
                     (mapc
                        '((X) (_L-Run (_Fn X) (inc D)))
                        L ) ) ) )
            (default _MaxDepth 13)
            (_L-Run A 1) ) ) ) )
		 
(de gosperGen (D)
   (if (=T D)
      (co 'gospergen)
      (co 'gospergen
         (let
            (A (chop "A-B--B+A++AA+B-")
               B (chop "+A-BB--B-A++A+B")
               _MaxDepth D
               _Plot '((L) (mapc yield L))
               _Fn
               '((X)
                  (cond
                     ((= X "A") A)
                     ((= X "B") B)
                     (T (list X)) ) )
               _L-Run
               '((L D)
                  (if (>= D _MaxDepth)
                     (_Plot L)
                     (mapc
                        '((X) (_L-Run (_Fn X) (inc D)))
                        L ) ) ) )
            (default _MaxDepth 13)
            (_L-Run A 1) ) ) ) )
		 
		 
