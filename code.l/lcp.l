: (pp 'lcp)                                                                        
(de lcp (L)
   (map
      '((SL)
         (if (cdr SL)
            (let
               (L1 (car SL)
                  L2 (cadr SL)
                  Cnt (min (length (car L1)) (length (car L2)))
                  N 1 )
               (do Cnt
                  (T
                     (<>
                        (car (nth (car L1) N))
                        (car (nth (car L2) N)) ) )
                  (inc 'N) )
               (set (cdddr L2) (dec N)) ) ) )
      L ) )
-> lcp
: (nil (setq *BitsSA (by car sort *BitsA)))                                        
-> NIL
: (nil (setq *BitsSA (mapcar '((E) (make (chain (head 3 E) (list NIL)))) *BitsSA)))
-> NIL
: (for N (head 10 *BitsSA) (println N))                                            
((1) 8228 1 NIL)
((2 1) 2146 0 NIL)
((2 2 1) 528 1 NIL)
((2 2 2 1) 127 2 NIL)
((2 2 2 2 1) 44 3 NIL)
((2 2 2 2 2 1) 6 4 NIL)
((2 2 2 2 2 2 1) 2 5 NIL)
((2 2 2 2 2 2 2 1) 1 6 NIL)
((2 2 2 2 2 2 4 3 1) 1 6 NIL)
((2 2 2 2 2 3 1) 1 5 NIL)
-> ((2 2 2 2 2 3 1) 1 5 NIL)
: (edit 'lcp)                                                                      
-> NIL
: (lcp *BitsSA)                                                                    
-> NIL
: (for N (head 10 *BitsSA) (println N))
((1) 8228 1 NIL)
((2 1) 2146 0 0)
((2 2 1) 528 1 1)
((2 2 2 1) 127 2 2)
((2 2 2 2 1) 44 3 3)
((2 2 2 2 2 1) 6 4 4)
((2 2 2 2 2 2 1) 2 5 5)
((2 2 2 2 2 2 2 1) 1 6 6)
((2 2 2 2 2 2 4 3 1) 1 6 6)
((2 2 2 2 2 3 1) 1 5 5)
-> ((2 2 2 2 2 3 1) 1 5 5)
: (mapc '((E) (let (P (car (nth E 4))) (if (> P 0) (set (nth E 1) (tail (* P -1) (car E)))))) *BitsSA)
-> NIL
: (for N (head 10 *BitsSA) (println N))                                                               
((1) 8228 1 NIL)
((2 1) 2146 0 0)
((2 1) 528 1 1)
((2 1) 127 2 2)
((2 1) 44 3 3)
((2 1) 6 4 4)
((2 1) 2 5 5)
((2 1) 1 6 6)
((4 3 1) 1 6 6)
((3 1) 1 5 5)
-> ((3 1) 1 5 5)
: 
