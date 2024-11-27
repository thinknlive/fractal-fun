: (pp 'plotLSys)  
(de plotLSys (L)
   (cond
      ((= (head (length A0) L) A0)
         (nil '(mapc prin L)) )
      ((= (head (length B0) L) B0)
         (nil '(mapc prin L)) )
      (T (nil '(mapc prin L))) ) )
-> plotLSys
: (pp 'fnLSys)  
(de fnLSys (L)
   (let (V NIL)
      (fish
         atom
         (mapcar
            '((X)
               (cond
                  ((= X "A")
                     (inc '*BitCnt)
                     (setq V
                        (car (nth A (rand 1 (length A)))) )
                     (mapc prin (tail (- (length A0)) V))
                     V )
                  ((= X "B")
                     (inc '*BitCnt)
                     (setq V
                        (car (nth B (rand 1 (length B)))) )
                     (mapc prin (tail (- (length B0)) V))
                     V )
                  (T X) ) )
            L ) ) ) )
-> fnLSys
: (pp 'runLSys)
(de runLSys (L D)
   (if (>= D MaxDepth)
      (plotLSys L)
      (map
         '((X)
            (runLSys (fnLSys (list (car X))) (inc D)) )
         L ) ) )
-> runLSys
: (edit 'plotLSys)
