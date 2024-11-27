# -------------------------------------------------------------------
# Binary encode given number.
#   Return list of 0,1 digits msb..lsb
#   Use Opt to:
#      pad to 8-bit boundary
#      reverse bit order lsb..msb
# 
# (if (car Opt) "pad to 8 bit boundary")
# (if (cdr Opt) "reverse the bits ie lsb..msb")

(de byte2bin (N Opt)
   (let (V NIL  C 0)
      (setq N (chop (bin N)))
      (mapcar
         '((B)
            (inc 'C) 
            (setq V (cons (format B) V)) )
         N )
      (setq V (flip V))
      (if (car Opt)
         (while (<> 0 (% C 8))
            (setq V (cons 0 V))
            (inc 'C) ) )
      (if (car (cdr Opt)) (setq V (flip V)))
      V ) )

(de bin2byte (L)
   (let (V 0  S 1)
      (mapcar
         '((X)
            (setq
               V (+ V (* X S))
               S (* S 2) ) )
         (reverse L) )
      V ) )

# ---------------------------------------------------------
fnLSys ((L)
   (let (V NIL  BitVal (rand 0 1))
      (fish
         atom
         (mapcar
            '((X)
               (cond
                  ((= X "A")
                     (setq BitVal (rand 0 1))
                     (setq V (car (nth A 1)))
                     (inc '*BitCnt 1)
                     (cond
                        ((= BitVal *BitValA) ":")
                        ((= 0 BitVal) (prin ".::"))
                        ((= 1 BitVal) (prin ".:."))
                        (T (throw "A: Data error")) )
                     (setq *BitValA BitVal)
                     V )
                  ((= X "B")
                     (setq BitVal (rand 0 1))
                     (setq V (car (nth B 1)))
                     (inc '*BitCnt 1)
                     (cond
                        ((= BitVal *BitValB) ":")
                        ((= 0 BitVal) (prin ".:."))
                        ((= 1 BitVal) (prin ".::"))
                        (T (throw "B: Data error")) )
                     (setq *BitValB BitVal)
                     V )
                  (T X) ) )
            L ) ) ) )

(=======)

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
