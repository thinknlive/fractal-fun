(de plotit (Size XO YO Src)
   (default Size 256)
   (default XO 0)
   (default YO 0)
   (default Src '(() (rand 0 1)))
   (ThueMorseGen T)
   (let
      (BitCnt 0
         Cnt 0
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0 PrvBit NIL
         N 0
         N2 0
         Val 0
         PP NIL
         PB NIL )
      (for X *Plot
         (let
            (PX (+ 1 XO (format (round (car X) 0)))
               PY (+ 1 YO (format (round (cadr X) 0)))
               A (caddr X)
               NP (list PX PY A))
            (when (and PP (=0 (% Cnt 2)))
               (setq PP (reverse PP))
               (setq
                  BitB (let (Sym NIL) (while (and (<> Sym "A") (<> Sym "B")) (setq Sym (ThueMorseGen))) (if (= "A" Sym) 1 0))
                  BitA (if (=0 BitB) 1 0)
                  BitC 1 #(Src)
                  BitD (Src) #(if (=0 BitC) 1 0)
                  N 1
                  XY1 (car (head 1 PP))
                  XY2 (car (tail -1 PP))
                  Val
                  (cond
                     ((= 1 BitB)
                        (cond
                           ((= (car XY1) (car XY2))
                              (if (= 0 (% (car XY1) 2))
                                 (prog
                                    (inc 'BitCnt 0)
                                    (if (=1 BitC) (list 1 2) (list 0) ))
                                 (prog
                                    (inc 'BitCnt 0)
                                    (if (=1 BitC) (list 0) (list 0) ))))
                           ((= (cadr XY1) (cadr XY2))
                              (if (=0 (% (cadr XY2) 2))
                                 (prog
                                    (inc 'BitCnt 1)
                                    (if (=1 BitC) (list 0) (list 0) ))
                                 (prog
                                    (inc 'BitCnt 0)
                                    (if (=1 BitC) (list 1 2)  (list 0) ))))
                           (T (throw "Data Error - Dir 1")) ) )
                     ((= 0 BitB)
                        (cond
                           ((= (car XY1) (car XY2))
                              (if (= 0 (% (car XY1) 2))
                                 (prog 
                                    (inc 'BitCnt 0) 
                                    (if (=1 BitC) (list 1 2) (list 0) ))
                                 (prog 
                                    (inc 'BitCnt 0) 
                                    (if (=1 BitC) (list 0) (list 0) ))))
                           ((= (cadr XY1) (cadr XY2))
                              (if (=0 (% (cadr XY2) 2))
                                 (prog
                                    (inc 'BitCnt 0)
                                    (if (=1 BitC) (list 0) (list 0) ))
                                 (prog 
                                    (inc 'BitCnt 0)
                                    (if (=1 BitC) (list 1 2) (list 0) ))))
                           (T (throw "Data Error - Dir 0")) ) )
                     (T (throw "Data Error - Bit")) ) )
               (while PP
                  (setq PB (pop 'PP))
                  (set (nth Img (car PB) (cadr PB))
                     (if (member N Val) 1 0) )
                  (inc 'N) )
               (setq PrvBit Bit)
               (inc 'N2) )
            (setq PP (cons (list PX PY A) PP))
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (HilbertGen T)
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )

