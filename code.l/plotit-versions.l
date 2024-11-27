(cond
	 ((=1 Bit)
			(cond
				 ((member Dir '(1 3)) (list 1 3))
				 ((member Dir '(0 2)) (list 2 3))
				 (T (throw "Data Error - Dir 0"))))
	 ((= 0 Bit)
			(cond
				 ((member Dir '(0 2)) (list 1 3))
				 ((member Dir '(1 3)) (list 2 3))
				 (T (throw "Data Error - Dir 1"))))
	 (T (throw "Data Error - Bit"))))

# --------------------------------------------------------------------
# BEST!

(de plotit (Size XO YO)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (default XO 0)
   (default YO 0)
   (let
      (BitCnt 0
         Cnt 0
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         N 0
         N2 0
         Val 0
         PP NIL
         PB NIL )
      (for X *Plot
         (let
            (PX (+ 1 XO (format (round (car X) 0)))
               PY (+ 1 YO (format (round (cadr X) 0)))
               A (caddr X) )
            (when (and PP (=0 (% Cnt 4)))
               (setq
                  Bit (rand 0 1)
                  Bit1 (rand 0 1)
                  N 1
                  Dir (caddar (tail 1 PP))
                  Val
                  (cond
                     ((=1 Bit)
                        (if (member Dir '(1 3))
                           (list 2 4)
                           (list 1 4) ) )
                     ((= 0 Bit)
                        (if (member Dir '(0 2))
                           (list 1 4)
                           (list 2 4) ) ) ) )
               "(msg (text "
               (inc 'BitCnt 1)
               (while PP
                  (setq PB (pop 'PP))
                  (set (nth Img (cadr PB) (car PB))
                     (if (member N Val) 1 0) )
                  (inc 'N) )
               (inc 'N2) )
            (setq PP (cons (list PX PY A) PP))
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )
-> plotit

# -----------------------------------------------------------------

(de plotit (Size XO YO)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (default XO 0)
   (default YO 0)
   (let
      (BitCnt 0
         Cnt 0
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         N 0
         N2 0
         Val 0
         PP NIL
         PB NIL )
      (for X *Plot
         (let
            (PX (+ 1 XO (format (round (car X) 0)))
               PY (+ 1 YO (format (round (cadr X) 0)))
               A (caddr X) )
            (when (and PP (=0 (% Cnt 4)))
               (setq
                  Bit (rand 0 1)
                  N 1
                  Dir (caddar (tail 1 PP))
                  Val
                  (cond
                     ((=1 Bit)
                        (if (member Dir '(1 3))
                           (if (=0 (rand 0 1))
                              (list 1 2)
                              (list 3 4) )
                           (list 0) ) )
                     ((= 0 Bit)
                        (if (member Dir '(0 2))
                           (if (=0 (rand 0 1))
                              (list 2 3)
                              (list 1 4) )
                           (list 0) ) ) ) )
               "(msg (text "
               (inc 'BitCnt 1)
               (while PP
                  (setq PB (pop 'PP))
                  (set (nth Img (cadr PB) (car PB))
                     (if (member N Val) 1 0) )
                  (inc 'N) )
               (inc 'N2) )
            (setq PP (cons (list PX PY A) PP))
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )
