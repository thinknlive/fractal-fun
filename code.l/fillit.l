(de fillit NIL
   (let
      (PX NIL
         PY NIL
         NX NIL
         NY NIL
         PXY NIL
         NXY NIL
         BitCnt 0
         SizeX (length (car *Img))
         SizeY (length *Img)
         Bits (need SizeX 0) )
      (setq *Img (reverse *Img))
      (for Y SizeY
         (when
            (and
               (> (% Y 6) 1)
               (< (% Y 6) 5) )
            (for Z SizeX
               (when
                  (and
                     (=0 (% Z 2))
                     (gt0 (% Z 6)) )
                  (inc 'BitCnt 1)
                  (set (nth Bits Z) (rand 0 1)) ) ) )
         (for X SizeX
            (setq
               PX (if (=0 (- X 1)) SizeX (- X 1))
               NX
               (if (> (+ X 1) SizeX)
                  1
                  (+ 1 X) )
               PrvX (car (nth *Img Y PX))
               NxtX (car (nth *Img Y NX))
               PY (if (=0 (- Y 1)) SizeY (- Y 1))
               NY
               (if (> (+ Y 1) SizeY)
                  1
                  (+ 1 Y) )
               PrvY (car (nth *Img PY X))
               NxtY (car (nth *Img NY X)) )
            (when
               (and
                  (=0 (% X 2))
                  (gt0 (% Y 6))
                  (gt0 (% X 6))
                  (=0 PrvX)
                  (=0 NxtX) )
               (set (nth *Img Y X) (car (nth Bits X))) ) ) )
      (setq *Img (reverse *Img))
      (out "hil.fillit.pbm"
         (prinl "P1")
         (prinl SizeX " " SizeY)
         (mapc prinl *Img) )
      (out "hil.fillit.bits" (for X *Img (mapc prin X)))
      (prinl "BitCnt: " BitCnt) )
   *Img )
