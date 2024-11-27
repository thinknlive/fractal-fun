# ------------------------------------------------------			
(de plotit (Size)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (let
      (BitCnt 0
         Cnt 1
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         N 0
         Val 0
         PP NIL
         PB NIL )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (when (and PP (=0 (% Cnt 4)))
               (setq
                  Bit (rand 0 1)
                  N 1
                  Val '(1 2 3 4) )
               (inc 'BitCnt)
               (while PP
                  (setq PB (pop 'PP))
                  (set (nth Img (cadr PB) (car PB))
                     (if (=1 Bit) 1 0) )
                  (inc 'N) ) )
            (setq PP (cons (list PX PY) PP))
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )

# ------------------------------------------------------			
(de plotit (Size)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (let
      (BitCnt 0
         Cnt 1
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         N 0
         Val 0
         PP NIL
         PB NIL )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (when (and PP (=0 (% Cnt 4)))
               (setq
                  Bit (rand 0 1)
                  N 1
                  Val '(1 2 3 4) )
               (inc 'BitCnt)
               (while PP
                  (setq PB (pop 'PP))
                  (set (nth Img (cadr PB) (car PB))
                     (if (=1 Bit) 1 0) )
                  (inc 'N) ) )
            (setq PP (cons (list PX PY) PP))
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )
			
# -------------------------------------------------------------------
(de plotit (Size)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (let
      (BitCnt 0
         Cnt 1
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         BitsN '((5 7 13 15) (5 7 13 15))
         Val 0
         Bits
         '((0 2 8 10 12 14 4 6)
            (0 2 8 10 12 14 5 7)
            (0 2 8 10 13 15 4 6)
            (0 2 8 10 13 15 5 7)
            (0 2 9 11 12 14 4 6)
            (0 2 9 11 12 14 5 7)
            (0 2 9 11 13 15 4 6)
            (0 2 9 11 13 15 5 7)
            (1 3 8 10 12 14 4 6)
            (1 3 8 10 12 14 5 7)
            (1 3 8 10 13 15 4 6)
            (1 3 8 10 13 15 5 7)
            (1 3 9 11 12 14 4 6)
            (1 3 9 11 12 14 5 7)
            (1 3 9 11 13 15 4 6)
            (1 3 9 11 13 15 5 7) ) )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (if (< (% Cnt 32) 16)
               (if (=0 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 0
                     BitCnt (+ 4 BitCnt)
                     Val (car (nth Bits (rand 1 (length Bits))))
                     Bit 0 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) )
               (if (=1 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 0
                     BitCnt (+ 4 BitCnt)
                     Val (car (nth Bits (rand 1 (length Bits))))
                     Bit 1 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) ) )
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      (prinl "Cnt: " Cnt "; BitCnt: " BitCnt)
      Img ) )

# -------------------------------------------------------------

(de plotit (Size)
   "01EF"
   "32DC"
   "478B"
   "569A"
   (default Size 256)
   (let
      (Cnt 1
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit (rand 0 1)
         ValA 0
         ValB 0 )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (if (< (% Cnt 32) 16)
               (if (=0 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 1
                     Val
                     (if (=0 (rand 0 1))
                        '(1 3 6 8 9 11 14 16)
                        '(1 3 6 8 9 11 14 16) )
                     Bit 0 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) )
               (if (=1 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 1
                     Val
                     (if (=0 (rand 0 1))
                        '(1 3 6 8 9 11 14 16)
                        '(1 3 6 8 9 11 14 16) )
                     Bit 1 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) ) )
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      Img ) )

# ------------------------------------------------------------------

(de plotit (Size)
   (default Size 256)
   (let
      (Cnt 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         Val 0 )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (if (< (% Cnt 32) 16)
               (if (=0 Bit)
                  (prog
                     (setq Val (x| 1 Val))
                     (set (nth Img PY PX) Val) )
                  (setq Val (rand 0 1)  Bit 0)
                  (set (nth Img PY PX) Val) )
               (if (=1 Bit)
                  (prog
                     (setq Val (x| 1 Val))
                     (set (nth Img PY PX) Val) )
                  (setq Val (rand 0 1)  Bit 1)
                  (set (nth Img PY PX) Val) ) )
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      Img ) )

# -------------------------------------------------------------

(de plotit (Size)
   (default Size 256)
   (let
      (Cnt 1
         Z 1
         Img (make (do Size (link (need Size 0))))
         Bit (rand 0 1)
         ValA 0
         ValB 0 )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (if (< (% Cnt 32) 16)
               (if (=0 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 1
                     Val
                     (if (=0 (rand 0 1))
                        '(1 3 6 8 9 11 14 16)
                        '(1 3 6 8 9 11 14 16) )
                     Bit 0 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) )
               (if (=1 Bit)
                  (prog
                     (setq Z (+ 1 Z))
                     (set (nth Img PY PX)
                        (if (member Z Val) 1 0) ) )
                  (setq
                     Z 1
                     Val
                     (if (=0 (rand 0 1))
                        '(1 3 6 8 9 11 14 16)
                        '(1 3 6 8 9 11 14 16) )
                     Bit 1 )
                  (set (nth Img PY PX)
                     (if (member Z Val) 1 0) ) ) )
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      (out "hil.bits" (for X Img (mapc prin X)))
      Img ) )

# -------------------------------------------------------------------

(de plotit (Size)
   (default Size 256)
   (let
      (Cnt 1
         Img (make (do Size (link (need Size 0))))
         Bit 0
         Val 0 )
      (for X *Plot
         (let
            (PX (+ 1 (format (round (car X) 0)))
               PY (+ 1 (format (round (cadr X) 0))) )
            (if (< (% Cnt 32) 16)
               (if (=0 Bit)
                  (prog
                     (setq Val (x| 1 Val))
                     (set (nth Img PY PX) Val) )
                  (setq Val (rand 0 1)  Bit 0)
                  (set (nth Img PY PX) Val) )
               (if (=1 Bit)
                  (prog
                     (setq Val (x| 1 Val))
                     (set (nth Img PY PX) Val) )
                  (setq Val (rand 0 1)  Bit 1)
                  (set (nth Img PY PX) Val) ) )
            (inc 'Cnt) ) )
      (out "hil.pbm"
         (prinl "P1")
         (prinl Size " " Size)
         (mapc prinl Img) )
      Img ) )

# ------------------------------------------------------------

(de Hilbert-Curve (Length Angle N)
   (let
      (A (chop "+BF-AFA-FB+")
         B (chop "-AF+BFB+FA-")
         C A
         _Fn
         '((L)
            (fish
               atom
               (mapcar
                  '((X)
                     (cond
                        ((= X "A") A)
                        ((= X "B") B)
                        (T X) ) )
                  L ) ) ) )
      (do N
         (setq C (_Fn C)) )
      (map
         '((P)
            (case (car P)
               (("F") (Plot-Line Length Angle))
               ("+" (setq Angle (+ Angle PI/2)))
               ("-" (setq Angle (- Angle PI/2)))
               (("A" "B") NIL)
               (T (msg (text "?Hilbert-Curve: No-match: @1" (car P)))) ) )
         C ) ) )

# ------------------------------------------------------------

(de bin2byte (L)
   (let (V 0  S 1)
      (mapcar
         '((X)
            (setq
               V (+ V (* X S))
               S (* S 2) ) )
         (reverse L) )
      V ) )

# ------------------------------------------------------------

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

# --------------------------------------------
# https://rosettacode.org/wiki/Bitmap#PicoLisp

# Create an empty image of 120 x 90 pixels
(setq *Ppm (make (do 256 (link (need 256)))))
 
# Fill an image with a given color
(de ppmFill (Ppm R G B)
   (for Y Ppm
      (map
         '((X) (set X (list R G B)))
         Y ) ) )
 
# Set pixel with a color
(de ppmSetPixel (Ppm X Y R G B)
   (set (nth Ppm Y X) (list R G B)) )
 
# Get the color of a pixel
(de ppmGetPixel (Ppm X Y)
   (get Ppm Y X) )
	 
# -----------------------------------------------------
# https://rosettacode.org/wiki/Bitmap/Write_a_PPM_file  

(de ppmWrite (Ppm File)
 (out File
		(prinl "P6")
		(prinl (length (car Ppm)) " " (length Ppm))
		(prinl 255)
		(for Y Ppm (for X Y (apply wr X))) ) )
			

# ------------------------------------------------------------
# https://rosettacode.org/wiki/Bitmap/Read_a_PPM_file#PicoLisp			

(de ppmRead (File)
   (in File
      (unless (and `(hex "5036") (rd 2))  # P6
         (quit "Wrong file format" File) )
      (rd 1)
      (let (DX 0  DY 0  Max 0  C)
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq DX (+ (* 10 DX) C)) )
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq DY (+ (* 10 DY) C)) )
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq Max (+ (* 10 Max) C)) )
         (prog1
            (make (do DY (link (need DX))))
            (for Y @
               (map
                  '((X) (set X (list (rd 1) (rd 1) (rd 1))))
                  Y ) ) ) ) ) )
									
			