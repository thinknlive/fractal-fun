# ----------------------------------------------------------------------------------------------------------------

# Returns bits ordered MSB...LSB
(de num2bits (N W)
   (default W 8)
   (let
      (Bite (mapcar format (chop (bin (rand 0 N))))
         Len (length Bite) )
      (when W)
      	(while (gt0 (% Len W))
     	    (setq Bite (cons 0 Bite))
          (inc 'Len) ) )
      Bite ) )

# Assumes bits are MSB...LSB
(de bits2num (Bits)
   (let (P 0  Res 0)
      (mapc
         '((B)
            (setq Res
               (+ Res (if (=1 B) (** 2 P) 0)) )
            (inc 'P) )
         (reverse Bits) )
      Res ) )

(de makegs (L)
   (make
      (for Bit L
         (cond
            ((=0 Bit) (link 1))
            ((=1 Bit) (link 1 0))
            (T (throw "Data error; makegs; bad bit")) ) ) ) )
            
# ----------------------------------------------------------------------------------------------------------------

(de leapT (L)
       (make (for (P L P (nth P 9)) (link P))) )

(de helperT (L)
   (leapT(leapT(leapT(leapT(leapT(leapT L)))))) )

(nil (setq *A (make (for N (** 2 17) (link (>> 2 (* N N)))))
            *B (helperT *A) ))
      
(de fastNth (L N)
   (dec 'N)  # convert 1-offset convention to 0-offset
   (let  (A (& 7 N)
          B (& 7 (setq N (>> 3 N)))
          C (& 7 (setq N (>> 3 N)))
          D (& 7 (setq N (>> 3 N)))
          E (& 7 (setq N (>> 3 N)))
          F (& 7 (setq N (>> 3 N)))
          G (>> 3 N) )
      (nth L (inc G) (inc F) (inc E) (inc D) (inc C) (inc B) (inc A)) ) )

(de MLup (A B)
  (-
     (car (fastNth *B (abs (+ A B))))
     (car (fastNth *B (abs (- A B)))) ) )      
   
(de mult-test2 ()
  (bench
  (do (** 2 20)  
    (let (
        A (rand 1 65535) 
        B (rand 1 65535)
        A+B (car (fastNth *B (abs (+ A B))))
        A-B (car (fastNth *B (abs (- A B)))) ) (- A+B A-B) ) ) ) )


(de mult-test1 ()
  (bench
  (do (** 2 24)  
    (let (
        A (rand 1 65535) 
        B (rand 1 65535) )
        (* A B) ) ) ) )


(de mult-test3 ()
  (native "./libmult.so" "initxy" NIL)
  (bench
  (do (** 2 24)  
    (let (
        A (rand 1 65535) 
        B (rand 1 65535))
        (native "./libmult.so" "multxy" 'N A B))))
  (native "./libmult.so" "finalxy" NIL) ) 
  

(native "./libmult.so" "initxy" NIL)
(native "./libmult.so" "multxy" 'N 13 26)
(native "./libmult.so" "finalxy" NIL) 
  
# ----------------------------------------------------------------------------------------------------------------

: (nil (setq *MULT (make (for N (** 2 9) (link (cons N (* N N )))))))     
-> NIL
: (balance '*MULTBL *MULT)                                                                                                                          
-> NIL
: (pp 'MLup)                                                         
(de MLup (A B)
   (>>
      2
      (-
         (cdr (lup *MULTBL (abs (+ A B))))
         (cdr (lup *MULTBL (abs (- A B)))) ) ) )
-> MLup
: (bench (do (** 2 24) (let (A (rand 1 255) B (rand 1 255)) (MLup A B))) )
21.450 sec
-> 5763
: (bench (do (** 2 24) (let (A (rand 1 255) B (rand 1 255)) (* A B))) )   
4.068 sec
-> 21222

#                2          2
# ab = ( ((a + b)  - (a - b)  ) ) / 4
#      
#
# (let (A (rand 1 255) B (rand 1 255)) (= (* A B) (>> 2 (- (** (abs (+ A B)) 2) (** (abs (- A  B)) 2))) ) )
# For a restricted integer number range, using a table of 'squares' / 4 
# reduces the multiplication to 3 additions (with two table lookups)
# (let (A (rand 1 255) B (rand 1 255))(= (* A B) (- (cdr (lup *MULTBL (abs (+ A B)))) (cdr (lup *MULTBL (abs (- A B)))))) )

# Table of squares/4  
(nil (setq *MULT (make (for N (** 2 9) (link (cons N (>> 2 (* N N )) ))))))
(balance '*MULTBL *MULT)

# Test functions
(de MLup (A B)
  (-
     (cdr (lup *MULTBL (abs (+ A B))))
     (cdr (lup *MULTBL (abs (- A B)))) ) )

# Multiplication by lookup     
(de mult-test2 ()
  (bench
  (do (** 2 20)  
    (let (
        A (rand 1 255) 
        B (rand 1 255)
        A+B (cdr (lup *MULTBL (abs (+ A B))))
        A-B (cdr (lup *MULTBL (abs (- A B)))) ) (- A+B A-B) ) ) ) )

# Built-in multiplication        
(de mult-test1 ()
  (bench
  (do (** 2 20)  
    (let (
        A (rand 1 255) 
        B (rand 1 255) )
        (* A B) ) ) ) )
        
: (mult-test1)
0.392 sec
-> 14151
: (mult-test2)
1.507 sec
-> 2249
: (= (* 13 41) (MLup 13 41))
-> T
        

# ----------------------------------------------------------------------------------------------------------------
# Monotonically increasing arrays

: (head 64 *BitsMsg)
-> (1 4 3 2 2 6 3 1 1 3 4 1 1 5 1 2 5 2 5 1 2 6 2 22 1 2 2 3 3 2 3 5 4 4 2 3 23 1 16 1 14 20 4 1 3 2 1 1 1 3 4 7 3 16 4 1 3 1 1 2 6 2 3 1)
: (nil (setq *BitsN (let (N (car *BitsMsg)) (make (link N)  (map '((L) (let (Sum (+ N (car L)))  (link Sum) (setq N Sum))) *BitsMsg)) )))
-> NIL
: (head 32 *BitsN)
-> (1 2 6 9 11 13 19 22 23 24 27 31 32 33 38 39 41 46 48 53 54 56 62 64 86 87 89 91 94 97 99 102)
: (tail 64 *BitsN)
-> (109908 109910 109911 109913 109916 109917 109924 109925 109927 109928 109929 109932 109933 109934 109936 109940 109941 109948 109949 109954 109956 109958 109960 109961 109962 109963 109965 109966 109968 109969 109970 109973 109974 109976 109978 109980 109981 109982 109983 109984 110003 110006 110009 110011 110015 110017 110019 110022 110023 110024 110027 110031 110034 110036 110038 110039 110040 110043 110048 110051 110052 110058 110064 110065)
: (/ 110065 8)
-> 13758
: (length *BitsMsg)
-> 54950
: (length *Bits)     
-> 165109
: (length *BitsBites)
-> 13770
: 

: (nil (balance '*Temp (make (for N (cadr (mmlit *BitsN)) (link (list N 0))))))
-> NIL
: (nil (for N *BitsN (inc (cdr (lup *Temp N)))))                
-> NIL
: (nil (setq *Temp2 (make (for N (idx '*Temp) (link (cadr N))))))
-> NIL
: (head 64 *Temp2)                                               
-> (0 0 1 0 0 1 0 1 0 0 0 0 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 1 1 1 0 0 0 0 0 0 0 1 1 0 1)
-> (  2 1   2 1 1 1       4   2 1       4 1     3 1 1 1 1 1     3 1 1       4 1         5 1     3 1   2     3             7   2 1 1

(nil (setq *BitsZMsg (ACDC_NZCompress *Temp2 (mmlit *Temp2))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))

# ----
(nil (setq *BitsN (let (N 0) (make (map '((L) (let (Sum (+ N (car L))) (link Sum) (setq N Sum))) *BitsMsg)) )))
(mmlit *BitsN)
(nil (balance '*Temp (make (for N (cadr (mmlit *BitsN)) (link (list N 0))))))
(nil (for N *BitsN (inc (cdr (lup *Temp N)))))
(nil (setq *Temp2 (make (for N (idx '*Temp) (link (cadr N))))))                                                                        
(nil (let (TEMP (copy *Temp2)) (setq *Temp3 (make (while TEMP (let (Bite (cut 8 'TEMP)) (link (bin2byte Bite))))))))                   
(length *Temp3)
(length *BitsBites)
(nil (setq *BitsZMsg (ACDC_NZCompress *Temp3 (mmlit *Temp3))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))             
(nil (setq *BitsZMsg (ACDC_NZCompress *BitsBites (mmlit *BitsBites))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))
# -----

# -------------------------------------------------------------------------------------------------------------------
# SUFFIX Arrays


: (setq *SA (chop "ABRACADABRA"))
-> ("A" "B" "R" "A" "C" "A" "D" "A" "B" "R" "A")

: (let (Cnt 0 Len (inc (length *SA))) (for L (sort (make (map '((L) (link (list L (inc 'Cnt) (dec 'Len)))) *SA))) (println L)))
(("A") 11)
(("A" "B" "R" "A") 8)
(("A" "B" "R" "A" "C" "A" "D" "A" "B" "R" "A") 1)
(("A" "C" "A" "D" "A" "B" "R" "A") 4)
(("A" "D" "A" "B" "R" "A") 6)
(("B" "R" "A") 9)
(("B" "R" "A" "C" "A" "D" "A" "B" "R" "A") 2)
(("C" "A" "D" "A" "B" "R" "A") 5)
(("D" "A" "B" "R" "A") 7)
(("R" "A") 10)
(("R" "A" "C" "A" "D" "A" "B" "R" "A") 3)

# Construct Suffix Array

(sort (make (map link *List)))

(for L (head 64 *BitsSA) (println (head 64 (car L)) (cdr L)))

# Compute Lcp's of Suffix Array

(de makeSA (Lst)
   (let (BitsSA NIL)
      (setq BitsSA
         (let (Cnt 0  Len (inc (length Lst)))
            (by
               car
               sort
               (make
                  (map
                     '((L)
                        (link
                           (list L (inc 'Cnt) (dec 'Len) 0) ) )
                     Lst ) ) ) ) )
      (map
         '((X)
            (when (cdr X)
               (let
                  (L1 (car X)
                     L2 (cadr X)
                     N1 (caddr L1)
                     N2 (caddr L2)
                     N (min N1 N2)
                     Lcp 0 )
                  (let (PL1 (car L1)  PL2 (car L2))
                     (loop
                        (T (<> (car PL1) (car PL2)))
                        (inc 'Lcp)
                        (setq PL1 (cdr PL1))
                        (setq PL2 (cdr PL2)) ) )
                  (set (cdddr L2) Lcp) ) ) )
         BitsSA )
      BitsSA ) )
        
      
# --------------------------------------------------------------------------------
(nil (setq *BitsWords 
  (let (Lst (copy *BitsBites)) 
    (make (while Lst (link 
      (+ (pop 'Lst) 
         (ifn Lst 0 (* (** 2 8)  (pop 'Lst))) 
      ) ))))))
(mmlit *BitsWords)

(nil (setq *BitsWords 
  (let (Lst (copy *Temp3)) 
    (make (while Lst (link 
      (+ (pop 'Lst) 
         (ifn Lst 0 (* (** 2 8)  (pop 'Lst))) 
      ) ))))))
(mmlit *BitsWords)

(nil (setq *BitsZMsg (ACDC_NZCompress *BitsBites (mmlit *BitBites))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))
(nil (setq *BitsZMsg (ACDC_NZCompress *BitsWords (mmlit *BitsWords))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))
(nil (setq *BitsZMsg (ACDC_NZCompress *BitsMsg (mmlit *BitsMsg))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))

(nil (setq *BitsWords
  (let (Lst (copy *BitsLzw)) 
    (make (while Lst (link 
      (+ (pop 'Lst) 
         (ifn Lst 0 (* (** 2 8)  (pop 'Lst))) 
         (ifn Lst 0 (* (** 2 16) (pop 'Lst))) 
         (ifn Lst 0 (* (** 2 24) (pop 'Lst))) 
      ) ))))))
(mmlit *BitsWords)

(nil (setq *BitsWords
  (let (Lst (copy *BitsLzw)) 
    (make (while Lst (link 
      (+ (pop 'Lst) 
         (ifn Lst 0 (* (** 2 8)  (pop 'Lst))) 
      ) ))))))
      
#------------------------------------------------------------------------------------------      
(mmlit *BitsWords)
(load "./pil-dev/picolisp-acdc/acdc.nz.l" "./pil-dev/picolisp-bbwt/bbwt.l" "./devit.l" "./lzw.l")
(load "./pil-dev/picolisp-acdc/acdc.pol.l" "./pil-dev/picolisp-bbwt/bbwt.l" "./devit.l" "./lzw.l")

(prog 
(nil (in "./pil-dev/picolisp-gosper/hil.bits" (let (Bit NIL) (setq *BitsRaw (make (while (setq Bit (char)) (if (= "1" Bit) (link 1) (link 0))))))))
(nil (setq *Bits (makeitl *BitsRaw)))

(nil (setq *Bits (makeit (length *BitsRaw) 25)))
(nil (setq *Bits (copy *BitsBits)))
(parseSymsFib *Bits) 

(nil (setq *Bits (makeitl *BitsBits)))

(nil (setq *Bits (makeit 65536 49)))

(nil (in "./test.txt" (setq *BitsBites (make (do (* 8192 8) (NIL (rd 1)) (link @))))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T NIL)) (link B))))))
(nil (setq *Bits (makeitl *BitsBits)))
(length *Bits)
(parseSyms *Bits)

(nil (setq *BitsZMsg (ACDC_NZCompress (reverse *BitsBBWT) (mmlit *BitsBBWT))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))
(nil (setq *BitsZMsg (ACDC_NZCompress (reverse *BitsMsg) (mmlit *BitsMsg))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))

(nil (setq *BitsBits (make (for N *BitsBites (for B (fibEncode N) (link B))))))

(nil (in "./pil-dev/picolisp-gosper/hilRC.png" (setq *BitsBites (make (while (rd 1) (link @))))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T NIL)) (link B))))))

(nil (setq *Bits (makeitl *BitsBits)))
(length *Bits)
(parseSyms *Bits)

(nil (setq *Bits (makeit (* 4 8192) 51)))
(parseSyms *Bits)

(de mzit ()
(nil (setq *BitsMsg (make (for E *BitsL (ifn (lup *BitsTS E) (throw "Data error")) (link (cadddr (lup *BitsTS E)))))))
(nil (setq *BitsBBWT (encodeBBWT *BitsMsg)))
(setq *BitsModel (make (for N *BBWT (link (list (car N) (sort (uniq (cadr N))) (length (cadr N)) )))))
(for N *BitsModel (println N))
(for L *BitsA (prinl (car L) " " (cadr L) " " (caddr L) " " (cadddr L)))
(nil (setq *BitsCMsg (ACDC_Compress *BitsMsg)))
(msg (pack "*BitsCMsg: " (/ (length *BitsCMsg) 8)))
(nil (setq *BitsCMsg (ACDC_MCompress (reverse *BitsMsg) *BitsModel)))
(msg (pack "*BitsCMsg: " (/ (length *BitsCMsg) 8)))
(out "test.bites" (for N *BitsMsg (wr N)))
)

(nil (setq *BitsBBWT (encodeBBWT *BitsBitesF)))
(setq *BitsModel (make (for N *BBWT (link (list (car N) (sort (uniq (cadr N))) (length (cadr N)) )))))
(for N *BitsModel (println N))

(for L *BitsA (prinl (car L) " " (cadr L) " " (caddr L) " " (cadddr L)))

(nil (setq *BitsZMsg (ACDC_NZCompress (reverse *BitsMsg) (mmlit *BitsN))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))
(nil (setq *BitsZMsg (ACDC_NZCompress *BitsLzw (mmlit *BitsLzw))) (println (/ (length *BitsZMsg) 8) (% (length *BitsZMsg) 8)))

(nil (setq *BitsZMsg (ACDC_NZCompress (reverse *BitsBites) (mmlit *BitsBites))))
(msg (pack "*BitsZMsg: " (/ (length *BitsZMsg) 8)))

(nil (setq *BitsCMsg (ACDC_MCompress (reverse *BitsLzw) *BitsModel)))
(msg (pack "*BitsLzw: " (/ (length *BitsCMsg) 8)))

(for N (head 32 *BitsModel (println N)))
  

(nil (setq *BitsBBWT (encodeBBWT *BitsLzw)))
(setq *BitsModel (make (for N *BBWT (link (list (car N) (sort (uniq (cadr N))) (length (cadr N)) )))))
(for N *BitsModel (println N))
(nil (setq *BitsCMsg (ACDC_Compress *BitsLzw)))
(/ (length *BitsCMsg) 8)
(nil (setq *BitsCMsg (ACDC_MCompress (reverse *BitsLzw) *BitsModel)))
(/ (length *BitsCMsg) 8)



(nil (setq *BitsCMsg (ACDC_MCompress (reverse *BitsMsg) *BitsModel)))

(out "test.bites" (for N *BitsMsg (wr N)))
(in "./pil-dev/picolisp-gosper/hil.png" (setq *BitsBites (make (while (rd 1) (link @)))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T NIL)) (link B))))))

(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T T)) (link B))))))

(nil (in "./pil-dev/picolisp-gosper/hilRC.png" (setq *BitsBites (make (while (rd 1) (link @))))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T NIL)) (link B))))))

(nil (in "./test.txt" (setq *BitsBites (make (do (* 4 8192) (NIL (rd 1)) (link @))))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T T)) (link B))))))

(nil (setq *BitsBits (make (for N *BitesLzw (for B (byte2bin N '(T T)) (link B))))))

(nil (setq *BitsBits (make (for N *BitesMFT (for B (byte2bin N '(T NIL)) (link B))))))

(nil (in "./Downloads/FixedSqrt.pdf" (setq *BitsBites (make (while (rd 1) (link @))))))
(nil (setq *BitsBits (make (for N *BitsBites (for B (byte2bin N '(T NIL)) (link B))))))

(de mzit ()
  (nil (setq *BitsMsg (make (for E *BitsL (ifn (lup *BitsTS E) (throw "Data error")) (link (cadddr (lup *BitsTS E)))))))
  (nil (setq *BitsBBWT (encodeBBWT *BitsMsg)))
  (setq *BitsModel (make (for N *BBWT (link (list (car N) (sort (uniq (cadr N))) (length (cadr N)) )))))
  (for L *BitsA (prinl (car L) " " (cadr L) " " (caddr L) " " (cadddr L)))
  (for N *BitsModel (println N))
  (nil (setq *BitsCMsg (ACDC_Compress *BitsMsg)))
  (msg (pack "*BitsCMsg: " (/ (length *BitsCMsg) 8)))
  (nil (setq *BitsCMsg (ACDC_MCompress (reverse *BitsMsg) *BitsModel)))
  (msg (pack "*BitsCMsg: " (/ (length *BitsCMsg) 8)))
  (out "test.bites" (for N *BitsMsg (wr N)))
)

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

(de makeit (N P Bits0 Bits1)
   (default N 1024)
   (default P 51)
   (default Bits0 (list 1))
   (default Bits1 (list 1 0))
   (make
      (do N
         (if (< (rand 1 100) P)
            (for R Bits0
               (link R) )
            (for R Bits1
               (link R) ) ) ) ) )


(de makeitl (L Bits0 Bits1)
   (default Bits0 (list 1))
   (default Bits1 (list 1 0))
   (let
      (Bits
         (make
            (for B L
               (if (=1 B)
                  (for R Bits1
                     (link R) )
                  (for R Bits0
                     (link R) ) ) ) ) )
      Bits ) )

(de parseSyms (L BitC)
   (default BitC 0)
   (setq *BitsL NIL  *BitsT NIL  *BitsA NIL  *BitsTS NIL)
   (let (Bits NIL)
      (setq *BitsL
         (make
            (while L
               (cond
                  ((= BitC (car L))
                     (setq Bits (reverse (cons (car L) Bits)))
                     (link Bits)
                     (if (lup *BitsT Bits)
                        (inc (cdr (lup *BitsT Bits)))
                        (idx '*BitsT (list Bits 1 (length Bits)) T) )
                     (setq Bits NIL) )
                  (T (setq Bits (cons (car L) Bits))) )
               (pop 'L) )
            (when Bits
               (setq Bits (reverse Bits))
               (link Bits)
               (if (lup *BitsT Bits)
                  (inc (cdr (lup *BitsT Bits)))
                  (idx '*BitsT (list Bits 1 (length Bits)) T) ) ) ) ) )
   (setq *BitsA
      (reverse
         (by
            cadr
            sort
            (make
               (for Bits (uniq *BitsL)
                  (link (lup *BitsT Bits)) ) ) ) ) )
   (setq *BitsA
      (let (Ndx 1)
         (make
            (for E *BitsA
               (link (append E (list Ndx)))
               (inc 'Ndx) ) ) ) )
   (for E *BitsA
      (idx '*BitsTS E T) ) )

: (pp 'parseSymsFib)
(de parseSymsFib (L BitC)
   (default BitC 1)
   (setq *BitsL NIL  *BitsT NIL  *BitsA NIL  *BitsTS NIL)
   (let (BitsCnt 0  Bits NIL  BitsF NIL  BitsNdx NIL)
      (setq *BitsL
         (make
            (while L
               (cond
                  ((= '(1 0) (head 2 L))
                     (setq BitNdx 2  Bits (cut 2 'L))
                     (loop
                        (if (= 0 (car (nth Bits BitNdx)))
                           (setq BitsF '(1)) )
                        (if (= 1 (car (nth Bits BitNdx)))
                           (setq BitsF '(1 0)) )
                        (NIL (= BitsF (head (length BitsF) L)))
                        (setq
                           Bits (append Bits (cut (length BitsF) 'L))
                           BitNdx (+ 1 BitNdx) ) )
                     (link Bits)
                     (setq BitsCnt (+ BitsCnt (length Bits)))
                     #(msg (text "10: @1 @2" Bits BitsCnt))
                     (if (lup *BitsT Bits)
                        (inc (cdr (lup *BitsT Bits)))
                        (idx '*BitsT (list Bits 1 (length Bits)) T) )
                     (setq Bits NIL  BitsF NIL  BitNdx NIL) )
                  (T (setq Bits (append Bits (cut 1 'L)))
                     (while
                        (and
                           L
                           (not (= '(1 0) (head 2 L))) )
                        (setq Bits (append Bits (cut 1 'L))) )
                     (link Bits)
                     (setq BitsCnt (+ BitsCnt (length Bits)))
                     #(msg (text "10: @1 @2" Bits BitsCnt))
                     (if (lup *BitsT Bits)
                        (inc (cdr (lup *BitsT Bits)))
                        (idx '*BitsT (list Bits 1 (length Bits)) T) ) ) ) )
            (when Bits
               (link Bits)
               (if (lup *BitsT Bits)
                  (inc (cdr (lup *BitsT Bits)))
                  (idx '*BitsT (list Bits 1 (length Bits)) T) ) ) ) ) )
   (setq *BitsA
      (by
         caddr
         sort
         (make
            (for Bits (uniq *BitsL)
               (link (lup *BitsT Bits)) ) ) ) )
   (setq *BitsA
      (let (Ndx 1)
         (make
            (for E *BitsA
               (link (append E (list Ndx)))
               (inc 'Ndx) ) ) ) )
   (for E *BitsA
      (idx '*BitsTS E T) ) )
-> parseSymsFib



(de parseSyms2 (L)
   (setq *BitsL NIL  *BitsT NIL *BitsA NIL *BitsTS NIL)
   (let (Bits NIL)
      (setq *BitsL
         (make
            (while L
               (cond
                  ((= 2 (car L))
                     (setq Bits (append Bits '(1 1 0)))
                     (link Bits)
                     (if (lup *BitsT Bits)
                        (inc (cdr (lup *BitsT Bits)))
                        (idx '*BitsT (list Bits 1) T) )
                     (setq Bits NIL) )
                  (T (setq Bits (append Bits (list (car L))))) )
               (pop 'L) )
            (when Bits
               (setq Bits (append Bits '(1 1 0)))
               (link Bits)
               (if (lup *BitsT Bits)
                  (inc (cdr (lup *BitsT Bits)))
                  (idx '*BitsT (list Bits 1) T) ) ) ) ) ) 
    (setq *BitsA (reverse (by cadr sort (make (for Bits (uniq *BitsL) (link (lup *BitsT Bits)))))))
    (setq *BitsA (let (Ndx 0) (make (for E *BitsA (link (append E (list Ndx))) (inc 'Ndx)))))
    (for E *BitsA (idx '*BitsTS E T)) ) 
