#define N 1000000

static int bit[N+1];

void update(unsigned int n, int x)
{
    for (; n <= N; bit[n] += x, n += n&-n);
}

int lookup(unsigned int n)
{
    int res = 0;
    for (; n; res += bit[n], n &= n-1);
    return res;
}

# -------------------------------------------------

(de bt_reset (N)
  (setq
    *BTBitsMask (- (** 2 32) 1)
    *BTBitsCnt N
    *BTBits (need *BTBitsCnt 0) ) )
  
(de bt_update (N X)
  (loop 
    (NIL (<= N *BTBitsCnt))
    (inc (nth *BTBits N) X)
    (setq N (+ N (& N (+ (x| N *BTBitsMask) 1))))
  )
  *BTBits )
  
(de bt_lookup (N)
  (let (Res 0)
    (loop
      (T (< N 1))
      (inc 'Res (car (nth *BTBits N)))
      (setq N (& N (- N 1)))
    )
    Res))
    

# ----------------------------------------------------------------------------------------
(pretty *Tree)
(by cdr sort (idx '*Tree))
(idx '*Tree)
(idx *Tree)
*Tree
(balance *Tree)
(prog (setq *Tree NIL) (for E (split *Bits2 2) (if (lup *Tree E) (inc (cdr (lup *Tree E))) (idx '*Tree (list E 1) T))) (idx '*Tree))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 65536 49)))
*Tree
(balance *Tree)
*Tree
(prog (setq *Tree NIL) (for E (split *Bits2 2) (if (lup *Tree E) (inc (cdr (lup *Tree E))) (idx '*Tree (list E 1) T))) (idx '*Tree))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 2048 49)))
*Tree
    
# ----------------------------------------------------------------------------------------
(de makeitl (L Bits0 Bits1)
   (default Bits0 (list 1))
   (default Bits1 (list 1 0))
   (make
      (for B L
         (if (=1 B)
            (for R Bits1
               (link R) )
            (for R Bits0
               (link R) ) ) ) ) )

: (pp 'makeit)                                                                                                                         
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
-> makeit
: (pp 'crunchit)
(de crunchit (Bits)
   (make
      (while Bits
         (cond
            ((= '(1 1 0) (head 3 Bits))
               (cut 3 'Bits)
               (link 2) )
            ((= '(1 0) (head 2 Bits))
               (cut 2 'Bits)
               (link 0) )
            ((= '(1) (head 1 Bits))
               (cut 1 'Bits)
               (link 1) )
            (T (throw "Data error")) ) ) ) )
-> crunchit

(nil (setq *Bits (makeit 2048 66)))
(nil (setq *Bits2 (crunchit *Bits)))
(prog (setq *Tree NIL) (for E (split *Bits2 2) (if (lup *Tree E) (inc (cdr (lup *Tree E))) (idx '*Tree (list E 1) T))) (idx '*Tree))

# ----------------------------------------------------------------------------------------
: (for H (head 50 *History) (prinl H))                                  
(for H (head 50 *History) (prinl H))
(for L (reverse (by cadr sort *BitsA)) (println L))
(reverse (by cadr sort *BitsA))
(by cadr sort *BitsA)
(setq *BitsA (make (for Bits (uniq *BitsL) (link (lup *BitsT Bits)))))
(make (for Bits (uniq *BitsL) (link (lup *BitsT Bits))))
*BitsL
(idx '*BitsT)
(pp 'parseSyms)
(view (idx '*BitsT))
(idx '*BitsT)
(idx *BitsT)
*BitsT
*BitsL
(for R (sort (uniq (parseSyms *Bits2))) (prinl R))
(edit 'parseSyms)
(for R (sort (uniq (parseSyms *Bits2))) (prinl R))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 65536 51)))
(for R (sort (uniq (parseSyms *Bits2))) (prinl R))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 65536 20)))
(for R (sort (uniq (parseSyms *Bits2))) (prinl R))
(for R (uniq (parseSyms *Bits2)) (prinl R))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 65536 80)))
(for R (uniq (parseSyms *Bits2)) (prinl R))
(for R (parseSyms *Bits2) (prinl R))
(length (fish atom (uniq (sort (parseSyms *Bits2)))))
(length (uniq (sort (parseSyms *Bits2))))
(nil (setq *Bits2 (crunchit *Bits)))
(nil (setq *Bits (makeit 65536 20)))

: (pp 'parseSyms)
(de parseSyms (L)
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
-> parseSyms      

: (pp 'parseSyms)                                                                      
(de parseSyms (L)
   (setq *BitsL NIL  *BitsT NIL  *BitsA NIL  *BitsTS NIL)
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
-> parseSyms

# ----------------------------------------------------------------------------------------

(de crunchit2 (Bits)
   (make
      (while Bits
         (cond
            ((= '(1 0 1) (head 3 Bits))
               (cut 3 'Bits)
               (link 2) )
            (T (link (pop 'Bits)))))))

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
            
# ----------------------------------------------------------------------------------------

: (nil (setq *Bits (makeit (* 65536 16) 51) *Bits2 (crunchit *Bits)) (parseSyms *Bits2))
-> NIL
: (nil (setq *BitsMsg (make (for E *BitsL (link (caddr (lup *BitsTS E)))))))            
-> NIL
: (nil (setq *BitsCMsg (ACDC_Compress *BitsMsg)))                                       
-> NIL
: (/ (length *BitsCMsg) 8)                                                              
-> 131342
: (/ (* 65536 16) 8)                                                                    
-> 131072
: (out "test.bites" (for N *BitsMsg (wr N)))                                            
-> 13

# ----------------------------------------------------------------------------------------

: (nil (setq *Bits (makeit (* 65536 8) 80) *Bits2 (crunchit *Bits)) (parseSyms *Bits2))
-> NIL
: (prog (setq *BitsTS NIL) (for E *BitsA (idx '*BitsTS E T)))                          
-> NIL
: (nil (setq *BitsMsg (make (for E *BitsL (link (caddr (lup *BitsTS E)))))))           
-> NIL
: (nil (setq *BitsCMsg (ACDC_Compress *BitsMsg)))                                      
-> NIL
: (/ (length *BitsCMsg) 8)                                                             
-> 48924
: (out "test.bites" (for N *BitsMsg (wr N)))                                           
-> 1
: (/ (length (fish atom (make (for E *BitsA (link (car E)))))) 8)                      
-> 405

# ----------------------------------------------------------------------------------------
(de parseit (L)
  (make
    (loop
      (cond
        ((= '(1 0) (head 2 L)) (link 1) (cut 2 'L))
        ((= '(0 1) (head 2 L)) (link 0) (cut 2 'L))
        (T (throw "Data Error: Bit")))
      (NIL L)))
)

# --------------
(de fibEncode (L)
  (make 
    (for N (splitit L) 
      (cond 
        ((= 0 N) (link 1 1)) 
        ((= 1 N) (link 0 1 1))
        ((= 2 N) (link 0 0 1 1))
        (T (throw "Data error: fib"))))))
      
# --------------
(de unaryDecode (L Comma)
  (let (N 0)
  (make
    (loop
      (ifn (= Comma (car L))
        (inc 'N)
        (link N)
        (setq N 0))
      (pop 'L)
      (NIL L) ) ) )
)


# --------------

(de splitit (L)
   (make
      (loop
        (cond
          ((= '(0 1) (head 2 L)) (link 1))
          ((= '(1 0) (head 2 L)) (link 2))
          ((= '(0 0) (head 2 L)) (link 0))
          ((= '(1 1) (head 2 L)) (link 0))
          (T (throw (pack "Data error: " (head 2 L)))))
        
        (cut 2 'L)
        (NIL L))))

# --------------

(de makeit (N P Bits0 Bits1)
   (default N 1024)
   (default P 51)
   (default Bits0 (list 0 1))
   (default Bits1 (list 1 0))
   (make
      (do N
         (if (< (rand 1 100) P)
            (for R Bits0
               (link R) )
            (for R Bits1
               (link R) ) ) ) ) )
               
# --------------

(de makeitl (L Bits0 Bits1)
   (default Bits0 (list 0))
   (default Bits1 (list 1))
   (make
      (for B L
         (if (=1 B)
            (for R Bits1
               (link R) )
            (for R Bits0
               (link R) ) ) ) ) )
               
# --------------
(de shearit (L)       
  (let (CL (copy L)) 
    (make 
      (while CL 
        (if (= '(1 0) (head 2 CL)) 
          (prog (link 0) (cut 2 'CL)) 
          (prog (link (car CL)) (cut 1 'CL)) ) ) ) ) )
          
# --------------
(de packit (L)
  (let (CL (copy L) Cnt 0) 
    (make (mapc '((B) 
      (cond 
        ((=0 B) (inc 'Cnt)) 
        (T (link Cnt) 
            (setq Cnt 0) ) ) ) 
      CL ) ) ) )

        
# --------------
(de countit (L)
   (let (M NIL)
      (mapc
         '((V)
            (let
               (Tmp (lup M V)
                  Cur (if Tmp (cadr Tmp) 0) )
               (when Tmp (idx 'M Tmp NIL))
               (setq Tmp (list V (+ 1 Cur)))
               (idx 'M Tmp T) ) )
         L )
      (idx 'M) ) )

# --------------
      
(de crunchit (Bits)
   (make
      (while Bits
         (cond
            ((= '(1 1 0) (head 3 Bits))
               (cut 3 'Bits)
               (link 2) )
            ((= '(1 0) (head 2 Bits))
               (cut 2 'Bits)
               (link 0) )
            ((= '(1) (head 1 Bits))
               (cut 1 'Bits)
               (link 1) )
            (T (throw "Data error")) ) ) ) )

# ===========================================================================

# --------------
(de parseit (L)
  (make
    (loop
      (cond
        ((= '(1 0) (head 2 L)) (link 1) (cut 2 'L))
        ((= '(0 1) (head 2 L)) (link 0) (cut 2 'L))
        (T (throw "Data Error: Bit")))
      (NIL L)))
)

(de fibEncode (L)
  (make 
    (for N (splitit L) 
      (cond 
        ((= 0 N) (link 1 1)) 
        ((= 1 N) (link 0 1 1))
        ((= 2 N) (link 0 0 1 1))
        (T (throw "Data error: fib"))))))
      
# --------------
(de unaryDecode (L Comma)
  (let (N 0)
  (make
    (loop
      (ifn (= Comma (car L))
        (inc 'N)
        (link N)
        (setq N 0))
      (pop 'L)
      (NIL L) ) ) )
)


# --------------

(de splitit (L)
   (make
      (loop
        (cond
          ((= '(0 1) (head 2 L)) (link 1))
          ((= '(1 0) (head 2 L)) (link 2))
          ((= '(0 0) (head 2 L)) (link 0))
          ((= '(1 1) (head 2 L)) (link 0))
          (T (throw (pack "Data error: " (head 2 L)))))
        
        (cut 2 'L)
        (NIL L))))

# --------------

(de makeit (N P Bits0 Bits1)
   (default N 1024)
   (default P 51)
   (default Bits0 (list 0 1))
   (default Bits1 (list 1 0))
   (make
      (do N
         (if (< (rand 1 100) P)
            (for R Bits0
               (link R) )
            (for R Bits1
               (link R) ) ) ) ) )
               
# --------------

(de makeitl (L Bits0 Bits1)
   (default Bits0 (list 0))
   (default Bits1 (list 1))
   (make
      (for B L
         (if (=1 B)
            (for R Bits1
               (link R) )
            (for R Bits0
               (link R) ) ) ) ) )
               
# --------------
(de shearit (L)       
  (let (CL (copy L)) 
    (make 
      (while CL 
        (if (= '(1 0) (head 2 CL)) 
          (prog (link 0) (cut 2 'CL)) 
          (prog (link (car CL)) (cut 1 'CL)) ) ) ) ) )
          
# --------------
(de packit (L)
  (let (CL (copy L) Cnt 0) 
    (make (mapc '((B) 
      (cond 
        ((=0 B) (inc 'Cnt)) 
        (T (link Cnt) 
            (setq Cnt 0) ) ) ) 
      CL ) ) ) )

        
# --------------
(de countit (L)
   (let (M NIL)
      (mapc
         '((V)
            (let
               (Tmp (lup M V)
                  Cur (if Tmp (cadr Tmp) 0) )
               (when Tmp (idx 'M Tmp NIL))
               (setq Tmp (list V (+ 1 Cur)))
               (idx 'M Tmp T) ) )
         L )
      (idx 'M) ) )

# --------------
(de encodeit (L)
  (let (N NIL Q NIL R NIL)
    (make
      (while L
        (setq N (pop 'L))
        (setq Q (/ N 4))
        (setq R (% N 4))
        (do Q (link 1))
        (cond
          ((= 0 R) (link 0 0 0))
          ((= 1 R) (link 0 0 1))
          ((= 2 R) (link 0 1 0))
          ((= 3 R) (link 0 1 1))
          (T (throw "Data error; encodeit"))))))
  )
     
# --------------
: (pp 'parseSyms)

(de parseSyms (L)
   (setq *BitsL NIL  *BitsT NIL  *BitsA NIL  *BitsTS NIL)
   (let (Bits NIL  BitC NIL)
      (setq *BitsL
         (make
            (while L
               (cond
                  ((= 2 (car L))
                     (if (or (not Bits) BitC)
                        (prog (setq BitC T Bits (append Bits '(2))))
                        (prog
                           (setq BitC NIL  Bits (append Bits '(2)))
                           (link Bits)
                           (if (lup *BitsT Bits)
                              (inc (cdr (lup *BitsT Bits)))
                              (idx '*BitsT (list Bits 1) T) )
                           (setq Bits NIL) ) ) )
                  (T
                     (when BitC
                        (link Bits)
                        (if (lup *BitsT Bits)
                           (inc (cdr (lup *BitsT Bits)))
                           (idx '*BitsT (list Bits 1) T) )
                        (setq Bits NIL  BitC NIL) )
                     (setq Bits (append Bits (list (car L)))) ) )
               (pop 'L) )
            (when Bits
               (link Bits)
               (if (lup *BitsT Bits)
                  (inc (cdr (lup *BitsT Bits)))
                  (idx '*BitsT (list Bits 1) T) ) ) ) ) )
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
-> parseSyms

# --------------------------------------------------------------
: (pp 'parseSyms)
(de parseSyms (L)
   (setq *BitsL NIL  *BitsT NIL  *BitsA NIL  *BitsTS NIL)
   (let (Bits NIL  BitC NIL)
      (setq *BitsL
         (make
            (while L
               (if (and Bits (<> BitC (car L)))
                  (prog
                     (setq Bits (append Bits (list (car L))))
                     (link Bits)
                     (if (lup *BitsT Bits)
                        (inc (cdr (lup *BitsT Bits)))
                        (idx '*BitsT (list Bits 1) T) )
                     (setq BitC NIL  Bits NIL) )
                  (prog
                     (setq
                        BitC (car L)
                        Bits (append Bits (list (car L))) ) ) )
               (pop 'L) )
            (when Bits
               (link Bits)
               (if (lup *BitsT Bits)
                  (inc (cdr (lup *BitsT Bits)))
                  (idx '*BitsT (list Bits 1) T) ) ) ) ) )
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
-> parseSyms

# ---------------------------------------------------------------------------------
-> parseSyms
: (pp 'parseSyms)

(de parseSyms (L BitC)
   (default BitC 1)
   (setq *BitsL NIL *BitsT NIL *BitsA NIL *BitsTS NIL)
   (let (Bits NIL)
      (setq *BitsL
         (make
            (while L
               (cond
                  ((<> BitC (car L))
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

-> parseSyms
     