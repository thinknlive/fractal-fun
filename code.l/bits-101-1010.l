(length *PBG)
(setq *PBG (make (for N *PBNE (for B (chop (cadr (lup M N))) (link (format B))))))
(make (for N *PBNE (for B (chop (cadr (lup M N))) (link B))))
(for N *PBNE (prin (cadr (lup M N))))
(lup M 12)
(lup 'M 12)
(idx 'M '(13 "110101") T)
(idx 'M '(12 "110100") T)
(idx 'M '(11 "110011") T)
(idx 'M '(10 "110010") T)
(idx 'M '(9 "110001") T)
(idx 'M '(8 "110000") T)
(idx 'M '(7 "1011") T)
(idx 'M '(6 "1010") T)
(idx 'M '(5 "1001") T)
(idx 'M '(4 "1000") T)
(idx 'M '(3 "011") T)
(idx 'M '(2 "010") T)
(idx 'M '(1 "001") T)
(idx 'M '(0 "000") T)
(off M)

(let (Cnts (need 16 0)) (mapc '((V) (inc (nth Cnts (dec V)))) *PBNE) Cnts)
(let (Cnts (need 16 0)) (mapc '((V) (inc (car (nth Cnts (dec V))))) *PBNE) Cnts)
(need 16 0)
(need 16)
(sort (uniq *PBNE))
(pp 'bin2byte)
(setq *PBNE (make (mapc '((V) (link (/ V 2))) *PBN)))
(sort (uniq (make (mapc '((V) (link (/ V 2))) *PBN)))
(make (mapc '((V) (link (/ V 2))) *PBN))
(sort (uniq (let (PBB (copy *PBB) Cnt 0) (make (mapc '((B) (cond ((=0 B) (inc 'Cnt)) (T (link Cnt) (setq Cnt 0)))) PBB)))))

(length *PBN)
(setq *PBN (let (PBB (copy *PBB) Cnt 0) (make (mapc '((B) (cond ((=0 B) (inc 'Cnt)) (T (link Cnt) (setq Cnt 0)))) PBB))))
(for L (head 64 *History) (prinl L))
(sort (uniq (let (PBB (copy *PBB) Cnt 0) (make (mapc '((B) (cond ((=0 B) (inc 'Cnt)) (T (link Cnt) (setq Cnt 0)))) PBB)))))
(uniq (let (PBB (copy *PBB) Cnt 0) (make (mapc '((B) (cond ((=0 B) (inc 'Cnt)) (T (link Cnt) (setq Cnt 0)))) PBB))))
(let (PBB (copy *PBB) Cnt 0) (make (mapc '((B) (cond ((=0 B) (inc 'Cnt)) (T (link Cnt) (setq Cnt 0)))) PBB)))
(head 32 *PBB)
(head 32 *ImgPBB)
(out "hilPB.bits" (prin *PBB))
(head 128 *PBB)
(length *PBB)
(setq *PBB (let (ImgPB (copy *ImgPB)) (make (while ImgPB (if (= '(1 0) (head 2 ImgPB)) (prog (link 0) (cut 2 'ImgPB)) (prog (link (car ImgPB)) (cut 1 'ImgPB)))))))
(length *ImgPB)
(out "hilPB.bits" (prin *ImgPB))

101
1010 101 1010

0000 101101101101 -- 101101101101 -- 01010101 -- 101101101101
1111 1010101010101010 -- 10 10 10 10 10 10 10 10 -- 00000000
1010 10101011010101
1011 101010110101010
0101 10110101011010

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
			