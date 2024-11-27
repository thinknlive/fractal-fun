(out "hilRC.pbm" (prinl "P1") (prinl (length *ImgRC) " " (length (car *ImgRC))) (mapc prinl *ImgRC))
(out "hilRC.bits" (for X *ImgRC (mapc prin X)))

# From top.left corner
# http://stackoverflow.com/questions/1779199/traverse-matrix-in-diagonal-strips
(de slicediag (L)
	(let (N (length L) S 0 Z NIL J NIL)
		(make
			(loop 
				(NIL (< S (- (* 2 N) 1)))
				(link
					(make
						(setq Z (if (< S N) 0 (- S N -1))
									J Z)
						(loop
							(NIL (<= J (- S Z))) 
							(link (car (nth L (+ 1 J) (+ 1 (- S J)))))
							(inc 'J) ) ) )
				(inc 'S) ) ) )
)

# From top.right corner
# http://stackoverflow.com/questions/1779199/traverse-matrix-in-diagonal-strips
(de slicediag (L)
	(let (N (length L) S 0 Z NIL J NIL)
		(make
			(loop 
				(NIL (< S (- (* 2 N) 1)))
				(link
					(make
						(setq Z (if (< S N) 0 (- S N -1))
									J Z)
						(loop
							(NIL (<= J (- S Z))) 
							(link (car (nth L (+ 1 J) (+ 1 (- (- N 1) (- S J))))))
							(inc 'J) ) ) )
				(inc 'S) ) ) )
)
			
						
						
(de slicendiceRC (L N)
		(default N 2)
		(let (CntY 0 CntX 0 NY 1 NX 1)
			(make 	 
				(do (* N N)
					(link
						(make
							(for R L
								(when (= CntY (% NY N))
									 (link
											(make
												 (for C R
														(when (= CntX (% NX N)) (link C))
														(inc 'NX) ) ) ) )
								(setq NX 1) 
								(inc 'NY)
							) 
						) 
					)
					(setq NY 1)
					(inc 'CntX)
					(setq CntX (% CntX N))
					(when (=0 CntX) (inc 'CntY))
					(setq CntY (% CntY N))
				)
			)				
		)
	)

(de crunchit (L)
		(let (Cnt 1 FileBits NIL FilePbm NIL)
			(for P L
				(setq FileBits (pack "hil0" Cnt ".bits")
							FilePbm  (pack "hil0" Cnt ".pbm"))
				(out FilePbm
					(prinl "P1")
					(prinl (length P) " " (length (car P)))
					(mapc prinl P) )
				(out FileBits (for X P (mapc prin X)))
				(inc 'Cnt)
			)))

(de sliceRC (L N)
	(default N 4)
	(let (CntY 1 CntX 1)
		(make 	 
			(for R L
				(if (gt0 (% CntY N))
					(link
						(make
							(for C R
								(if (gt0 (% CntX N)) (link C))
								(inc 'CntX) 
							) ) ) 
				)
				(setq CntX 1) 
				(inc 'CntY)
			) 
		) 
	)
)

(de sliceR (L N M)
	(default N 0)
	(default M 0)
	(let (CntY 1)
		(make 	 
			(for R L
				(if (= M (% CntY N))
					(link R))
				(inc 'CntY)
			) 
		) 
	)
)

(de sliceC (L N M)
	(default N 0)
	(default M 0)
	(let (CntX 1)
		(make
			(for R L
				(link
					(make
						(setq CntX 1)
						(for C R
							(if (= M (% CntX N))
								(link C))
							(inc 'CntX)
						)
					)
				)
			)
		)
	)
)

(de resetsrc ()
	(setq *BitSrc NIL *BitCnt 0)
)

(de srcit () 
	(when (not *BitSrc)
		(setq *BitSrc (if (=0 (rand 0 1)) (list 1 0 1) (list 1 0 1 0))))
	(inc '*BitCnt)
	(pop '*BitSrc)
)

(out "hil01SD.pbm" (prinl "P1") (prinl (length (car *hil01SD)) " " (length *hil01SD)) (mapc prinl *hil01SD))
(out "hil01SD.bits" (for X *hil01SD (mapc prin X)))

: (for R (reverse (head 8 (nth *History 2))) (prinl R))
(vi 'plotit)
(nil (ld) (setq *Img (plotit 512 0 0 '(() (rand 0 1)))))
(nil (setq *ImgSD (slicendiceRC *Img 2)) (crunchit *ImgSD))
(nil (setq *ImgRC (sliceRC *Img 4)))
(out "hilRC.pbm" (prinl "P1") (prinl (length (car *ImgRC)) " " (length *ImgRC)) (mapc prinl *ImgRC))
(out "hilRC.bits" (for X *ImgRC (mapc prin X)))
(head 8 *History)
(for R (head 8 (nth *History 2)) (prinl R))
			
: (nil (ld) (setq *Img (plotit 512 0 0 '(() (if (< (rand 1 100) 51) 0 1)) )))                         

(de sliceit ()
 (nil (setq *ImgSD (slicendiceRC *Img 2)) (crunchit *ImgSD))                                         
 (nil (setq *ImgRC (sliceRC *Img 4)))                                               
 (nil (out "hilRC.bits" (for X *ImgRC (mapc prin X))))
 (nil (out "hilRC.pbm" (prinl "P1") (prinl (length (car *ImgRC)) " " (length *ImgRC)) (mapc prinl *ImgRC)))
)

(nil (setq *Img01SD (sliceRC (car *ImgSD 2))))                                               
			