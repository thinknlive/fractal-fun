Simple compressor:
Block scan: 256 bytes

00 0111
11 1011
01 1101
10 1110

Up to 256 bytes
Count bit pairs in block, allocate by frequency
Write two bytes, one byte is length, one byte is the frequency allocation





# -------------------------------------------------------------------
(de jzenc (File)
   (let
      (NipCnts
         (list
            (0 (0) NIL)
            (1 (1 1 0) NIL)
            (2 (1 0) NIL)
            (3 (1 1 1 0) NIL) )
         Block NIL
         BlockOut NIL 
				 BlockCopy NIL
         Byte NIL
         Bits NIL
         Bit0 NIL
         Bit1 NIL
         Nip NIL
         Nip00 '(0)
         Nip01 '(1 0)
         Nip10 '(1 1 0)
         Nil11 '(1 1 1 0)
         ByteCnt NIL
         BlockCnt 0
         SizeIn 0
         SizeOut 0)
      (out (pack File ".jz")
         (in File
            (loop
               (setq ByteCnt 0)
               (set (cdar (nth NipCnts 1)) 0)
               (set (cdar (nth NipCnts 2)) 0)
               (set (cdar (nth NipCnts 3)) 0)
               (set (cdar (nth NipCnts 4)) 0)
               (setq Block
                  (make
                     (while
                        (and
                           (setq Byte (rd 1))
                           (gt0 (% (inc 'ByteCnt) 255)) )
                        (setq Bits (byte2bin Byte '(T . T)))
                        (for B Bits
                           (link B) )
                        (loop
                           (NIL (car Bits))
                           (setq
                              Bit0 (pop 'Bits)
                              Bit1 (pop 'Bits)
                              Nip (+ Bit0 (* 2 Bit1)) )
                           (cond
                              ((= 0 Nip) (inc (cdar (nth NipCnts 1))))
                              ((= 1 Nip) (inc (cdar (nth NipCnts 2))))
                              ((= 2 Nip) (inc (cdar (nth NipCnts 3))))
                              ((= 3 Nip) (inc (cdar (nth NipCnts 4))))
                              (T (throw "Data error: Nip")) ) ) ) ) )
               (inc 'BlockCnt)
               (when (gt0 (length Block))
                  (setq NipCnts (reverse (by cadr sort NipCnts)))
                  (msg "BlockCnt: " BlockCnt "; ByteCnt: " ByteCnt "; NipCnts: " (glue "," NipCnts))
                  (set (cdar (nth NipCnts 1)) '(0))
                  (set (cdar (nth NipCnts 2)) '(1 0))
                  (set (cdar (nth NipCnts 3)) '(1 1 0))
                  (set (cdar (nth NipCnts 4))
                     '(1 1 1 0) )
                  (msg (text "Length IN: @1" (length Block)))
                  (setq SizeIn (byte2bin (/ (length Block) 8) '(T . T)))
									(when (or (> (/ (length Block) 8) 255) (gt0 (% (length Block) 8)) )
										(throw "Data error: SizeOut") )
									(setq BlockCopy (copy Block))
                  (setq BlockOut
                     (make
                        (loop
                           (NIL Block)
                           (setq
                              Bit0 (++ Block)
                              Bit1 (++ Block)
                              Bits (+ Bit0 (* 2 Bit1)) )
                           (setq Nip
                              (find '((B) (= Bits (car B))) NipCnts) )
                           (for B (cadr Nip)
                              (link B) ) ) ) )
                  (msg (text "Length OUT: @1" (length BlockOut)))
									(setq SizeOut (length BlockOut))
									(if (> SizeOut (length BlockCopy))
										(prog
											(msg (text "---> No compression"))
											(setq Nip (byte2bin 0 '(T . T)))
											(prin SizeIn)
											(prin Nip)
											(prin BlockCopy) 
										)
										(prog
											(setq Nip
												 (make
														(for N NipCnts
															 (cond
																	((= 0 (car N))
																		 (for B '(0 0)
																				(link B) ) )
																	((= 1 (car N))
																		 (for B '(0 1)
																				(link B) ) )
																	((= 2 (car N))
																		 (for B '(1 0)
																				(link B) ) )
																	((= 3 (car N))
																		 (for B '(1 1)
																				(link B) ) )
																	(T (throw "Data Error: Nip map")) ) ) ) )
											(prin SizeIn)
											(prin Nip)
											(prin BlockOut) 
										) 
									)
								)
								(NIL Byte) ) ) ) ) )

(de jzdec (File)
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


(de bin2byte (L)
	(let (V 0  S 1)
		(mapcar
			 '((X)
					(setq
						 V (+ V (* X S))
						 S (* S 2) ) )
			 (reverse L) )
		V ) )
