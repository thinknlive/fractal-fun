(de encodeMFT (L)
   (let (Table (make (for N 256 (link (dec N)))))
      (mapcar
         '((C)
            (dec
               (prog1
                  (index C Table)
                  (rot Table @) ) ) )
         L ) ) )
 
(de decodeMFT (L)
   (let (Table (make (for N 256 (link (dec N)))))
			(mapcar
				'((N)
					 (prog1
							(get Table (inc 'N))
							(rot Table N) ) )
				L ) ) ) 