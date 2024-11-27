: (pp 'gosperGen)
(de gosperGen (D)
   (if (=T D)
      (co 'gospergen)
      (co 'gospergen
         (let
            (A (chop "A-B--B+A++AA+B-")
               B (chop "+A-BB--B-A++A+B")
               _MaxDepth D
               _Plot '((L) (mapc yield L))
               _Fn
               '((X)
                  (cond
                     ((= X "A") A)
                     ((= X "B") B)
                     (T (list X)) ) )
               _L-Run
               '((L D)
                  (if (>= D _MaxDepth)
                     (_Plot L)
                     (mapc
                        '((X) (_L-Run (_Fn X) (inc D)))
                        L ) ) ) )
            (default _MaxDepth 13)
            (_L-Run A 1) ) ) ) )
-> gosperGen

: (pp 'gencoder)
(de gencoder (Bits)
   (use (Bit B C)
      (while (setq Bit (car Bits))
         (setq B NIL)
         (do T
            (setq C (gosperGen))
            (setq B
               (cond
                  ((and (= C "A")) (if (= Bit 0) "A" "B"))
                  ((and (= C "B")) (if (= Bit 0) "B" "A"))
                  ((and (= C "+")) (if (= Bit 0) "+" "-"))
                  ((and (= C "-")) (if (= Bit 0) "-" "+"))
                  (T (throw "Data error") ) ) )
            (T B) )
         (prin B)
         (setq Bits (cdr Bits)) ) ) )
-> gencoder




**** JUNK? ****
gencoder ((Bits)
   (use (Bit B C)
      (gosperGen T)
      (while (setq Bit (car Bits))
         (setq B NIL)
         (do T
            (setq C (gosperGen))
            (when (or (= C "-") (= C "+"))
               (setq B
                  (cond
                     ((and (= C "-") (= Bit 1)) "-")
                     ((and (= C "+") (= Bit 1)) "+")
                     (T C) ) ) )
            (T B) )
         (prin B)
         (setq Bits (cdr Bits)) ) ) )

(=======)
