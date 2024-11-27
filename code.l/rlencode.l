  (for XY (head 1 *Plot) 
   (let (PX (+ 1 (format (round (car XY) 0)))
       PY (+ 1 (format (round (cadr XY) 0))))
   (get (nth *Img PX PY)) ))
 
(de consecDups (Lst)
  (make
    (let Last NIL
      (for X Lst
        (if (= X (car Last))
          (conc Last (cons X))
          (link (setq Last (cons X))) ) ) ) ) )

(de rlencode (Lst)
  (mapcar
    '((X) (list (length X) (car X)))
    (consecDups Lst) ) )

(de rlencodem (Lst)
  (mapcar
    '((X)
        (if (cdr X)
          (list (length X) (car X))
          (car X) ) )
    (consecDups Lst) ) )

# Fibonacci Code
(idx 'M  '(1    "0000"  "11")  T)
(idx 'M  '(2    "0001"  "011")  T)
(idx 'M  '(3    "0011"  "0011")  T)
(idx 'M  '(5    "0100"  "1011")  T)
(idx 'M  '(7    "0101"  "00011")  T)
(idx 'M  '(9    "0110"  "10011")  T)
(idx 'M  '(11   "0111"  "01011")  T)
(idx 'M  '(13   "1000"  "000011")  T)
(idx 'M  '(15   "1001"  "100011")  T)
(idx 'M  '(17   "1010"  "010011")  T)
(idx 'M  '(19   "1011"  "001011")  T)
(idx 'M  '(21   "1100"  "101011")  T)
(idx 'M  '(23   "1101"  "0000011")  T)
(idx 'M  '(25   "1110"  "1000011")  T)
(idx 'M  '(27   "1111"  "0100011")  T)

# Exponential Golomb Code
(idx 'M '(0 "000") T)
(idx 'M '(1 "001") T)
(idx 'M '(2 "010") T)
(idx 'M '(3 "011") T)
(idx 'M '(4 "1000") T)
(idx 'M '(5 "1001") T)
(idx 'M '(6 "1010") T)
(idx 'M '(7 "1011") T)
(idx 'M '(8 "110000") T)
(idx 'M '(9 "110001") T)
(idx 'M '(10 "110010") T)
(idx 'M '(11 "110011") T)
(idx 'M '(12 "110100") T)
(idx 'M '(13 "110101") T)

