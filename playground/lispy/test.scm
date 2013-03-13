(letrec
  ((S (lambda (x y z) (x z (y z))))
   (K (lambda (x y) x))
   (I (lambda (x) x))
   ; hack since + etc isn't implemented yet:
   (+ 0) (* 0) (= 0) (- 0)
   (factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))
  )
  (I I))
