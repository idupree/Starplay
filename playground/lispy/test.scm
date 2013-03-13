(letrec
  ((S (lambda (x y z) (x z (y z))))
   (K (lambda (x y) x))
   (I (lambda (x) x)))
  ; this is a comment
  (I I))
