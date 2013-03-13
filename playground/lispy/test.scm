(letrec  ;or let* for the Y stuff
  ((S (lambda (x y z) (x z (y z))))
   (K (lambda (x y) x))
   (I (lambda (x) x))
   ; hack since + etc isn't implemented yet:
   (+ 0) (* 0) (= 0) (- 0)
   (factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))
   ;(fix (lambda (f) (f (fix f))
   ;λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))
   ;https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator
   (Y (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))
   (fac (lambda (rec) (lambda (x) (if (= x 0) 1 (* x (rec (- x 1)))))))
   (factorial2 (Y fac))
  )
  ;(I I)
  (factorial2 4)
  )
