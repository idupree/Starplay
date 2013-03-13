(letrec
  (
  ; Miscellaneous little testing
  (curried+ (lambda (x) (lambda (y) (+ x y))))
  (f (lambda (x) (if (= x 4) 1 (* x (- x 1)))))
  (f2 (lambda (x) (+ (f1 x) 1)))
  (f1 (lambda (x) (+ x 1)))
  
  ; Well-known functional-programming functions
  (foldl (lambda (f z seq)
    (letrec
      ((rec (lambda (acc view)
        (if view
          (rec (f acc (table-view-get-value view))
               (table-view-next view))
          acc))))
      (rec z (table-view-min seq)))))
  (map (lambda (f seq)
    (letrec
      ((rec (lambda (view)
              (letrec
                ((updated (table-view-set view
                            (f (table-view-get-value view))))
                 (next (table-view-next updated)))
                (if next (rec next) (table-unview updated))))))
      (rec (table-view-min seq)))))

  ; SKI combinator calculus
  (S (lambda (x y z) (x z (y z))))
  (K (lambda (x y) x))
  (I (lambda (x) x))

  ; factorial defined using letrec's recursion mechanisms
  (factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))

  ; factorial defined using the intrinsic abilities of the
  ; untyped lambda calculus:
  ; Not this way: it only works in non-strictly-evaluated languages:
  ; (fix (lambda (f) (f (fix f))
  ; This way works:
  ; https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator
  ; λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))
  (Y (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))
  (fac (lambda (rec) (lambda (x) (if (= x 0) 1 (* x (rec (- x 1)))))))
  (factorial2 (Y fac))
  )
  (foldl + 0
    (map (lambda (x) (+ 10 x))
      (table-sequence
        (f 4)
        (f2 4)
        ((curried+ 3) 4)
        (factorial 4)
        (factorial2 4))))
  )
