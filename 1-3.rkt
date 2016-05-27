#lang sicp

(define (sum-max x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((and (< y x) (< y z)) (+ x z))
        ((and (< z x) (< z y)) (+ x y))
        (else (+ x y))))
