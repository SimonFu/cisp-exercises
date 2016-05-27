#lang sicp

(define (cube-root x)
  (define (abs x)
  (if (< x 0)
      (- x)
      x))
  
  (define (square x) (* x x))

  (define (good-enough? guess x)
    (< (/ (abs (- (improve guess x)
              guess))
        guess)
     0.00001))
  
  (define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))

  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  
  (cube-root-iter 1.0 x))