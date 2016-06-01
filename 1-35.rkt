#lang sicp

(define (fix-point f first-guess)
  (define (abs x)
    (if (< x 0)
        (- x)
        x))
  
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0)