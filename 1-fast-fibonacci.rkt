#lang sicp

(define (fast_exp x n)
  (define (even? x)
    (= (remainder x 2) 0))
  
  (define (square x)
    (* x x))
  
  (cond ((= n 1) x)
        ((even? n) (square (fast_exp x (/ n 2))))
        (else (* x (fast_exp x (- n 1))))))