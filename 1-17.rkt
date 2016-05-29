#lang sicp

(define (fast_exp x n)
  (define (even? x)
    (= (remainder x 2) 0))
  
  (define (square x)
    (* x x))
  
  (define (exp_iter val x n)
    (cond ((= n 1) (* val x))
          ((even? n) (exp_iter val (square x) (/ n 2)))
          (else (exp_iter (* val x) x (- n 1)))))
  
  (exp_iter 1 x n))