#lang sicp

(define (fast_mul a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  (cond ((= b 0) 0)
        ((even? b) (fast_mul (double a) (halve b)))
        (else (+ (fast_mul a (- b 1)) a))))