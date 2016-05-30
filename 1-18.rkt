#lang sicp

(define (fast_mul a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))

  (define (fast_mul_iter val a b)
    (cond ((= b 0) val)
        ((even? b) (fast_mul_iter val (double a) (halve b)))
        (else (fast_mul_iter (+ val a) a (- b 1)))))
  (fast_mul_iter 0 a b))