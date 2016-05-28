#lang sicp

;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (fib (- n 1))
;                 (fib (- n 2))))))

(define (fib n)
  (define (fib_iter a b counter)
    (if (= counter 0)
        a
        (fib_iter b (+ a b) (- counter 1))))
  (fib_iter 0 1 n))