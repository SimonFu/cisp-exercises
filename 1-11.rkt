#lang sicp

; Recursion implement
;(define (f n)
;  (if (< n 3)
;      n
;      (+ (f (- n 1))
;         (* 2 (f (- n 2)))
;         (* 3 (f (- n 3))))))

(define (f n)
  (define (f_iter a b c counter)
    (if (= counter 0)
        a
        (f_iter b c (+ c
                       (* 2 b)
                       (* 3 a))
                (- counter 1))))
  (f_iter 0 1 2 n))