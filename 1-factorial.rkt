#lang sicp

;(define (factorial n)
;  (if (= n 1)
;      1
;      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (factorial-iter product counter)
    (if (> counter n)
        product
        (factorial-iter (* counter product) (+ counter 1))))
  (factorial-iter 1 1))