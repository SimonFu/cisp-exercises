#lang sicp

;(define (exp x n)
;  (if (= n 0)
;      1
;      (* x (exp x (- n 1)))))

(define (exp x n)
  (define (exp_iter val counter)
         (if (= counter 0)
             val
             (exp_iter (* x val) (- counter 1))))
  (exp_iter 1 n))