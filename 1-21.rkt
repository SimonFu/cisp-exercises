#lang sicp

(define (square x) (* x x))

(define (divisor? n test-divisor)
  (= (remainder n test-divisor) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)