#lang sicp

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter (term a))
          (combiner (term a)
                    (filtered-accumulate
                     filter
                     combiner
                     null-value
                     term
                     (next a)
                     next b))
          (filtered-accumulate
           filter
           combiner
           null-value
           term
           (next a)
           next b))))


(define (prime? n)
  (define (square x) (* x x))
  (define (divisor? n test-divisor)
    (= (remainder n test-divisor) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divisor? n test-divisor) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (inc x)
  (+ x 1))

(define (integer x)
  x)

(define (sum-prime a b)
  (filtered-accumulate prime? + 0 integer a inc b))