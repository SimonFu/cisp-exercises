#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate
                 combiner
                 null-value
                 term
                 (next a)
                 next b))))

(define (mul term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (inc x)
  (+ x 1))

(define (integer x)
  x)
(define (pi-term x)
  (define (even? x)
    (= (remainder x 2) 0))
  (if (even? x)
      (/ x (+ x 1))
      (/ (+ x 1) x)))