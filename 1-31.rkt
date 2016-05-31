#lang sicp

;(define (mul term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* result (term a)))))
;  (iter a 1))

(define (mul term a next b)
  (if (> a b)
      1
      (* (term a) (mul term
                       (+ a 1) next b))))

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