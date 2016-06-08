#lang sicp

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (define (car-iter z counter)
    (if (> (remainder z 2) 0)
        counter
        (car-iter (/ z 2) (+ counter 1))))
  (car-iter z 0))

(define (cdr z)
  (define (car-iter z counter)
    (if (> (remainder z 3) 0)
        counter
        (car-iter (/ z 3) (+ counter 1))))
  (car-iter z 0))

(define z (cons 5 6))
(car z)
(cdr z)