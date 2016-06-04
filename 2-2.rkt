#lang sicp

(define (mk-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mk-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (mid-segment s)
  (let ((start-point (start-segment s))
        (end-point (end-segment s)))
    (mk-point (/ (+ (x-point start-point)
                    (x-point end-point))
                 2)
              (/ (+ (y-point start-point)
                    (y-point end-point))
                 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))