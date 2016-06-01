#lang sicp

(define (search f neg-pt pos-pt)
  (define (positive? x) (> x 0))
  (define (negative? x) (< x 0))
  (define (abs x)
    (if (< x 0)
        (- x)
        x))
  (define (close-enough? x y)
    (< (abs (- x y)) 0.0001))
  (define (average x y)
    (/ (+ x y) 2))
  (let ((midpt (average neg-pt pos-pt)))
    (if (close-enough? neg-pt pos-pt)
        midpt
        (let ((test-value (f midpt)))
              (cond ((positive? test-value)
                     (search f neg-pt midpt))
                    ((negative? test-value)
                     (search f midpt pos-pt))
                    (else midpt))))))

(search (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

