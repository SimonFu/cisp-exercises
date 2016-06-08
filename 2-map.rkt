#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items)
  (map (lambda (x) (* x 10)) items))

(define ls (list 1 2 3 4 5 6 7 8 9))
(define sq (list 1 4 9 16 25 36 49 64 81))

(scale-list ls)
(scale-list sq)