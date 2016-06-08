#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (* (car items) (car items))
;            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define ls (list 1 2 3 4 5 6 7 8 9))

(square-list ls)