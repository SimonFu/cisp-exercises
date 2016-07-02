#lang sicp

(define (element-of-set? item s)
  (cond ((null? s) false)
        ((= item (car s)) true)
        ((< item (car s)) false)
        (else (element-of-set? item (cdr s)))))

(newline)
(display (element-of-set? 4 '(2 3 4 5 6)))

(define (ajoin-set item s)
  (cond ((null? s) (cons item '()))
        ((= item (car s)) s)
        ((< item (car s)) (cons item s))
        (else (cons (car s)(ajoin-set item (cdr s))))))

(newline)
(display (ajoin-set 2 '(2 3 4 5 6)))