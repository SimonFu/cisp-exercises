#lang sicp

(define x1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x1)))))

(define x2 (list (list 7)))
(car (car x2))

(define x3 (list 1 2 3 4 5 6 7))
(car (cdr (cdr (cdr (cdr (cdr (cdr x3)))))))