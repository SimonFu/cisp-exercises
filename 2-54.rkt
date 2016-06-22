#lang sicp

(define (equal? x y)
  (cond ((and (null? x) (null? y)) true)
        ((or (null? x) (null? y)) false)
        ((and (pair? (car x)) (pair? (car y)))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((and (not (pair? (car x))) (not (pair? (car y))))
         (and (eq? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        (else false)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))