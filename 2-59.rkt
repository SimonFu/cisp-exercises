#lang sicp

(define (element-of-set? item s)
  (cond ((null? s) false)
        ((equal? item (car s)) true)
        (else (element-of-set? item (cdr s)))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (union-set (cdr s1) (cons (car s1) s2)))))

(display (union-set '(a (t r) c t y) '(b c (t r) a d)))