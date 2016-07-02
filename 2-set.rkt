#lang sicp

(define (element-of-set? item s)
  (cond ((null? s) false)
        ((equal? item (car s)) true)
        (else (element-of-set? item (cdr s)))))

(display (element-of-set? 'a '(b c (c a) d)))

(define (ajoin-set item s)
  (if (element-of-set? item s)
      s
      (cons item s)))

(newline)
(display (ajoin-set 'a '(b c a d)))

(define (intersection-set s1 s2)
  (cond ((null? s1) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(newline)
(display (intersection-set '(a (t r) c) '(b c (t r) a d)))