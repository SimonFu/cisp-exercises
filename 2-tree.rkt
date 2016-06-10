#lang sicp

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
  
  (define tree (cons (list 1 2) (list 3 4)))
  
  (count-leaves tree)

(count-leaves (list tree tree))
(count-leaves (list))