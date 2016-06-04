#lang sicp

(define (mk-rat n d)
  (cond ((< (* n d) 0) (if (< n 0)
                           (cons n d)
                           (cons (- n) (- d))))
        (else (if (< n 0)
                   (cons (- n) (- d))
                   (cons n d)))))