#lang sicp

(define (kass-triangle p n)
  (define (kass-triangle-iter x layer)
    (cond ((= x 1) 1)
          ((= x layer) 1)
          (else (+ (kass-triangle-iter (- x 1) (- layer 1))
                   (kass-triangle-iter x (- layer 1))))))
  (kass-triangle-iter p n))