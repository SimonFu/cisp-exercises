#lang sicp

(define (exchange-money total items)
  (cond ((< total 0) 0)
        ((= total 0) 1)
        ((null? items) 0)
        (else (+
               (exchange-money (- total (car items)) items)
               (exchange-money total (cdr items))))))

(exchange-money 100 (list 100 50 20 10 5 1 0.5 0.1))