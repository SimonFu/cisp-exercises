#lang sicp

(define (exchange-money total items)
  (cond ((< total 0) 0)
        ((= total 0) 1)
        ((null? items) 0)
        (else (+
               (exchange-money (- total (car items)) items)
               (exchange-money total (cdr items))))))

(exchange-money 10000 (list 10000 5000 2000 1000 500 100 50 10))