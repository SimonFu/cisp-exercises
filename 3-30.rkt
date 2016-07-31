#lang sicp

(define (ripple-carry-adder a b s c-out)
  (define (inter a b c-in s)
    (let ((sum 0)
          (carry 0))
      (cond ((null? a) (set-signal! c-out c-in))
            (else (begin (full-adder a b c-in sum carry)
                         (set-signal! (car s) sum)
                         (iter (cdr a) (cdr b) carry (cdr s)))))))
  (iter a b 0 s))