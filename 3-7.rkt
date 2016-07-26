#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (> amount balance)
        "Not enough money!"
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
               balance))
  
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else "Wrong mothed"))
        (lambda (x) (display "Incorrect password\n"))))
  dispatch)

(define (make-joint acc acc-pw password)
  (define (withdraw amount)
    ((acc acc-pw 'withdraw) amount))
  (define (deposit amount)
    ((acc acc-pw 'deposit) amount))
  
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else "Wrong mothed"))
        (lambda (x) (display "Incorrect password\n"))))
  dispatch)

(define peter-acc (make-account 100 '1234))
(define paul-acc (make-joint peter-acc '1234 '4321))
((paul-acc '4321 'withdraw) 40)
((peter-acc '1234 'withdraw) 10)
((paul-acc '4321 'withdraw) 0)