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
  
  (define (call-the-cops)
    (lambda (x) (display "Somebody steals my money!\n")))
  
  (let ((wrong-times 0))
    (lambda (pw m)
      (if (eq? pw password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else "Wrong mothed"))
          (if (>= wrong-times 7)
              (call-the-cops)
              (lambda (x) (begin (set! wrong-times (+ wrong-times 1))
                                 (display "Incorrect password\n"))))))))

(define acc (make-account 100 '1234))
((acc '1234 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)
((acc '4321 'withdraw) 40)