#lang sicp

(define (make-monitored func)
  (let ((init 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) init)
            ((eq? x 'reset-count) (set! init 0))
            (else (begin (set! init (+ init 1))
                         (func x)))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 4)
(s 'how-many-calls?)
(s 'reset-count)
(s 4)
(s 'how-many-calls?)