#lang sicp

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay (+ and-gate-delay (* inverter-delay 2))
                   (lambda ()
                     (set-signal! output new-value)))))
  (define (logical-or a1 a2)
    (let ((inverter-a1 0)
          (inverter-a2 0)
          (and-a1-a2 0)
          (or-output 0))
      (inverter a1 inverter-a1)
      (inverter a2 inverter-a2)
      (and-gate inverter-a1 inverter-a2 and-a1-a2)
      (inverter and-a1-a2 or-output)
      or-output)
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)