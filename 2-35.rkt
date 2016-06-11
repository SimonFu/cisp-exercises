#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (define (handler x)
                       (cond ((pair? x)
                              (+ (handler (car x))
                                 (handler (cdr x))))
                             ((null? x) 0)
                             (else 1)))
                     (handler x))
                   t)))

(count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7)))
