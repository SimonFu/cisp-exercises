#lang sicp

(define (for-each proc items)
  (if (null? items)
      ((lambda ()
         (newline)
         true))
      ((lambda (proc items)
         (proc (car items))
         (for-each proc (cdr items))) proc items)))

(define ls (list 1 2 3 4 5 6 7 8 9))

(for-each (lambda (x)
            (newline)
            (display x)) ls)