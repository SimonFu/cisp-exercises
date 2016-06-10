#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (subtree s)
  (if (null? s)
      (list nil)
      (let ((rest (subtree (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

(subtree (list 1 2 3))