#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (* x x))) tree))

;(define (square-tree tree)
;  (cond ((null? tree) tree)
;        ((not (pair? tree)) (* tree tree))
;        (else (cons (square-tree (car tree))
;                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))