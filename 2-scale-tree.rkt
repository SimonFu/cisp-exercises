#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;(define (scale-tree tree factor)
;  (cond ((null? tree) tree)
;        ((not (pair? tree)) (* tree factor))
;        (else (cons (scale-tree (car tree) factor)
;                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (x)
         (if (pair? x)
             (scale-tree x factor)
             (* factor x))) tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)