#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predict sequence)
  (if (null? sequence)
      nil
      (if (predict (car sequence))
          (cons (car sequence)
                (filter predict (cdr sequence)))
          (filter predict (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-square tree)
  (accumulate +
              0
              (map (lambda (x) (* x x))
                   (filter odd?
                      (enumerate-tree tree)))))
              

(sum-odd-square (list 1 (list 2 (list 3 4)) 5))