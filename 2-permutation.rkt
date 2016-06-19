#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predict sequence)
  (if (null? sequence)
      nil
      (if (predict (car sequence))
          (cons (car sequence)
                (filter predict (cdr sequence)))
          (filter predict (cdr sequence)))))

(define (remove item s)
  (filter (lambda (x)
            (not (= x item)))
          s))

(define (permutation s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutation (remove x s))))
               s)))

(permutation (list 1 2 3))