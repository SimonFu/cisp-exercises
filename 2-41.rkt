#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predict sequence)
  (if (null? sequence)
      nil
      (if (predict (car sequence))
          (cons (car sequence)
                (filter predict (cdr sequence)))
          (filter predict (cdr sequence)))))

(define (triple n s)
  (filter (lambda (x)
            (= (+ (car x)
                  (cadr x)
                  (cadr (cdr x)))
               s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(triple 15 20)