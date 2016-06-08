#lang sicp

(define (reverse items)
  (define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))
  
  (define (rm-last items)
    (if (null? (cdr items))
               nil
               (cons (car items) (rm-last (cdr items)))))
  
  (if (null? items)
      nil
      (cons (last-pair items) (reverse (rm-last items)))))

(define ls (list 1 2 3 4 5 6 7 8 9))
(define sq (list 1 4 9 16 25 36 49 64 81))

(reverse ls)
(reverse sq)