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
      (cons (if (pair? (last-pair items))
                (reverse (last-pair items))
                (last-pair items)) (reverse (rm-last items)))))

(list (list 1 2) (list 3 4))
(reverse (list (list 1 2) (list 3 4)))