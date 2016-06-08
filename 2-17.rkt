#lang sicp

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(define ls (list 1 2 3 4 5 6 7 8 9))
(define sq (list 1 4 9 16 25 36 49 64 81))

(last-pair ls)
(last-pair sq)