#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter items counter)
    (if (null? items)
        counter
        (length-iter (cdr items) (+ counter 1))))
  (length-iter items 0))

(define (append dst src)
  (if (null? dst)
      src
      (cons (car dst) (append (cdr dst) src))))

(define ls (list 1 2 3 4 5 6 7 8 9))
(define sq (list 1 4 9 16 25 36 49 64 81))

(list-ref ls 4)
(list-ref ls 8)
(list-ref ls 2)

(length ls)

(append ls sq)
(append sq ls)