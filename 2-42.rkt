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

(define (empty-board)
  (list nil))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (safe? k positions)
  (define (element n l)
    (if (= (- n 1) 0)
        (car l)
        (element (- n 1) (cdr l))))
  (define (iter row col k)
    (if (= k 0)
        true
        (and (not (= row (element k positions)))
             (not (= (abs (- k col)) (abs (- (element k positions) row))))
             (iter row col (- k 1)))))

  (if (null? positions)
      true
      (iter (element k positions) k (- k 1))))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list nil)
        (filter (lambda (positions)
                  (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)